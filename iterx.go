package iterx // import "code.nkcmr.net/iterx"

import (
	"cmp"
	"iter"
	"slices"
)

func Map11[A, B any](in iter.Seq[A], mapfn func(A) B) iter.Seq[B] {
	return func(yield func(B) bool) {
		for a := range in {
			if !yield(mapfn(a)) {
				break
			}
		}
	}
}

func Map12[A, B, C any](in iter.Seq[A], mapfn func(A) (B, C)) iter.Seq2[B, C] {
	return func(yield func(B, C) bool) {
		for a := range in {
			if !yield(mapfn(a)) {
				break
			}
		}
	}
}

func Map21[A, B, C any](in iter.Seq2[A, B], mapfn func(A, B) C) iter.Seq[C] {
	return func(yield func(C) bool) {
		for a, b := range in {
			if !yield(mapfn(a, b)) {
				break
			}
		}
	}
}

func Map22[A, B, C, D any](in iter.Seq2[A, B], mapfn func(A, B) (C, D)) iter.Seq2[C, D] {
	return func(yield func(C, D) bool) {
		for a, b := range in {
			if !yield(mapfn(a, b)) {
				break
			}
		}
	}
}

func StopOnError[V any](erriter func(yield func(V) bool) error) iter.Seq2[V, error] {
	return func(yield func(V, error) bool) {
		err := erriter(func(v V) bool {
			return yield(v, nil)
		})
		if err != nil {
			_ = yield(zero[V](), err)
			return
		}
	}
}

func HandleError[V, R any](erriter iter.Seq2[V, error], fn func(iter.Seq[V]) R) (r R, err error) {
	r = fn(func(yield func(V) bool) {
		for v, iterErr := range erriter {
			if iterErr != nil {
				err = iterErr
				break
			}
			if !yield(v) {
				break
			}
		}
	})
	return
}

func SliceCollectError[E any](seq iter.Seq2[E, error]) ([]E, error) {
	return HandleError(seq, slices.Collect)
}

func SliceSortedError[E cmp.Ordered](seq iter.Seq2[E, error]) ([]E, error) {
	return HandleError(seq, slices.Sorted)
}

func SliceSortedFuncError[E cmp.Ordered](seq iter.Seq2[E, error], cmp func(E, E) int) ([]E, error) {
	return HandleError(seq, func(s iter.Seq[E]) []E {
		return slices.SortedFunc(s, cmp)
	})
}

func Filter[V any](in iter.Seq[V], filterfn func(V) bool) iter.Seq[V] {
	return func(yield func(V) bool) {
		for v := range in {
			if !filterfn(v) {
				continue
			}
			if !yield(v) {
				break
			}
		}
	}
}

func FilterErr[V any](in iter.Seq[V], filterfn func(V) (bool, error)) iter.Seq2[V, error] {
	return StopOnError(func(yield func(V) bool) error {
		for v := range in {
			ok, err := filterfn(v)
			if err != nil {
				return err
			}
			if !ok {
				continue
			}
			if !yield(v) {
				break
			}
		}
		return nil
	})
}

func Filter2[K, V any](in iter.Seq2[K, V], filterfn func(K, V) bool) iter.Seq2[K, V] {
	return func(yield func(K, V) bool) {
		for k, v := range in {
			if !filterfn(k, v) {
				continue
			}
			if !yield(k, v) {
				break
			}
		}
	}
}

func zero[V any]() V {
	var zv V
	return zv
}

// GeneratorResult is the result of a generator. Calling it will block until the
// associated iterator has finished.
type GeneratorResult[R any] func() (R, error)

// Generator is an iterator that can have a final result.
func Generator[V, R any](gf func(yield func(V) bool) (R, error)) (iter.Seq[V], GeneratorResult[R]) {
	done := make(chan struct{})
	var result R
	var err error
	return func(yield func(V) bool) {
			defer close(done)
			result, err = gf(yield)
		}, func() (R, error) {
			<-done
			return result, err
		}
}

// Generator2 is just like [Generator] except it allows the iterator to have 2
// values for [iter.Seq2]
func Generator2[K, V, R any](gf func(yield func(K, V) bool) (R, error)) (iter.Seq2[K, V], GeneratorResult[R]) {
	done := make(chan struct{})
	var result R
	var err error
	return func(yield func(K, V) bool) {
			defer close(done)
			result, err = gf(yield)
		}, func() (R, error) {
			<-done
			return result, err
		}
}
