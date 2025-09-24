use crate::forms::augmented::Blocking;
use crate::forms::augmented::Deadending;
use crate::forms::augmented::Dicot;
use crate::forms::augmented::Full;
//use crate::ShortPartizan;
//use crate::ShortPartizanOwned;

// From Full to other types
impl From<Full> for Dicot {
    fn from(value: Full) -> Self {
        Dicot::new(value.augmented())
    }
}

impl From<Full> for Blocking {
    fn from(value: Full) -> Self {
        Blocking::new(value.augmented())
    }
}

impl From<Full> for Deadending {
    fn from(value: Full) -> Self {
        Deadending::new(value.augmented())
    }
}

// From Dicot to other types
impl From<Dicot> for Full {
    fn from(value: Dicot) -> Self {
        Full::new(value.augmented())
    }
}

impl From<Dicot> for Blocking {
    fn from(value: Dicot) -> Self {
        Blocking::new(value.augmented())
    }
}

impl From<Dicot> for Deadending {
    fn from(value: Dicot) -> Self {
        Deadending::new(value.augmented())
    }
}

// From Blocking to other types
impl From<Blocking> for Full {
    fn from(value: Blocking) -> Self {
        Full::new(value.augmented())
    }
}

impl From<Blocking> for Dicot {
    fn from(value: Blocking) -> Self {
        Dicot::new(value.augmented())
    }
}

impl From<Blocking> for Deadending {
    fn from(value: Blocking) -> Self {
        Deadending::new(value.augmented())
    }
}

// From Deadending to other types
impl From<Deadending> for Full {
    fn from(value: Deadending) -> Self {
        Full::new(value.augmented())
    }
}

impl From<Deadending> for Dicot {
    fn from(value: Deadending) -> Self {
        Dicot::new(value.augmented())
    }
}

impl From<Deadending> for Blocking {
    fn from(value: Deadending) -> Self {
        Blocking::new(value.augmented())
    }
}

//impl<T: ShortPartizanOwned> From<T> for Augmented {
//    fn from(value: T) -> Self {
//        Augmented::new(
//            value.left().into_iter().map(T::into).collect(),
//            value.right().into_iter().map(T::into).collect(),
//        )
//    }
//}

//impl<T: ShortPartizanOwned> From<T> for Form {
//    fn from(value: T) -> Self {
//        Form::new(
//            value.left().into_iter().map(T::into).collect(),
//            value.right().into_iter().map(T::into).collect(),
//        )
//    }
//}
//
//impl From<Form> for Augmented {
//    fn from(value: Form) -> Self {
//        Augmented::new(
//            &value
//                .left()
//                .cloned()
//                .map(Augmented::from)
//                .collect::<Vec<_>>(),
//            &value
//                .right()
//                .cloned()
//                .map(Augmented::from)
//                .collect::<Vec<_>>(),
//        )
//    }
//}
//
//// TODO: move to macro
//impl<T: ShortPartizanOwned> From<T> for Normal {
//    fn from(value: T) -> Self {
//        Normal::new(Form::new(
//            value.left().into_iter().map(T::into).collect(),
//            value.right().into_iter().map(T::into).collect(),
//        ))
//    }
//}
//
//// TODO: move to macro
//impl<T: ShortPartizanOwned> From<T> for Misere {
//    fn from(value: T) -> Self {
//        Misere::new(Form::new(
//            value.left().into_iter().map(T::into).collect(),
//            value.right().into_iter().map(T::into).collect(),
//        ))
//    }
//}
