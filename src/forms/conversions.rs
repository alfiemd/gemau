use super::form::Form;
use super::misere::Misere;
use super::normal::Normal;
use crate::ShortPartizan;
use crate::ShortPartizanOwned;
use crate::forms::augmented::Augmented;

impl From<Form> for Normal {
    fn from(value: Form) -> Self {
        Normal::new(value)
    }
}

impl From<Normal> for Form {
    fn from(value: Normal) -> Self {
        value.inner()
    }
}

impl From<Form> for Misere {
    fn from(value: Form) -> Self {
        Misere::new(value)
    }
}

impl From<Misere> for Form {
    fn from(value: Misere) -> Self {
        value.inner()
    }
}

impl From<Normal> for Misere {
    fn from(value: Normal) -> Self {
        Misere::new(value.inner())
    }
}

impl From<Misere> for Normal {
    fn from(value: Misere) -> Self {
        Normal::new(value.inner())
    }
}

impl<T: ShortPartizanOwned> From<T> for Form {
    fn from(value: T) -> Self {
        Form::new(
            value.left().into_iter().map(T::into).collect(),
            value.right().into_iter().map(T::into).collect(),
        )
    }
}

impl From<Form> for Augmented {
    fn from(value: Form) -> Self {
        Augmented::new(
            &value
                .left()
                .cloned()
                .map(Augmented::from)
                .collect::<Vec<_>>(),
            &value
                .right()
                .cloned()
                .map(Augmented::from)
                .collect::<Vec<_>>(),
        )
    }
}

// TODO: move to macro
impl<T: ShortPartizanOwned> From<T> for Normal {
    fn from(value: T) -> Self {
        Normal::new(Form::new(
            value.left().into_iter().map(T::into).collect(),
            value.right().into_iter().map(T::into).collect(),
        ))
    }
}

// TODO: move to macro
impl<T: ShortPartizanOwned> From<T> for Misere {
    fn from(value: T) -> Self {
        Misere::new(Form::new(
            value.left().into_iter().map(T::into).collect(),
            value.right().into_iter().map(T::into).collect(),
        ))
    }
}
