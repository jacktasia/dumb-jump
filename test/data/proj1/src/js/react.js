
// normal react es6 class component
class MyComponent extends Component {
  render() {
    return <h1>My Component!</h1>;
  }
}

<MyComponent>{children}</MyComponent>

// exotic react component

const MyComponent3 = ({
  prop1,
  prop2,
}, {
  context1,
  context2,
}) => (
  <div>{prop1} {context1}<div>
);

<MyComponent3>{children3}</MyComponent3>

// stateless component mycomponent2
const MyComponent2 = props => <div>{props.text}</div>

<MyComponent2>{children2}</MyComponent2>

// inline jsx
const mySomething = <div>...</div>;

<div>{mySomething}</div>

// decorated Component
class MyBaseComponent extends Component .....
const DecoratedComponent = Radium(Rezponsive(MyBaseComponent);
export default DecoratedComponent

<DecoratedComponent />
