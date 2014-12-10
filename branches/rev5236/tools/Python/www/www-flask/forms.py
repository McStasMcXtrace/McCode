from wtforms import Form, BooleanField, SelectField, validators


class SimListForm(Form):
    simulation = SelectField('Simulation', choices=[('h', 'Herp'), ('d', 'Derp')])
