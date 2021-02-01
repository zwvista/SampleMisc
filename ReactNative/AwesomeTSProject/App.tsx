import * as React from 'react';
import { Button, View } from 'react-native';
import { createDrawerNavigator } from '@react-navigation/drawer';
import { NavigationContainer } from '@react-navigation/native';
import { createStackNavigator } from '@react-navigation/stack';
import Ionicons from 'react-native-vector-icons/Ionicons';

function HomeScreen({ navigation }:any) {
  return (
    <View style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
      <Button
        onPress={() => navigation.navigate('Notifications')}
        title="Go to notifications"
      />
    </View>
  );
}

function NotificationsScreen({ navigation }:any) {
  return (
    <View style={{ flex: 1, alignItems: 'center', justifyContent: 'center' }}>
      <Button onPress={() => navigation.goBack()} title="Go back home" />
    </View>
  );
}

const Drawer = createDrawerNavigator();

function draw() {
  return (

      <Drawer.Navigator initialRouteName="Home" >
        <Drawer.Screen name="Home" component={HomeScreen} 
        options={{
        drawerIcon: () => <Ionicons name='md-home' size={30} color='#130f40' />,
          }}
         />
        <Drawer.Screen name="Notifications" component={NotificationsScreen}
        options={{
        drawerIcon: () => <Ionicons name='md-notifications' size={30} color='#130f40' />,
          }}
         />
      </Drawer.Navigator>

  );
}



const Stack = createStackNavigator();

function Def(){
  return (
    <NavigationContainer>
    <Stack.Navigator>
    <Stack.Screen
        name="Home"
        component={draw}
        options={{
          title: 'My home',
          headerStyle: {
            backgroundColor: '#5f27cd',
          },
          headerTintColor: '#fff',
          headerTitleStyle: {
            fontWeight: 'bold',
          },

          headerLeft: ({navigation}) =>  (<Ionicons name='md-menu' style={{paddingLeft: "20px"}} size={30} color='white' onPress={() => navigation.toggleDrawer()} />)


        }}

      />
    </Stack.Navigator>
    </NavigationContainer>    
  )
}

export default Def;