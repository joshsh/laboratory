
#include "ext.h"             // Max Header : standard Max include, always required 
#include "ext_obex.h"    // Max Object Extensions : required for new style Max object 

// Data Structure for this object 
typedef struct _simp 
	{ 
		t_object s_obj;      // t_object header: must always be the 1st field; used by Max  
		long s_value;   // something else 
		void       *outlet;   // Pointer to outlet  
	} t_simp; 
	
	
	// Prototypes for methods: need a method for each incoming message 
	void *simp_new(); 
void simp_free(t_simp *x); 
void simp_int(t_simp *x, long value); 
void simp_bang(t_simp *x); 


// global pointer to our class definition that is setup in main() 
static t_class *s_simp_class;  

/**************************************************************************/ 
// Main() Function - Object Class Definition 
int main() 
{ 
	t_class *c; 
	
	c = class_new("simplemax", (method)simp_new,  
				  (method)simp_free, sizeof(t_simp), 0L, 0);    
	class_addmethod(c, (method)simp_int, "int", A_LONG, 0); 
	class_addmethod(c, (method)simp_bang, "bang", 0); 
	
	class_register(CLASS_BOX, c); 
	
	s_simp_class = c; 
	
	return 0; 
} 

/**************************************************************************/ 
// Object Life 
// Create an instance of our object 
void *simp_new() 
{ 
	t_simp *x = (t_simp *)object_alloc(s_simp_class); 
	if(x){ 
        x->outlet = intout(x);     // Create the outlet   
		x->s_value = 0; 
    } 
	
	return x; 
} 

//  
void simp_free(t_simp *x) 
{ 
	; 
} 

/**************************************************************************/ 
// Methods bound to input/inlets 

// INT input 
void simp_int(t_simp *x, long n) 
{ 
	x->s_value = n; 
} 

// BANG input 
void simp_bang(t_simp *x) 
{ 
	post("value is %ld",x->s_value); 
	outlet_int(x->outlet, x->s_value); 
} 
