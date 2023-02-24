%{
    #include <stdio.h>
    #include <string.h>
    #include<stdarg.h>
    #include<ctype.h>
    #include<stdlib.h>
    #include<stdbool.h>
    extern FILE* yyin;
    extern char* yytext;
    extern int yylineno;
    int yylex();  
    struct variabila
    {
        char *nume;
        char *tip;
        int valoare;
        _Bool const1;
        float float_valoare;
        char char_valoare;
        _Bool bool_valoare;
        char * string_valoare;
    };
    struct vector
    {
        char *nume;
        char *tip;
        int dimensiune;
        int valoare;
    };
    struct clasa
    {
        char*nume;
        char *private;
        char*public;
        char*metode;
    };
    struct functie
    {
        char *nume;
        char *tip;
        char *arg; 
        char tipuri[1001];//pastram tipul parmaetrilor functiilor 
        int nr;//nr de parametri

    };
    struct clasa Clase[256];
    struct vector Vectori[256][256];
    struct variabila Variabile[256];
    struct functie Functii[150];
    int nr_variabile=0;
    int nr_arrays=0;
    int nr_functii=0;
    int nr_clase=0;
    int i=0;
    int yyerror(char * s)
    { 
        printf("eroare: %s la linia:%d\n",s,yylineno);
    }
     int variabila_declarata(char *nume)
    {
        for(int i=0;i<nr_variabile;i++)
        {
            if(strcmp(Variabile[i].nume,nume)==0) 
                return i;
        }
        return -1;
    }
     int return_valoare_variabila_int(char* nume)
    {
        int ok =variabila_declarata(nume);
        if (ok == -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu este  declarata",nume);
            yyerror(error);
        }
        return Variabile[ok].valoare;
    }
    char return_valoare_variabila_char(char* nume)
    {
        int ok =variabila_declarata(nume);
        if (ok == -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu este  declarata",nume);
            yyerror(error);
        }
        return Variabile[ok].char_valoare;
    }
    float return_valoare_variabila_float(char* nume)
    {
        int ok =variabila_declarata(nume);
        if (ok == -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu este  declarata",nume);
            yyerror(error);
        }
        return Variabile[ok].float_valoare;
    }
    char* return_tip_variabila(char *nume)
    {
        int ok=variabila_declarata(nume);
        if(ok==-1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu este declarata",nume);
            yyerror(error);
            exit(1);
        }
        return Variabile[ok].tip;
    }
    ////////////////////////////////////////////////////////
    enum Tipuri
    {
        OP,IDENTIFIER,NUMBER,INT,CHAR
    };
    struct NOD
    {
        int valoare;
        int tip;
        struct NOD* stanga;
        struct NOD* dreapta;
    } ;
     struct NOD* AST(char *radacina,struct NOD* stanga,struct NOD* dreapta,int tip)
     {
        struct NOD* nod_nou=malloc(sizeof(struct NOD));
        nod_nou->stanga=stanga;
        nod_nou->dreapta=dreapta;
        nod_nou->tip=tip;
        if(tip==INT)
        {
            nod_nou->valoare=*((int*)radacina);
        }else if(tip==OP)
        {
             nod_nou->valoare=*((char*)radacina);
        }else if(tip==IDENTIFIER)
        {
            int ok=variabila_declarata(radacina);
            if(ok==-1)
            {
                printf("VARIABILA NU EXISTA");
                exit(1);
            }
            char *x; x=return_tip_variabila(radacina);
            if(strcmp(x,"int")==0)
               nod_nou->valoare=ok;

        }else  nod_nou->valoare=0;
        printf("NODURI %d\n",nod_nou->valoare);
        return nod_nou;
     }
     int EvalAST(struct NOD* radacina)
     {
          if(radacina->tip==INT)
        {
            return radacina->valoare;
        }else if(radacina->tip==OP)
        {
            switch(radacina->tip)
            {
                case '+':
                    return EvalAST(radacina->stanga) + EvalAST(radacina->dreapta);
               case '*':
                   return EvalAST(radacina->stanga) * EvalAST(radacina->dreapta);
               case '-':
                   return EvalAST(radacina->stanga) - EvalAST(radacina->dreapta);
               case '/':
                   return EvalAST(radacina->stanga) / EvalAST(radacina->dreapta);
            }
        }else  if(radacina->tip==IDENTIFIER)
        {
            return radacina->valoare;
        }    
        else return -1;
     }

    //////////////////////////////////////////////////////////
    int vector_declarat(char *nume)
    {
        for(int i=0;i<nr_arrays;i++)
        {
            if(strcmp(Vectori[i]->nume,nume)==0)
                return i;
        }
        return -1;
    }
    void declara_variabila_farainit(char*tip,char*nume,_Bool const1)
    {
        if(variabila_declarata(nume)!=-1)
        {
            char error[100];
            sprintf(error,"Variabila %s este deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        else
        {
            if(const1==1)
            {
                char error[100];
                sprintf(error,"Variabila %s este constanta deci nu poate fi declarata fara initialzare",nume);
                yyerror(error);
                exit(1);
            }
            else
            {
                Variabile[nr_variabile].const1=0;
                Variabile[nr_variabile].tip=strdup(tip);
                Variabile[nr_variabile].nume=strdup(nume);
                nr_variabile++;
            }
        }
    }
    void declara_vector_farainit(char *tip, char*nume, int dimensiune)
    {
        if(vector_declarat(nume)!=-1)
        {
            char error[100];
            sprintf(error,"Variabila %s este deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        else
        {
            Vectori[nr_arrays]->tip=strdup(tip);
            Vectori[nr_arrays]->nume=strdup(nume);
            Vectori[nr_arrays]->dimensiune=dimensiune;
            nr_arrays++;
        }
    }
    _Bool mai_mic(int a,int b)
    {
        if(a<b)
            return 1;
        else 
            return 0;
    }
    _Bool or(int a,int b)
    {
        if(a || b)
            return 1;
        else 
            return 0;
    }
    int IsNumeric(char *str)
    {
        for(int i=0;str[i];i++) 
        if(strchr("0123456789",str[i])==NULL) 
            return 0;
        return 1;
    }
    void declara_variabila_cuint(char* tip, char* nume, int valoare,_Bool const1)
    {
        if (variabila_declarata(nume) != -1)
        {
            char error[100];
            sprintf(error,"Variabila %s este deja declarata",nume);
             yyerror(error);
            exit(1);
        }
        Variabile[nr_variabile].tip=strdup(tip);
        Variabile[nr_variabile].nume=strdup(nume);
        Variabile[nr_variabile].const1=const1;
        Variabile[nr_variabile].valoare=valoare;    
        printf("Valoare lui %s este %d \n",Variabile[nr_variabile].nume,Variabile[nr_variabile].valoare);
        nr_variabile++;
    }
    void declara_variabila_cuchar(char* tip, char* nume, char valoare,_Bool const1)
    {
        if (variabila_declarata(nume) != -1)
        {
            char error[100];
            sprintf(error,"Variabila %s este deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp("char",tip)==0)
        {
            Variabile[nr_variabile].tip=strdup(tip);
            Variabile[nr_variabile].nume=strdup(nume);
            Variabile[nr_variabile].const1=const1;
            Variabile[nr_variabile].char_valoare=valoare;    
            printf("Valoare lui %s este %c \n",Variabile[nr_variabile].nume,Variabile[nr_variabile].char_valoare);
            nr_variabile++;
        }
        else
        {
            char error[100];
            sprintf(error,"Declarare gresita");
            yyerror(error);
            exit(1);
        }
    }
    void declara_variabila_cubool(char* tip, char* nume, bool valoare,_Bool const1)
    {
        if (variabila_declarata(nume) != -1)
        {
            char error[100];
            sprintf(error,"Variabila %s este deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp("bool",tip)==0)
        {
            Variabile[nr_variabile].tip=strdup(tip);
            Variabile[nr_variabile].nume=strdup(nume);
            Variabile[nr_variabile].const1=const1;
            Variabile[nr_variabile].bool_valoare=valoare;    
            printf("Valoare lui %s este %d \n",Variabile[nr_variabile].nume,Variabile[nr_variabile].bool_valoare);
            nr_variabile++;
        }
        else
        {
            char error[100];
            sprintf(error,"Declarare gresita");
            yyerror(error);
            exit(1);
        }
    }
    void declara_variabila_custring(char* tip, char* nume, char* valoare,_Bool const1)
    {
        if (variabila_declarata(nume) != -1)
        {
            char error[100];
            sprintf(error,"Variabila %s este deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp("string",tip)==0)
        {
            Variabile[nr_variabile].tip=strdup(tip);
            Variabile[nr_variabile].nume=strdup(nume);
            Variabile[nr_variabile].const1=const1;
            Variabile[nr_variabile].string_valoare=strdup(valoare);    
            printf("Valoare lui %s este %s \n",Variabile[nr_variabile].nume,Variabile[nr_variabile].string_valoare);
            nr_variabile++;
        }
        else
        {
            char error[100];
            sprintf(error,"Declarare gresita");
            yyerror(error);
            exit(1);
        } 
    }
    void declara_variabila_cufloat(char* tip, char* nume, float valoare,_Bool const1)
    {
        if (variabila_declarata(nume) != -1)
        {
            char error[100];
            sprintf(error,"Variabila %s este deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp("float",tip)==0)
        {
            Variabile[nr_variabile].tip=strdup(tip);
            Variabile[nr_variabile].nume=strdup(nume);
            Variabile[nr_variabile].const1=const1;
            Variabile[nr_variabile].float_valoare=valoare;    
            printf("Valoare lui %s este %f \n",Variabile[nr_variabile].nume,Variabile[nr_variabile].float_valoare);
            nr_variabile++;
        }
        else
        {
            char error[100];
            sprintf(error,"Declarare gresita");
            yyerror(error);
            exit(1);
        } 
    }

    void atribuire_valoare_char(char* nume, char valoare)
    {
        int ok=variabila_declarata(nume);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"1Variabila %s nu e declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(Variabile[ok].const1==1)
        {
            char error[100];
            sprintf(error,"Variabila %s este constanta deci nu ii putem schimba valoare",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp(Variabile[ok].tip,"char")==0)
        {
            Variabile[ok].char_valoare = valoare; 
            printf("Valoare lui %s este %c \n",Variabile[ok].nume,Variabile[ok].char_valoare);

        }else {
            char error[100];
            sprintf(error,"Variabila %s nu este de tip char",nume);
            yyerror(error);
            exit(1);

        }
                  
    }
    void atribuire_valoare_float(char* nume, float valoare)
    {
        int ok= variabila_declarata(nume);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"2Variabila %s nu e declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(Variabile[ok].const1==1)
        {
            char error[100];
            sprintf(error,"Variabila %s este constanta deci nu ii putem schimba valoare",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp(Variabile[ok].tip,"float")==0)
        {
            Variabile[ok].float_valoare = valoare;
           printf("Valoare lui %s este %f \n",Variabile[ok].nume,Variabile[ok].float_valoare);
        }else {
            char error[100];
            sprintf(error,"Variabila %s nu este de tip float",nume);
            yyerror(error);
            exit(1);
        }

    }
    void atribuire_valoare_bool(char* nume, _Bool valoare)
    {
        int ok= variabila_declarata(nume);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu e declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(Variabile[ok].const1==1)
        {
            char error[100];
            sprintf(error,"Variabila %s este constanta deci nu ii putem schimba valoare",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp(Variabile[ok].tip,"bool")==0)
        {
            Variabile[ok].bool_valoare = valoare;
            printf("Valoare lui %s este %d \n",Variabile[ok].nume,Variabile[ok].bool_valoare);
        }else {
            char error[100];
            sprintf(error,"Variabila %s nu este de tip bool",nume);
            yyerror(error);
            exit(1);

        }
    }
    void atribuire_valoare_string(char* nume, char* valoare)
    {
        int ok= variabila_declarata(nume);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"4Variabila %s nu e declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(Variabile[ok].const1==1)
        {
            char error[100];
            sprintf(error,"Variabila %s este constanta deci nu ii putem schimba valoare",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp(Variabile[ok].tip,"string")==0)
        {
            Variabile[ok].string_valoare = strdup(valoare);
        printf("Valoare lui %s este %s \n",Variabile[ok].nume,Variabile[ok].string_valoare );
        }else {
            char error[100];
            sprintf(error,"Variabila %s nu este de tip string",nume);
            yyerror(error);
            exit(1);

        }
            
    }
    void atribuire_valoare_variabila(char* nume, char* variabila)
    {
        if (variabila_declarata(nume) == -1)
        {
            char error[100];
            sprintf(error,"5Variabila %s nu e deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        int ok= variabila_declarata(variabila);
        int ok2=variabila_declarata(nume);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu poate lua valoare unei variabile nedeclarate %s",nume, variabila);
            yyerror(error);
            exit(1);
        }
        if (strcmp(Variabile[ok2].tip, Variabile[ok].tip) != 0)
        {
            char error[100];
            sprintf(error,"Variabila %s nu are acelasi tip cu variabila %s \n",nume, variabila);
            yyerror(error);
            exit(1);
        }
        if(strcmp(Variabile[ok2].tip,"int")==0)
        {
            Variabile[ok2].valoare=Variabile[ok].valoare;
            printf("Valoare lui %s este %d \n",nume,Variabile[ok2].valoare);
        } 
        else  if(strcmp(Variabile[ok2].tip,"char")==0)
        {
            Variabile[ok2].char_valoare=Variabile[ok].char_valoare;
            printf("Valoare lui %s este %c \n",Variabile[ok2].nume,Variabile[ok2].char_valoare);
        }
        else  if(strcmp(Variabile[ok2].tip,"bool")==0)
        {
            Variabile[ok2].bool_valoare=Variabile[ok].bool_valoare;
            printf("Valoare lui %s este %d \n",Variabile[ok2].nume,Variabile[ok2].bool_valoare);
        }
        else  if(strcmp(Variabile[ok2].tip,"float")==0)
        {
            Variabile[ok2].float_valoare=Variabile[ok].float_valoare;
            printf("Valoare lui %s este %f \n",Variabile[ok2].nume,Variabile[ok2].float_valoare);
        }
        else  if(strcmp(Variabile[ok2].tip,"string")==0)
        {
            Variabile[ok2].string_valoare=strdup(Variabile[ok].string_valoare);
            printf("Valoare lui %s este %s \n",Variabile[ok2].nume,Variabile[ok2].string_valoare);
        }
    }
    void atribuire_valoare_int(char* nume, int valoare)
    {
        
        int ok= variabila_declarata(nume);
        if (ok== -1)
        {
            printf("aici %s",nume);
            char error[100];
            sprintf(error,"6Variabila %s nu e declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(Variabile[ok].const1==1)
        {
            char error[100];
            sprintf(error,"Variabila %s este constanta deci nu ii putem schimba valoare",nume);
            yyerror(error);
            exit(1);
        }
        if(strcmp(Variabile[ok].tip,"int")==0)
        {  
            Variabile[ok].valoare = valoare;
            printf("Valoare lui %s este %d \n",Variabile[ok].nume,valoare);
        }
        else if(strcmp(Variabile[ok].tip,"bool")==0)
            Variabile[ok].bool_valoare = valoare;  
        else {
            char error[100];
            sprintf(error,"Variabila %s nu este de tip int",nume);
            yyerror(error);
            exit(1);

        }     
    }
    void atribuire_pozitie_vector(char *nume,int pozitie, int valoare)
    {
        int ok= vector_declarat(nume);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu e declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(pozitie<=Vectori[ok]->dimensiune)
        {
            Vectori[ok][pozitie].valoare=valoare;
            printf("Valoare lui %s pe pozitia %d este %d \n",Vectori[ok]->nume,pozitie,Vectori[ok][pozitie].valoare);
        }
        else 
            printf("Pozitia este mai mare decat dimenziunea vectorului");
    }
    int return_valoare_pozitie(char *nume, int pozitie)
    {
        int ok= vector_declarat(nume);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu e declarata",nume);
            yyerror(error);
            exit(1);
        }
        if(pozitie<=Vectori[ok]->dimensiune)
        {
            return Vectori[ok][pozitie].valoare;
        }
        else {
            char error[100];
            sprintf(error,"Pozitia este mai mare decat dimensiunea vectorului");
            yyerror(error);
            exit(1);

        }
    }
    int functie_declarata(char*nume)
    {
        for(int i=0;i<nr_functii;i++)
        {
            if(strcmp(Functii[i].nume,nume)==0) 
                 return i;
        }
        return -1;
    }
      int functie_declarata2(char*nume,char*arg)
    {
        for(int i=0;i<nr_functii;i++)
        {
            if(strcmp(Functii[i].nume,nume)==0) 
            {
                //printf("%ld %s MARA\n",strlen(Functii[i].arg),Functii[i].arg);
                //printf("%ld %s NICO\n",strlen(arg),arg);
                if(strcmp(Functii[i].arg,arg)==0) 
                   return i;
                 else return -1;
            }
               
        }
        return -1;
    }
    int clasa_declarata(char*nume)
    {
        for(int i=0;i<nr_clase;i++)
        {
            if(strcmp(Clase[i].nume,nume)==0) 
                return i;
        }
        return -1;
    }
    void declarare_clasa(char*nume,char *private,char*public,char*metode)
    {
        if(clasa_declarata(nume)!=-1)
        {
            char error[100];
            sprintf(error,"Clasa %s este deja declarata",nume);
            yyerror(error);
        }
        else
        {
            Clase[nr_clase].nume=nume;
            Clase[nr_clase].private=private;
            Clase[nr_clase].public=public;
            nr_clase++;
        }
    } 
    void declarare_functie(char*tip,char*nume,char *arg)
    {
        if(functie_declarata2(nume,arg)!=-1)
        {
            char error[100];
            sprintf(error,"Functia %s este deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        else
        {
            Functii[nr_functii].tip=tip;
            Functii[nr_functii].nume=nume;
            Functii[nr_functii].arg=arg;
            char sir[1001]; strcpy(sir,Functii[nr_functii].arg);
            int a=0;
            char *p;
            p=strtok(sir," ,");
            while(p!=NULL)
            {
                if(strcmp(p,"int")==0 || strcmp(p,"char")==0 || strcmp(p,"float")==0 || strcmp(p,"bool")==0 || strcmp(p,"string")==0)
                    {
                        a++;
                        strcat(Functii[nr_functii].tipuri,p);
                        strcat(Functii[nr_functii].tipuri," ");
                    }
                p=strtok(NULL," ,");
            }
            Functii[nr_functii].nr=a;  
            printf("Ati declarat o functie de tipul %s, cu numele %s cu parametri %s \n",Functii[nr_functii].tip,Functii[nr_functii].nume,Functii[nr_functii].arg);
            printf("Functia are %d parametri. Tipurile parametrilor sunt: %s\n ",Functii[nr_functii].nr,Functii[nr_functii].tipuri);
            nr_functii++;
        }
    }
    _Bool return_valoare_variabila_bool(char* nume)
    {
        int ok =variabila_declarata(nume);
        if (ok == -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu este  declarata",nume);
            yyerror(error);
        }
        return Variabile[ok].bool_valoare;
    }
    void declarare_cu_variabila(char* tip, char* nume, char* variabila)
    {
        if (variabila_declarata(nume) != -1)
        {
            char error[100];
            sprintf(error,"Variabila %s e deja declarata",nume);
            yyerror(error);
            exit(1);
        }
        int ok= variabila_declarata(variabila);
        if (ok== -1)
        {
            char error[100];
            sprintf(error,"Variabila %s nu poate fi initializata cu o variabila nedeclarata %s",nume, variabila);
            yyerror(error);
            exit(1);
        }
        if (strcmp(tip, Variabile[ok].tip) != 0)
        {
            char error[100];
            sprintf(error,"Variabila %s nu are acelasi tip cu variabila %s \n",nume, variabila);
            yyerror(error);
            exit(1);
        }
        Variabile[nr_variabile].tip = strdup(tip);
        Variabile[nr_variabile].nume = strdup(nume);
        Variabile[nr_variabile].valoare = Variabile[ok].valoare;
        nr_variabile++;
    }
    char* tip_functie(char *nume)
    {
        int ok=functie_declarata(nume);
        if(ok==-1)
        {
            char error[100];
            sprintf(error,"Functia %s nu este declarata",nume);
            yyerror(error);
            exit(1);
        }
        return Functii[ok].tip;
    }
    void verificare_param(char *nume,char*param)
    {
        int ok=functie_declarata(nume);
        if(ok==-1)
        {
            char error[100];
            sprintf(error,"Functia %s nu este declarata",nume);
            yyerror(error);
            exit(1);
        }
        printf("LUNGIME %ld functia %s primeste parametri %s \n",strlen(param),nume,param);
        printf("LUNGIME %ld PARAMETRI %s\n",strlen(param),Functii[ok].tipuri);
        if(strcmp(param,Functii[ok].tipuri)==0)
        {
            printf("Tipuri parametri corespunzatori \n");
        }else{
            char error[1200];
            sprintf(error,"Functia %s trebuie sa aiba parametri de tipul %s \n",nume,Functii[ok].tipuri);
            yyerror(error);
            exit(1);

        }     
    }
   
%}
    %token SUM MOD DIF DIV MUL BGIN END TYPEOF START START2 START3
    %token BOPEN BCLOSE VIRGULA MIC MARE MICEGAL MAREEGAL VERIFIC
    %token AND OR DIFERIT NOT PDESCHISA PINCHISA ADESCHISA AINCHISA ID_OBIECT
    %token IF WHILE ELSE CONST CLASS EVAL PV FOR
    %left SUM DIF OR AND 
    %left MUL DIV MIC MARE 
    %start progr
    %union
    {
        int num;
        char *str;
        char car;
        float val_float;
        _Bool val_bool;
    }
    %token <car> CARACTER
    %token <num> NR 
    %token <str> TIP ID ASSIGN STRING IDFUNCTIE ARRAY IDCLASA PRIVATE PUBLIC
    %token <val_float> FLOAT
    %token <val_bool> BOOL 
    %type <num> expresie expresie_variabile_int 
    %type <val_float> expresie_float 
    %type <val_bool> conditii conditii_aritmetic_variabila_int conditii_aritmetic_int conditii_boolene conditii_aritmetic_float
    %type <str> apeluri_functii parametru parametri param_lista
    
    %%
    progr: START DECLARATII bloc_functii {printf("program corect sintactic\n");}
        ;
    bloc_functii: START2 functii bloc_clase 
                ;
    bloc_clase: START3 clase bloc
              ;
    bloc: BGIN instructiuni END
        ;

    DECLARATII: DECLARATIE 
            | DECLARATII DECLARATIE
            ;

    DECLARATIE: TIP ID PV{ declara_variabila_farainit($1,$2,0);}
            | TIP ID ASSIGN conditii PV{declara_variabila_cubool($1,$2,$4,0);}
            | TIP ID ASSIGN expresie PV{ declara_variabila_cuint($1,$2,$4,0);}
            | TIP ID ASSIGN CARACTER PV{declara_variabila_cuchar($1,$2,$4,0);}
            | TIP ID ASSIGN expresie_float PV {declara_variabila_cufloat($1,$2,$4,0);}
            | TIP ID ASSIGN ID PV{declarare_cu_variabila($1,$2,$4);}
            | TIP ID ASSIGN STRING PV{ declara_variabila_custring($1,$2,$4,0);}
            | TIP ID ASSIGN BOOL PV{ declara_variabila_cubool($1,$2,$4,0);}
            | array_declarare PV
            | CONST TIP ID PV{declara_variabila_farainit($2,$3,1);}
            | CONST TIP ID ASSIGN STRING PV{ declara_variabila_custring($2,$3,$5,1);}
            | CONST TIP ID ASSIGN BOOL PV{ declara_variabila_cubool($2,$3,$5,1);}
            | CONST TIP ID ASSIGN expresie PV{ declara_variabila_cuint($2,$3,$5,1);}
            | CONST TIP ID ASSIGN expresie_float PV{ declara_variabila_cufloat($2,$3,$5,1);}
            ;
    array_declarare: ARRAY TIP ID BOPEN NR BCLOSE {declara_vector_farainit($2,$3,$5);}
                ;
    instructiuni: instructiune 
                | instructiuni instructiune 
    instructiune: expresie PV
                | expresie_float PV
                | ID ASSIGN expresie PV{atribuire_valoare_int($1,$3);}
                | ID ASSIGN conditii PV{atribuire_valoare_bool($1,$3);}
                | ID ASSIGN CARACTER PV{atribuire_valoare_char($1,$3);}
                | ID ASSIGN FLOAT PV{atribuire_valoare_float($1,$3);}
                | ID ASSIGN BOOL PV{atribuire_valoare_bool($1,$3);}
                | ID ASSIGN STRING PV{atribuire_valoare_string($1,$3);}
                | ID ASSIGN ID PV{atribuire_valoare_variabila($1,$3);}
                | ID BOPEN NR BCLOSE ASSIGN expresie PV{atribuire_pozitie_vector($1,$3,$6);}
                | blocks 
                | conditii
                | EVAL PDESCHISA expresie PINCHISA PV{printf("valoarea expresiei este %d \n",$3);}
                | EVAL PDESCHISA expresie_float PINCHISA PV{ printf("valoarea expresiei este %f \n",$3);}
                | EVAL PDESCHISA ID PINCHISA PV{
                            char *x=return_tip_variabila($3);
                            if(strcmp(x,"int")==0)
                            {
                                printf("valoarea expresiei este %d \n",return_valoare_variabila_int($3));
                            } 
                            else if(strcmp(x,"char")==0) 
                                printf("valoarea expresiei este %d \n",return_valoare_variabila_char($3));
                            else if(strcmp(x,"float")==0) 
                                printf("valoarea expresiei este %f \n",return_valoare_variabila_float($3));   
                            else if(strcmp(x,"bool")==0) 
                                printf("valoarea expresiei este %d \n",return_valoare_variabila_bool($3));   
                            } 
                | TYPEOF PDESCHISA expresie PINCHISA PV{printf("tipul expresiei este INT \n");}
                | TYPEOF PDESCHISA expresie_float PINCHISA PV{ printf("tipul expresiei este FLOAT \n");}            
                | TYPEOF PDESCHISA ID PINCHISA PV{char *x=return_tip_variabila($3); printf("tipul expresiei este %s \n",x);}  
                | apeluri_functii
                | declarare_obiect
                ;
    functii: functie
           | functii functie
           ;
    functie: TIP IDFUNCTIE parametri ADESCHISA instructiuni AINCHISA{declarare_functie($1,$2,$3);}
        ;
    parametri: PDESCHISA PINCHISA {$$=malloc(300); $$[0]=0;}
            | PDESCHISA parametru PINCHISA {$$=$2;}
            ;
    parametru: TIP ID {strcat($$," ");strcat($$,$2);}
            | parametru VIRGULA TIP ID {strcat($$,","); strcat($$,$3);strcat($$," "); strcat($$,$4);}
            ;
    apeluri_functii: IDFUNCTIE PDESCHISA param_lista PINCHISA PV  {$$=malloc(300); $$[0]=0;
        strcat($$,$1);verificare_param($1,$3);}
                    ;
    
    param_lista: ID { $$=malloc(300); $$[0]=0;
                    char *x=return_tip_variabila($1);
                        strcat($$,x); strcat($$," ");
                       }
            | expresie {strcat($$,"int");strcat($$," ");}
            | apeluri_functii {char *x;printf("%s MARA",$1);
                  x=tip_functie($1); 
                 strcat($$,x);}
            | expresie_float {strcat($$,"float");strcat($$," "); }
            | param_lista VIRGULA ID {char *x=return_tip_variabila($3);
                             strcat($$,x);
                             strcat($$," ");}  
            | param_lista VIRGULA expresie {strcat($$,"int");strcat($$," ");}  
            | param_lista VIRGULA expresie_float {strcat($$,"float");strcat($$," ");}  
            | param_lista VIRGULA apeluri_functii {char *x;printf("%s MARA",$3);
                  x=tip_functie($3); 
                 strcat($$,x); strcat($$," ");}

            ;
    clase: clasa
         | clase clasa
         ;
    clasa: CLASS IDCLASA ADESCHISA PRIVATE DECLARATII PUBLIC DECLARATII AINCHISA PV 
        | CLASS IDCLASA ADESCHISA DECLARATII AINCHISA PV 
        | CLASS IDCLASA ADESCHISA PRIVATE DECLARATII AINCHISA PV 
        | CLASS IDCLASA ADESCHISA PUBLIC DECLARATII AINCHISA PV
        ;        
    declarare_obiect: IDCLASA ID_OBIECT PV
            ;        
    blocks: IF PDESCHISA instructiuni PINCHISA ADESCHISA instructiuni AINCHISA
        | IF PDESCHISA instructiuni PINCHISA ADESCHISA instructiuni AINCHISA ELSE ADESCHISA instructiuni AINCHISA
        | WHILE PDESCHISA instructiuni PINCHISA ADESCHISA instructiuni AINCHISA
        | FOR PDESCHISA instructiuni PV conditii PV instructiuni PINCHISA ADESCHISA instructiuni AINCHISA
        ;
    conditii_aritmetic_int: expresie MIC expresie {$$= $1 < $3;}
        | expresie MARE expresie {$$= $1 > $3;}
        | expresie MICEGAL expresie {$$= $1 <= $3;}
        | expresie MAREEGAL expresie {$$=$1 >= $3;}
        | expresie VERIFIC expresie {$$=$1 == $3;}
        | expresie DIFERIT expresie {$$= $1 != $3;}
        ;
    conditii_aritmetic_variabila_int: expresie_variabile_int MIC expresie_variabile_int {$$= $1 < $3;}
        | expresie_variabile_int MARE expresie_variabile_int {$$= $1 > $3;}
        | expresie_variabile_int MICEGAL expresie_variabile_int {$$= $1 <= $3;}
        | expresie_variabile_int MAREEGAL expresie_variabile_int {$$=$1 >= $3;}
        | expresie_variabile_int VERIFIC expresie_variabile_int {$$=$1 == $3;}
        | expresie_variabile_int DIFERIT expresie_variabile_int {$$= $1 != $3;}
        ;
    expresie_variabile_int: ID {
                                int x; 
                                if(strcmp("int",return_tip_variabila($1))==0)
                                    x=return_valoare_variabila_int($1);
                                else if(strcmp("float",return_tip_variabila($1))==0)
                                    x=return_valoare_variabila_float($1);
                                $$=x;
                               }
        ;              
    conditii_aritmetic_float: expresie_float MIC expresie_float {$$= $1 < $3;}
        | expresie_float MARE expresie_float {$$= $1 > $3;}
        | expresie_float MICEGAL expresie_float {$$= $1 <= $3;}
        | expresie_float MAREEGAL expresie_float {$$=$1 >= $3;}
        | expresie_float VERIFIC expresie_float {$$=$1 == $3;}
        | expresie_float DIFERIT expresie_float{$$= $1 != $3;}
        ;
    conditii_boolene:  expresie OR expresie {$$= $1 || $3;}
        | expresie AND expresie {$$= $1 && $3;}
        | NOT expresie {$$= !($2);}
        ;
    conditii:  conditii_aritmetic_int {$$=$1;}
        | conditii_aritmetic_float {$$=$1;}
        | conditii_boolene{$$=$1;}
        |conditii_aritmetic_variabila_int{$$=$1;}
        ;
    expresie: SUM expresie expresie {$$=$2+$3;}
        |SUM ID expresie { int a; char *x=return_tip_variabila($2);
          if(strcmp(x,"int")==0) a=return_valoare_variabila_int($2);$$=$3+a;}
        |MUL ID expresie { int a; char *x=return_tip_variabila($2);
          if(strcmp(x,"int")==0) a=return_valoare_variabila_int($2);$$=$3*a;}
        | DIF expresie expresie {$$=$2-$3;}
        | SUM apeluri_functii expresie {$$=$3;}
        | SUM expresie apeluri_functii {$$=$2;}
        | SUM ID ID { int a,b; char *x=return_tip_variabila($2);
        char *y=return_tip_variabila($3);
          if(strcmp(x,"int")==0 && strcmp(y,"int")==0) {b=return_valoare_variabila_int($2);
          a=return_valoare_variabila_int($3);$$=a+b;}else 
           {char error[101];
            strcat(error,"Expresia trebuie sa contina aceleasi tipuri de date");
            yyerror(error);
            exit(1);}
          }
        | MUL ID ID { int a,b; char *x=return_tip_variabila($2);
        char *y=return_tip_variabila($3);
          if(strcmp(x,"int")==0 && strcmp(y,"int")==0) {b=return_valoare_variabila_int($2);
          a=return_valoare_variabila_int($3);$$=a*b;}else 
           {char error[101];
            strcat(error,"Expresia trebuie sa contina aceleasi tipuri de date");
            yyerror(error);
            exit(1);}
          }
        | DIV ID ID { int a,b; char *x=return_tip_variabila($2);
        char *y=return_tip_variabila($3);
          if(strcmp(x,"int")==0 && strcmp(y,"int")==0) {b=return_valoare_variabila_int($2);
          a=return_valoare_variabila_int($3);$$=a/b;}else 
           {char error[101];
            strcat(error,"Expresia trebuie sa contina aceleasi tipuri de date");
            yyerror(error);
            exit(1);}
          }
         | MOD ID ID { int a,b; char *x=return_tip_variabila($2);
        char *y=return_tip_variabila($3);
          if(strcmp(x,"int")==0 && strcmp(y,"int")==0) {b=return_valoare_variabila_int($2);
          a=return_valoare_variabila_int($3);$$=a%b;}else 
           {char error[101];
            strcat(error,"Expresia trebuie sa contina aceleasi tipuri de date");
            yyerror(error);
            exit(1);}
          }
        | DIF ID ID { int a,b; char *x=return_tip_variabila($2);
        char *y=return_tip_variabila($3);
          if(strcmp(x,"int")==0 && strcmp(y,"int")==0) {b=return_valoare_variabila_int($2);
          a=return_valoare_variabila_int($3);$$=a-b;}else 
           {char error[101];
            strcat(error,"Expresia trebuie sa contina aceleasi tipuri de date");
            yyerror(error);
            exit(1);}
          }
        | MUL expresie expresie {$$=$2*$3;}
        | DIV expresie expresie {$$=$2/$3;}
        | MOD expresie expresie {$$=$2%$3;}
        | ID BOPEN NR BCLOSE {$$=return_valoare_pozitie($1,$3);}
         | NR {$$=$1;}
        ; 
    expresie_float: SUM expresie_float expresie_float {$$=$2+$3;}
        | DIF expresie_float expresie_float {$$=$2-$3;}
        | MUL expresie_float expresie_float {$$=$2*$3;}
        | DIV expresie_float expresie_float {$$=$2/$3;}
        | FLOAT {$$=$1;}
        ; 
%%

    int main(int argc, char** argv){
    yyin=fopen(argv[1],"r");
    yyparse();
    FILE *f=fopen("symbol_table.txt","w");
    fprintf(f,"Variabilele folosite sunt:\n");
    for(int i=0;i<nr_variabile;i++)
    {
        char *x=return_tip_variabila(Variabile[i].nume);
        if(strcmp(x,"int")==0)
            fprintf(f,"%d. NUME: %s; TIP: %s; VALOARE: %d; E CONSTANTA: %d\n",i+1,Variabile[i].nume,Variabile[i].tip,Variabile[i].valoare,Variabile[i].const1);
        else if(strcmp(x,"char")==0)
            fprintf(f,"%d. NUME: %s; TIP: %s; VALOARE: %c; E CONSTANTA: %d\n",i+1,Variabile[i].nume,Variabile[i].tip,Variabile[i].char_valoare,Variabile[i].const1);
        else if(strcmp(x,"float")==0)
            fprintf(f,"%d. NUME: %s; TIP: %s; VALOARE: %f; E CONSTANTA: %d\n",i+1,Variabile[i].nume,Variabile[i].tip,Variabile[i].float_valoare,Variabile[i].const1);
        else if(strcmp(x,"bool")==0)
        {
            if(Variabile[i].bool_valoare==0)
                fprintf(f,"%d. NUME: %s; TIP: %s; VALOARE: %s; E CONSTANTA: %d\n",i+1,Variabile[i].nume,Variabile[i].tip,"FALSE",Variabile[i].const1);
            else      
                fprintf(f,"%d. NUME: %s; TIP: %s; VALOARE: %s; E CONSTANTA: %d\n",i+1,Variabile[i].nume,Variabile[i].tip,"TRUE",Variabile[i].const1);
        }
        else if(strcmp(x,"string")==0)
        fprintf(f,"%d. NUME: %s; TIP: %s; VALOARE: %s; E CONSTANTA: %d\n",i+1,Variabile[i].nume,Variabile[i].tip,Variabile[i].string_valoare,Variabile[i].const1);
    }
    if(nr_variabile==0)
        fprintf(f,"NONE\n");

    FILE *tabel=fopen("symbol_table_functions.txt","w");
    fprintf(tabel,"Functiile folosite sunt:\n");
    for(int i=0;i<nr_functii;i++)
        fprintf(tabel,"%d. TIP: %s NUME: %s PARAMETRI: %s \n",i+1,Functii[i].tip,Functii[i].nume,Functii[i].arg);
    return 0;
}    