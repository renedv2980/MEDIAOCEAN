*          DATA SET ACPRO64    AT LEVEL 006 AS OF 08/22/02                      
*PHASE T60B64A,*                                                                
*INCLUDE TIMEOUT                                                                
*INCLUDE KHDUMMY                                                                
*                                                                               
         TITLE 'T60B64 - BILLING AUTHORIZATION'                                 
T60B64   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T60B64**,R7,RR=R2,CLEAR=YES                         
         LR    R4,RC                                                            
         USING MYD,R4                                                           
         ST    R2,MYRELO                                                        
         SR    R2,R2                                                            
*                                                                               
         L     RF,=V(TIMEOUT)                                                   
         A     RF,MYRELO                                                        
         ST    RF,TIMEOUT                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         NI    GENSTAT1,X'FF'-OKADDEL                                           
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY             VALIDATE THE KEY                             
*                                                                               
         CLC   CONACT(4),=C'HIST'  IS THIS ACTION HISTORY?                      
         BE    OKEXIT              YES, DONE WITH THIS                          
*                                                                               
         CLC   CONACT(6),=C'DELETE'                                             
         BE    DELEIT                                                           
*                                                                               
         CLC   CONACT(7),=C'RESTORE'                                            
         BE    RESTIT                                                           
*                                                                               
         CLI   ACTNUM,ACTADD       ARE WE ADDING?                               
         BNE   *+8                 NO                                           
         BAS   RE,PROCPF           YES, LOOK FOR PF KEYS                        
         B     MODEX                                                            
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE04                                                           
*                                                                               
         BAS   RE,VKEY             VALIDATE THE KEY                             
*                                                                               
         CLC   CONACT(4),=C'HIST'  IS THIS ACTION HISTORY?                      
         BE    OKEXIT              YES, DONE WITH THIS                          
*                                                                               
         CLI   ACTNUM,ACTCHA       FOR ACTION=CHANGE, LOOK FOR PF KEYS          
         BNE   *+8                                                              
         BAS   RE,PROCPF                                                        
         BAS   RE,VREC             VALIDATE RECORD                              
         BAS   RE,DREC             DISPLAY NEW RECORD                           
*                                                                               
         CLI   ACTNUM,ACTADD       IS THIS AN ADD ?                             
         BNE   MODE2A              NO, MUST BE CHANGE                           
*                                                                               
         CP    ELMCNT,=P'0'        ANY USER FIELDS ?                            
         BE    MODE10              NO                                           
         CLI   VERUSER,1           YES, USER FIELDS BEEN VERIFIED?              
         BE    MODE10              YES                                          
*                                                                               
         LA    R2,AUTUSEDH         SETUP CURSOR                                 
         MVI   MYMSGNO1,UPUSER                                                  
         B     INFEXIT                                                          
*                                                                               
MODE2A   CP    ELMCNT,=P'0'        ANY USER FIELDS ?                            
         BE    MODE12              NO, DONE                                     
         CLI   VERUSER,1           USER FIELDS VERIFIED ?                       
         BE    MODE12              YES, ALL DONE                                
*                                                                               
         LA    R2,AUTUSEDH         SETUP CURSOR                                 
         ST    R2,ACURFORC                                                      
         MVI   MYMSGNO1,UPUSER     SETUP CURSOR AND MESSAGE                     
         B     INFEXIT                                                          
*                                                                               
MODE04   CLI   MODE,DISPKEY                                                     
         BNE   MODE06                                                           
         BAS   RE,DKEY             DISPLAY KEY                                  
         BAS   RE,VKEY             VALIDATE KEY                                 
         B     MODEX                                                            
*                                                                               
MODE06   CLI   MODE,DISPREC                                                     
         BNE   MODE08                                                           
         BAS   RE,PROCPF                                                        
         BAS   RE,DREC             DISPLAY RECORD                               
*                                                                               
MODE08   CLI   MODE,PROCPFK        PFKEY PRESSED                                
         BNE   MODEX                                                            
         BAS   RE,PROCPF                                                        
         B     MODEX                                                            
*                                                                               
MODE10   BAS   RE,ADDFUND          ADD FUNDING RECORD NOW                       
*                                                                               
MODE12   MVI   PASS,0                                                           
*                                                                               
MODEX    B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              DISPLAY THE KEY                                        *         
*              SET FIELD LENGTHS BECAUSE VKEY IS CALLED AGAIN         *         
***********************************************************************         
*                                                                               
         USING AUTRECD,R6                                                       
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         LA    R2,AUTOGRH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKOGR),AUTKOGR                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,AUTOFCH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKOFC),AUTKOFC                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,AUTCLIH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKCLI),AUTKCLI                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,AUTPROH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKPRO),AUTKPRO                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,AUTMGRH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKMGR),AUTKMGR                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,AUTMEDH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKMED),AUTKMED                                           
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,AUTNUMH                                                       
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'AUTKNUM),AUTKNUM                                           
         BAS   RE,MOVEFLD                                                       
         B     OKEXIT                                                           
*                                                                               
MOVEFLD  NTR1                                                                   
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         OI    6(R2),X'80'                                                      
         STC   RF,5(R2)                                                         
*                                                                               
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE KEY                                       *         
***********************************************************************         
*                                                                               
         USING AUTRECD,R6                                                       
VKEY     NTR1                                                                   
         XC    SVFLDS,SVFLDS       CLEAR KEY SAVE AREA                          
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,C'N'                                                      
         XC    EFFVALS,EFFVALS                                                  
         XC    MEDIA,MEDIA                                                      
         XC    MGROUP,MGROUP                                                    
         MVC   CLICODE,SPACES                                                   
         MVC   PRODCODE,PRODCODE                                                
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FOR THIS                            
*                                                                               
         LA    R2,AUTOGRH          OFFICE GROUP                                 
         BAS   RE,TSTCHG                                                        
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         CLC   =C'ALL',8(R2)       TEST FOR ALL                                 
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
         MVC   SVOGR,EFFOFG                                                     
*                                                                               
VKEY2    LA    R2,AUTOFCH          OFFICE                                       
         BAS   RE,TSTCHG                                                        
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   AUTOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
         MVC   SVOFC,EFFOFFC                                                    
*                                                                               
VKEY4    LA    R2,AUTCLIH          CLIENT                                       
         BAS   RE,TSTCHG                                                        
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   AUTOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   AUTOFCH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALCLI                                                           
         MVC   SVCLI,CLICODE                                                    
*                                                                               
VKEY6    LA    R2,AUTPROH          PRODUCT                                      
         BAS   RE,TSTCHG                                                        
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   AUTCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
         MVC   SVPRO,PRODCODE                                                   
*                                                                               
VKEY8    LA    R2,AUTMGRH          MEDIA GROUP                                  
         BAS   RE,TSTCHG                                                        
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         GOTO1 VALMG                                                            
         MVC   SVMGR,MGROUP                                                     
*                                                                               
VKEY10   LA    R2,AUTMEDH          MEDIA                                        
         BAS   RE,TSTCHG                                                        
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   AUTMGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALMED                                                           
         MVC   SVMED,MEDIA                                                      
*                                                                               
         USING AUTRECD,R6                                                       
VKEY12   LA    R6,KEY                                                           
         MVC   AUTKEY,SPACES                                                    
         MVI   AUTKTYP,AUTKTYPQ                                                 
         MVI   AUTKSUB,AUTKSUBQ                                                 
         MVC   AUTKCPY(3),CUL                                                   
         MVC   AUTKOGR,SVOGR                                                    
         MVC   AUTKOFC,SVOFC                                                    
         MVC   AUTKCLI,SVCLI                                                    
         MVC   AUTKPRO,SVPRO                                                    
         MVC   AUTKMGR,SVMGR                                                    
         MVC   AUTKMED,SVMED                                                    
*                                                                               
         LA    R2,AUTNUMH          THIS IS THE ONLY REQUIRED FIELD              
         GOTO1 ANY                                                              
         MVC   AUTKNUM,WORK                                                     
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         CLC   CONACT(4),=C'HIST'  IS THIS ACTION HISTORY?                      
         BE    VKEYX               YES, ALL DONE                                
*                                                                               
         CLC   SAVEKEY,KEY                                                      
         MVC   SAVEKEY,KEY                                                      
         BNE   VKEY14                                                           
         CLI   CALLER,X'00'        DID SOMEONE CALL ME ?                        
         BE    VKEY16              NO                                           
*                                                                               
VKEY14   BAS   RE,CLEARU           YES, CLEAR USER FIELDS                       
         MVI   PASS,0                                                           
*                                                                               
VKEY16   CLI   KEYCHG,C'Y'         DID ANYTHING CHANGE ?                        
         BNE   VKEYX               NO, EXIT                                     
         BAS   RE,LOAD             YES, RELOAD BUFFER                           
*                                                                               
VKEYX    B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE RECORD                                    *         
***********************************************************************         
*                                                                               
VREC     NTR1                                                                   
         MVI   VERUSER,0           CLEAR USER FIELD VERIFIED SWITCH             
*                                                                               
         LA    R2,AUTAMTH          VERIFY AUTHORIZED AMOUNT                     
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'80',8(R2)),(RF)                                  
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BNE   ERREXIT                                                          
         CLC   4(8,R1),=PL8'99999999999'                                        
         BH    ERREXIT                                                          
         ZAP   SVAAMT,6(6,R1)      SAVE AMOUNT                                  
         BNP   ERREXIT             ZERO IS INVALID                              
*                                                                               
         LA    R2,AUTAUTDH         DESCRIPTION IS NOT REQUIRED                  
         CLI   5(R2),0                                                          
         BE    VREC02                                                           
         GOTO1 ANY                                                              
         GOTO1 NAMEIN                                                           
*                                                                               
VREC02   GOTO1 PERSIN                                                           
*                                                                               
         USING AUTRECD,R6                                                       
         L     R6,AIO                                                           
         LA    R2,AUTSTATH         CHECK THE STATUS                             
         CLI   5(R2),0             ANY CHANGE?                                  
         BE    VREC04              NO, DEFAULT IS ACTIVE                        
         MVI   ERROR,INVALID                                                    
         NI    AUTRSTA,X'FF'-AUTRDACT                                           
         CLI   8(R2),C'A'          IS RECORD ACTIVE ?                           
         BE    VREC04              YES, ALL SET                                 
         CLI   8(R2),C'I'          NO, IS IT INACTIVE                           
         BNE   ERREXIT             NO, ERROR                                    
         OI    AUTRSTA,AUTRDACT    YES, MARK IT SO                              
*                                                                               
VREC04   CLI   ACTNUM,ACTADD       ARE WE ADDING?                               
         BE    VREC06              YES, ADD HISTORY ELEMENT                     
*                                                                               
         CLI   PASS,1                                                           
         BE    *+14                                                             
         CP    SVAAMT,OLDAMT       NO, DID THEY CHANGE THE AMOUNT?              
         BE    VREC08              NO, DON'T ADD THIS ELEMENT                   
         LA    R2,AUTAMTH          YES, MAKE SURE NOT OVER FUNDED               
         MVI   ERROR,LESSFUND      SET UP FOR ERROR                             
         CP    SVAAMT,SVFAMT       COMPARE IT TO THE AMOUNT FUNDED              
         BL    ERREXIT             OVER FUNDED AMOUNT, CAN'T CHANGE             
*                                                                               
         USING AUTHELD,R6                                                       
VREC06   LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   AUTHEL,AUTHELQ                                                   
         MVI   AUTHLN,AUTHLNQ                                                   
         MVC   AUTHPER,TWAALIAS                                                 
         ZAP   AUTHAMT,SVAAMT                                                   
*                                                                               
         GOTO1 GETFACT,DMCB,(0,0)  GET CURRENT TIME                             
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         ZAP   DUB,FATIME          SAVE THE TIME                                
         AP    DUB,=P'60000'       ADJUST FOR DDS TIME                          
         CP    DUB,=P'240000'      MAKE SURE WE HAVEN'T GONE TOO FAR            
         BNH   *+10                                                             
         SP    DUB,=P'240000'                                                   
         ZAP   AUTHTIME,DUB        SAVE THE TIME                                
         MVC   AUTHDATE,4(R1)      SAVE THE DATE                                
         MVC   SVDATIME,AUTHDATE                                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=CL8'ACCFIL'),AIO,(R6),=C'ADD=END'              
*                                                                               
VREC08   CLI   PASS,1                                                           
         MVI   PASS,1                                                           
         BNE   VRECX                                                            
*                                                                               
         MVI   ELCODE,UFSELQ                                                    
         GOTO1 REMELEM             REMOVE OLD 'A2' ELEMENTS                     
         BAS   RE,GETTAB           GET ELEMENT TABLE                            
*                                                                               
         LA    R0,10                                                            
         L     R3,AELMTAB                                                       
         LA    R5,AUTUSEHH                                                      
*                                                                               
VREC10   SR    R2,R2                                                            
         IC    R2,0(R5)                                                         
         AR    R2,R5                                                            
         USING AUTUSEHH,R5                                                      
         CLC   AUTUSEH,SPACES      SKIP IF NO HEADER FOR USER FIELD             
         BNH   VREC26                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(UFSDATA-UFSELD),0(R3)                                    
         MVI   ELEMENT+2,X'00'     REMOVE SEQUENCE NUMBER                       
         MVC   ELEMENT+UFSDATA-UFSELD(L'AUTUSED),SPACES                         
         CLI   5(R2),0             MOVE DATA IF ENTERED                         
         BNE   VREC12                                                           
         MVI   ERROR,REQFLD        PREPARE FOR MISSING REQUIRED FIELD           
         TM    UFSSTAT-UFSELD(R3),X'80'                                         
         BO    ERREXIT                                                          
         B     VREC24              NOT ENTERED, ADD WITH BLANK DATA             
*                                                                               
         USING UFSELD,R6                                                        
VREC12   LA    R6,ELEMENT                                                       
         SR    RF,RF                                                            
         IC    RF,5(R2)            GET LENGTH OF INPUT DATA                     
         SR    R1,R1                                                            
         IC    R1,UFSMXLN          GET MAXIMUM LENGTH ALLOWED                   
         CR    RF,R1                                                            
         MVI   ERROR,INP2LONG      ERROR IF INPUT LONGER THAN MAX               
         BH    ERREXIT                                                          
         CLI   UFSEDIT-UFSELD(R3),C'N' VALIDATE FOR NUMERIC IF                  
         BNE   VREC16               EDIT = N                                    
         LR    R1,RF               SAVE INPUT LENGTH                            
         XC    WORK,WORK                                                        
         MVI   ERROR,NOTNUM        PREPARE ERROR MESSAGE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)       MOVE IN JUST ZONES                           
         LA    RE,WORK                                                          
         LR    R1,RF               RESET LENGTH                                 
*                                                                               
VREC14   CLI   0(RE),X'F0'                                                      
         BNE   ERREXIT                                                          
         LA    RE,1(0,RE)                                                       
         BCT   R1,VREC14                                                        
*                                                                               
VREC16   CLI   UFSEDIT-UFSELD(R3),C'D'                                          
         BNE   VREC18                                                           
         MVI   ERROR,INVDATE                                                    
         ST    RF,SAVERF                                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB   VALIDATE FOR DATE IF                 
         OC    DMCB,DMCB                    EDIT = D                            
         BZ    ERREXIT                                                          
         L     RF,SAVERF                                                        
*                                                                               
VREC18   CLI   UFSEDIT-UFSELD(R3),C'C'                                          
         BNE   VREC22                                                           
         LR    R1,RF               SAVE INPUT LENGTH                            
         XC    WORK,WORK                                                        
         MVI   ERROR,NOTCHAR       PREPARE ERROR MESSAGE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)       MOVE IN JUST ZONES                           
         LR    R1,RF               RESET LENGTH                                 
         LA    RE,WORK                                                          
*                                                                               
VREC20   CLI   0(RE),X'F0'                                                      
         BE    ERREXIT                                                          
         LA    RE,1(0,RE)                                                       
         BCT   R1,VREC20                                                        
*                                                                               
VREC22   IC    R1,UFSLN            GET LENGTH OF RECORD                         
         AR    R1,RF               ADD INPUT LENGTH TO RECORD TO                
         STC   R1,UFSLN             GET NEW RECORD LENGTH                       
         BCTR  RF,0                MOVE TO RECORD BASED ON                      
         EX    RF,*+8               INPUT LENGTH                                
         B     *+10                                                             
         MVC   UFSDATA(0),8(R2)    MOVE DATA TO RECORD                          
*                                                                               
VREC24   GOTO1 ADDELEM             ADD THE NEW ELEMENT                          
*                                                                               
VREC26   SR    RF,RF               BUMP TO NEXT HEADER                          
         IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         LA    R3,ELMLNG(R3)                                                    
         BCT   R0,VREC10           DO FOR ALL 10 FIELDS                         
         DROP  R5,R6                                                            
*                                                                               
         MVI   VERUSER,1           USER FIELDS VERIFIED                         
*                                                                               
VRECX    B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              DISPLAY THE RECORD                                     *         
***********************************************************************         
*                                                                               
         USING AUTRECD,R6                                                       
DREC     NTR1                                                                   
         L     R6,AIO                                                           
         MVC   SAVEKEY,0(R6)                                                    
         GOTO1 PERSOUT             DATE OF LAST ACTIVITY                        
         LA    R2,AUTLACTH                                                      
         MVC   8(20,R2),WORK+20                                                 
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         LA    R2,AUTAUTDH         PRINT THE DESCRIPTION                        
         GOTO1 NAMEOUT                                                          
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         CLC   CONACT(4),=C'HIST'  IS THIS ACTION HISTORY?                      
         BE    DREC00              YES, SKIP OVER STATUS                        
*                                                                               
         LA    R2,AUTSTATH                                                      
         MVI   8(R2),C'A'                                                       
         TM    AUTRSTA,AUTRDACT    IS RECORD ACTIVE ?                           
         BZ    *+8                 YES                                          
         MVI   8(R2),C'I'          NO                                           
         OI    6(R2),X'80'                                                      
*                                                                               
         USING AUTHELD,R6                                                       
DREC00   MVI   ELCODE,AUTHELQ      GET DATA ELEMENT                             
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE THIS ONE                           
         ST    R6,SAVER6                                                        
*                                                                               
         CLC   CONACT(4),=C'HIST'  IS THIS ACTION HISTORY?                      
         BNE   DREC06              NO, DISPLAY ALL FIELDS                       
*                                                                               
         USING LND,R2                                                           
         LA    R2,HISDATH          STARTING DATE                                
*                                                                               
DREC02   MVC   LNDDAT,AUTHDATE                                                  
         OI    LNDDATH+6,X'80'                                                  
         MVC   LNDPER,AUTHPER                                                   
         OI    LNDPERH+6,X'80'                                                  
         CURED (P6,AUTHAMT),(12,LNDAAMT),2,ALIGN=RIGHT                          
         OI    LNDAAMTH+6,X'80'                                                 
*                                                                               
         LA    R2,LNDLNQ(R2)                                                    
         BAS   RE,NEXTEL                                                        
         BNE   DREC04              NO MORE, GET PREVIOUS ONE                    
         ST    R6,SAVER6           SAVE ADDRESS AND PRINT IT                    
         BE    DREC02                                                           
*                                                                               
DREC04   L     R6,SAVER6           GET ADDRESS OF LAST AUTHEL                   
         LA    R2,AUTAMTH                                                       
         CURED (P6,AUTHAMT),(12,8(R2)),2,ALIGN=RIGHT                            
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
         B     OKEXIT                                                           
*                                                                               
DREC06   BAS   RE,NEXTEL           ANY MORE?                                    
         BNE   DREC08              NO                                           
         ST    R6,SAVER6           YES, SAVE ADDRESS AND LOOK AGAIN             
         B     DREC06                                                           
*                                                                               
DREC08   L     R6,SAVER6           GET ADDRESS OF LAST AUTHEL                   
         LA    R2,AUTAMTH                                                       
         CURED (P6,AUTHAMT),(12,8(R2)),2,ALIGN=RIGHT                            
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
         MVC   OLDAMT,AUTHAMT                                                   
*                                                                               
         LA    R2,AUTAAMTH                                                      
         CURED (P6,AUTHAMT),(12,8(R2)),2,ALIGN=RIGHT                            
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
         ZAP   DUB,AUTHAMT         SAVE AUTHORIZED AMUNT                        
*                                                                               
         CLI   ACTNUM,ACTADD       ARE WE ADDING?                               
         BE    DREC10              YES, FUND RECORD NOT THERE YET               
         CLI   ACTNUM,ACTDEL       ARE WE DELETING?                             
         BE    DREC10              YES, FUND RECORD NO LONGER THERE             
*                                                                               
         MVC   AIO,AIO2            CHANGE AIO AREA FOR THIS READS               
*                                                                               
         USING FUNRECD,R6                                                       
         LA    R6,KEY              READ THE FUNDING RECORD                      
         MVC   FUNKEY,SPACES                                                    
         MVC   FUNKEY,SAVEKEY      USE AUTHORIZATION KEY                        
         MVI   FUNKTYP,FUNKTYPQ    BUT CHANGE TYPE AND SUBTYPE                  
         MVI   FUNKSUB,FUNKSUBQ                                                 
         GOTO1 READ                                                             
*                                                                               
         USING FUNELD,R6                                                        
         MVI   ELCODE,FUNELQ       GET DATA ELEMENT                             
         BAS   RE,GETELIO                                                       
         BE    *+6                 HAVE TO HAVE THIS ALSO                       
         DC    H'0'                                                             
*                                                                               
         LA    R2,AUTFAMTH                                                      
         CURED (P6,FUNAMT),(12,8(R2)),2,ALIGN=RIGHT                             
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
         SP    DUB,FUNAMT          GET THE DIFFERENCE                           
         ZAP   SVFAMT,FUNAMT       SAVE THE FUNDED AMOUNT                       
*                                                                               
DREC10   LA    R2,AUTUAMTH                                                      
         CURED (P8,DUB),(12,8(R2)),2,ALIGN=RIGHT                                
         OI    6(R2),X'80'         TRANSMIT THIS FIELD                          
         MVC   AIO,AIO1            CHANGE AIO BACK                              
*                                                                               
         BAS   RE,CLEARU                                                        
         BAS   RE,DISUSER          DISPLAY USER HEADERS                         
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,USERDATA          AND USER SELECT DATA                        
         BAS   RE,PUTTAB           SAVE ELEMENT TABLE                           
         MVI   PASS,1                                                           
*                                                                               
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              PROCESS PF KEYS                                        *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,AUTNUMH                                                       
         CLI   PFKEY,PF2           JOB FUNDING                                  
         BE    PROCPF2                                                          
         CLI   PFKEY,PF1           AUTHORIZATION HISTORY                        
         BNE   PROCPFX                                                          
*                                                                               
PROCPF1  MVI   PFKEY,0             AUTHORIZATION HISTORY                        
         GOTO1 VCALL,WORK,=C'AUTH',=C'HISTORY',                        X        
               (1,SVOGR),(2,SVOFC),(6,SVCLI),(6,SVPRO),                X        
               (1,SVMGR),(1,SVMED),(19,AUTNUM),0                                
*                                                                               
PROCPF2  MVI   PFKEY,0             FUND MAINTENANCE                             
         GOTO1 VCALL,WORK,=C'FUND',=C'MAINT',                          X        
               (1,SVOGR),(2,SVOFC),(6,SVCLI),(6,SVPRO),                X        
               (1,SVMGR),(1,SVMED),(19,AUTNUM),0                                
*                                                                               
PROCPFX  B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              SEE IF OFFICE, CLIENT OR PRODUCT CHANGED               *         
***********************************************************************         
*                                                                               
TSTCHG   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO                                           
         MVI   KEYCHG,C'Y'         YES, INDICATE SO                             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              CLEAR USER TABLE AND SCREEN DATA                       *         
***********************************************************************         
*                                                                               
CLEARU   NTR1                                                                   
         BAS   RE,GETTAB           GET ELEMENT TABLE                            
*                                                                               
         L     R2,AELMTAB          CLEAR TABLE OF USER ELEMENTS                 
         LA    R3,320                                                           
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         LA    R2,AUTUSEHH                                                      
         LA    R0,10               CLEAR USER SELECT HEADERS AND DATA           
*                                                                               
DCLEAR   XC    8(L'AUTUSEH,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         BAS   RE,UBUMP            SKIP TO NEXT FIELD                           
         XC    8(L'AUTUSED,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         BAS   RE,UBUMP                                                         
         BCT   R0,DCLEAR                                                        
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              DISPLAY SELECTIVE USER HEADERS                         *         
***********************************************************************         
*                                                                               
         USING USERD,R5                                                         
DISUSER  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         ZAP   ELMCNT,=P'0'        CLEAR COUNTER                                
         LA    R5,USERKEY          LOOK FOR COMPANY LEVEL                       
         XC    USERKEY,USERKEY                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(AUTKOGR-AUTKEY),KEY  EXIT IF NO RECS                     
         BNE   OKEXIT                                                           
         CLC   KEYSAVE(L'AUTKEY),KEY                                            
         BNE   DISMG                                                            
         BAS   RE,USERTABL         SAVE HEADERS IF FOUND                        
*                                                                               
DISMG    OC    MGROUP,MGROUP       LOOK FOR COMPANY, MEDIA GROUP                
         BZ    DISMED                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USERMGR,MGROUP                                                   
         BAS   RE,READHIU                                                       
         BNE   DISMED                                                           
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
*                                                                               
DISMED   OC    MEDIA,MEDIA         LOOK FOR COMPANY, MEDIA                      
         BZ    DISOGR                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   DISOGR                                                           
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
*                                                                               
DISOGR   OC    EFFOFG,EFFOFG       LOOK FOR COMPANY, OFFICE GROUP               
         BZ    DISOFF                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USEROGR,EFFOFG                                                   
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(AUTKOFC-AUTKEY),KEY  SKIP IF NO OGR RECS                 
         BNE   DISOFF                                                           
         CLC   KEYSAVE(L'AUTKEY),KEY                                            
         BNE   DUOG020                                                          
         BAS   RE,USERTABL                                                      
*                                                                               
DUOG020  OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE GROUP,              
         BZ    DUOG040              MEDIA GROUP                                 
         MVC   USERMGR,MGROUP                                                   
         BAS   RE,READHIU                                                       
         BNE   DUOG040                                                          
         BAS   RE,USERTABL                                                      
*                                                                               
DUOG040  OC    MEDIA,MEDIA         LOOK FOR COMPANY, OFFICE GROUP,              
         BZ    DISOFF               MEDIA                                       
         XC    USERMGR,USERMGR                                                  
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   DISOFF                                                           
         BAS   RE,USERTABL                                                      
*                                                                               
DISOFF   CLC   EFFOFFC,SPACES      LOOK FOR COMPANY, OFFICE                     
         BNH   DISCLI                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USEROFC,EFFOFFC                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(AUTKCLI-AUTKEY),KEY                                      
         BNE   DISCLI              SKIP IF NO OFFICE RECORDS                    
         CLC   KEYSAVE(L'AUTKEY),KEY                                            
         BNE   DUOF020                                                          
         BAS   RE,USERTABL                                                      
*                                                                               
DUOF020  OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE,                    
         BZ    DUOF040              MEDIA GROUP                                 
         MVC   USERMGR,MGROUP                                                   
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL                                                      
         XC    USERMGR,USERMGR                                                  
*                                                                               
DUOF040  OC    MEDIA,MEDIA         LOOK FOR COMPANY, OFFICE,                    
         BZ    DISCLI               MEDIA                                       
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(L'AUTKEY),KEY                                            
         BNE   DISCLI                                                           
         BAS   RE,USERTABL                                                      
*                                                                               
DISCLI   XC    USERKEY,USERKEY     LOOK FOR COMPANY, CLIENT                     
         CLC   CLICODE,SPACES                                                   
         BNH   DISHEAD                                                          
         MVC   USERCLI,CLICODE                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(AUTKPRO-AUTKEY),KEY                                      
         BNE   DISHEAD             SKIP IF NO CLIENT RECS                       
         CLC   KEYSAVE(L'AUTKEY),KEY                                            
         BNE   DISC02                                                           
         BAS   RE,USERTABL                                                      
*                                                                               
DISC02   OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT, MEDIA              
         BZ    DISC04               GROUP                                       
         MVC   USERMGR,MGROUP                                                   
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL                                                      
         XC    USERMGR,USERMGR                                                  
*                                                                               
DISC04   OC    MEDIA,MEDIA         LOOK FOR COMPANY, CLIENT, MEDIA              
         BZ    DISC06                                                           
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL                                                      
         XC    USERMED,USERMED                                                  
*                                                                               
DISC06   CLC   PRODCODE,SPACES                                                  
         BNH   DISHEAD                                                          
         MVC   USERPRO,PRODCODE    LOOK FOR COMPANY, CLIENT,                    
         BAS   RE,READHIU           PRODUCT                                     
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT,                    
         BZ    DISC08               PRODUCT, MEDIA GROUP                        
         MVC   USERMGR,MGROUP                                                   
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL                                                      
         XC    USERMGR,USERMGR                                                  
*                                                                               
DISC08   OC    MEDIA,MEDIA         LOOK FOR COMPANY, CLIENT,                    
         BZ    DISHEAD              PRODUCT, MEDIA                              
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL                                                      
         XC    USERMED,USERMED                                                  
*                                                                               
DISHEAD  BAS   RE,USERHEAD                                                      
         MVC   AIO,AIO1                                                         
         B     OKEXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              ADD FUNDING RECORD AND ELEMENT W/0 DOLLARS             *         
***********************************************************************         
*                                                                               
         USING FUNRECD,R6                                                       
ADDFUND  NTR1                                                                   
         LR    R6,R2               SAVE CURSOR POSITION                         
*                                                                               
         L     R2,AIO2             CLEAR AIO2                                   
         LA    R3,2000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         LR    R2,R6               RESTORE CURSOR                               
*                                                                               
         MVC   AIO,AIO2                                                         
         L     RF,AIO1                                                          
         LA    R6,KEY                                                           
         MVC   FUNKEY,SPACES       BUILD THE KEY                                
         MVC   FUNKEY,0(RF)        GET KEY OF AUTH RECORD                       
         MVI   FUNKTYP,FUNKTYPQ    MODIFY FOR FUND                              
         MVI   FUNKSUB,FUNKSUBQ                                                 
*                                                                               
         OI    DMINBTS,X'08'       SEE IF RECORD ALREADY THERE                  
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVC   FUNCTION,=CL8'DMWRT'                                             
         CLC   KEYSAVE(42),KEY                                                  
         BE    ADDF02              IT'S THERE, UPDATE IT                        
         MVC   FUNCTION,=CL8'DMADD'                                             
*                                                                               
         L     R2,AIO2             NOT THERE, CLEAR IO AREA                     
         LA    R3,2000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         MVC   FUNKEY,KEYSAVE      REBUILD THE KEY                              
         B     *+8                                                              
*                                                                               
ADDF02   NI    ACCOSTAT(R6),X'7F'                                               
         GOTO1 PERSIN                                                           
*                                                                               
         USING FUNELD,R6                                                        
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FUNEL,FUNELQ                                                     
         MVI   FUNLN,FUNLNQ                                                     
         MVC   FUNDATE(L'FUNDATE+L'FUNTIME),SVDATIME                            
         ZAP   FUNAMT,=P'0'        INITIALIZE TO ZEROS                          
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,FUNCTION,=C'ACCFIL ',(R6),(R6),DMWORK               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVC   KEY,0(R6)                                                        
*                                                                               
ADDFX    B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                 BUMP UP TO NEXT USER SELECT FIELD                   *         
***********************************************************************         
*                                                                               
UBUMP    SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*                      FORMAT KEY AND READ HIGH                       *         
***********************************************************************         
*                                                                               
READHIU  NTR1                                                                   
         LA    R6,KEY                                                           
         USING UFSRECD,R6                                                       
         MVC   UFSKEY,USERKEY                                                   
         MVI   UFSKTYP,UFSKTYPQ                                                 
         MVI   UFSKSUB,UFSKSUBQ                                                 
         MVC   UFSKCPY(3),CUL                                                   
         GOTO1 HIGH                                                             
         CLC   UFSKEY,KEYSAVE                                                   
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                  GET USER SELECT HEADINGS INTO TABLE                *         
***********************************************************************         
*                                                                               
         USING ELMTABD,R3                                                       
         USING UFSELD,R6                                                        
USERTABL NTR1                                                                   
         MVI   ELCODE,UFSELQ                                                    
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
USET020  BAS   RE,NEXTEL                                                        
         BNE   OKEXIT                                                           
         TM    UFSSTAT,UFSSAUTH    ONLY TAKE THESE USER FIELDS                  
         BZ    USET020                                                          
         CLC   UFSDESC,SPACES                                                   
         BE    USET020                                                          
         L     R3,AELMTAB                                                       
         LA    R0,10                                                            
*                                                                               
USET040  OC    ELMDATA,ELMDATA     IS THIS SPOT FILLED?                         
         BZ    USET080             NO, KEEP LOOKING                             
         CLC   UFSCODE,ELMCODE     YES, DO THE CODES MATCH ?                    
         BNE   USET080             NO                                           
         OC    UFSCUT,UFSCUT       YES, DO WE HAVE A CUTOFF DATE ?              
         BZ    USET060             NO, REPLACE ELEMENT                          
         CLC   UFSCUT,TODAYP       YES, CAN WE TAKE IT ?                        
         BH    USET060             YES                                          
*                                                                               
         BCTR  R0,0                                                             
         MVC   ELMDATA,ELMLNG(R3)                                               
         LA    R3,ELMLNG(R3)                                                    
         BCT   R0,*-10                                                          
         XC    ELMDATA,ELMDATA     CLEAR LAST SPOT                              
         SP    ELMCNT,=P'1'        DECREASE THE COUNT BY 1                      
         B     USET020             GET NEXT ELEMENT                             
*                                                                               
USET060  MVC   ELMDATA,0(R6)       REPLACE ELEMENT                              
         B     USET020             NEXT NEXT ONE                                
*                                                                               
USET080  LA    R3,ELMLNG(R3)       GET NEXT                                     
         BCT   R0,USET040                                                       
*                                                                               
         L     R3,AELMTAB          NO MATCH, LOOK FOR VACANT SPOT               
         LA    R0,10                                                            
*                                                                               
USET100  OC    ELMDATA,ELMDATA     IS THIS SPOT FILLED ?                        
         BNZ   USET120             YES, KEEP LOOKING                            
         OC    UFSCUT,UFSCUT       NO, DO WE HAVE A CUTOFF DATE ?               
         BZ    USET110             NO, ADD ELEMENT                              
         CLC   UFSCUT,TODAY        YES, CAN WE TAKE IT ?                        
         BNH   USET020             NO                                           
*                                                                               
USET110  MVC   ELMDATA,0(R6)       ADD ELEMENT                                  
         AP    ELMCNT,=P'1'        KEEP A COUNT                                 
         B     USET020             GET NEXT ELEMENT                             
*                                                                               
USET120  LA    R3,ELMLNG(R3)       GET NEXT                                     
         BCT   R0,USET100                                                       
         B     OKEXIT                                                           
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              DISPLAY USER FIELD HEADINGS FROM TABLE                 *         
***********************************************************************         
*                                                                               
USERHEAD NTR1                                                                   
         LA    R0,10                                                            
         LA    R2,AUTUSEHH                                                      
         L     R3,AELMTAB                                                       
*                                                                               
         USING AUTUSEHH,R2                                                      
         USING ELMTABD,R3                                                       
USEH020  OC    ELMDATA,ELMDATA                                                  
         BZ    USEH040                                                          
         MVC   AUTUSEH,6(R3)       MOVE HEADER TO SCREEN                        
         OI    AUTUSEHH+6,X'80'    TRANSMIT FIELD                               
         NI    AUTUSEHH+1,X'FF'-X'08'    MAKE SURE LOW INTENSITY                
         TM    ELMSTAT,X'80'       IS FIELD REQUIRED ?                          
         BZ    *+8                 NO                                           
         OI    AUTUSEHH+1,X'08'    YES, MAKE HIGH INTENSITY                     
         BAS   RE,UBUMP                                                         
         BAS   RE,UBUMP                                                         
*                                                                               
USEH040  LA    R3,ELMLNG(R3)                                                    
         BCT   R0,USEH020                                                       
         B     OKEXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*              GET USER FIELD DATA                                    *         
***********************************************************************         
*                                                                               
         USING UFSELD,R6                                                        
USERDATA NTR1                                                                   
         CP    ELMCNT,=P'0'                                                     
         BZ    OKEXIT                                                           
         MVI   ELCODE,UFSELQ                                                    
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
USED020  BAS   RE,NEXTEL                                                        
         BNE   OKEXIT                                                           
         TM    UFSSTAT,UFSSAUTH                                                 
         BZ    USED020                                                          
         LA    R0,10                                                            
         LA    R2,AUTUSEHH                                                      
         L     R3,AELMTAB                                                       
*                                                                               
         USING AUTUSEHH,R2                                                      
USED040  CLC   UFSCODE,UFSCODE-UFSELD(R3) MATCH ON USER CODE                    
         BNE   USED060                                                          
         SR    R1,R1                                                            
         IC    R1,UFSLN            GET RECORD LENGTH                            
         SH    R1,=AL2(UFSDATA-UFSEL) SUBTRACT LENGTH LESS DATA                 
         BZ    USED020             NO DATA, GET NEXT ELEMENT                    
         BCTR  R1,0                RESULT IS LENGTH OF DATA                     
         EX    R1,*+8                                                           
         B     USED020                                                          
         MVC   AUTUSED(0),UFSDATA                                               
*                                                                               
USED060  BAS   RE,UBUMP            SKIP TO DATA                                 
         BAS   RE,UBUMP            SKIP TO NEXT HEADER                          
         LA    R3,ELMLNG(R3)       SKIP TO NEXT TABLE ENTRY                     
         BCT   R0,USED040                                                       
         B     USED020                                                          
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              ACTION = DELETE                                        *         
*                                                                     *         
*              VERIFY THAT AUTHORIZATION RECORD EXISTS.               *         
*              READ FOR FUND RECORD AND MAKE SURE NO JOBS ARE FUNDED. *         
*              IF OK, DELETE AUTHORIZATION AND FUND RECORDS.          *         
*              IF NG, GENERATE ERROR MESSAGE.                         *         
***********************************************************************         
*                                                                               
DELEIT   LA    R2,AUTNUMH                                                       
         BAS   RE,VKEY                                                          
*                                                                               
         GOTO1 READ                GET RECORD IN BUFFER                         
*                                                                               
         MVC   AIO,AIO2            CHANGE AIO AREA FOR THIS READS               
*                                                                               
         USING FUNRECD,R6                                                       
         LA    R6,KEY              READ THE FUNDING RECORD                      
         MVC   FUNKEY,SPACES                                                    
         MVC   FUNKEY,SAVEKEY      USE AUTHORIZATION KEY                        
         MVI   FUNKTYP,FUNKTYPQ    BUT CHANGE TYPE AND SUBTYPE                  
         MVI   FUNKSUB,FUNKSUBQ                                                 
         GOTO1 READ                READ FOR FUNDING RECORD                      
*                                                                               
         USING FUNELD,R6                                                        
         MVI   ELCODE,FUNELQ       GET DATA ELEMENT                             
         BAS   RE,GETELIO                                                       
         BE    *+6                 HAVE TO HAVE THIS ALSO                       
         DC    H'0'                                                             
*                                                                               
         USING FJNELD,R6                                                        
         MVI   ELCODE,FJNELQ       ANY JOBS FUNDED YET?                         
         BAS   RE,GETELIO                                                       
         BNE   DELE02              NO, OK TO DELETE                             
         MVI   ERROR,AUTHFUN                                                    
         B     ERREXIT                                                          
*                                                                               
DELE02   L     R6,AIO              GET FUNDING RECORD                           
         OI    ACCOSTAT(R6),X'80'  MARK FOR DELETION                            
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AIO,AIO1            GET AUTHORIZATION RECORD                     
         L     R6,AIO                                                           
         OI    ACCOSTAT(R6),X'80'  MARK FOR DELETION                            
         GOTO1 WRITE                                                            
*                                                                               
         BAS   RE,DREC             DISPLAY UPDATED RECORD                       
         MVI   MYMSGNO1,IAUTDEL    SET UP MESSAGE                               
         B     INFEXIT                                                          
         EJECT                                                                  
***********************************************************************         
*              ACTION = RESTORE                                       *         
*                                                                     *         
*              VERIFY THAT AUTHORIZATION RECORD EXISTS & IS DELETED.  *         
*              READ FOR FUND RECORD AND MAKE SURE NO JOBS ARE FUNDED. *         
*              IF OK, DELETE AUTHORIZATION AND FUND RECORDS.          *         
*              IF NG, GENERATE ERROR MESSAGE.                         *         
***********************************************************************         
*                                                                               
RESTIT   LA    R2,AUTNUMH                                                       
         BAS   RE,VKEY                                                          
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 READ                READ THE RECORD FIRST                        
*                                                                               
         MVC   AIO,AIO2            CHANGE AIO AREA FOR THIS READS               
*                                                                               
         USING FUNRECD,R6                                                       
         LA    R6,KEY              READ THE FUNDING RECORD                      
         MVC   FUNKEY,SPACES                                                    
         MVC   FUNKEY,SAVEKEY      USE AUTHORIZATION KEY                        
         MVI   FUNKTYP,FUNKTYPQ    BUT CHANGE TYPE AND SUBTYPE                  
         MVI   FUNKSUB,FUNKSUBQ                                                 
         GOTO1 READ                READ FOR FUNDING RECORD                      
*                                                                               
         TM    ACCOSTAT(R6),X'80'  IS IT DELETED?                               
         BO    *+12                IT MUST BE                                   
         MVI   ERROR,RECNTDEL                                                   
         B     ERREXIT                                                          
*                                                                               
         L     R6,AIO              GET FUNDING RECORD                           
         NI    ACCOSTAT(R6),X'FF'-X'80'  UNMARK THE DELETION                    
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AIO,AIO1            GET AUTHORIZATION RECORD                     
         L     R6,AIO                                                           
         NI    ACCOSTAT(R6),X'FF'-X'80'  UNMARK THE DELETION                    
         GOTO1 WRITE                                                            
*                                                                               
         BAS   RE,DREC             DISPLAY UPDATED RECORD                       
         MVI   MYMSGNO1,IAUTRES    SET UP MESSAGE                               
         B     INFEXIT                                                          
         EJECT                                                                  
***********************************************************************         
*              LOAD THE BUFFER WHERE ELMTAB WILL BE SAVED             *         
***********************************************************************         
*                                                                               
LOAD     NTR1                                                                   
         L     RE,=V(DUMMY)                                                     
         A     RE,MYRELO                                                        
         ST    RE,DMCB             SET LOAD POINT FOR BUFFER                    
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'        SET SET BUFFER OVERLAY NUMBER                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'        IS LOAD OK ?                                 
         BNE   *+6                 YES                                          
         DC    H'0'                NO, BLOW UP                                  
         MVC   AELMTAB,DMCB        SAVE THE ADDRESS                             
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*              PUT THE ELMTAB TO TWA 2                                          
***********************************************************************         
*                                                                               
PUTTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2            PAGE = TWA2                                  
         MVC   DMCB+10(2),TERM     TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=C'TEMPSTR',,AELMTAB                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              GET THE ELMTAB BACK FROM TWA2                          *         
***********************************************************************         
*                                                                               
GETTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2                                                         
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'TEMPSTR',,BUFF                      
         LA    RE,BUFF                                                          
         ST    RE,AELMTAB                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
OKEXIT   CR    R8,R8                                                            
EXIT     XIT1                                                                   
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
NOREST   MVI   ERROR,RECNTDEL                                                   
         B     ERREXIT                                                          
*                                                                               
NODELETE MVI   ERROR,CANTDEL                                                    
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
INFEXIT  ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 INFOXIT                                                          
*                                                                               
MYRELO   DS    A                   RELO                                         
TIMEOUT  DS    A                   TIMEOUT                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        DATAMGR COMMANDS                                                       
ADDDM    DC    CL8'DMADD'                                                       
READDM   DC    CL8'DMREAD'                                                      
         LTORG                                                                  
         EJECT                                                                  
USERD    DSECT                                                                  
         DS    CL1                                                              
         DS    CL1                                                              
         DS    CL3                                                              
USEROGR  DS    CL1                                                              
USEROFC  DS    CL2                                                              
USERCLI  DS    CL6                                                              
USERPRO  DS    CL6                                                              
USERMGR  DS    CL1                                                              
USERMED  DS    CL1                                                              
         EJECT                                                                  
ELMTABD  DSECT                                                                  
ELMDATA  DS    0CL32                                                            
         DS    CL4                                                              
ELMCODE  DS    CL2                                                              
ELMDESC  DS    CL12                                                             
ELMEDIT  DS    CL8                                                              
         DS    CL1                                                              
ELMSTAT  DS    XL1                                                              
         DS    CL4                                                              
ELMLNG   EQU   *-ELMDATA                                                        
         EJECT                                                                  
LND      DSECT                                                                  
LNDDATH  DS    CL8                                                              
LNDDAT   DS    CL8                                                              
LNDPERH  DS    CL8                                                              
LNDPER   DS    CL8                                                              
LNDAAMTH DS    CL8                                                              
LNDAAMT  DS    CL12                                                             
LNDLNQ   EQU   *-LND                                                            
         EJECT                                                                  
HISTRYD  DSECT                                                                  
HISTBLK  DS    50CL80                                                           
HISTLNQ  EQU   *-HISTBLK                                                        
         EJECT                                                                  
MYD      DSECT                                                                  
KEYCHG   DS    C                                                                
SAVER6   DS    A                   SAVE AREA FOR R6                             
SAVERE   DS    A                   SAVE AREA FOR RE                             
SAVERF   DS    A                   SAVE AREA FOR RF                             
SVDATIME DS    CL12                SAVE DATE AND TIME FOR FUNEL                 
FUNCTION DS    CL8                 SAVE DATAMGR COMMAND                         
USERKEY  DS    CL42                                                             
TODAY    DS    CL3                                                              
FLDH     DS    CL8                                                              
FLD      DS    CL80                                                             
*                                                                               
MYDEND   EQU   *                                                                
         EJECT                                                                  
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACPROWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROBED                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROBBD                                                       
         ORG   T60BFFD+2304                                                     
SVFLDS   DS    0CL(SVFLDLQ)                                                     
SVOGR    DS    CL1                 OFFICE GROUP                                 
SVOFC    DS    CL2                 OFFICE                                       
SVCLI    DS    CL6                 CLIENT                                       
SVPRO    DS    CL6                 PRODUCT                                      
SVMGR    DS    CL1                 MEDIA GROUP                                  
SVMED    DS    CL1                 MEDIA                                        
SVAUTH   DS    CL19                AUTHORIZATION NUMBER                         
SVAAMT   DS    PL6                 AUTHORIZATION AMOUNT                         
SVFLDLQ  EQU   *-SVOGR                                                          
*                                                                               
ELMCNT   DS    PL8                 COUNT OF ELMTAB ENTRIES                      
AELMTAB  DS    A                   A(ELEMENT TABLE)                             
VERUSER  DS    X                   FLAG FOR USER FIELD VERIFICATION             
SAVEKEY  DS    CL42                SAVE AREA FOR KEY                            
PASS     DS    X                                                                
OLDAMT   DS    PL6                 OLD AUTHORIZATION AMOUNT                     
SVFAMT   DS    PL6                 FUNDED AMOUNT                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACPRO64   08/22/02'                                      
         END                                                                    
