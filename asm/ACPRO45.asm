*          DATA SET ACPRO45    AT LEVEL 058 AS OF 04/30/97                      
*PHASE T60B45A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B45 - STUDIO LINK RECORD MAINT'                              
T60B45   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B45**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         ST    R2,RELO                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
*                                                                               
         L     R2,ATWA                                                          
         AH    R2,CURDISP                                                       
         ST    R2,ACURSOR                                                       
*                                                                               
         CLI   EMULATE,C'Y'        OLD ACC FILE                                 
         BE    LNK05               CAN'T USE THIS                               
         MVC   CONHEAD(L'OLDACC),OLDACC                                         
         B     MYERROR                                                          
*                                                                               
LNK05    BAS   RE,SETOLDAC                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   LNK10                                                            
         BAS   RE,VKEY                                                          
         B     LNKX                                                             
LNK10    CLI   MODE,VALREC                                                      
         BNE   LNKX                                                             
         BAS   RE,VREC                                                          
*                                                                               
LNKX     BAS   RE,XMITSCRN                                                      
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* VALKEY LOGIC                                                                  
*----------------------------------------------------------------------         
*                                                                               
VKEY     NTR1                                                                   
*                                                                               
         LA    RE,TABLE                                                         
         ST    RE,APOTAB                                                        
*                                                                               
         BAS   RE,VALHED                                                        
*                                                                               
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BE    VK20                                                             
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   VKEYX                                                            
*                                                                               
VK20     MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
*                                                                               
         BAS   RE,CLRSCRN          CLEAR SCREEN DATA                            
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* VALREC LOGIC-DISPLAY OR EDIT                                                  
*----------------------------------------------------------------------         
*                                                                               
VREC     NTR1                                                                   
*                                                                               
         LA    RE,TABLE                                                         
         ST    RE,APOTAB                                                        
*                                                                               
         GOTO1 VALNSAVE,DMCB,PROSCLIH,STDATA REVAL STUDIO JOB                   
*                                                                               
         XC    LNKMODE,LNKMODE                                                  
         BAS   RE,CHKLINK          SET AGENCY JOB, LINK IF NEEDED               
*                                                                               
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BNE   VREC05                                                           
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
         CLI   LNKMODE,LNKADD      DO I NEED TO ADD A JOB LINK                  
         BNE   VREC02              NO                                           
*                                                                               
         BAS   RE,LINKJOBS         WRITE JOB LINK ELEMENTS                      
         MVI   MYMSG,LNKADD                                                     
         B     VRECX                                                            
*                                                                               
VREC02   BAS   RE,INITTBL          PREVIOUSLY ESTABLISHED LINK                  
*                                                                               
         BAS   RE,BLDTBL           LIST ORDERS                                  
*                                                                               
         BAS   RE,DISTBL                                                        
*                                                                               
         B     VRECX                                                            
*                                                                               
*                                  PROCESS SCREEN DATA                          
VREC05   BAS   RE,PROCPF           PROCESS PF KEYS                              
*                                                                               
         BAS   RE,LINKJOBS         WRITE JOB LINK ELEMENTS                      
*                                                                               
         MVC   MYMSG,LNKMODE       SAVE ACTION HERE                             
*                                                                               
         BAS   RE,EDTSCR           EDIT PURCHACE ORDER FIELDS                   
*                                                                               
         BAS   RE,BLDTBL           REFRESH DISPLAY                              
*                                                                               
         BAS   RE,CLRSCR           CLEAR PO DATA                                
*                                                                               
         BAS   RE,DISTBL                                                        
*                                                                               
VRECX    CLI   MYMSG,LNKADD                                                     
         BNE   VRECX10                                                          
         MVC   CONHEAD(L'ADDMSG),ADDMSG                                         
         B     VRECXX                                                           
*                                                                               
VRECX10  CLI   MYMSG,LNKDEL                                                     
         BNE   VRECX20                                                          
         MVC   CONHEAD(L'DELMSG),DELMSG                                         
         B     VRECXX                                                           
*                                                                               
VRECX20  OC    PFACTION,PFACTION   GLOBAL OPEN/CLOSE                            
         BNZ   VRECX25             CHANGES COMPLETED                            
*                                                                               
         CLI   MYMSG,C'A'          PO ACTION PROCESSED                          
         BNE   VRECX30             NO "DISPLAY" MESSAGE                         
*                                                                               
VRECX25  MVC   CONHEAD(L'CHAMSG),CHAMSG                                         
         B     VRECXX                                                           
*                                                                               
VRECX30  MVC   CONHEAD(L'DISMSG),DISMSG                                         
         B     VRECXX                                                           
*                                                                               
VRECXX   LA    R2,PROACT1H                                                      
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO VALIDATE THE STUDIO JOB                                        
*----------------------------------------------------------------------         
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         BAS   RE,TSTKEY           SEE IF ANY KEY VALUES CHANGED                
*                                                                               
         GOTO1 VALNSAVE,DMCB,PROSCLIH,STDATA                                    
*                                                                               
VALHED6  EQU   *                                                                
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   LA    R2,PROSCLIH                                                      
         BAS   R1,TESTIT                                                        
         LA    R2,PROSCLIH                                                      
         BAS   R1,TESTIT                                                        
         LA    R2,PROSJOBH                                                      
         LR    R1,RE                                                            
*                                                                               
TESTIT   TM    4(R2),X'80'         I/P THIS TIME                                
         BNOR  R1                  NO, TEST NEXT FIELD                          
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                  RETURN FROM TSTKEY                           
         EJECT                                                                  
VALACT   NTR1                                                                   
         LA    R4,SCRMAX                                                        
         LA    R2,PROACT1H                                                      
VA10     BAS   RE,SETLIN                                                        
         L     R2,AACT                                                          
         CLI   5(R2),0                                                          
         BE    VA20                COOL                                         
         CLI   8(R2),C'*'                                                       
         BE    VA20                                                             
         CLI   8(R2),C'O'                                                       
         BE    VA15                                                             
         CLI   8(R2),C'C'                                                       
         BE    VA15                                                             
         CLI   8(R2),C'U'                                                       
         BNE   VA30                                                             
*                                                                               
VA15     MVI   MYMSG,C'A'          SAVE ACTION                                  
*                                                                               
VA20     L     R2,ANEXTLIN                                                      
         BCT   R4,VA10                                                          
         B     XIT                                                              
*                                                                               
VA30     MVC   CONHEAD(L'BADACT),BADACT                                         
         B     MYERROR                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* READ AN ORDER RECORD                                                          
*----------------------------------------------------------------------         
*                                                                               
VALORD   NTR1                                                                   
         L     R2,APONUM                                                        
         XR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ORDNUM,SPACES                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ORDNUM(0),8(R2)                                                  
         BAS   RE,POKEY                                                         
         GOTO1 READ                                                             
*                                                                               
         USING ORDRECD,R6                                                       
         L     R6,AIOKEY                                                        
         TM    ORDKSTAT,ORDSLDEL   IS ORDER DELETED                             
         BO    VOERR5                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING ORDELD,R6                                                        
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   VOERR2                                                           
         CLC   ORDJOB+1(2),CUL+1   IS THIS A PRODUCION PO                       
         BNE   VOERR3                                                           
         CLC   ORDJOB+3(12),AGJOB12 IS THIS ORDER FOR THIS AGENCY JB            
         BNE   VOERR2                                                           
*                                                                               
         MVI   ELCODE,LNKELQ       GET LINK EL                                  
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
*                                                                               
         USING LNKELD,R6                                                        
         CLC   LNKSTJB,STJOB12                                                  
         BNE   VOERR4                                                           
         CLC   LNKAGJB,AGJOB12                                                  
         BNE   VOERR4                                                           
*                                                                               
         B     XIT                                                              
*                                                                               
VOERR    BAS   RE,SETOLDAC                                                      
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 VERRCUR                                                          
*                                                                               
         USING ORDELD,R6                                                        
VOERR2   MVC   CONHEAD(L'WRNGJOB),WRNGJOB                                       
         MVC   CONHEAD+39(12),ORDJOB+3                                          
         L     R2,APONUM                                                        
         B     MYERROR                                                          
*                                                                               
VOERR3   MVC   CONHEAD(L'EXPACC),EXPACC                                         
         L     R2,APONUM                                                        
         B     MYERROR                                                          
*                                                                               
         USING LNKELD,R6                                                        
VOERR4   MVC   CONHEAD(L'ALRDLNK),ALRDLNK                                       
         MVC   CONHEAD+24(12),LNKSTJB                                           
         MVC   CONHEAD+37(12),LNKAGJB                                           
         L     R2,APONUM                                                        
         B     MYERROR                                                          
*                                                                               
VOERR5   MVC   CONHEAD(L'ORDISDEL),ORDISDEL                                     
         L     R2,APONUM                                                        
         B     MYERROR                                                          
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* MAKE SURE ORDNUM IS NOT ALREADY IN THE TABLE                                  
*----------------------------------------------------------------------         
*                                                                               
POCHKDUP NTR1                                                                   
         USING POTABD,R5                                                        
         L     R5,APOTAB                                                        
POC20    CLI   0(R5),X'FF'                                                      
         BE    XIT                 YES, ALL DOME                                
*                                                                               
         CLC   PONUM,ORDNUM                                                     
         BE    POCERR                                                           
         LA    R5,PONEXT                                                        
         B     POC20                                                            
*                                                                               
POCERR   MVC   CONHEAD(L'DUPPO),DUPPO                                           
         MVC   CONHEAD+23(6),ORDNUM                                             
         L     R2,APONUM                                                        
         B     MYERROR                                                          
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* CHECK IF A LINK EXISTS ON A STUDIO JOB.                                       
*   IF IT DOES VALIDATE AGENCY JOB AND WRITE TO SCREEN, PROCTECTED              
*   SET LINKSTAT                                                                
*----------------------------------------------------------------------         
*                                                                               
         USING LNKELD,R6                                                        
CHKLINK  NTR1                                                                   
         GOTO1 RDOPT,STDATA        GET STUDIO JOB OPTIONS                       
*                                                                               
         CLI   GOSTUDIO,C'Y'                                                    
         BNE   CLINK500                                                         
*                                                                               
         TM    STJOBSTA,JOBSXJOB   IS THIS AN EXPENSE JOB                       
         BO    CLINK500            YES                                          
*                                                                               
         MVI   ELCODE,LNKELQ       GET LINK EL FROM STUDIO JOB                  
         BAS   RE,GETELIO                                                       
         BNE   CLINK300            LINK NOT FOUND, INVALID STUDIO JOB           
*                                                                               
VK10     MVC   STUDTYPE,LNKSTUD                                                 
         BAS   RE,DISTYPE                                                       
*                                                                               
         OC    LNKAGJB,LNKAGJB     AGENCY JOB SET                               
         BZ    CL50                NO, ASK FOR ONE                              
*                                                                               
         BAS   RE,SETAJOB          SET AGENCY JOB FIELDS FROM ELEMENT           
         B     CL60                                                             
*                                                                               
CL50     LA    R2,PROACLIH         GET NICE MESSAGE IF NO I/P                   
         CLI   5(R2),0                                                          
         BE    CLINK100                                                         
         CLI   8(R2),C' '                                                       
         BNH   CLINK100                                                         
         LA    R2,PROAPROH                                                      
         CLI   5(R2),0                                                          
         BE    CLINK100                                                         
         CLI   8(R2),C' '                                                       
         BNH   CLINK100                                                         
         LA    R2,PROAJOBH                                                      
         CLI   5(R2),0                                                          
         BE    CLINK100                                                         
         CLI   8(R2),C' '                                                       
         BNH   CLINK100                                                         
         MVI   LNKMODE,LNKADD      ADDING LINK                                  
*                                                                               
CL60     GOTO1 VALNSAVE,DMCB,PROACLIH,AGDATA  VALIDATE AGENCY JOB               
         LA    R2,PROACLIH         FLAG AS SUCH                                 
         OI    4(R2),X'20'                                                      
*                                                                               
         GOTO1 RDOPT,AGDATA        SET AGENCY JOB OPTIONS                       
*                                                                               
         CLI   GOSTUDIO,C'Y'       CANT LINK TO A STUDIO JOB                    
         BE    CLINK200                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
CLINK100 MVC   CONHEAD(L'PLSLINK),PLSLINK                                       
         LA    R2,PROACLIH                                                      
         B     MYERROR                                                          
*                                                                               
CLINK200 MVC   CONHEAD(L'CANTLINK),CANTLINK                                     
         LA    R2,PROACLIH                                                      
         B     MYERROR                                                          
*                                                                               
CLINK300 MVC   CONHEAD(L'INVSJOB),INVSJOB                                       
         LA    R2,PROSJOBH                                                      
         B     MYERROR                                                          
*                                                                               
CLINK500 MVC   CONHEAD(L'BADSTUD),BADSTUD                                       
         LA    R2,PROSJOBH                                                      
         B     MYERROR                                                          
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*- SET AGENCY JOB FIELDS FROM LINK ELEMENT                                      
*---------------------------------------------------------------------          
*                                                                               
         USING LNKELD,R6                                                        
SETAJOB  NTR1                                                                   
         LA    R2,PROACLIH                                                      
         LA    R3,LNKAGJB                                                       
         ZIC   R1,LCLI                                                          
         BAS   RE,ELTOFLD                                                       
*                                                                               
         BAS   RE,BUMPTOUN                                                      
         LA    R3,LNKAGJB                                                       
         ZIC   R1,LCLI                                                          
         AR    R3,R1                                                            
         IC    R1,LPRO                                                          
         BAS   RE,ELTOFLD                                                       
*                                                                               
         BAS   RE,BUMPTOUN                                                      
         LA    R3,LNKAGJB                                                       
         ZIC   R1,LCLIPRO                                                       
         AR    R3,R1                                                            
         IC    R1,LJOB                                                          
         BAS   RE,ELTOFLD                                                       
*        NI    4(R2),X'FF'-X'20'   RE VALIDATE FIELD                            
*        NI    4(R2),X'80'                                                      
         B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------          
*- MOVE THE DATA AT 0(R3) TO THE FIELD AT 0(R2), LENGTH IS R1                   
*---------------------------------------------------------------------          
ELTOFLD  NTR1                                                                   
         STC   R1,5(R2)                                                         
         MVC   LISTAR,SPACES                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),0(R3)                                                  
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*- PROCESS LINK ELEMENTS TO ALL CONCERNED                                       
*---------------------------------------------------------------------          
*                                                                               
         USING LNKELD,R6                                                        
LINKJOBS NTR1                                                                   
         BAS   RE,SETNEWAC                                                      
         BAS   RE,SJOBKEY                                                       
         BAS   RE,LINKREC                                                       
         BAS   RE,SETOLDAC                                                      
*                                                                               
         B     XIT                                                              
* ---------------------------------------------------------------------         
*   ADD OR REMOVE A LINK ELEMENT FOR THE RECORD IN KEY                          
*        LNKMODE MUST BE SET TO LNKADD OR LNKDEL                                
* ---------------------------------------------------------------------         
LINKREC  NTR1  WORK=(R5,PTRBLKLN)                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 VSAVPTRS,DMCB,AIO,(R5)                                           
*                                                                               
         CLI   LNKMODE,LNKADD      ADD THE LINK ELEMENT                         
         BNE   *+8                                                              
         BAS   RE,LINK                                                          
         CLI   LNKMODE,LNKDEL      DELETE THE LINK                              
         BNE   *+8                                                              
         BAS   RE,UNLINK                                                        
*                                                                               
         GOTO1 PUTREC              WRITE THE RECORD                             
         GOTO1 VCHGPTRS,DMCB,AIO,(R5)                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
PTRBLKLN EQU   10*ACCKLEN                                                       
         EJECT                                                                  
*---------------------------------------------------------------------          
*- ADD A LINK ELEMENT TO THE RECORD IN AIO                                      
*---------------------------------------------------------------------          
*                                                                               
         USING LNKELD,R6                                                        
LINK     NTR1                                                                   
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   LN10                                                             
*                                                                               
         GOTO1 REMELEM                                                          
*                                                                               
LN10     XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   LNKEL,LNKELQ                                                     
         MVI   LNKLN,LNKLNQ                                                     
         MVC   LNKSTUD,STUDTYPE                                                 
         MVC   LNKSTJB,STJOB12                                                  
         MVC   LNKAGJB,AGJOB12                                                  
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
*---------------------------------------------------------------------          
*- REMOVE THE LINK ELEMENT FROM THE RECORD IN AIO                               
*---------------------------------------------------------------------          
*                                                                               
         USING LNKELD,R6                                                        
UNLINK   NTR1                                                                   
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   UNLX                                                             
*                                                                               
         L     R5,AIO              IF THIS IS A STUDIO JOB                      
         CLC   CUL,0(R5)                                                        
         BNE   UL50                                                             
*                                                                               
         XC    LNKAGJB,LNKAGJB     CLEAR AGENCY JOB TO UNLINK                   
         B     UNLX                                                             
*                                                                               
UL50     GOTO1 REMELEM                                                          
UNLX     B     XIT                                                              
*                                                                               
* ---------------------------------------------------------------------         
*        VALIDATE AND SAVE A STRING OF FIELDS CONTAINING A CLI, PRODUCT         
*        JOB, AND WRITE THE JOB NAME TO THE LAST FIELD                          
* ---------------------------------------------------------------------         
*                                                                               
VALNSAVE NTR1                                                                   
         MVI   OPTION,0            NO NAME FIELDS TO BE SHOWN                   
         BAS   RE,SETOLDAC         PROGEN ROUTINES ARE EMULATED                 
*                                                                               
         USING SVD,R3                                                           
         L     R3,DMCB+4                                                        
         L     R2,DMCB             P1 ID FIELD START                            
         GOTO1 VALCLI                                                           
         MVC   SVCLI,CLICODE                                                    
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         GOTO1 VALPROD                                                          
         MVC   SVPRO,PRODCODE                                                   
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         MVI   OPTION,C'Y'         DISPLAY NAME IN FOLLOWING FIELD              
         GOTO1 VALJOB                                                           
*                                                                               
         MVC   SVJOB,JOBNUM                                                     
         L     RE,AIO                                                           
         MVC   SVJOB12,3(RE)       SAVE 12 BYTE JOB                             
         MVC   SVSTAT,JOBSTAT      30 EL STAT                                   
         MVC   SVJSTAT,JOBJSTAT    26 EL STAT                                   
*                                                                               
         TM    SVSTAT,RSTSACIC     AC IS CLOSED                                 
         BNO   XIT                                                              
*                                                                               
JBCLOERR MVC   CONHEAD(L'NOCJ),NOCJ                                             
         B     MYERROR                                                          
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO STORE PURCHASE ORDERS KEYS IN A TABLE                          
*----------------------------------------------------------------------         
*                                                                               
BLDTBL   NTR1                                                                   
*                                                                               
         USING POTABD,R5                                                        
         L     R5,APOTAB                                                        
         MVI   0(R5),X'FF'                                                      
         XC    TBCOUNT,TBCOUNT                                                  
         LA    R0,POMAX                                                         
*                                                                               
         BAS   RE,SETNEWAC                                                      
         L     R3,AIOKEY           SET A(KEY)                                   
*                                                                               
         BAS   RE,STAGKEY          BUILD POINTER KEY                            
         GOTO1 HIGH                READ POINTERS                                
         CLC   KEYSAVE(L'STCKEY),0(R3)                                          
         BNE   BLDTBX                                                           
*                                                                               
         GOTO1 SEQ                 GET NEXT                                     
*                                                                               
         USING STCRECD,R3                                                       
BLDTB10  CLC   KEYSAVE(STCKAJB-STCKEY+L'STCKAJB),0(R3)                          
         BNE   BLDTBX                                                           
*                                                                               
         MVC   LASTPTR,STCKEY      SAVE POINTER KEY                             
*                                                                               
         OC    STCKPO,STCKPO       ORDER NUMBER DEFINED                         
         BNZ   *+6                 DUPLICATE POINTERS ON FILE                   
         DC    H'0'                                                             
*                                                                               
         MVC   ORDNUM,STCKPO       SAVE ORDER NUMBER                            
         BAS   RE,POKEY            BUILD PURCHACE ORDER KEY                     
*                                                                               
         USING ORDRECD,R4                                                       
         L     R4,AIOKEY                                                        
         LA    R3,LASTPTR                                                       
         MVC   ORDKDA,STCKDA                                                    
         GOTO1 GETREC              GET PURCHASE ORDER                           
*                                                                               
         L     R4,AIO                                                           
         TM    ORDRSTAT,ORDSLDEL   HAS ORDER BEEN DELETED                       
         BNO   BLDTB30                                                          
*                                                                               
         BAS   RE,UNLKPO           UNLINK THE ORDER                             
         XC    LNKMODE,LNKMODE     DON'T BOTHER USER WITH MESSAGE               
         B     BLDTB50             GET NEXT                                     
*                                                                               
BLDTB30  BAS   RE,MOVETB           MOVE ORDER TO TABLE                          
*                                                                               
         ZIC   RF,TBCOUNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,TBCOUNT                                                       
*                                                                               
         LA    R5,PONEXT                                                        
*                                                                               
BLDTB50  L     R3,AIOKEY                                                        
         MVC   STCKEY,LASTPTR                                                   
         MVI   STCKPO+L'STCKPO+1,X'FF'  READ NEXT POINTER                       
*                                                                               
         GOTO1 HIGH                                                             
         BCT   R0,BLDTB10                                                       
*                                                                               
BLDTBX   MVI   0(R5),X'FF'         END OF TABLE                                 
         BAS   RE,SETOLDAC                                                      
*                                                                               
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO DISPLAY THE TABLE ON THE SCREEN                                
*----------------------------------------------------------------------         
*                                                                               
DISTBL   NTR1                                                                   
*                                                                               
         XC    ACURFORC,ACURFORC   CLEAR CURSOR POSITION                        
*                                                                               
         USING POTABD,R5                                                        
         L     R5,APOTAB                                                        
         LA    R4,SCRMAX                                                        
         LA    R2,PROACT1H                                                      
         BAS   RE,SETLIN                                                        
         L     R2,AACT                                                          
*                                                                               
DISTB30  CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    DISTBX              YES                                          
*                                                                               
         BAS   RE,TBLTOSCR         MOVE TABLE ENTRY TO SCREEN LINE              
*                                                                               
         LA    R5,PONEXT                                                        
         L     R2,ANEXTLIN                                                      
         BAS   RE,SETLIN                                                        
         L     R2,AACT                                                          
         BCT   R4,DISTB30                                                       
*                                                                               
DISTBX   EQU   *                                                                
*                                                                               
         OC    ACURFORC,ACURFORC   WAS THIS SET WHILE DISPLAYING                
         BNZ   DISTBXX                                                          
         LA    R2,PROACT1H         PUT CURSOR AT FIRST UNPRO                    
         BAS   RE,BUMPTOUN                                                      
         ST    R2,ACURFORC                                                      
*                                                                               
DISTBXX  B     XIT                                                              
         EJECT                                                                  
DISTYPE  NTR1                                                                   
         LA    R2,PROSTYPH                                                      
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'STUDTYPE),STUDTYPE                                      
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO INITIALIZE THE TABLE                                           
*----------------------------------------------------------------------         
*                                                                               
INITTBL  NTR1                                                                   
         L     R5,APOTAB                                                        
         MVI   0(R5),X'FF'                                                      
         XC    TBCOUNT,TBCOUNT                                                  
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* BUILD THE KEYS                                                                
*----------------------------------------------------------------------         
*              BUILD THE STUDIO JOB KEY                                         
SJOBKEY  EQU   *                                                                
         USING ACTRECD,R6                                                       
         L     R6,AIOKEY                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA(3),CUL                                                  
         MVC   ACTKACT,STJOB12                                                  
         BR    RE                                                               
*                                                                               
*              BUILD THE AGENCY JOB KEY                                         
AJOBKEY  EQU   *                                                                
         USING ACTRECD,R6                                                       
         L     R6,AIOKEY                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA(3),CUL                                                  
         MVC   ACTKACT,AGJOB12                                                  
         BR    RE                                                               
*                                                                               
*              BUILD THE PURCHASE ORDER KEY                                     
POKEY    EQU   *                                                                
         USING ORDRECD,R6                                                       
         USING POTABD,R5                                                        
         L     R6,AIOKEY                                                        
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUL                                                      
         MVC   ORDKORD,ORDNUM                                                   
         BR    RE                                                               
*                                                                               
*        BUILD THE STUDIO/AGENCY JOB POINTER KEY                                
STAGKEY  EQU   *                                                                
         USING STCRECD,R6                                                       
         L     R6,AIOKEY                                                        
         XC    STCKEY,STCKEY                                                    
         MVI   STCKTYP,STCKTYPQ                                                 
         MVI   STCKSUB,STCKSUBQ                                                 
         MVC   STCKCPY,CUL                                                      
         MVC   STCKSTY,STUDTYPE                                                 
         MVC   STCKSJB,STJOB12                                                  
         MVC   STCKAJB,AGJOB12                                                  
         BR    RE                                                               
         DROP  R6                                                               
* --------------------------------------------------------------------          
*        BUILD AN ORDER TRANSACTION KEY FROM THE ORDER IN AIO                   
*---------------------------------------------------------------------          
*                                                                               
POTRNKEY NTR1  WORK=(R6,L'TRNKEY)                                               
         USING TRNRECD,R5                                                       
         L     R5,AIOKEY           BUILD JOB/EXP TRANSACTION KEY                
         MVC   0(L'TRNKEY,R6),0(R5)                                             
         MVC   TRNKEY,SPACES       CLEAR OUT KEY                                
         XC    TRNKSTA,TRNKSTA                                                  
*                                                                               
         USING ORDRECD,R6                                                       
         MVC   TRNKREF,ORDKORD     EXTRACT DATA FROM ORDER KEY                  
*                                                                               
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ORDELD,R6                                                        
         MVC   TRNKCULA,ORDJOB     JOB                                          
         MVC   TRNKWORK,=2C'*'     WORKCODE IS '**'                             
         MVC   TRNKCULC,ORDSUP                                                  
         MVC   TRNKDATE,ORDDATE                                                 
         XC    TRNKSBR,TRNKSBR                                                  
         XC    TRNKSTA,TRNKSTA                                                  
         B     XIT                                                              
         DROP  R6                                                               
*                                  BUILD TRANS RECORD IN IO2                    
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO MOVE THE TABLE ENTRY AT 0(R5) TO THE SCREEN LINE               
* STARTING AT AACT                                                              
* THIS ROUTINE ALSO MAINTAINS THE ESTTOT ANBD BILLTOT ACCUMULATORS              
*----------------------------------------------------------------------         
*                                                                               
         USING POTABD,R5                                                        
TBLTOSCR NTR1                                                                   
         L     R2,APONUM                                                        
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'PONUM),PONUM                                            
         BAS   RE,MOVEFLD                                                       
         OI    1(R2),X'20'         PROTECT PO NUMBER WHEN ENTERED               
         MVI   5(R2),L'PONUM       SET LENGTH                                   
*                                                                               
         L     R2,AODATE           DATE OPENED                                  
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   WORK(L'POODATE),POODATE                                          
         BAS   RE,PUTDATE                                                       
*                                                                               
         L     R2,ADDATE           DUE DATE                                     
         MVC   LISTAR,SPACES                                                    
         CLC   PODUE,=X'FFFFFF'    CLOSED                                       
         BNE   TBT50               NO                                           
         MVC   LISTAR(8),=CL8'CLOSED'                                           
         BAS   RE,MOVEFLD                                                       
         B     TBT60                                                            
*                                                                               
TBT50    MVC   WORK(L'PODUE),PODUE                                              
         BAS   RE,PUTDATE                                                       
*                                                                               
TBT60    MVC   LISTAR,SPACES                                                    
         L     R2,AAMT             AMOUNT                                       
         EDIT  (P6,POAMT),(11,LISTAR),2,MINUS=YES                               
         BAS   RE,MOVEFLD                                                       
*                                                                               
TBTX     B     XIT                                                              
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*        PUT THE DATE IN WORK TO THE FIELD AT 0(R2)                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*                                                                               
PUTDATE  NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,WORK),(8,LISTAR)                                  
*                                                                               
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO PROCESS PF KEYS                                                
*----------------------------------------------------------------------         
*                                                                               
PROCPF   NTR1                                                                   
*                                                                               
         XC    PFACTION,PFACTION   GLOBAL ACTION TO OPERATE ON ORDERS           
         CLI   PFKEY,0                                                          
         BE    PROCPFX                                                          
*                                                                               
         CLI   PFKEY,2                                                          
         BNE   PROCPF3                                                          
         MVI   PFACTION,POACOPEN                                                
         B     PROCPFX                                                          
*                                                                               
PROCPF3  CLI   PFKEY,3                                                          
         BNE   PROCPF4                                                          
         MVI   PFACTION,POACCLOS                                                
         B     PROCPFX                                                          
*                                                                               
PROCPF4  CLI   PFKEY,4                                                          
         BNE   PROCPF5                                                          
*                                  DELETE JOB LINK                              
         BAS   RE,SETNEWAC                                                      
         BAS   RE,STAGKEY          MAKE SURE THERE ARE NO POLINKS               
         L     R6,AIOKEY                                                        
         USING STCRECD,R6                                                       
         MVI   STCKPO,1                                                         
         GOTO1 HIGH                                                             
         CLC   0((STCKAJB-STCKEY)+L'STCKAJB,R6),KEYSAVE                         
         BE    NODEL                                                            
*                                                                               
         BAS   RE,CHKBILLS         IS THERE BILLING ON THE STUDIO JOB           
         BE    NODELBIL                                                         
*                                                                               
         MVI   LNKMODE,LNKDEL                                                   
         B     PROCPFX                                                          
*    -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -              
PROCPF5  CLI   PFKEY,5                                                          
         BNE   BADPFK                                                           
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'JOB',=C'ESTIMATE',(6,PROSCLI),(6,PROSPRO),X        
               (6,PROSJOB),0                                                    
         B     PROCPFX                                                          
*    -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -              
PROCPFX  MVI   PFKEY,0                                                          
         BAS   RE,SETOLDAC                                                      
         B     XIT                                                              
*                                                                               
BADPFK   MVC   CONHEAD(L'INVPFK),INVPFK                                         
         LA    R2,ACURSOR                                                       
         B     MYERROR                                                          
*                                                                               
NODEL    MVC   CONHEAD(L'HASPOS),HASPOS                                         
         LA    R2,PROACT1H                                                      
         B     MYERROR                                                          
*                                                                               
NODELBIL MVC   CONHEAD(L'HASBILLS),HASBILLS                                     
         LA    R2,PROSCLIH                                                      
         B     MYERROR                                                          
         EJECT                                                                  
CHKBILLS NTR1                                                                   
         BAS   RE,SJOBKEY                                                       
         USING TRNRECD,R6                                                       
         L     R6,AIOKEY                                                        
         MVC   TRNKWORK,=C'99'                                                  
         GOTO1 HIGH                                                             
         CLC   0(TRNKWORK-TRNKEY+L'TRNKWORK,R6),KEYSAVE                         
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* EDIT THE FIELDS ON THE SCREEN AND REBUILD THE TABLE DATA                      
*----------------------------------------------------------------------         
*                                                                               
EDTSCR   NTR1                                                                   
*                                                                               
         BAS   RE,SETNEWAC                                                      
*                                                                               
         BAS   RE,VALACT           VALIDATE SCREEN ACTIONS                      
*                                                                               
         BAS   RE,INITTBL          REBUILD TABLE FROM SCREEN DATA               
         LA    R2,PROACT1H                                                      
         LA    R4,SCRMAX                                                        
         L     R5,APOTAB                                                        
         USING POTABD,R5                                                        
*                                                                               
EDTS20   BAS   RE,SETLIN           SET FIELD ADCONS                             
         MVI   0(R5),X'FF'         ASSUME ALL DONE                              
*                                                                               
         BAS   RE,CHKSCR           ANYTHING ON THIS SCREEN LINE                 
         BNE   EDTS70              NO, LEAVE TABLE BE                           
*                                                                               
         MVI   POACTION,POACNONE                                                
*                                                                               
         OC    PFACTION,PFACTION   HAS A GLOBAL ACTION BEEN REQUESTED           
         BZ    EDTS27              NO                                           
         MVC   POACTION,PFACTION                                                
         B     EDTS30              SKIP INDIVIDUAL ACTIONS                      
*                                                                               
EDTS27   L     R2,AACT                                                          
         CLI   5(R2),0             ANY ACTION                                   
         BE    EDTS30              MAYBE ITS A NEW ORDER                        
*                                                                               
         CLI   8(R2),C'O'          RE OPEN THIS PO                              
         BNE   *+8                                                              
         MVI   POACTION,POACOPEN                                                
*                                                                               
         CLI   8(R2),C'C'          CLOSE THIS PO                                
         BNE   *+8                                                              
         MVI   POACTION,POACCLOS                                                
*                                                                               
         CLI   8(R2),C'U'          UNLINK THIS PO                               
         BNE   *+8                                                              
         MVI   POACTION,POACUNLK                                                
*                                                                               
EDTS30   L     R2,APONUM                                                        
*                                                                               
         CLI   5(R2),0             ANY ORDER NUMBER                             
         BE    EDTS70              NO, GET NEXT LINE                            
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         BAS   RE,VALORD           VALIDATE/READ PURCHASE ORDER                 
*                                                                               
         BAS   RE,POCHKDUP         MAKE SURE NOT ALREADY IN TABLE               
*                                                                               
         TM    1(R2),X'20'         IS FIELD PROTECTED                           
         BO    EDTS50              COOL                                         
*                                                                               
         MVI   POACTION,POACLINK                                                
*                                                                               
EDTS50   BAS   RE,MOVETB           SAVE IN TABLE                                
*                                                                               
         BAS   RE,TBLTOSCR         DISPLAY PO DATA                              
*                                                                               
         BAS   RE,PROCPO           PROCESS THIS PURCHASE ORDER                  
*                                                                               
         LA    R5,PONEXT           BUMP PO TABLE                                
*                                                                               
         L     R2,AACT                                                          
         CLI   5(R2),0             WAS THERE AN ACTION                          
         BE    EDTS70              NO                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVI   LISTAR,C'*'                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
EDTS70   L     R2,ANEXTLIN                                                      
         BCT   R4,EDTS20                                                        
*                                                                               
EDTSX    BAS   RE,SETNEWAC                                                      
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PROCESS THE A LINE FROM THE PURCHASE ORDER TABLE                       
*        ASSUMES THE PO IS IN AIO                                               
*----------------------------------------------------------------------         
         USING POTABD,R5                                                        
PROCPO   NTR1                                                                   
*                                                                               
*                                                                               
         CLI   POACTION,POACOPEN                                                
         BNE   *+8                                                              
         BAS   RE,OPENPO                                                        
*                                                                               
         CLI   POACTION,POACCLOS                                                
         BNE   *+8                                                              
         BAS   RE,CLOSPO                                                        
*                                                                               
         CLI   POACTION,POACLINK                                                
         BNE   *+8                                                              
         BAS   RE,LINKPO                                                        
*                                                                               
         CLI   POACTION,POACUNLK                                                
         BNE   *+8                                                              
         BAS   RE,UNLKPO                                                        
*                                                                               
*                                                                               
PPOX     B     XIT                                                              
*                                                                               
*----------------------------------------------------------------------         
*        OPEN THE PURCHASE ORDER IN AIO                                         
*----------------------------------------------------------------------         
OPENPO   NTR1  WORK=(R5,L'TRNKEY)                                               
         L     R6,AIO                                                           
         MVC   0(L'TRNKEY,R5),0(R6)    SAVE ORDER KEY                           
         USING ORDRECD,R6                                                       
         OC    PFACTION,PFACTION   GLOBAL OPEN                                  
         BZ    OP40                NO VALIDATE                                  
*                                                                               
         TM    AGACCSTA,RSTSACIL+RSTSACIC JOB CLOSED/LOCODE                     
         BNZ   XIT                 YES, IGNORE                                  
*                                                                               
         TM    ORDRSTAT,ORDSFMCH   ALREADY OPEN                                 
         BNO   XIT                 YES, IGNORE                                  
*                                                                               
OP40     TM    ORDRSTAT,ORDSFMCH                                                
         BNO   OPERR               NO, OPEN IS INVALID                          
*                                                                               
         TM    AGACCSTA,RSTSACIL+RSTSACIC JOB CLOSED/LOCODE                     
         BNZ   OPERR1              CAN'T OPEN PO'S ON CLOSED/LOCKED JOB         
*                                                                               
OP50     NI    ORDRSTAT,X'FF'-ORDSFMCH     SET RECORD STATUS                    
         GOTO1 PUTREC                                                           
*                                                                               
         L     R6,AIOKEY                                                        
         NI    ORDKSTAT,X'FF'-ORDSFMCH     SET KEY STATUS                       
         MVC   AIO,AIO3            WRITE TRASHES AIO W/OLDKEY                   
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1            AIO NOW ORDER RECORD                         
*                                                                               
         BAS   RE,POTRNKEY         BUILD THE TRNSACTION KEY FOR THIS PO         
         OI    DMINBTS,X'08'                                                    
         MVC   AIO,AIO2            READ/BUILD TRAN IN(TO) AIO2                  
         GOTO1 HIGH                IS IS STILL THERE?                           
         CLC   KEYSAVE(L'TRNKEY),0(R6)       DID I GET THE KEY                  
         BNE   OP70                NO, REBUILD                                  
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         BAS   RE,RESTTRN          TURN OFF BITS AND WRITE/PUT                  
         B     OPX                                                              
*                                                                               
OP70     MVC   AIO,AIO1            REBUILD KEY, BLOWN AWAY BY THE HIGH          
         L     R6,AIOKEY           RESTORE AIOKEY TO ORDER KEY                  
         MVC   0(L'TRNKEY,R6),0(R5)                                             
         BAS   RE,POTRNKEY                                                      
         MVC   AIO,AIO2                                                         
         BAS   RE,BLDTRN           BUILD TRANSACTION IN AIO2                    
         GOTO1 ADDREC                                                           
*                                                                               
OPX      NI    DMINBTS,X'FF'-X'08'                                              
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
OPERR    MVC   CONHEAD(L'CANTOPEN),CANTOPEN                                     
         B     MYERROR                                                          
*                                                                               
OPERR1   MVC   CONHEAD(L'JOBISCLO),JOBISCLO                                     
         B     MYERROR                                                          
         EJECT                                                                  
*                                                                               
CLOSPO   NTR1  WORK=(R5,L'TRNKEY)                                               
         L     R6,AIO                                                           
         MVC   0(L'TRNKEY,R5),0(R6)    SAVE ORDER KEY                           
         USING ORDRECD,R6                                                       
         OC    PFACTION,PFACTION   GLOBAL CLOSE                                 
         BZ    CP40                NO, VALIDATE                                 
*                                                                               
         TM    ORDRSTAT,ORDSFMCH                                                
         BO    XIT                 ALREADY CLOSED                               
*                                                                               
CP40     TM    ORDRSTAT,ORDSFMCH                                                
         BO    CLOERR              NO, ALREADY CLOSED IS ERROR                  
*                                                                               
CP50     OI    ORDRSTAT,ORDSFMCH   SET CLOSED BIT                               
         GOTO1 PUTREC                                                           
*                                                                               
         L     R6,AIOKEY                                                        
         OI    ORDKSTAT,ORDSFMCH   SET CLOSED BIT IN KEY                        
         MVC   AIO,AIO3            WRITE TRASHES AIO                            
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1            READDRESS PURCHASE ORDER                     
*                                                                               
         BAS   RE,POTRNKEY         BUILD KEY                                    
         GOTO1 HIGH                IS IS THERE?                                 
         BNE   CPX                 NO, COOL                                     
*                                                                               
         GOTO1 READ                READ TRN KEY                                 
         GOTO1 GETREC                                                           
         BAS   RE,DELTRN                                                        
*                                                                               
CPX      B     XIT                                                              
*                                                                               
CLOERR   MVC   CONHEAD(L'NOTOPEN),NOTOPEN                                       
         B     MYERROR                                                          
         EJECT                                                                  
LINKPO   NTR1                                                                   
         L     R6,AIO                                                           
         USING ORDRECD,R6                                                       
         MVC   ORDNUM,ORDKORD      SET ORDNUM                                   
         BAS   RE,POKEY                                                         
         MVI   LNKMODE,LNKADD                                                   
         BAS   RE,LINKREC                                                       
         B     XIT                                                              
*                                                                               
UNLKPO   NTR1                                                                   
         L     R6,AIO                                                           
         USING ORDRECD,R6                                                       
         MVC   ORDNUM,ORDKORD      SET ORDNUM                                   
         BAS   RE,POKEY                                                         
         MVI   LNKMODE,LNKDEL                                                   
         BAS   RE,LINKREC                                                       
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* -------------------------------------------------------------------           
*        BUILD TRANS RECORD IN AIO2, ASSUMES KEY IS IN AIOKEY AND               
*        ORDER IS IN AIO1                                                       
* -------------------------------------------------------------------           
*                                                                               
BLDTRN   NTR1                                                                   
         L     RE,AIO2             CLEAR AIO2                                   
         LA    RF,2000                                                          
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         USING TRNRECD,R5                                                       
         L     R5,AIOKEY                                                        
         XC    TRNKSTA,TRNKSTA                                                  
         L     R5,AIO2             BUILD KEY IN AIO2                            
         L     R6,AIOKEY                                                        
         MVC   TRNKEY,0(R6)                                                     
         XC    TRNRSTA,TRNRSTA                                                  
         MVC   TRNRLEN,DATADISP                                                 
*                                                                               
         USING TRNELD,R5           BUILD 44 ELEMENT                             
         XC    ELEMENT,ELEMENT                                                  
         LA    R5,ELEMENT                                                       
         USING ORDRECD,R6                                                       
         L     R6,AIO1                                                          
         MVC   TRNREF,ORDKORD                                                   
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ORDELD,R6                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1     MAKE 1 BYTE NARRATIVE                        
         MVC   TRNDATE,ORDDATE                                                  
         MVI   TRNSTAT,X'80'                                                    
         MVI   TRNTYPE,X'0C'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,TRNDATE),(0,WORK)                                 
         MVC   TRNBTCH(1),WORK+1   SAVE MOS                                     
         MVC   TRNBTCH+1(1),WORK+3                                              
         CLI   WORK+2,C'1'                                                      
         BNE   BT20                                                             
         NI    TRNBTCH+1,X'C3'     (10=A,11=B,12=C)                             
         ZIC   R1,TRNBTCH+1                                                     
         LA    R1,1(R1)                                                         
         STC   R1,TRNBTCH+1                                                     
BT20     MVC   TRNBTCH+2(4),SPACES                                              
         ZAP   TRNAMNT,=P'0'      HAS NO AMOUNT                                 
         MVC   TRNANAL,=2C'*'                                                   
         MVI   TRNNARR,C' '                                                     
         MVC   AIO,AIO2            ADD TRANSACTION ELEMENT                      
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,COPYEL                                                        
*                                                                               
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,COPYEL                                                        
         B     XIT                                                              
*                                                                               
*        COPY AN ELEMENT FROM AIO1 TO AIO2                                      
*                                                                               
COPYEL   NTR1                                                                   
         MVC   AIO,AIO1                                                         
         BAS   RE,GETELIO                                                       
         XC    ELEMENT,ELEMENT                                                  
         XR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         MVC   AIO,AIO2            ADD TRANSACTION ELEMENT                      
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        MARK A TRANSACTION AS DELETED                                          
*                                                                               
DELTRN   NTR1                                                                   
         USING TRNRECD,R6                                                       
         L     R6,AIO                                                           
         OI    TRNRSTAT,TRNSDELT                                                
         GOTO1 PUTREC                                                           
*                                                                               
         L     R6,AIOKEY                                                        
         OI    TRNKSTAT,TRNSDELT                                                
         GOTO1 WRITE                                                            
         B     XIT                                                              
*                                                                               
*        RESTORE A TRANSACTION                                                  
*                                                                               
RESTTRN  NTR1                                                                   
         USING TRNRECD,R6                                                       
         L     R6,AIO                                                           
         NI    TRNRSTAT,X'FF'-TRNSDELT                                          
         GOTO1 PUTREC                                                           
*                                                                               
         L     R6,AIOKEY                                                        
         NI    TRNKSTAT,X'FF'-TRNSDELT                                          
         GOTO1 WRITE                                                            
         B     XIT                                                              
*----------------------------------------------------------------------         
*        SEE IF THERE IS DATA ON THE SCREEN LINE                                
*        RETURNS CC EQ IF I SHOULD WRITE THE LINE TO TABLE                      
*----------------------------------------------------------------------         
CHKSCR   NTR1                                                                   
         XC    BYTE,BYTE                                                        
         XR    R3,R3               FOR ICM IN CHKFLD                            
*                                                                               
         L     R2,AACT                                                          
         BAS   RE,CHKFLD                                                        
*                                                                               
         L     R2,APONUM                                                        
         BAS   RE,CHKFLD                                                        
*                                                                               
CHKSX    CLI   BYTE,GOTDATA                                                     
         B     XIT                                                              
*                                                                               
CHKFLD   TM    1(R2),FATBPROT      IS FIELD PROTECTED?                          
         BO    CHKFX               YES, MUST BE FILLED                          
*                                                                               
         ICM   R3,1,5(R2)                                                       
         BZR   RE                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BNHR  RE                                                               
CHKFX    MVI   BYTE,GOTDATA                                                     
         BR    RE                                                               
GOTDATA  EQU   X'FF'                                                            
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* SUB-ROUTINE TO READ THE JOB OPTIONS                                           
* JOB DATA US PASSED IN 0(R1)                                                   
*----------------------------------------------------------------------         
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,0(R1)                                                   
         MVC   GOSELPRO,6(R1)                                                   
         MVC   GOSELJOB,12(R1)                                                  
         MVI   GOWHICH,C'N'        NEW OPTIONS ONLY                             
         LA    RE,GOBLOCKX                                                      
         ST    RE,GOAEXT                                                        
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO SAVE THE PURCHACE ORDER IN AIO TO THE TABLE ENTRY AT           
* 0(R5)                                                                         
*----------------------------------------------------------------------         
*                                                                               
MOVETB   NTR1                                                                   
         USING POTABD,R5                                                        
         USING ORDRECD,R6                                                       
         L     R6,AIO                                                           
         MVC   PONUM,ORDKORD                                                    
         XC    PODUE,PODUE                                                      
         TM    ORDRSTAT,ORDSFMCH   CLOSED? (FULLY MATCHED)                      
         BNO   *+10                                                             
         MVC   PODUE,=X'FFFFFF'                                                 
*                                                                               
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                WHAT IS THIS?                                
*                                                                               
         USING ORDELD,R6                                                        
         MVC   POODATE,ORDDATE     DATE OPENED?                                 
         CLI   PODUE,X'FF'         IS PO CLOSED                                 
         BE    *+10                YES, DONT SET DUE DATE                       
         MVC   PODUE,ORDDDTE                                                    
*                                                                               
         ZAP   POAMT,=P'0'                                                      
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING OAMELD,R6                                                        
MT50     AP    POAMT,OAMAMNT                                                    
         BAS   RE,NEXTEL                                                        
         BE    MT50                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*----------------------------------------------------------------------         
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
* SUB-ROUTINE TO SET ADCONS FOR A LIST FIELD LINE                               
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,NFIELDS                                                       
         LA    R1,AACT                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTLIN                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*     CLEAR AND UNPROTECT THE AGENCY JOB AND DATA LINES ON THE SCREEN           
*---------------------------------------------------------------------          
*                                                                               
CLRSCRN  NTR1                                                                   
         BAS   RE,CLRSCR           CLEAR DATA LINES                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
CLRAJOB  NTR1                                                                   
         MVC   LISTAR,SPACES                                                    
         LA    R2,PROACLIH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,PROAPROH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,PROAJOBH                                                      
         BAS   RE,MOVEFLD                                                       
         LA    R2,PROAJNMH                                                      
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
*---------------------------------------------------------------------          
*     CLEAR THE DATA LINES ON THE SCREEN                                        
*---------------------------------------------------------------------          
*                                                                               
CLRSCR   NTR1                                                                   
*                                                                               
         LA    R4,SCRMAX                                                        
         LA    R2,PROACT1H         R2=A(FIRST DATA FIELD)                       
         XR    R6,R6                                                            
         LA    R6,1                                                             
*                                                                               
CLRS20   LA    R6,NFIELDS                                                       
         BAS   RE,SETLIN                                                        
         L     R2,AACT                                                          
         MVC   LISTAR,SPACES                                                    
*                                                                               
CLRS30   BAS   RE,MOVEFLD                                                       
         BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         BCT   R6,CLRS30                                                        
*                                                                               
         L     R2,APONUM                                                        
         NI    1(R2),X'FF'-X'20'                                                
*                                                                               
         L     R2,ANEXTLIN                                                      
*                                                                               
         BCT   R4,CLRS20                                                        
*                                                                               
         B     XIT                                                              
*                                                                               
XMITSCRN LA    R2,CONHEADH                                                      
         SR    RF,RF                                                            
XS10     IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   XS10                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT WHOLE SCREEN           
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*     CLEAR TABLE                                                               
*---------------------------------------------------------------------          
*                                                                               
*                                                                               
INITTAB  NTR1                                                                   
*                                                                               
         L     R5,APOTAB                                                        
         XR    R6,R6                                                            
         LA    R6,1                                                             
         LA    R4,POMAX                                                         
*                                                                               
         USING POTABD,R5                                                        
INITT20  MVC   PONUM,SPACES                                                     
         LA    R5,PONEXT                                                        
         BCT   R4,INITT20                                                       
*                                                                               
         MVI   0(R5),X'FF'                                                      
         MVI   TBCOUNT,POMAX                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
SETNEWAC NTR1                      SET DA PARMS                                 
         LA    RF,BIGKEY                                                        
         ST    RF,AIOKEY                                                        
         GOTO1 VSETNEW                                                          
         B     XIT                                                              
*                                                                               
SETOLDAC NTR1                      SET EMU PARMS                                
         LA    RF,KEY                                                           
         ST    RF,AIOKEY                                                        
         GOTO1 VSETEMU                                                          
         B     XIT                                                              
*                                                                               
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
MYERROR  ST    R2,ACURFORC                                                      
         MVI   ERROR,X'FE'                                                      
         OI    GENSTAT2,USMYOK                                                  
         BAS   RE,SETOLDAC                                                      
         BAS   RE,XMITSCRN         TRANSMIT SCREEN                              
         GOTO1 ERREX2                                                           
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         SPACE 1                                                                
DISMSG   DC    C'DATA DISPLAYED - ENTER CHANGES'                                
CHAMSG   DC    C'CHANGES COMPLETED'                                             
ADDMSG   DC    C'LINK HAS BEEN ADDED'                                           
DELMSG   DC    C'LINK HAS BEEN DELETED'                                         
INVSJOB  DC    C'ERROR - NO STUDIO TYPE DEFINED ON JOB RECORD.'                 
BADSTUD  DC    C'ERROR - INVALID STUDIO JOB.'                                   
CANTLINK DC    C'ERROR - AGENCY JOB DEFINED AS STUDIO.'                         
PLSLINK  DC    C'PLEASE ENTER THE AGENCY JOB.'                                  
BADACT   DC    C'ERROR - INVALID ACTION.'                                       
INVPFK   DC    C'ERROR - PF KEY NOT DEFINED FOR LINK MAINT.'                    
CANTOPEN DC    C'ERROR - PURCHASE ORDER IS ALREADY OPEN.'                       
NOTOPEN  DC    C'ERROR - PURCHASE ORDER IS ALREADY CLOSED,'                     
JOBISCLO DC    C'ERROR - AGENCY JOB IS CLOSED/LOCKED.'                          
HASPOS   DC    C'ERROR - MUST REMOVE PURCHASE ORDERS FIRST.'                    
HASBILLS DC    C'ERROR - CAN''T DELETE LINK, STUDIO JOB IS BILLED.'             
WRNGJOB  DC    C'ERROR - THIS PURCHASE ORDER IS FOR JOB XXXXXXXXXXXX.'          
ALRDLNK  DC    C'ERROR - ORDER LINKED TO XXXXXXXXXXXX/XXXXXXXXXXXX.'            
DUPPO    DC    C'ERROR - PURCHASE ORDER XXXXXX IS ALREADY LINKED.'              
EXPACC   DC    C'ERROR - THIS PURCHASE ORDER IS FOR AN EXPENSE ACCOUNT'         
ORDISDEL DC    C'ERROR - THIS PURCHASE ORDER HAS BEEN DELETED.'                 
NOCJ     DC    C'ERROR - JOB IS CLOSED.'                                        
OLDACC   DC    C'ERROR - FILE NOT CONVERTED.'                                   
*                                                                               
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACJOBBERD                                                                      
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENFILE                                                                      
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
LOCAL    DS    0X                                                               
MYDUB    DS    D                                                                
RELO     DS    A                                                                
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
AIOKEY   DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
AACT     DS    A                                                                
APONUM   DS    A                                                                
AODATE   DS    A                                                                
ADDATE   DS    A                                                                
AAMT     DS    A                                                                
NFIELDS  EQU   (*-AACT)/L'AACT                                                  
ANEXTLIN DS    A                                                                
*                                                                               
APOTAB   DS    A                                                                
PL16     DS    PL16                                                             
*                                                                               
AGDATA   DS    0C                                                               
AGCLI    DS    CL6                                                              
AGPRO    DS    CL6                                                              
AGJOB    DS    CL6                                                              
AGJOB12  DS    CL12                                                             
AGJOBNM  DS    CL36                                                             
AGACCSTA DS    CL1                 STATUS BYTE FROM 30 ELEMENT                  
AGJOBSTA DS    CL1                 STATUS BYTE FROM 26 ELEMENT                  
*                                                                               
STDATA   DS    0C                                                               
STCLI    DS    CL6                                                              
STPRO    DS    CL6                                                              
STJOB    DS    CL6                                                              
STJOB12  DS    CL12                                                             
STJOBNM  DS    CL36                                                             
STACCSTA DS    CL1                 STATUS BYTE FROM 30 ELEMENT                  
STJOBSTA DS    CL1                 STATUS BYTE FROM 26 ELEMENT                  
*                                                                               
ORDNUM   DS    CL6                                                              
*                                                                               
*                                                                               
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
MYMSG    DS    C                                                                
*                                                                               
LNKMODE  DS    C                                                                
LNKADD   EQU   C'M'                                                             
LNKDEL   EQU   C'D'                                                             
*                                                                               
PFACTION DS    C                   GLOBAL ACTION REQUESTED VIA PFKEY            
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOXBLOCK                                                     
LASTPTR  DS    CL(L'STCKEY+L'STCKDA+L'STCKSTA)                                  
TABLE    DS    ((POTABLN*POMAX)+1)C                                             
         EJECT                                                                  
*                                                                               
*                                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROB5D                                                       
         SPACE 2                                                                
SVSAVE   DS    0D                                                               
STUDTYPE DS    CL(L'GOTYPE)                                                     
TBCOUNT  DS    CL1                                                              
         DS    CL((SAVAREA-SVSAVE)-(*-SVSAVE))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
POMAX    EQU   22                  N'PO'S ALLOWED FOR A JOB                     
SCRMAX   EQU   POMAX               MAX LINES ON SCREEN                          
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
PFKINS   EQU   2                                                                
PFKDEL   EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER TABLE                                                          
*        TBSEQ=X'FF' IS END OF TABLE                                            
*                                                                               
POTABD DSECT                                                                    
POREC    DS    0C                                                               
PONUM    DS    CL6                 ORDER NUMBER                                 
POODATE  DS    XL3                 YMD DATE OPENED                              
PODUE    DS    XL3                 DUE DATE, HI VALS IF CLOSED                  
POAMT    DS    PL6                 ORDER AMOUNT                                 
POACTION DS    CL1                 ACTION REQUESTED                             
POACOPEN EQU   C'O'                                                             
POACCLOS EQU   C'C'                                                             
POACLINK EQU   C'L'                                                             
POACUNLK EQU   C'U'                                                             
POACNONE EQU   C' '                                                             
POLINE   DS    CL1                 LINE GENERATING THIS TABLE ENTRY             
*                                                                               
PONEXT   EQU   *                                                                
POTABLN  EQU   *-POTABD                                                         
         SPACE 2                                                                
SVD      DSECT                     COVERS DATA AREAS FOR VALNSAVE               
SVCLI    DS    CL6                                                              
SVPRO    DS    CL6                                                              
SVJOB    DS    CL6                                                              
SVJOB12  DS    CL12                                                             
SVNAME   DS    CL36                                                             
SVSTAT   DS    CL1                 STATUS FROM 30 ELEMENT                       
SVJSTAT  DS    CL1                 STATUS FROM 24 ELEMENT                       
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACPRO45   04/30/97'                                      
         END                                                                    
