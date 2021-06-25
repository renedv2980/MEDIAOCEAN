*          DATA SET BUFIL02    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T50202A                                                                  
*INCLUDE DAYVAL                                                                 
         TITLE 'T50202 - BUDGET CONTROL LFM - CLIENT RECORD'                    
T50202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI02**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         GOTO1 VSETADD                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         CLC   =C'LOOK',CONACT     TEST FOR 'LOOK'                              
         BNE   *+16                                                             
         CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   TRAPERR             NO-NOT ALLOWED                               
         MVI   TRACEOPT,YES                                                     
*                                                                               
         CLI   ACTNUM,ACTREP                                                    
         BE    VKEYX                                                            
         LA    R2,CLICLTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST         EDIT FIELD FROM START                        
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES           ZERO FILL OUTPUT                             
         GOTO1 VFVAL                                                            
*                                                                               
         CLI   FLDH+5,0                                                         
         BNE   VKEY1                                                            
         MVI   ERROR,MISSING                                                    
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX               OK TO HAVE NO START KEY                      
         B     TRAPERR                                                          
*                                                                               
VKEY1    MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,2                                                         
         BL    TRAPERR                                                          
         CLI   FLDH+5,3                                                         
         BH    TRAPERR                                                          
         MVC   CLTCODE,FLD         SAVE INPUT CODE                              
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX                                                            
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BNE   *+12                                                             
         OI    WHENOK,X'01'        BYPASS GENCON MAINTENANCE                    
         B     VKEYX                                                            
*                                                                               
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    *+16                                                             
         CLI   ACTNUM,ACTREST      TEST FOR RESTORE                             
         BNE   VKEY2                                                            
         MVI   NDDELRSW,YES        READ DELETES FOR ACTION RESTORE              
         OI    WHENOK,X'01'        BYPASS GENCON FOR DELETE/RESTORE             
*                                                                               
VKEY2    GOTO1 VSETKEY                                                          
*                                                                               
VKEY4    LA    R1,VKEYHK                                                        
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0             TEST FOR ERROR                               
         BE    VKEYX                                                            
         L     R2,FADDR                                                         
         GOTO1 VNODERR                                                          
*                                                                               
VKEYX    B     XIT                                                              
         SPACE 2                                                                
* NODIO HOOK ROUTINE FOR VALKEY                                                 
*                                                                               
VKEYHK   ST    RE,SAVERE                                                        
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BE    VKEYHKX                                                          
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNE   VKEYHKX                                                          
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCCLT     TEST FOR CLIENT RECORD                       
         BZ    VKEYHKX                                                          
         CLI   NDLEV,1             TEST NODIO IS IN SYNC                        
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  EXTRACT KEY FROM RECORD                      
         MVC   SVNKEY,NODKEY       SAVE NODAL KEY                               
*                                                                               
VKEYHKX  L     RE,SAVERE           RETURN TO NODIO                              
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
*        DISPLAY KEY                                                            
*                                                                               
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         USING BURECD,R4                                                        
*                                                                               
         ZIC   R1,SELLISTN         INDEX INTO LIST DIRECTORY                    
         MH    R1,=H'6'            LENGTH OF LIST DIRECTORY ENTRY               
         LA    R1,LISTDIR(R1)                                                   
         CLI   0(R1),C'D'          TEST FOR SELECT FOR DELETE                   
         BE    *+12                                                             
         CLI   0(R1),C'R'          TEST FOR SELECT FOR RESTORE                  
         BNE   DKEY2                                                            
         MVI   ERROR,INVACT        PREVENT THEM                                 
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
DKEY2    MVC   DUB,BUKCODE                                                      
         OC    DUB,SPACES                                                       
         MVC   CLICLT,DUB                                                       
         OI    CLICLTH+6,X'80'                                                  
*                                                                               
         GOTO1 VTRACE                                                           
         MVC   SVNKEY,NODKEY       READ REC THRU NODIO FOR SEL FOR CHA          
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE CLIENT RECORD                                                        
*                                                                               
VREC     L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         CLI   ACTNUM,ACTADD       TEST IF ADDING                               
         BE    VREC1                                                            
*                                                                               
         GOTO1 VGETVAL             NEED RECORD VALUES FOR ACTION CHANGE         
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    VREC17                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    VREC18                                                           
         MVI   ELCODE,BUCLTELQ     DELETE EXISTING CLIENT ELEMENT               
         GOTO1 REMELEM                                                          
         B     VREC2                                                            
*                                                                               
VREC1    LR    RE,R4               CLEAR IO AREA FOR ADD                        
         L     RF,SIZEIO                                                        
         XCEF                                                                   
*                                                                               
         MVC   BUKEY,NDKEY         INITIALIZE KEY W MASTER                      
         MVC   BURLEN,DATADISP                                                  
         OI    BURCTYP,BUKCCLT     CLIENT RECORD INDICATOR                      
*                                                                               
         MVI   FIRSTCLT,YES        SET TO ADDING FIRST CLIENT                   
         LA    R3,NDLVTAB                                                       
         LA    R3,NDLVTABL(R3)     R3=A(CLIENT LEVEL TABLE ENTRY)               
         USING NDLVTABD,R3                                                      
         OC    NDLVNOD,NDLVNOD     TEST IF NODE ESTABLISHED FOR CLIENT          
         BZ    *+8                 NO                                           
         MVI   FIRSTCLT,NO         NO-SO FIRST CLIENT ALREADY ADDED             
*                                                                               
VREC2    XC    ELEM(BUCLTLNQ),ELEM                                              
         LA    R6,ELEM             INITIALIZE CLIENT DESCRIPTION ELEM           
         USING BUCLTD,R6                                                        
         MVI   BUCLTEL,BUCLTELQ                                                 
         MVI   BUCLTLEN,BUCLTLNQ                                                
*                                                                               
         LA    R2,CLINAMH                                                       
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   BUCLTNAM,FLD        EXTRACT CLIENT NAME                          
*                                                                               
* EDIT FISCAL YEAR START MONTH/DATE                                             
*                                                                               
VREC3    LA    R2,CLIFISH                                                       
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         GOTO1 DATVAL,DMCB,(1,FLD),DUB                                          
         MVI   ERROR,INVDATE                                                    
         OC    0(4,R1),0(R1)       TEST FOR VALID DATE                          
         BZ    TRAPERR                                                          
         CLC   FLDH+5(1),3(R1)     TEST FOR EXTRA CHARACTERS                    
         BNE   TRAPERR                                                          
         MVC   DUB(2),=C'80'       SET DUMMY YEAR                               
         GOTO1 DATCON,DMCB,DUB,(3,FULL)                                         
         MVC   BUCLTST,FULL+1      EXTRACT MONTH/DAY                            
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VREC4                                                            
*                                                                               
         CLC   BUCLTST,CLTSTART    TEST FOR FISCAL YR START CHANGE              
         BE    VREC4               NO                                           
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)     POINT TO NEXT LEVEL                          
         MVI   ERROR,NOCHANGE                                                   
         OC    NDLVNOD,NDLVNOD     TEST IF CLIENT HAS PRODUCTS                  
         BNZ   TRAPERR             STOP THE CHANGE                              
*                                                                               
* EDIT FISCAL YEAR START DAY OF WEEK                                            
*                                                                               
VREC4    LA    R2,CLIDAYH                                                       
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   BUCLTDAY,X'40'      DEFAULT IS MONDAY                            
         CLI   FLDH+5,0                                                         
         BE    VREC6                                                            
*                                                                               
* DAYVAL IS NOT CORE-RESIDENT IN UK                                             
*                                                                               
         ZIC   R0,FLDH+5                                                        
         GOTO1 =V(DAYVAL),DMCB,((R0),FLD),BUCLTDAY,DUB,RR=RELO                  
*&&UK*&& MVC   BUCLTDAY,0(R1)      UK DAYVAL IS DIFFERENT                       
         MVI   ERROR,INVDAY                                                     
         CLI   BUCLTDAY,0                                                       
         BE    TRAPERR                                                          
*                                                                               
         LA    R0,7                COUNTER OF DAYS                              
         LA    R1,X'40'            DAY BIT MASK                                 
         SR    RE,RE               RE=N'DAYS                                    
VREC5    EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BUCLTDAY,0                                                       
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         SRL   R1,1                                                             
         BCT   R0,VREC5                                                         
         CH    RE,=H'1'            TEST ONLY ONE DAY BIT ON                     
         BNE   TRAPERR                                                          
*                                                                               
VREC6    CLI   ACTNUM,ACTADD                                                    
         BE    VREC8                                                            
         CLC   BUCLTDAY,CLTDAY                                                  
         BE    VREC8                                                            
         L     R3,NDLEVPTR                                                      
         LA    R3,NDLVTABL(R3)     POINT TO NEXT ENTRY                          
         MVI   ERROR,NOCHANGE                                                   
         OC    NDLVNOD,NDLVNOD     TEST IF CHANGE ALLOWED                       
         BNZ   TRAPERR             NO-CLIENT HAS PRODUCTS                       
*                                                                               
* EDIT MONTH TYPE FIELD                                                         
*                                                                               
VREC8    LA    R2,CLIMONH                                                       
         GOTO1 VGETFLD,DMCB,(R2)                                                
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   VREC8A              YES                                          
         MVI   BUCLTMON,2          DEFAULT=CALENDAR MONTH                       
         MVC   CLIMON,SPACES                                                    
         MVC   CLIMON(5),=C'CALEN'                                              
         OI    CLIMONH+6,X'80'                                                  
         B     VREC12                                                           
*                                                                               
VREC8A   LA    R0,TYPES            R0=COUNTER OF TYPES                          
         LA    RE,TYPTAB           RE=A(MONTH TYPE TABLE)                       
         USING TYPTABD,RE                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
VREC9    CLC   FLDH+5(1),TYPMINL   TEST IF MIN LEN FOR TYPE                     
         BL    *+12                                                             
         EX    R1,TYPCOMP                                                       
         BE    VREC10              FOUND A MATCH                                
         LA    RE,TYPTABL(RE)                                                   
         BCT   R0,VREC9                                                         
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VREC10   MVC   BUCLTMON,TYPNUM     EXTRACT TYPE NUMBER                          
*                                                                               
VREC12   CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VREC15                                                           
         CLC   BUCLTMON,CLTTYPE    TEST FOR CHANGE IN TYPE                      
         BE    VREC15                                                           
         L     R3,NDLEVPTR                                                      
         LA    R3,NDLVTABL(R3)                                                  
         MVI   ERROR,NOCHANGE                                                   
         OC    NDLVNOD,NDLVNOD     TEST FOR CHILDREN                            
         BNZ   TRAPERR             YES-CLIENT HAS PRODUCTS                      
*                                                                               
VREC15   GOTO1 ADDELEM                                                          
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VREC20              YES-SPECIAL CODE FOR I/O                     
*                                                                               
* I/O ROUTINE FOR PUT                                                           
*                                                                               
VREC16   MVI   IOOPT,YES                                                        
         GOTO1 VCHAACTV                                                         
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VSETKEY                                                          
         GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    VRECX                                                            
         LA    R2,CLICLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* I/O ROUTINE FOR DELETE                                                        
*                                                                               
VREC17   LA    R2,CLICLTH                                                       
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)     POINT TO NEXT LEVEL                          
         MVI   ERROR,DELERR                                                     
         OC    NDLVNOD,NDLVNOD     TEST IF CLIENT HAS PRODUCTS                  
         BNZ   TRAPERR             YES-SO CANNOT DELETE IT                      
*                                                                               
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'DELETE',SVNKEY,0                          
         CLI   NDERR,0                                                          
         BE    VREC17A                                                          
         GOTO1 VNODERR                                                          
*                                                                               
VREC17A  MVC   CONHEAD(L'DELMSG),DELMSG                                         
         OI    6(R2),X'81'         XMIT MODIFIED TO STOP DOUBLE                 
         B     VREC19              DELETE                                       
*                                                                               
* I/O ROUTINE FOR RESTORE                                                       
*                                                                               
VREC18   LA    R2,CLICLTH                                                       
         BAS   RE,REST             RESTORE RECORD                               
         MVC   CONHEAD(L'RESMSG),RESMSG                                         
         B     VREC19                                                           
*                                                                               
* RE-READ DELETED/RESTORED CLIENT FOR DISPLAY                                   
*                                                                               
VREC19   GOTO1 VSETKEY                                                          
         CLI   ACTNUM,ACTDEL                                                    
         BNE   *+8                                                              
         MVI   NDDELRSW,YES                                                     
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,NDLEVPTR                                                      
         MVC   DMDSKADD,NDLVDA                                                  
         B     DREC                NOW DISPLAY DELETED/RESTORED CLIENT          
*                                                                               
* I/O ROUTINE FOR ADD (AFTER CHECKING FOR DUPLICATE, READ HIGH TO               
* POSITION ADD)                                                                 
*                                                                               
VREC20   MVI   IOOPT,YES                                                        
         GOTO1 VADDACTV                                                         
         GOTO1 VSETKEY             INITIALIZE NODIO KEY                         
*                                                                               
         XC    NDHOOK,NDHOOK       CHECK FOR DUPLICATE KEYS                     
         MVI   NDDELRSW,YES        RETURN DELETED RECORDS                       
         MVC   NDIOA,AIO2          GET RECORD INTO IO2                          
         MVC   NDIOA2,AIO3                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    VREC22              YES-GO AHEAD WITH ADD                        
         CLI   NDERR,0                                                          
         BE    VREC21              FOUND RECORD-HAVE DUPLICATE                  
         LA    R2,CLICLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC21   L     R4,NDIOA                                                         
         MVI   ERROR,DELEXIST                                                   
         LA    R2,CLICLTH                                                       
         XC    KEY,KEY             READ DIRECTORY ENTRY TO                      
         MVC   KEY(L'BUKEY),BUKEY  DISCOVER IF CLIENT IS DELETED                
         OI    DMINBTS,X'80'                                                    
         GOTO1 READ                                                             
         TM    KEY+(BUKCSTA-BUKEY),X'80'                                        
         BO    *+8                 YES                                          
         MVI   ERROR,RECEXIST                                                   
         B     TRAPERR                                                          
*                                                                               
VREC22   GOTO1 VSETKEY             NOW POSITION FOR ADD                         
         MVI   NDDELRSW,0          DO NOT WANT DELETES                          
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0             TEST FOUND SOMETHING                         
         BE    VREC23              YES-ADD BEFORE IT                            
         CLI   NDERR,NDRNFERR      TEST FOUND NOTHING                           
         BE    VREC24              YES-ADD RECORD AT END                        
         LA    R2,CLICLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC23   GOTO1 VSETKEY             INITIALIZE NODIO KEY                         
         L     R4,NDIOA                                                         
         MVC   DUB,BUKCODE         EXTRACT CODE FROM RETURNED RECORD            
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2         GET RECORD INTO IO2                          
         LA    R0,L'CLTCODE                                                     
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'ADD'),NODKEY,((R0),DUB),   X        
               0,0                                                              
         CLI   NDERR,0                                                          
         BE    VREC25                                                           
         LA    R2,CLICLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC24   GOTO1 VSETKEY                                                          
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'ADD'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    VREC25                                                           
         LA    R2,CLICLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC25   L     R4,NDIOA                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  EXTRACT KEY FROM RECORD                      
         MVC   CONHEAD(L'ADDMSG),ADDMSG                                         
         MVC   TWAKEYSV,KEY                                                     
         CLI   FIRSTCLT,YES        TEST IF ADDING FIRST CLIENT                  
         BNE   VRECX               NO                                           
         MVI   NDOPENSW,NO         FORCE RE-OPENING OF NODIO FILE               
         XC    SVUSRNAM,SVUSRNAM   ON NEXT TRANSACTION                          
         XC    SVUSRADD,SVUSRADD                                                
*                                                                               
VRECX    B     XIT                                                              
         SPACE 2                                                                
TYPCOMP  CLC   FLD(0),TYPNAME                                                   
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO RESTORE A CLIENT RECORD AND POSITION TO ORIGINAL PLACE         
*                                                                               
REST     NTR1                                                                   
         GOTO1 VSETKEY                                                          
         XC    NDHOOK,NDHOOK                                                    
         MVI   NDDELRSW,0          DO NOT READ DELETES                          
         MVC   NDIOA,AIO2          READ HIGH FOR FIND CODE                      
         MVC   NDIOA2,AIO3         SEQUENTIAL POSITION                          
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    REST2                                                            
         CLI   NDERR,NDRNFERR      TEST FOR RECORD NOT FOUND                    
         BE    REST4               YES-RESTORE TO END                           
         GOTO1 VNODERR                                                          
*                                                                               
REST2    GOTO1 VSETKEY                                                          
         L     R3,NDLEVPTR                                                      
         MVC   DUB,NDLVCOD         EXTRACT CODE OF RETURNED RECD.               
         LA    R0,L'CLTCODE                                                     
         MVC   NDIOA,AIO1          RESTORE I/O AREA POINTERS                    
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'RES'),NODKEY,((R0),DUB),0           
         CLI   NDERR,0                                                          
         BE    RESTX                                                            
         GOTO1 VNODERR                                                          
*                                                                               
REST4    GOTO1 VSETKEY                                                          
         MVC   NDIOA,AIO1          RESTORE CLIENT TO END                        
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'RES'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    RESTX                                                            
         GOTO1 VNODERR                                                          
*                                                                               
RESTX    B     XIT                                                              
         EJECT                                                                  
* DISPLAY CLIENT RECORD                                                         
*                                                                               
DREC     GOTO1 VCLEARF,DMCB,CLINAMH,CLILAST                                     
         L     R4,AIO                                                           
         USING BURECD,R4                                                        
         GOTO1 VGETVAL             GET CLIENT RECORD VALUES                     
         MVC   CLINAM,CLTNAM                                                    
         BAS   RE,DISFIS           FISCAL YEAR START                            
         MVC   CLIFIS,WORK                                                      
         BAS   RE,DISDAY                                                        
         MVC   CLIDAY,WORK                                                      
         BAS   RE,DISTYPE                                                       
         MVC   CLIMON,WORK                                                      
*                                                                               
DREC2    CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DRECX                                                            
         MVC   CLIDA(3),=C'DA='                                                 
         GOTO1 HEXOUT,DMCB,DMDSKADD,CLIDA+3,4,=C'TOG'                           
         MVC   CLIDA+12(5),=C'NODE='                                            
         GOTO1 (RF),DMCB,BUKNODE,CLIDA+17,4,=C'TOG'                             
         OI    CLIDAH+6,X'80'      XMIT                                         
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
* LIST CLIENT RECORDS/PRINT CLIENT REPORT                                       
*                                                                               
PREP     LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LIST     MVI   NDUPDTSW,NO         DO NOT READ FOR UPDATE                       
         CLI   ACTNUM,ACTREP       TEST FOR REPORT                              
         BE    LIST2               YES                                          
         CLI   THISPF,PF7          TEST PF7=PREVIOUS PAGE                       
         BNE   LIST2                                                            
         LR    RE,R7                                                            
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R2,CONACTH                                                       
         CR    RE,R2               TEST ANYTHING CHANGED AFTER RECORD           
         BH    LIST2               YES                                          
         LA    R3,NDLVTAB+NDLVTABL                                              
         USING NDLVTABD,R3                                                      
         OC    NDLVNOD,NDLVNOD     TEST FOR ANY CLIENTS                         
         BNZ   LIST8               YES-PERFORM BACKWARDS READ                   
*                                                                               
LIST2    OC    KEY,KEY             TEST FOR FIRST TIME                          
         BNZ   LIST4                                                            
*                                                                               
         MVI   SVNCLTS,0                                                        
         XC    SVFSTKEY,SVFSTKEY                                                
         GOTO1 VSETKEY                                                          
         LA    R1,LISTHK           SET IN-LINE HOOK                             
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST6                                                            
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    XIT                 YES-NO MORE RECORDS SO EXIT                  
         CLI   ACTNUM,ACTREP                                                    
         BE    XIT                                                              
         LA    R2,CLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* RE-SET FOR SEQUENTIAL WITH A READ                                             
*                                                                               
LIST4    MVI   SVNCLTS,0                                                        
         XC    SVFSTKEY,SVFSTKEY                                                
         MVC   NODKEY,SVNKEY                                                    
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST6                                                            
         LA    R2,CLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* READ SEQUENTIAL TO FILL SCREEN OR UP TO EOF                                   
*                                                                               
LIST6    SR    RE,RE                                                            
         ICM   RE,1,NDSQBACK       SET SEQUENTIAL BACK POINTER                  
         BZ    *+10                TO READ ONLY CLIENTS                         
         BCTR  RE,0                                                             
         STC   RE,NDSQBACK                                                      
*                                                                               
         LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'SEQ',NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    XIT                                                              
         LA    R2,CLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* GO BACK TO PREVIOUS SCREEN                                                    
*                                                                               
LIST8    BAS   RE,BACK             BACK UP TO START OF PREVIOUS SCREEN          
         MVC   NODKEY,NODKEYSV     SET START POINT OF READ                      
         MVI   SVNCLTS,0                                                        
         XC    SVFSTKEY,SVFSTKEY                                                
         LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,NDSQBACK                                                    
         BZ    *+10                                                             
         BCTR  RE,0                RESET SQBACK TO INCLUDE ALL CLIENTS          
         STC   RE,NDSQBACK                                                      
         GOTO1 (RF),(R1),NODBLKD,=C'SEQ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         LA    R2,CLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* HOOK TO PROCESS RECORDS RETURNED BY NODIO                                     
*                                                                               
LISTHK   NTR1                                                                   
         CLI   NDMODE,NDFRST       TEST FOR FIRST TIME AT LEVEL                 
         BNE   LISTHK2                                                          
         CLI   NDLEV,1             TEST FOR RIGHT LEVEL                         
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SET SKIP LOWER LEVELS TO READ                
         B     LISTHKX             ONLY CLIENTS                                 
*                                                                               
LISTHK2  CLI   NDMODE,NDPROC                                                    
         BNE   LISTHKX                                                          
         CLI   NDLEV,1             TEST FOR LEVEL 1 RECORD                      
         BNE   LISTHKX                                                          
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCCLT     MAKE SURE IT IS A CLIENT                     
         BZ    LISTHKX                                                          
*                                                                               
         GOTO1 VGETVAL                                                          
         CLI   ACTNUM,ACTREP       TEST FOR REPORT                              
         BE    LISTHK4                                                          
*                                                                               
         L     R3,NDLEVPTR                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),NDLVKEY EXTRACT KEY                                 
         MVC   DMDSKADD,NDLVDA     AND THE DISK ADDRESS                         
         OC    SVFSTKEY,SVFSTKEY   TEST FOR FIRST KEY ON SCREEN                 
         BNZ   *+10                YES                                          
         MVC   SVFSTKEY,NODKEY                                                  
         MVC   SVNKEY,NODKEY       SAVE LAST NODAL KEY                          
         ZIC   R1,SVNCLTS          INCREMENT N'CLIENT RECS ON SCREEN            
         LA    R1,1(R1)                                                         
         STC   R1,SVNCLTS                                                       
*                                                                               
         LA    RF,DISLIST                                                       
         CLI   TRACEOPT,YES                                                     
         BNE   *+8                                                              
         LA    RF,TRALIST                                                       
         BASR  RE,RF                                                            
         GOTO1 LISTMON                                                          
         B     LISTHKX                                                          
*                                                                               
LISTHK4  BAS   RE,DISREP                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 (RF),PARAS,(R8)                                                  
*                                                                               
LISTHKX  B     XIT                                                              
*                                                                               
LISTHD   DC    C'CODE NAME                  START   DAY   MONTH'                
TRACEHD  DC    C'CODE NODE   D/A      FORWARD  BACKWARD ATTACH FIRST   X        
                LAST'                                                           
         EJECT                                                                  
* SUB-ROUTINE TO READ BACKWARDS ON FILE TO FIND START OF PREVIOUS               
* SCREEN (ON EXIT, NODKEYSV CONTAINS START KEY)                                 
*                                                                               
BACK     NTR1                                                                   
         MVC   NODKEY,SVFSTKEY     START WITH FIRST KEY ON LIST SCREEN          
         MVC   NODKEYSV,NODKEY     SET START KEY FOR 1ST PAGE                   
         XC    NDHOOK,NDHOOK                                                    
         CLI   SVNCLTS,0           TEST ANYTHING ON SCREEN                      
         BNE   BACK1               YES                                          
         MVC   NODKEY,SVNKEY                                                    
         LA    R1,BACKHK                                                        
         ST    R1,NDHOOK                                                        
*                                                                               
BACK1    GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BACK2    LA    R1,BACKHK                                                        
         ST    R1,NDHOOK                                                        
         ZIC   R1,NDSQBACK                                                      
         BCTR  R1,0                                                             
         STC   R1,NDSQBACK                                                      
         MVI   NDSKIP,YES                                                       
         GOTO1 VNODIO,DMCB,NODBLKD,=C'BSEQ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    BACKX                                                            
         CLI   NDERR,NDRNFERR                                                   
         BE    BACKX                                                            
         LA    R2,CLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
BACKX    B     XIT                                                              
         SPACE 2                                                                
*  HOOK ROUTINE FOR BACKWARDS READ                                              
*                                                                               
BACKHK   ST    RE,SAVERE                                                        
         CLI   NDMODE,NDFRST                                                    
         BNE   BACKHK2                                                          
         CLI   NDLEV,1                                                          
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SKIP READ OF LOWER LEVEL RECORDS             
         B     BACKHKX                                                          
*                                                                               
BACKHK2  CLI   NDMODE,NDPROC                                                    
         BNE   BACKHKX                                                          
         CLI   NDLEV,1             CHECK FOR A CLIENT                           
         BNE   BACKHKX                                                          
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCCLT                                                  
         BZ    BACKHKX                                                          
*                                                                               
         MVC   NODKEYSV,NODKEY     SAVE LAST KEY FOUND                          
         ZIC   R1,NCLTS            UPDATE N'CLIENTS FOUND                       
         LA    R1,1(R1)                                                         
         STC   R1,NCLTS                                                         
         CLC   NCLTS,NLISTS        TEST IF SCREEN FILLED                        
         BL    *+8                 NO                                           
         MVI   NDMODE,NDEND        YES-STOP NODIO READ                          
*                                                                               
BACKHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* HEADLINE PRINTING ROUTINE HOOK                                                
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+1(11),=C'ALL CLIENTS'                                         
         ICM   R3,15,ABOX          TEST OFF LINE                                
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
         MVI   BOXROWS+6,C'T'      SET UP FOR BOXES                             
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   18(R2),C'L'                                                      
         MVI   26(R2),C'C'                                                      
         MVI   48(R2),C'C'                                                      
         MVI   56(R2),C'C'                                                      
         MVI   61(R2),C'C'                                                      
         MVI   67(R2),C'R'                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY LIST DATA LINE (PRECEDE WITH GETVAL CALL)              
*                                                                               
DISLIST  NTR1                                                                   
         XC    CLLHED,CLLHED       DISPLAY HEADLINE                             
         MVC   CLLHED(L'LISTHD),LISTHD                                          
         OI    CLLHEDH+6,X'80'                                                  
         LA    R2,LISTAR                                                        
         USING LSTLIND,R2                                                       
         MVC   LISTAR,SPACES                                                    
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVC   LSTCLT,BUKCODE      EXTRACT ACTUAL CLIENT CODE                   
         MVC   LSTNAM,CLTNAM                                                    
         BAS   RE,DISFIS           FISCAL YEAR START                            
         MVC   LSTSTART,WORK                                                    
*                                                                               
         BAS   RE,DISDAY           DAY                                          
         MVC   LSTDAY,WORK                                                      
*                                                                               
         BAS   RE,DISTYPE          FISCAL MONTH TYPE                            
         MVC   LSTMONTH,WORK                                                    
*                                                                               
DISLISTX B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY A NODAL TRACE FOR A CLIENT                             
* AT ENTRY, R3=A(LEVEL TABLE ENTRY), R4=A(RECORD)                               
*                                                                               
TRALIST  NTR1                                                                   
         MVC   CLLHED,SPACES       CLEAR LIST HEADLINE                          
         MVC   CLLHED(L'TRACEHD),TRACEHD SET LIST HEADLINE                      
         OI    CLLHEDH+6,X'80'     XMIT                                         
         LA    R2,LISTAR                                                        
         USING TRALIND,R2                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   TRACOD,CLTCODE                                                   
         OC    TRACOD,SPACES                                                    
         GOTO1 HEXOUT,DMCB,BUKNODE+1,TRANODE,3,=C'TOG'                          
         GOTO1 (RF),DMCB,DMDSKADD,TRADA,4,=C'TOG'                               
         LA    R6,BUFRSTEL                                                      
*                                                                               
TRALIST2 CLI   0(R6),0             TEST FOR EOR                                 
         BE    TRALISTX                                                         
*                                                                               
         CLI   0(R6),X'B8'         TEST FOR FORWARD ATTACHMENT ELEM             
         BNE   TRALIST3                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRAFWD(0),2(R6)     EXTRACT CODE                                 
         B     TRALIST8                                                         
*                                                                               
TRALIST3 CLI   0(R6),X'B9'         TEST FOR BACKWARD ATTACHMENT ELEM            
         BNE   TRALIST4                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRABKWD(0),2(R6)                                                 
         B     TRALIST8                                                         
*                                                                               
TRALIST4 CLI   0(R6),X'B1'         TEST FOR NODE ATTACHMENT ELEM                
         BNE   TRALIST5                                                         
         GOTO1 HEXOUT,DMCB,3(R6),TRAATT,3,=C'TOG'                               
         B     TRALIST8                                                         
*                                                                               
TRALIST5 CLI   0(R6),X'B6'         TEST FOR FIRST CODE AT NEXT LEVEL            
         BNE   TRALIST6                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRAFIRST(0),2(R6)                                                
         B     TRALIST8                                                         
*                                                                               
TRALIST6 CLI   0(R6),X'B7'         TEST FOR LAST CODE AT NEXT LEVEL             
         BNE   TRALIST8                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRALAST(0),2(R6)                                                 
*                                                                               
TRALIST8 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     TRALIST2                                                         
*                                                                               
TRALISTX B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY PRINT LINE (PRECEDE WITH GETVAL CALL)                  
*                                                                               
DISREP   NTR1                                                                   
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVC   PRTCLT,CLTCODE      EXTRACT ACTUAL CLIENT CODE                   
         MVC   PRTNAM,CLTNAM                                                    
         BAS   RE,DISFIS           FISCAL YEAR START                            
         MVC   PRTFIS,WORK                                                      
*                                                                               
         BAS   RE,DISDAY           DAY                                          
         MVC   PRTDAY,WORK                                                      
*                                                                               
         BAS   RE,DISTYPE          FISCAL MONTH TYPE                            
         MVC   PRTMON,WORK                                                      
*                                                                               
DISREPX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* DISPLAY FISCAL START DATE (OUTPUT AT WORK)                                    
*                                                                               
DISFIS   ST    RE,SAVERE                                                        
         MVC   WORK(L'CLIFIS),SPACES CLEAR MAXIMUM OUTPUT AREA                  
         MVC   DUB+1(2),CLTSTART                                                
         MVI   DUB,80                                                           
         GOTO1 DATCON,DMCB,(3,DUB),(7,WORK)                                     
         B     DISX                                                             
         SPACE 2                                                                
* DISPLAY FISCAL YEAR START DAY (OUTPUT AT WORK)                                
*                                                                               
DISDAY   ST    RE,SAVERE                                                        
         LA    RE,DAYTAB           RE=A(DAY TABLE)                              
         LA    R0,DAYS             R0=COUNTER                                   
DISDAY1  CLC   CLTDAY,0(RE)                                                     
         BE    DISDAY2                                                          
         LA    RE,L'DAYTAB(RE)                                                  
         BCT   R0,DISDAY1                                                       
         DC    H'0'                                                             
*                                                                               
DISDAY2  MVC   WORK(3),1(RE)                                                    
         B     DISX                                                             
         SPACE 2                                                                
* DISPLAY FISCAL YEAR TYPE (OUTPUT AT WORK)                                     
*                                                                               
DISTYPE  ST    RE,SAVERE                                                        
         LA    R0,TYPES                                                         
         LA    RE,TYPTAB                                                        
         USING TYPTABD,RE                                                       
         CLC   CLTTYPE,TYPNUM      MATCH ON TYPE NUMBER                         
         BE    DISTYPE2                                                         
         LA    RE,TYPTABL(RE)                                                   
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
DISTYPE2 MVC   WORK(L'TYPNAME),TYPNAME                                          
         B     DISX                                                             
         SPACE 2                                                                
DISX     L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* ROUTINES ETC.                                                                 
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
ADDMSG   DC    C'CLIENT RECORD ADDED TO FILE'                                   
DELMSG   DC    C'CLIENT RECORD DELETED'                                         
RESMSG   DC    C'CLIENT RECORD RESTORED'                                        
         SPACE 2                                                                
* TABLE OF MOBILE TYPES AND THEIR EXPANDED VALUES                               
*                                                                               
TYPTAB   DS    0XL(TYPTABL)                                                     
         DC    AL1(00),AL1(1),CL5'BROAD'                                        
         DC    AL1(02),AL1(1),CL5'CALEN'                                        
         DC    AL1(06),AL1(3),CL5'544'                                          
         DC    AL1(07),AL1(3),CL5'454'                                          
         DC    AL1(08),AL1(3),CL5'445'                                          
         DC    AL1(10),AL1(3),CL5'444'                                          
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
         SPACE 2                                                                
* TABLE OF DAY BITS AND THEIR CORRESPONDING OUTPUT                              
*                                                                               
DAYTAB   DS    0CL4                                                             
         DC    X'40',CL3'MON'                                                   
         DC    X'20',CL3'TUE'                                                   
         DC    X'10',CL3'WED'                                                   
         DC    X'08',CL3'THU'                                                   
         DC    X'04',CL3'FRI'                                                   
         DC    X'02',CL3'SAT'                                                   
         DC    X'01',CL3'SUN'                                                   
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         EJECT                                                                  
* CLIENT REPORT SPEC POOL                                                       
*                                                                               
HEDSPECS DS    0H                                                               
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,49,C'CLIENT REPORT'                                           
         SSPEC H2,49,C'-------------'                                           
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
         SSPEC H8,20,C'CLIENT'                                                  
         SSPEC H9,21,C'CODE'                                                    
         SSPEC H8,28,C'CLIENT NAME'                                             
         SSPEC H8,50,C'FISCAL'                                                  
         SSPEC H9,50,C'START'                                                   
         SSPEC H8,58,C'DAY'                                                     
         SSPEC H8,63,C'MONTH'                                                   
         DC    X'00'                                                            
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF2D                                                       
         EJECT                                                                  
* DSECT TO COVER LIST SCREEN                                                    
*                                                                               
         ORG   CONTAGH                                                          
*      ++INCLUDE BUFILE2D                                                       
         EJECT                                                                  
* DSECT TO COVER OVERLAY WORKING STORAGE                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
FIRSTCLT DS    C                   ADDING FIRST CLIENT (Y,N)                    
TRACEOPT DS    C                   Y=TRACE OPTION ACTIVE FOR LIST               
NCLTS    DS    X                   N'CLIENTS FOUND GOING BACKWARDS              
         ORG   TWA1USER                                                         
SVNCLTS  DS    X                   N'CLIENTS ON LIST SCREEN                     
SVFSTKEY DS    CL(L'NODKEY)        1ST REC ON LIST SCREEN NODAL KEY             
         SPACE 2                                                                
* DSECT TO COVER MONTH TYPE TABLE                                               
*                                                                               
TYPTABD  DSECT                                                                  
TYPNUM   DS    X                   TYPE NUMBER                                  
TYPMINL  DS    X                   MINIMUM LENGTH FOR VALIDATION                
TYPNAME  DS    CL5                                                              
TYPTABL  EQU   *-TYPTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER LIST DISPLAY LINE                                              
*                                                                               
LSTLIND  DSECT                                                                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSTCLT   DS    CL3                                                              
         DS    CL2                 SPARE                                        
LSTNAM   DS    CL20                                                             
         DS    CL2                 SPARE                                        
LSTSTART DS    CL5                                                              
         DS    CL3                 SPARE                                        
LSTDAY   DS    CL3                                                              
         DS    CL3                 SPARE                                        
LSTMONTH DS    CL5                                                              
         SPACE 2                                                                
* DSECT TO COVER TRACE LIST LINE                                                
*                                                                               
TRALIND  DSECT                                                                  
TRACOD   DS    CL3                 CLIENT CODE                                  
         DS    CL2                                                              
TRANODE  DS    CL6                 CLIENT'S NODE                                
         DS    C                                                                
TRADA    DS    CL8                 DISK ADDRESS                                 
         DS    C                                                                
TRAFWD   DS    CL8                 FORWARD POINTER (CURRENT LEVEL)              
         DS    C                                                                
TRABKWD  DS    CL8                 BACKWARD POINTER (CURRENT LEVEL)             
         DS    C                                                                
TRAATT   DS    CL6                 NODE OF ATTACHED LEVEL                       
         DS    C                                                                
TRAFIRST DS    CL8                 FIRST CODE (NEXT LEVEL)                      
         DS    C                                                                
TRALAST  DS    CL8                 LAST CODE (NEXT LEVEL)                       
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    CL20                                                             
PRTCLT   DS    CL3                 CLIENT CODE                                  
         DS    CL4                                                              
PRTNAM   DS    CL20                CLIENT NAME                                  
         DS    CL2                                                              
PRTFIS   DS    CL5                 FISCAL YEAR START                            
         DS    CL3                                                              
PRTDAY   DS    CL3                 DAY                                          
         DS    CL2                                                              
PRTMON   DS    CL5                 FISCAL MONTH TYPE                            
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
PF7      EQU   X'07'                                                            
PF8      EQU   X'08'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012BUFIL02   05/01/02'                                      
         END                                                                    
