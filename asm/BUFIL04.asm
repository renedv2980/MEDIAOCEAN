*          DATA SET BUFIL04    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T50204A                                                                  
         TITLE 'T50204 - BUDGET CONTROL LFM - PLAN RECORD'                      
T50204   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI04**,RA,RR=R2                                              
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
         ST    RB,MYBASE                                                        
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
         LA    R2,PLACLTH          VALIDATE CLIENT                              
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,PLLCLTH                                                       
         GOTO1 VVALCLT,PARAS,(R2),0                                             
*                                                                               
         MVC   SVCLTVAL,CLTVALS    SAVE CLIENT RECORD VALUES                    
         CLI   ACTNUM,ACTLIST      TEST FOR ACTION LIST                         
         BE    VKEY2               YES-SKIP DISPLAY OF CLIENT NAME              
         XC    PLACLN,PLACLN       CLEAR CLIENT NAME                            
         MVC   PLACLN(L'CLTNAM),CLTNAM                                          
         OI    PLACLNH+6,X'80'     XMIT                                         
*                                                                               
* EDIT AND VALIDATE PRODUCT                                                     
*                                                                               
VKEY2    LA    R2,PLAPRDH          VALIDATE PRODUCT                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,PLLPRDH                                                       
*                                                                               
         SR    R0,R0                                                            
         CLI   ACTNUM,ACTREP       TEST FOR REPORT                              
         BNE   *+8                                                              
         ICM   R0,8,MISSPARM       YES-ALLOW NO INPUT ('ALL')                   
*                                                                               
         GOTO1 VVALPRD,PARAS,(R2),(R0)                                          
         OC    PRDCODE,PRDCODE     TEST IF 'ALL' INPUT                          
         BZ    VKEYX               YES-ALL DONE WITH EDIT                       
*                                                                               
         MVC   SVPRDVAL,PRDVALS    SAVE AWAY PRODUCT VALUES                     
         CLI   ACTNUM,ACTLIST      TEST FOR ACTION LIST                         
         BE    VKEY4               YES-SKIP DISPLAY OF PRODUCT NAME             
         XC    PLAPRN,PLAPRN       CLEAR PRODUCT NAME                           
         MVC   PLAPRN(L'PRDNAM),PRDNAM                                          
         OI    PLAPRNH+6,X'80'     XMIT                                         
         CLI   ACTNUM,ACTREP       TEST FOR REPORT                              
         BE    VKEYX               YES-EDIT ALL DONE                            
*                                                                               
* EDIT AND VALIDATE PLAN CODE                                                   
*                                                                               
VKEY4    LA    R2,PLACODH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,PLLCODH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES                                                        
         GOTO1 VFVAL                                                            
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEYX                                                            
         B     TRAPERR                                                          
*                                                                               
         MVC   PLANCODE,FLD                                                     
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX               YES-ALL DONE                                 
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BNE   VKEY5                                                            
         OI    WHENOK,X'01'        BYPASS GENCON MAINTENANCE                    
         B     VKEYX                                                            
*                                                                               
VKEY5    CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    *+16                YES                                          
         CLI   ACTNUM,ACTREST      TEST FOR RESTORE                             
         BNE   *+12                NO                                           
         MVI   NDDELRSW,YES        READ DELETED RECORDS FOR RESTORE             
         OI    WHENOK,X'01'        BYPASS GENCON MAINT. FOR DEL/RES             
*                                                                               
         GOTO1 VSETKEY                                                          
         LA    R1,VKEY6            IN-LINE HOOK                                 
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    VKEYX                                                            
         L     R2,FADDR                                                         
         GOTO1 VNODERR                                                          
*                                                                               
* NODIO HOOK ROUTINE FOR PLAN                                                   
*                                                                               
VKEY6    ST    RE,SAVERE                                                        
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BE    VKEY8                                                            
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNE   VKEY8                                                            
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCPLN     TEST FOR PLAN RECORD                         
         BZ    VKEY8                                                            
         CLI   NDLEV,3             TEST NODIO IS IN SYNC                        
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 VGETVAL             EXTRACT RECORD VALUES                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  EXTRACT KEY FROM RECORD                      
         MVC   SVNKEY,NODKEY       SAVE NODAL KEY                               
*                                                                               
VKEY8    L     RE,SAVERE           RETURN TO NODIO                              
         BR    RE                                                               
*                                                                               
VKEYX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*        DISPLAY KEY                                                            
*                                                                               
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         USING BURECD,R4                                                        
         MVC   CLTVALS,SVCLTVAL    RESTORE CLIENT VALUES                        
         MVC   PRDVALS,SVPRDVAL    RESTORE PRODUCT VALUES                       
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
DKEY2    XC    DUB,DUB                                                          
         MVC   DUB(L'CLTCODE),CLTCODE                                           
         OC    DUB,SPACES                                                       
         MVC   PLACLT,DUB                                                       
         OI    PLACLTH+6,X'80'                                                  
*                                                                               
         MVC   PLACLN,CLTNAM                                                    
         OI    PLACLNH+6,X'80'                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(L'PRDCODE),PRDCODE                                           
         OC    DUB,SPACES                                                       
         MVC   PLAPRD,DUB                                                       
         OI    PLAPRDH+6,X'80'                                                  
*                                                                               
         MVC   PLAPRN,PRDNAM                                                    
         OI    PLAPRNH+6,X'80'                                                  
*                                                                               
         MVC   DUB,BUKCODE         DISPLAY PLAN CODE                            
         OC    DUB,SPACES                                                       
         MVC   PLACOD,DUB                                                       
         OI    PLACODH+6,X'80'                                                  
*                                                                               
         GOTO1 VTRACE                                                           
         MVC   SVNKEY,NODKEY       SAVE NODAL KEY (SELECT FOR CHANGE)           
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE PLAN RECORD                                                          
*                                                                               
VREC     L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVC   CLTVALS,SVCLTVAL    RESTORE CLIENT VALUES                        
         MVC   PRDVALS,SVPRDVAL    RESTORE PRODUCT VALUES                       
*                                                                               
         CLI   ACTNUM,ACTADD       TEST IF ADDING                               
         BE    VREC1                                                            
         GOTO1 VGETVAL             EXTRACT PLAN VALUES BEFORE CHANGE            
         CLI   ACTNUM,ACTDEL                                                    
         BE    VREC17                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    VREC18                                                           
         MVI   ELCODE,BUPLNELQ                                                  
         GOTO1 REMELEM                                                          
         B     VREC2                                                            
*                                                                               
VREC1    LR    RE,R4               CLEAR IO AREA FOR ADD                        
         L     RF,SIZEIO                                                        
         XCEF                                                                   
*                                                                               
         MVC   BUKEY,NDKEY         INITIALIZE KEY W MASTER                      
         MVC   BURLEN,DATADISP                                                  
         OI    BURCTYP,BUKCPLN     PLAN RECORD INDICATOR                        
         XC    ELEM(BUPLNLNQ),ELEM CLEAR AREA FOR PLAN ELEM                     
*                                                                               
VREC2    LA    R6,ELEM             INITIALIZE PLAN DESCRIPTION ELEM             
         USING BUPLND,R6                                                        
         MVI   BUPLNEL,BUPLNELQ                                                 
         MVI   BUPLNLEN,BUPLNLNQ                                                
         CLI   ACTNUM,ACTADD       TEST FOR ACTION ADD                          
         BNE   *+8                 NO                                           
         OI    BUPLNIND,BUPLNCOD   RESERVE 'X' CODES ON PLAN                    
*                                                                               
         LA    R2,PLANAMH          EDIT PLAN NAME                               
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   BUPLNNAM,FLD                                                     
*                                                                               
* EDIT AND VALIDATE PLAN PERIOD                                                 
*                                                                               
VREC4    LA    R2,PLASEH                                                        
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         GOTO1 VFVAL                                                            
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0            TEST FOR NOTHING FOUND                       
         BE    TRAPERR             NO                                           
*                                                                               
VREC5    MVI   ERROR,INVDATE                                                    
*                                                                               
         XC    FULL,FULL                                                        
         SR    R0,R0                                                            
*                                                                               
         CLI   CLTTYPE,10          TEST FOR 444 FISCAL                          
         BE    *+8                 YES-DON'T PASS ST MON OF FISCAL YEAR         
         ICM   R0,8,CLTSTART                                                    
         ICM   R0,4,CLTTYPE                                                     
         ICM   R0,2,YESPARM                                                     
         GOTO1 VMONVAL,DMCB,FLD,FULL,(R0)                                       
         OC    4(4,R1),4(R1)                                                    
         BZ    SPERR                                                            
         MVC   BUPLNST,4(R1)                                                    
         MVC   BUPLNEND,6(R1)                                                   
*                                                                               
VREC6    CLI   BUPLNST,0           TEST OPEN-ENDED PLAN                         
         BE    VREC8               YES-SKIP FISCAL YEAR LIMIT CHECK             
*                                                                               
         MVI   ERROR,INVEBFRS                                                   
         CLC   BUPLNST,BUPLNEND                                                 
         BH    TRAPERR                                                          
*                                                                               
         MVI   ERROR,MOREFIS                                                    
*                                                                               
         LA    RF,12               RF=N'MONTHS IN YEAR                          
         CLI   CLTTYPE,10                                                       
         BNE   *+8                                                              
         LA    RF,13                                                            
         ST    RF,FULL                                                          
         ZIC   R1,BUPLNST          GET START YEAR                               
         M     R0,FULL             CONVERT TO MONTHS                            
         ZIC   R0,BUPLNST+1                                                     
         AR    R1,R0               ADD IN MONTHS                                
         ZIC   RF,BUPLNEND         GET END YEAR                                 
         M     RE,FULL                                                          
         ZIC   RE,BUPLNEND+1                                                    
         AR    RF,RE               CONVERT END YEAR/MONTH TO MONTHS             
         SR    RF,R1               RF=N'MONTHS BETWEEN                          
         LA    RF,1(RF)            N'MONTHS INCLUSIVE                           
         C     RF,FULL             TEST FOR MORE THAN FISCAL YEAR               
         BH    TRAPERR                                                          
*                                                                               
VREC8    CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VREC10                                                           
         CLC   BUPLNST(4),PLANST   TEST FOR CHANGE IN PLAN PERIOD               
         BE    VREC10              NO                                           
         MVI   ERROR,PERCHERR                                                   
         TM    BUPLNIND,BUPLNDAT   TEST IF PLAN HAS DATA RECORDS                
         BO    TRAPERR                                                          
*                                                                               
* EDIT OPTIONS FIELD                                                            
*                                                                               
VREC10   NC    BUPLNIND,=X'8400'   TURN OFF ALL BITS EXCEPT DATA+CODE           
         BAS   RE,OPTED                                                         
         GOTO1 ADDELEM             ADD PLAN ELEMENT                             
*                                                                               
* EDIT FISCAL MONTH OVERRIDES FIELD                                             
*                                                                               
VREC12   MVI   ELCODE,BUINELQ      DELETE SCREEN INPUT + FISCAL                 
         GOTO1 REMELEM             OVERRIDE ELEMENTS                            
         MVI   ELCODE,BUFOVELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,PLAFOVH                                                       
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         XC    FLAST,FLAST                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    VREC14              NO                                           
*                                                                               
VREC13   LA    R6,ELEM                                                          
         USING BUIND,R6                                                         
         MVI   BUINEL,BUINELQ                                                   
         MVI   BUINSEQ,1                                                        
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUINPUT(0),FLD      MOVE SCREEN INPUT TO ELEM                    
         LA    R1,BUINPUT-BUIND+1(R1) COMPUTE ELEM LENGTH                       
         STC   R1,BUINLEN                                                       
         GOTO1 ADDELEM                                                          
         BAS   RE,FOVED            EDIT FISCAL OVERRIDES                        
*                                                                               
VREC14   CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VREC20              YES-SPECIAL CODE FOR I/O                     
*                                                                               
* I/O ROUTINE FOR PUT                                                           
*                                                                               
VREC15   MVI   IOOPT,YES                                                        
         GOTO1 VCHAACTV                                                         
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',SVNKEY,0                             
         CLI   NDERR,0                                                          
         BE    VRECX                                                            
         LA    R2,PLACODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* I/O ROUTINE FOR DELETE                                                        
*                                                                               
VREC17   LA    R2,PLACODH                                                       
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)     POINT TO NEXT LEVEL ENTRY                    
         MVI   ERROR,DELERR                                                     
         OC    NDLVNOD,NDLVNOD     TEST IF PLAN HAS OUTLINES                    
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
VREC18   LA    R2,PLACODH                                                       
         BAS   RE,REST             RESTORE RECORD                               
         MVC   CONHEAD(L'RESMSG),RESMSG                                         
         B     VREC19                                                           
*                                                                               
* RE-READ DELETED/RESTORED PLAN FOR DISPLAY                                     
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
         B     DREC                NOW DISPLAY DELETED/RESTORED PLAN            
*                                                                               
* I/O ROUTINE FOR ADD (READ TO CHECK FOR DUPLICATES AND THEN HIGH TO            
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
         BE    VREC22              YES-PROCEED WITH ADD                         
         CLI   NDERR,0                                                          
         BE    VREC21              FOUND A DUPLICATE                            
         LA    R2,PLACODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC21   L     R4,NDIOA                                                         
         MVI   ERROR,DELEXIST                                                   
         LA    R2,PLACODH                                                       
         XC    KEY,KEY             READ DIRECTORY POINTER TO                    
         MVC   KEY(L'BUKEY),BUKEY  DISCOVER IF RECORD IS DELETED                
         OI    DMINBTS,X'08'                                                    
         GOTO1 READ                                                             
         TM    KEY+(BUKCSTA-BUKEY),X'80'                                        
         BO    *+8                 YES                                          
         MVI   ERROR,RECEXIST                                                   
         B     TRAPERR                                                          
*                                                                               
VREC22   GOTO1 VSETKEY                                                          
         MVI   NDDELRSW,0                                                       
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0             TEST FOUND SOMETHING                         
         BE    VREC23              YES-ADD BEFORE RETURNED RECORD               
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    VREC24              YES-ADD AT END                               
         LA    R2,PLACODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC23   GOTO1 VSETKEY             INITIALIZE NODIO KEY                         
         L     R4,NDIOA                                                         
         MVC   DUB,BUKCODE         EXTRACT CODE FROM RETURNED RECORD            
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2         GET RECORD INTO IO2                          
         LA    R0,L'PLANCODE                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'ADD'),NODKEY,((R0),DUB),0,0         
         CLI   NDERR,0                                                          
         BE    VREC25                                                           
         LA    R2,PLACODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC24   GOTO1 VSETKEY                                                          
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'ADD'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    VREC25                                                           
         LA    R2,PLACODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC25   L     R4,NDIOA                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  EXTRACT KEY FROM RECORD                      
         MVC   CONHEAD(L'ADDMSG),ADDMSG                                         
         MVC   TWAKEYSV,KEY                                                     
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO RESTORE A PLAN RECORD AND POSITION TO ORIGINAL PLACE           
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
         LA    R0,L'PLANCODE                                                    
         MVC   NDIOA,AIO1          RESTORE I/O AREA POINTERS                    
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'RES'),NODKEY,((R0),DUB),0           
         CLI   NDERR,0                                                          
         BE    RESTX                                                            
         GOTO1 VNODERR                                                          
*                                                                               
REST4    GOTO1 VSETKEY                                                          
         MVC   NDIOA,AIO1          RESTORE PLAN TO END                          
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'RES'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    RESTX                                                            
         GOTO1 VNODERR                                                          
*                                                                               
RESTX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT FISCAL OVERRIDE FIELD                                     
*                                                                               
* SCAN FOR SYSTEM(/MEDIA)=MONTH TYPE, ETC.                                      
*                                                                               
FOVED    NTR1                                                                   
         LA    R2,PLAFOVH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
*                                                                               
FOVED2   XC    FTERM,FTERM         SET FOR DELIMITER AFTER SYSTEM               
         MVC   FTERM(2),=C'/='                                                  
         GOTO1 VFVAL                                                            
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0            TEST FOR ANYTHING FOUND                      
         BNE   FOVED3              YES                                          
         CLI   FSTOP,X'FF'         TEST FOR END OF INPUT                        
         BE    FOVEDX              YES-ALL DONE                                 
         B     FOVEDR                                                           
*                                                                               
FOVED3   LA    R3,SYSTAB           R3=A(SYSTEM TABLE)                           
         USING SYSTABD,R3                                                       
         LA    R0,SYSTEMS                                                       
         ZIC   R1,FLDH+5                                                        
         CH    R1,=Y(L'SYSNAME)                                                 
         BH    FOVEDR                                                           
         BCTR  R1,0                                                             
         MVI   ERROR,SYSERR                                                     
*                                                                               
FOVED4   CLC   FLDH+5(1),SYSMINL                                                
         BL    FOVED5                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSNAME                                                   
         BE    FOVED6                                                           
*                                                                               
FOVED5   LA    R3,SYSTABL(R3)                                                   
         BCT   R0,FOVED4                                                        
         B     FOVEDR                                                           
*                                                                               
FOVED6   XC    ELEM,ELEM           START BUILDING OVERRIDE ELEMENT              
         LA    R6,ELEM                                                          
         USING BUFOVD,R6                                                        
         MVI   BUFOVEL,BUFOVELQ                                                 
         MVI   BUFOVLEN,BUFOVLNQ                                                
         MVC   BUFOVSYS,SYSOVSYS   OVERLAY SYSTEM NUMBER                        
         MVC   BUFOVCOD,SYSTYP                                                  
         MVC   BYTE,SYSCTL         EXTRACT SYSTEM VALIDITY BITS                 
*                                                                               
FOVED8   CLI   FSTOP,SLASH         TEST IF SLASH FOLLOWED SYSTEM                
         BNE   FOVED15                                                          
         XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUALS                                                     
         MVI   ERROR,INVALID                                                    
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    FOVEDR                                                           
         CLI   FLDH+5,L'MEDNAME                                                 
         BH    FOVEDR                                                           
         MVI   ERROR,MEDERR                                                     
*                                                                               
FOVED9   CLI   BYTE,0              TEST IF SYSTEM HAS MEDIA CODES               
         BNE   FOVED10             YES                                          
         CLI   FLDH+5,1            TEST FOR 1 BYTE INPUT                        
         BNE   FOVEDR                                                           
         MVC   BUFOVMED,FLD        EXTRACT 1-BYTE MEDIA VALUE                   
         B     FOVED15                                                          
*                                                                               
FOVED10  LA    R3,MEDTAB           R3=A(MEDIA TABLE)                            
         USING MEDTABD,R3                                                       
         LA    R0,MEDIAS                                                        
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
FOVED12  CLC   FLDH+5(1),MEDMINL                                                
         BL    FOVED13                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),MEDNAME                                                   
         BE    FOVED14                                                          
*                                                                               
FOVED13  LA    R3,MEDTABL(R3)                                                   
         BCT   R0,FOVED12                                                       
         CLI   FLDH+5,3                                                         
         BNE   FOVEDR                                                           
         CLC   FLD(3),=C'ALL'                                                   
         BE    FOVED15             ALLOW MEDIA=ALL                              
         B     FOVEDR                                                           
*                                                                               
FOVED14  MVC   BUFOVMED,MEDCOD     EXTRACT MEDIA CODE                           
         NC    BYTE,MEDSYSV                                                     
         BZ    FOVEDR              MEDIA INCOMPATIBLE WITH SYSTEM               
*                                                                               
FOVED15  GOTO1 HELLO,PARAS,(C'G',SYSFIL),(BUFOVEL,(R4)),(3,BUFOVSYS),0          
         CLI   12(R1),0            TEST IF ELEMENT FOUND FOR SAME               
         BNE   FOVED20             OVERLAY SYSTEM/CODE/MEDIA                    
         MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'DUPMSG),DUPMSG                                            
         B     FOVEDR                                                           
*                                                                               
FOVED20  XC    FTERM,FTERM         SEARCH FOR MONTH TYPE                        
         MVI   FTERM,COMMA                                                      
         MVI   ERROR,INVALID                                                    
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    FOVEDR                                                           
         CLI   FLDH+5,L'TYPNAME                                                 
         BH    FOVEDR                                                           
*                                                                               
         LA    R0,TYPES                                                         
         LA    R3,TYPTAB                                                        
         USING TYPTABD,R3                                                       
         MVI   ERROR,TYPERR                                                     
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
FOVED22  CLC   FLDH+5(1),TYPMINL                                                
         BL    FOVED23                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),TYPNAME                                                   
         BE    FOVED24                                                          
*                                                                               
FOVED23  LA    R3,TYPTABL(R3)                                                   
         BCT   R0,FOVED22                                                       
         B     FOVEDR                                                           
*                                                                               
FOVED24  MVC   BUFOVMON,TYPNUM                                                  
         MVI   ERROR,INVALID                                                    
         CLI   CLTTYPE,10          TEST FOR 444 FISCAL CLIENT                   
         BE    FOVEDR              YES-NO OVERRIDES ARE POSSIBLE                
         GOTO1 ADDELEM                                                          
         B     FOVED2                                                           
*                                                                               
FOVEDX   B     XIT                                                              
*                                                                               
FOVEDR   B     SPERR                                                            
         DROP  R3,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EDIT PLAN OPTIONS (KEY=PARAMETER VALUE,...)                    
*                                                                               
OPTED    NTR1                                                                   
         USING BUPLND,R6                                                        
         LA    R2,PLAOPTH          TEST FOR NO DATA IN FIELD                    
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    OPTEDX                                                           
         XC    FLAST,FLAST         START EDIT AT BEGINNING OF FIELD             
*                                                                               
OPTED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUALS        LOOK FOR EQUALS SIGN                         
         GOTO1 VFVAL                                                            
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0                                                         
         BNE   OPTED4                                                           
         CLI   FSTOP,X'FF'         TEST FOR EOF                                 
         BE    OPTED10             YES                                          
         B     OPTEDR                                                           
*                                                                               
OPTED4   CLI   FLDH+5,L'OPTNAME    VALIDATE THE KEYWORD                         
         BH    OPTEDR                                                           
         LA    R0,OPTIONS                                                       
         LA    R3,OPTTAB                                                        
         USING OPTTABD,R3                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
OPTED6   CLC   FLDH+5(1),OPTMINL   TEST FOR MINIMUM LENGTH FOR THIS KEY         
         BL    OPTED6A             NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),OPTNAME                                                   
         BE    OPTED7              VALID KEYWORD                                
OPTED6A  LA    R3,OPTTABL(R3)                                                   
         BCT   R0,OPTED6                                                        
         B     OPTEDR                                                           
*                                                                               
OPTED7   XC    FTERM,FTERM         EXTRACT PARAMETER VALUE                      
         MVI   FTERM,COMMA                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    OPTEDR              NO                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,OPTROUT        GET DISP TO VALIDATION ROUTINE               
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         B     OPTED2                                                           
*                                                                               
OPTED10  TM    BUPLNIND,BUPLNDUP   TEST FOR DATA DUPLICATION                    
         BZ    OPTEDX                                                           
         TM    BUPLNIND+1,BUPLNSHR TEST FOR DRD SHARE                           
         BZ    OPTEDX              NO                                           
*                                                                               
         MVI   ERROR,SUPPLIED      DATA DUP AND DRD SHARE ARE IN                
         MVC   WORK,SPACES         CONFLICT                                     
         MVC   WORK(L'CONFLMSG),CONFLMSG                                        
         B     OPTEDR                                                           
*                                                                               
OPTEDX   B     XIT                                                              
         SPACE 2                                                                
OPTEDR   B     SPERR                                                            
         DROP  R3                                                               
         EJECT                                                                  
* PLAN OPTIONS PARAMETER VALIDATION ROUTINES                                    
*                                                                               
OPTDUP   ST    RE,SAVERE           DUPLICATE=(YES,NO) IN EXTRACT                
         CLI   FLDH+5,3                                                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OI    BUPLNIND,BUPLNDUP                                                
         EX    R1,YESCOMP                                                       
         BE    OPTX                                                             
         NI    BUPLNIND,X'FF'-BUPLNDUP                                          
         EX    R1,NOCOMP                                                        
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
*&&US                                                                           
OPTPCD   ST    RE,SAVERE           PRINTCD=SUBTRACT                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OI    BUPLNIND,BUPLNSCD                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'SUBTRACT'                                              
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
OPTPO    ST    RE,SAVERE           PORD=ACTUALS                                 
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OI    BUPLNIND,BUPLNACT                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'ACTUALS'                                               
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
OPTDATE  ST    RE,SAVERE           DATE=BILLABLE                                
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OI    BUPLNIND,BUPLNBIL                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'BILLABLE'                                              
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
         USING OPTTABD,R3                                                       
OPTTEST  ST    RE,SAVERE           TESTEST=NO TESTBUY=NO                        
         CLI   FLDH+5,3                                                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OC    BUPLNIND(1),OPTEQU  TURN ON OPTION BIT                           
         EX    R1,NOCOMP           TEST PARM=NO                                 
         BE    OPTX                YES                                          
         MVC   BYTE,OPTEQU                                                      
         XI    BYTE,X'FF'          CREATE MASK FOR 'ANDING'                     
         NC    BUPLNIND(1),BYTE    TURN OFF OPTION BIT                          
         EX    R1,YESCOMP          TEST PARM=YES                                
         BE    OPTX                                                             
         B     OPTEDR                                                           
         DROP  R3                                                               
         SPACE 2                                                                
OPTDRD   ST    RE,SAVERE           DRD SHARE=YES,NO                             
         CLI   FLDH+5,3                                                         
         BH    OPTEDR                                                           
         OI    BUPLNIND+1,BUPLNSHR                                              
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BE    OPTX                                                             
         NI    BUPLNIND+1,X'FF'-BUPLNSHR                                        
         EX    R1,NOCOMP                                                        
         BE    OPTX                                                             
         B     OPTEDR                                                           
*&&                                                                             
         SPACE 2                                                                
OPTX     L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO'                                                    
         EJECT                                                                  
* DISPLAY PLAN RECORD                                                           
*                                                                               
DREC     GOTO1 VCLEARF,DMCB,PLANAMH,PLALAST                                     
         GOTO1 VCLEARF,DMCB,(1,PLATITH),PLALAST                                 
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         GOTO1 VGETVAL             GET PLAN RECORD VALUES                       
         MVC   PLANAM,PLANNAM                                                   
         GOTO1 VPEROUT,PARAS,(1,PLANST),WORK                                    
         MVC   PLASE,WORK                                                       
         MVI   ELCODE,BUINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DREC1                                                            
         USING BUIND,R6                                                         
         ZIC   R1,BUINLEN                                                       
         SH    R1,=Y(BUINPUT-BUIND+1)                                           
         EX    R1,*+8                                                           
         B     DREC1                                                            
         MVC   PLAFOV(0),BUINPUT                                                
*                                                                               
DREC1    MVC   HALF,PLANIND                                                     
         NI    HALF,X'FF'-BUPLNDAT                                              
         OC    HALF,HALF           TEST ANY INDICATORS ON                       
         BZ    DREC2                                                            
         GOTO1 DISOPT,PARAS,PLAOPTH                                             
*                                                                               
DREC2    CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DREC4                                                            
         MVC   PLADA(3),=C'DA='    DISPLAY DISK ADDRESS                         
         GOTO1 HEXOUT,DMCB,DMDSKADD,PLADA+3,4,=C'TOG'                           
         MVC   PLADA+12(5),=C'NODE='                                            
         GOTO1 (RF),DMCB,BUKNODE,PLADA+17,4,=C'TOG'                             
         OI    PLADAH+6,X'80'      XMIT FIELD                                   
*                                                                               
DREC4    MVI   ELCODE,BUSNELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DRECX                                                            
         USING BUSND,R6                                                         
         ZIC   R3,BUSNLEN                                                       
         SH    R3,=Y(BUSNAPS-BUSND)                                             
         LA    RE,L'BUSNAPS                                                     
         SR    R2,R2                                                            
         DR    R2,RE               R3=N'SNAPSHOTS                               
*                                                                               
         MVI   MORESW,NO                                                        
         LA    R0,MAXSNAPS                                                      
         CR    R3,R0               TEST IF TOO MANY SNAPSHOTS                   
         BNH   DREC6                                                            
         LR    R3,R0               YES-LIMIT IS MAXSNAPS                        
         MVI   MORESW,YES                                                       
*                                                                               
DREC6    L     R2,AIO2             R2=OUTPUT POINTER                            
         LR    R4,R6                                                            
         ZIC   R0,BUSNLEN                                                       
         AR    R4,R0               R4=SNAPSHOT POINTER                          
         LA    R6,L'BUSNAPS                                                     
         SR    R4,R6               BACK UP TO LAST SNAPSHOT                     
*                                                                               
DREC8    GOTO1 DATCON,DMCB,(3,0(R4)),(8,0(R2))                                  
         MVI   8(R2),C' '                                                       
         LA    R2,9(R2)                                                         
         SR    R4,R6               BACK UP TO PREVIOUS SNAPSHOT                 
         BCT   R3,DREC8                                                         
*                                                                               
DREC10   L     R0,AIO2             GET START OF OUTPUT STREAM                   
         SR    R2,R0               R2=L'STREAM TO CHOP                          
         LA    R0,NUMLINES         R0=N'SCREEN LINES                            
         LA    R3,L'PLASNA1        R3=L'SCREEN LINE                             
         GOTO1 CHOPPER,DMCB,AIO2,((R3),AIO3),((R3),(R0)),C'LEN=',(R2)           
*                                                                               
         ICM   R0,15,DMCB+8        N'CHOPPER BLOCKS                             
         BZ    DRECX               CHOPPER ERROR                                
         MVC   PLATIT+31(17),=C'*** SNAPSHOTS ***'                              
         L     R1,AIO3             R3=A(CHOPPED OUTPUT)                         
         LA    R2,PLASNA1H         R2=A(SCREEN FIELD HEADER)                    
         SR    RE,RE               CLEAR WORK REGISTER                          
*                                                                               
DREC11   MVC   8(L'PLASNA1,R2),0(R1)                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE               NEXT SCREEN FIELD HEADER                     
         LA    R1,0(R3,R1)         NEXT CHOPPED BLOCK                           
         BCT   R0,DREC11                                                        
*                                                                               
DREC12   CLI   MORESW,YES          TEST IF OVERFLOW SNAPSHOTS                   
         BNE   DRECX                                                            
         LA    R2,PLASNA4+(PERLINE-1*9)                                         
         MVC   0(4,R2),=C'MORE'    NOTE OVERFLOW TO USER                        
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
* LIST PLAN RECORDS                                                             
*                                                                               
LIST     MVI   NDUPDTSW,NO         DO NOT READ FOR UPDATE                       
         MVI   NLISTS,14           OVERRIDE N'LIST LINES                        
         CLI   THISPF,PF7          TEST PF7=PREVIOUS PAGE                       
         BNE   LIST1                                                            
         LR    RE,R7                                                            
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R2,CONACTH                                                       
         CR    RE,R2               TEST ANYTHING CHANGED AFTER RECORD           
         BH    LIST1               YES                                          
         LA    R3,NDLVTAB+(3*NDLVTABL)                                          
         USING NDLVTABD,R3                                                      
         OC    NDLVNOD,NDLVNOD     TEST FOR ANY PLANS                           
         BNZ   LIST8               YES-PERFORM BACKWARDS READ                   
*                                                                               
LIST1    OC    KEY,KEY             TEST FOR FIRST TIME                          
         BNZ   LIST4                                                            
*                                                                               
         MVI   SVNPLANS,0                                                       
         XC    SVFSTKEY,SVFSTKEY                                                
         GOTO1 VSETKEY                                                          
         LA    R1,LISTHK           SET IN-LINE HOOK                             
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST2                                                            
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    XIT                 YES-NO MORE RECORDS SO EXIT                  
         LA    R2,PLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
LIST2    OC    PLANCODE,PLANCODE   TEST FOR START CODE                          
         BZ    LIST6               NO                                           
         ZIC   RE,NDSQBACK                                                      
         BCTR  RE,0                DECREMENT BACK LIMIT TO                      
         STC   RE,NDSQBACK         READ UNTIL END OF PRODUCT                    
         B     LIST6                                                            
*                                                                               
* RE-SET FOR SEQUENTIAL WITH A READ                                             
*                                                                               
LIST4    MVI   SVNPLANS,0                                                       
         XC    SVFSTKEY,SVFSTKEY                                                
         MVC   NODKEY,SVNKEY       GET LAST SAVED NODAL KEY                     
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST5                                                            
         LA    R2,PLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
LIST5    ZIC   RE,NDSQBACK                                                      
         BCTR  RE,0                DECREMENT BACK LIMIT TO                      
         STC   RE,NDSQBACK         READ UNTIL END OF PRODUCT                    
*                                                                               
* READ SEQUENTIAL TO FILL SCREEN OR UP TO EOF                                   
*                                                                               
LIST6    LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'SEQ',NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         LA    R2,PLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* GO BACK TO PREVIOUS SCREEN                                                    
*                                                                               
LIST8    BAS   RE,BACK             BACK UP TO START OF PREVIOUS SCREEN          
         MVC   NODKEY,NODKEYSV     SET START POINT OF READ                      
         MVI   SVNPLANS,0                                                       
         XC    SVFSTKEY,SVFSTKEY                                                
         LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RE,NDSQBACK                                                      
         BCTR  RE,0                                                             
         STC   RE,NDSQBACK                                                      
         GOTO1 (RF),(R1),NODBLKD,=C'SEQ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         LA    R2,PLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* HOOK TO PROCESS RECORDS RETURNED BY NODIO                                     
*                                                                               
LISTHK   ST    RE,SAVERE                                                        
         CLI   NDMODE,NDFRST       TEST FOR FIRST TIME AT LEVEL                 
         BNE   LISTHK2                                                          
         CLI   NDLEV,3             TEST FOR RIGHT LEVEL                         
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SET SKIP LOWER LEVELS TO READ                
         B     LISTHKX             ONLY PLANS                                   
*                                                                               
LISTHK2  CLI   NDMODE,NDPROC                                                    
         BNE   LISTHKX                                                          
         CLI   NDLEV,3             TEST FOR LEVEL 3 RECORD                      
         BNE   LISTHKX                                                          
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCPLN     MAKE SURE IT IS A PLAN                       
         BZ    LISTHKX                                                          
*                                                                               
         GOTO1 VGETVAL                                                          
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),NDLVKEY EXTRACT KEY                                 
         MVC   DMDSKADD,NDLVDA     AND THE DISK ADDRESS                         
         MVC   SVNKEY,NODKEY       EXTRACT NODAL KEY                            
         OC    SVFSTKEY,SVFSTKEY   TEST FOR FIRST KEY                           
         BNZ   *+10                NO                                           
         MVC   SVFSTKEY,NODKEY     YES-SAVE THE KEY                             
         ZIC   R1,SVNPLANS         INCREMENT N'PLANS ON SCREEN                  
         LA    R1,1(R1)                                                         
         STC   R1,SVNPLANS                                                      
         LA    RF,DISLIST          REGULAR LIST DISPLAY                         
         CLI   TRACEOPT,YES                                                     
         BNE   *+8                                                              
         LA    RF,TRALIST          NODAL TRACE DISPLAY                          
         BASR  RE,RF                                                            
         GOTO1 LISTMON                                                          
*                                                                               
LISTHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
LISTHD   DC    C'CODE   NAME                    START  END'                     
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
         CLI   SVNPLANS,0          TEST ANYTHING ON SCREEN                      
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
         ZIC   R1,NDSQBACK         SUPPRESS CLIENT AND PRODUCT READ             
         BCTR  R1,0                                                             
         STC   R1,NDSQBACK                                                      
         MVI   NDSKIP,YES                                                       
         GOTO1 VNODIO,DMCB,NODBLKD,=C'BSEQ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    BACKX                                                            
         CLI   NDERR,NDRNFERR                                                   
         BE    BACKX                                                            
         LA    R2,PLLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
BACKX    B     XIT                                                              
         SPACE 2                                                                
*  HOOK ROUTINE FOR BACKWARDS READ                                              
*                                                                               
BACKHK   ST    RE,SAVERE                                                        
         CLI   NDMODE,NDFRST                                                    
         BNE   BACKHK2                                                          
         CLI   NDLEV,3                                                          
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SUPPRESS READING OUTLINES                    
         B     BACKHKX                                                          
*                                                                               
BACKHK2  CLI   NDMODE,NDPROC                                                    
         BNE   BACKHKX                                                          
         CLI   NDLEV,3             CHECK FOR A PLAN                             
         BNE   BACKHKX                                                          
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCPLN                                                  
         BZ    BACKHKX                                                          
*                                                                               
         MVC   NODKEYSV,NODKEY     SAVE LAST KEY FOUND                          
         ZIC   R1,NPLANS           UPDATE N'PLANS FOUND                         
         LA    R1,1(R1)                                                         
         STC   R1,NPLANS                                                        
         CLC   NPLANS,NLISTS       TEST IF SCREEN FILLED                        
         BL    *+8                 NO                                           
         MVI   NDMODE,NDEND        YES-STOP NODIO READ                          
*                                                                               
BACKHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY LIST DATA LINE (PRECEDE WITH GETVAL CALL)              
*                                                                               
DISLIST  NTR1                                                                   
         XC    PLLHED,PLLHED       DISPLAY HEADLINE                             
         MVC   PLLHED(L'LISTHD),LISTHD                                          
         OI    PLLHEDH+6,X'80'                                                  
         LA    R2,LISTAR                                                        
         USING LSTLIND,R2                                                       
         MVC   LISTAR,SPACES                                                    
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVC   LSTCOD,BUKCODE      EXTRACT ACTUAL PLAN CODE                     
         MVC   LSTNAM,PLANNAM                                                   
         GOTO1 VPEROUT,PARAS,(1,PLANST),WORK                                    
         CLI   PLANST,0            TEST FOR OPEN-ENDED PLAN                     
         BE    DISLIST2            YES                                          
         MVC   LSTSTART,WORK                                                    
         MVC   LSTEND,WORK+7                                                    
         B     DISLISTX                                                         
*                                                                               
DISLIST2 MVC   LSTSTART(3),WORK                                                 
         MVC   LSTEND(3),WORK+4                                                 
*                                                                               
DISLISTX B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY A NODAL TRACE FOR A PLAN                               
* AT ENTRY, R3=A(LEVEL TABLE ENTRY), R4=A(RECORD)                               
*                                                                               
TRALIST  NTR1                                                                   
         MVC   PLLHED,SPACES       CLEAR LIST HEADLINE                          
         MVC   PLLHED(L'TRACEHD),TRACEHD SET LIST HEADLINE                      
         OI    PLLHEDH+6,X'80'     XMIT                                         
         LA    R2,LISTAR                                                        
         USING TRALIND,R2                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   TRACOD,PLANCODE                                                  
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
* PRINT PLAN REPORT                                                             
*                                                                               
PREP     LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 VSETKEY                                                          
         MVI   NDUPDTSW,NO                                                      
         LA    R1,PREP2                                                         
         ST    R1,NDHOOK                                                        
*                                                                               
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BNE   PREPX                                                            
         GOTO1 (RF),DMCB,NODBLKD,=C'SEQ',NODKEY,0                               
*                                                                               
PREPX    B     XIT                                                              
         SPACE 1                                                                
* NODIO HOOK ROUTINE TO PROCESS RECORDS FOR PRINTING                            
*                                                                               
PREP2    NTR1                                                                   
         CLI   NDMODE,NDFRST       FIRST TIME FOR LEVEL                         
         BNE   PREP4                                                            
         CLI   NDLEV,3             TEST FOR PLAN                                
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SKIP READING OUTLINES                        
         B     PREPX                                                            
*                                                                               
PREP4    CLI   NDMODE,NDLAST       LAST TIME FOR LEVEL                          
         BNE   PREP6                                                            
         MVI   NDSKIP,NO                                                        
         B     PREPX                                                            
*                                                                               
PREP6    CLI   NDMODE,NDPROC       PROCESS RECORD                               
         BNE   PREPX                                                            
         GOTO1 VGETVAL                                                          
         CLI   NDLEV,2             TEST FOR PRODUCT                             
         BNE   *+12                                                             
         MVI   FORCEHED,YES        BREAK  A PAGE FOR NEW PRODUCT                
         B     PREPX                                                            
*                                                                               
         CLI   NDLEV,3             TEST FOR PLAN                                
         BNE   PREPX                                                            
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVC   PRTCODE,PLANCODE                                                 
         OC    PRTCODE,SPACES                                                   
         MVC   PRTNAM,PLANNAM                                                   
         GOTO1 VPEROUT,PARAS,(1,PLANST),WORK                                    
         MVI   WORK+6,C' '         ERASE DASH                                   
         MVC   PRTPER,WORK                                                      
         MVC   HALF,PLANIND                                                     
         NI    HALF,X'FF'-BUPLNDAT                                              
         OC    HALF,HALF           TEST FOR ANY ACTIVE OPTIONS                  
         BZ    PREP7               NO                                           
*                                                                               
         XC    FLDH,FLDH                                                        
         MVI   FLDH,L'FLDH+L'FLD                                                
         MVC   FLD,SPACES                                                       
         GOTO1 DISOPT,PARAS,FLDH                                                
         LA    R0,L'FLD            FIND L'OUTPUT                                
         LA    R1,FLD+L'FLD-1                                                   
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         LA    R3,L'PRTOPT                                                      
         GOTO1 CHOPPER,DMCB,((R0),FLD),((R3),PRTOPT),(C'P',2),0                 
*                                                                               
PREP7    MVI   ELCODE,BUINELQ                                                   
         L     R4,NDIOA            LOOK FOR SCREEN INPUT ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   PREP8               NONE FOUND                                   
         USING BUIND,R6                                                         
         ZIC   R0,BUINLEN          PRINT FISCAL MONTH OVERRIDES                 
         SH    R0,=Y(BUINPUT-BUIND)                                             
         LA    R3,L'PRTFOV                                                      
         GOTO1 CHOPPER,DMCB,((R0),BUINPUT),((R3),PRTFOV),(C'P',2),0             
*                                                                               
PREP8    GOTO1 SPOOL,PARAS,(R8)                                                 
         BASR  RE,RF               SKIP A LINE                                  
         B     PREPX                                                            
         DROP  R2,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY PLAN OPTIONS                                           
*                                                                               
* AT ENTRY, P1=A(OUTPUT FIELD HEADER)                                           
*                                                                               
DISOPT   NTR1                                                                   
         L     R4,0(R1)            R4=A(OUTPUT FLD HEADER)                      
         L     R3,AIO3                                                          
         MVI   0(R3),C' '                                                       
         MVC   1(255,R3),0(R3)     CLEAR 256 BYTES                              
         LA    R0,OPTIONS          R0=OPTION COUNTER                            
         LA    RE,OPTTAB           RE=A(OPTION TABLE)                           
         USING OPTTABD,RE                                                       
         SR    R1,R1                                                            
         SR    R2,R2               R2=N'OPTIONS TO OUTPUT                       
         MVC   TESTOPT,TESTOPT1    SET EXECUTED INSTRUCTION                     
         LA    RF,OPTTAB2          RF=A(SECOND BYTE OPTIONS)                    
*                                                                               
DISOPT2  IC    R1,OPTEQU           GET EQUATED BIT SETTING                      
         EX    R1,TESTOPT                                                       
         BZ    DISOPT4             OPTION NOT SET                               
*                                                                               
         MVC   0(L'OPTNAME,R3),OPTNAME 1ST HALF IS OPTION NAME                  
         MVC   10(L'OPTVAL,R3),OPTVAL  2ND HALF IS OPTION VALUE                 
         LA    R2,1(R2)            INCREMENT OPTION COUNT                       
         LA    R3,20(R3)           NEXT UNSCAN BOLCK                            
*                                                                               
DISOPT4  LA    RE,OPTTABL(RE)      NEXT OPTION ENTRY                            
         CR    RE,RF               TEST FOR SECOND BYTE OPTIONS                 
         BNE   *+10                                                             
         MVC   TESTOPT,TESTOPT2    YES-SET NEW EXECUTED INSTRUCTION             
         BCT   R0,DISOPT2                                                       
*                                                                               
         GOTO1 UNSCAN,DMCB,((R2),AIO3),(R4),0                                   
*                                                                               
DISOPTX  B     XIT                                                              
*                                                                               
TESTOPT1 TM    PLANIND,0                                                        
TESTOPT2 TM    PLANIND+1,0                                                      
         DROP  RE                                                               
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(3),CLTCODE                                                 
         OC    H4+10(3),SPACES                                                  
         MVC   H4+15(L'CLTNAM),CLTNAM                                           
*                                                                               
         MVC   H5+10(3),PRDCODE                                                 
         OC    H5+10(3),SPACES                                                  
         MVC   H5+15(L'PRDNAM),PRDNAM                                           
*                                                                               
         ICM   R3,15,ABOX                                                       
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
         MVI   BOXROWS+6,C'T'      SET UP FOR BOXES                             
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   0(R2),C'L'                                                       
         MVI   6(R2),C'C'                                                       
         MVI   28(R2),C'C'                                                      
         MVI   35(R2),C'C'                                                      
         MVI   43(R2),C'C'                                                      
         MVI   75(R2),C'C'                                                      
         MVI   106(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
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
MISSPARM DC    C'M'                                                             
YESPARM  DC    C'Y'                                                             
ADDMSG   DC    C'PLAN RECORD ADDED TO FILE'                                     
DELMSG   DC    C'PLAN RECORD DELETED'                                           
RESMSG   DC    C'PLAN RECORD RESTORED'                                          
DUPMSG   DC    C'** ERROR - DUPLICATE SYSTEM/MEDIA OVERRIDES **'                
CONFLMSG DC    C'** ERROR - CANNOT USE BOTH DUPLICATION AND DRD SHARES X        
               **'                                                              
         SPACE 2                                                                
* TABLE OF VALID SYSTEMS                                                        
*                                                                               
SYSTAB   DS    0CL(SYSTABL)                                                     
*                                                                               
*&&US                                                                           
         DC    CL7'SPOT   ',AL1(1),X'02',C'S',AL1(SPOTB)                        
         DC    CL7'NETWORK',AL1(1),X'03',C'N',AL1(NETB)                         
         DC    CL7'NTWK   ',AL1(1),X'03',C'N',AL1(NETB)                         
         DC    CL7'PRINT  ',AL1(3),X'04',C'P',AL1(PRINTB)                       
         DC    CL7'PRNT   ',AL1(3),X'04',C'P',AL1(PRINTB)                       
         DC    CL7'PRODUCT',AL1(3),X'06',C'P',AL1(0)                            
*&&                                                                             
*                                                                               
*&&UK                                                                           
         DC    CL7'MEDLINE',AL1(1),X'04',C'M',AL1(0)                            
         DC    CL7'PRODUCT',AL1(1),X'06',C'P',AL1(0)                            
*&&                                                                             
SYSTEMS  EQU   (*-SYSTAB)/L'SYSTAB                                              
         SPACE 2                                                                
MEDTAB   DS    0CL(MEDTABL)                                                     
*&&US                                                                           
         DC    CL8'TV      ',AL1(2),C'T',AL1(SPOTB)  TV                         
         DC    CL8'RADIO   ',AL1(1),C'R',AL1(SPOTB)  RADIO                      
         DC    CL8'NETRADIO',AL1(4),C'X',AL1(SPOTB)  NTWK RADIO                 
         DC    CL8'NRADIO  ',AL1(2),C'X',AL1(SPOTB)  NTWK RADIO                 
         DC    CL8'COMBO   ',AL1(2),C'C',AL1(SPOTB)  COMBINED (CANADA)          
         DC    CL8'NETWORK ',AL1(3),C'N',AL1(SPOTB+NETB)  NETWORK               
         DC    CL8'NTWK    ',AL1(3),C'N',AL1(SPOTB+NETB)  NETWORK               
         DC    CL8'MAGAZINE',AL1(1),C'M',AL1(PRINTB) MAGAZINE                   
         DC    CL8'MAGS    ',AL1(1),C'M',AL1(PRINTB) MAGAZINE                   
         DC    CL8'NEWS    ',AL1(3),C'N',AL1(PRINTB) NEWSPAPER                  
         DC    CL8'OUTDOOR ',AL1(1),C'O',AL1(PRINTB) OUTDOOR                    
         DC    CL8'SUPPLEM ',AL1(1),C'S',AL1(PRINTB) SUPPLEMENTS                
         DC    CL8'TRADE   ',AL1(2),C'T',AL1(PRINTB) TRADE                      
*&&                                                                             
MEDIAS   EQU   (*-MEDTAB)/L'MEDTAB                                              
         SPACE 2                                                                
* TABLE OF MONTH TYPES                                                          
*                                                                               
TYPTAB   DS    0XL(TYPTABL)                                                     
         DC    CL5'BROAD',AL1(1),AL1(00)                                        
         DC    CL5'CALEN',AL1(1),AL1(02)                                        
         DC    CL5'544',AL1(3),AL1(06)                                          
         DC    CL5'454',AL1(3),AL1(07)                                          
         DC    CL5'445',AL1(3),AL1(08)                                          
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
         SPACE 2                                                                
* TABLE OF FORMAT OPTIONS (COVERED BY OPTTABD)                                  
*                                                                               
OPTTAB   DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'DUPDATA',AL1(2),AL1(BUPLNDUP),AL2(OPTDUP-T50204)             
         DC    CL8'YES'                                                         
*&&US                                                                           
*                                                                               
         DC    CL8'PRINTCD',AL1(2),AL1(BUPLNSCD),AL2(OPTPCD-T50204)             
         DC    CL8'SUBTRACT'                                                    
*                                                                               
         DC    CL8'PORD',AL1(2),AL1(BUPLNACT),AL2(OPTPO-T50204)                 
         DC    CL8'ACTUALS'                                                     
*                                                                               
         DC    CL8'DATE',AL1(2),AL1(BUPLNBIL),AL2(OPTDATE-T50204)               
         DC    CL8'BILLABLE'                                                    
*                                                                               
         DC    CL8'TESTEST',AL1(5),AL1(BUPLNTES),AL2(OPTTEST-T50204)            
         DC    CL8'NO'                                                          
*                                                                               
         DC    CL8'TESTBUY',AL1(5),AL1(BUPLNTBU),AL2(OPTTEST-T50204)            
         DC    CL8'NO'                                                          
*                                                                               
OPTTAB2  DC    CL8'DRDSHARE',AL1(3),AL1(BUPLNSHR),AL2(OPTDRD-T50204)            
         DC    CL8'YES'                                                         
*                                                                               
*&&                                                                             
OPTIONS  EQU   (*-OPTTAB)/L'OPTTAB                                              
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,50,C'PLAN REPORT'                                             
         SSPEC H2,50,C'-----------'                                             
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
         SSPEC H8,2,C'CODE'                                                     
         SSPEC H8,8,C'NAME'                                                     
         SSPEC H8,30,C'START'                                                   
         SSPEC H8,37,C'END'                                                     
         SSPEC H8,45,C'OPTIONS'                                                 
         SSPEC H8,77,C'FISCAL MONTH TYPE OVERRIDES'                             
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF4D                                                       
         EJECT                                                                  
* DSECT TO COVER LIST SCREEN                                                    
*                                                                               
         ORG   CONTAGH                                                          
*      ++INCLUDE BUFILE4D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
MYBASE   DS    A                                                                
MORESW   DS    C                   MORE SNAPSHOTS THAN CAN FIT ON SCR.          
TRACEOPT DS    C                   Y=TRACE OPTION ACTIVE FOR LIST               
NPLANS   DS    X                   N'PLANS FOUND GOING BACKWARDS                
THREE    DS    XL3                                                              
         DS    0D                                                               
TESTOPT  DS    XL(L'TESTOPT1)                                                   
         ORG   TWA1USER                                                         
SVCLTVAL DS    CL(L'CLTVALS)       CLIENT VALUES SAVE AREA                      
SVPRDVAL DS    CL(L'PRDVALS)       PRODUCT VALUES SAVE AREA                     
SVNPLANS DS    X                   N'PLAN RECORDS ON LIST SCREEN                
SVFSTKEY DS    CL(L'NODKEY)        1ST REC ON LIST SCREEN NODAL KEY             
         SPACE 2                                                                
* DSECT TO COVER LIST DISPLAY LINE                                              
*                                                                               
LSTLIND  DSECT                                                                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSTCOD   DS    CL3                 PRODUCT CODE                                 
         DS    CL4                 SPARE                                        
LSTNAM   DS    CL20                                                             
         DS    CL4                 SPARE                                        
LSTSTART DS    CL6                 PLAN START (M/Y)                             
         DS    C                   SPARE                                        
LSTEND   DS    CL6                                                              
         SPACE 2                                                                
* DSECT TO COVER TRACE LIST LINE                                                
*                                                                               
TRALIND  DSECT                                                                  
TRACOD   DS    CL3                 PLAN CODE                                    
         DS    CL2                                                              
TRANODE  DS    CL6                 PLAN'S NODE                                  
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
* DSECT TO COVER A PRINT LINE                                                   
*                                                                               
PRTD     DSECT                                                                  
         DS    C                                                                
PRTCODE  DS    CL3                                                              
         DS    CL3                                                              
PRTNAM   DS    CL20                                                             
         DS    CL2                                                              
PRTPER   DS    CL13                                                             
         DS    CL2                                                              
PRTOPT   DS    CL30                                                             
         DS    CL2                                                              
PRTFOV   DS    CL30                                                             
         SPACE 2                                                                
* DSECT TO COVER SYSTEM TABLE                                                   
*                                                                               
SYSTABD  DSECT                                                                  
SYSNAME  DS    CL7                 SYSTEM KEYWORD NAME                          
SYSMINL  DS    X                   MINIMUM LENGTH FOR KEYWORD                   
SYSOVSYS DS    X                   OVERLAY SYSTEM NUMBER                        
SYSTYP   DS    C                   SYSTEM TYPE CODE                             
SYSCTL   DS    X                   CONTROL BITS FOR VALID MEDIA                 
SYSTABL  EQU   *-SYSTABD           TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* DSECT TO COVER MEDIA TABLE                                                    
*                                                                               
MEDTABD  DSECT                                                                  
MEDNAME  DS    CL8                                                              
MEDMINL  DS    X                                                                
MEDCOD   DS    C                   MEDIA CODE                                   
MEDSYSV  DS    X                   VALID SYSTEM BITS                            
MEDTABL  EQU   *-MEDTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER MONTH TYPE TABLE                                               
*                                                                               
TYPTABD  DSECT                                                                  
TYPNAME  DS    CL5                                                              
TYPMINL  DS    X                                                                
TYPNUM   DS    X                                                                
TYPTABL  EQU   *-TYPTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER FORMAT OPTIONS TABLE                                           
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                                                              
OPTMINL  DS    X                                                                
OPTEQU   DS    X                   OPTION BIT SETTING EQUATE                    
OPTROUT  DS    AL2                 DISPLACEMENT TO PARAMETER ROUTINE            
OPTVAL   DS    CL8                 PARAMETER VALUE                              
OPTTABL  EQU   *-OPTTABD                                                        
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
COMMA    EQU   C','                                                             
LINELEN  EQU   PLASNA2H-PLASNA1H                                                
NUMLINES EQU   (PLALAST-PLASNA1H)/LINELEN                                       
PERLINE  EQU   L'PLASNA1/9                                                      
MAXSNAPS EQU   (PERLINE*NUMLINES)-1                                             
*                                                                               
SPOTB    EQU   X'80'                                                            
NETB     EQU   X'40'                                                            
PRINTB   EQU   X'20'                                                            
PRODB    EQU   X'10'                                                            
*                                                                               
PF7      EQU   X'07'                                                            
PF8      EQU   X'08'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032BUFIL04   05/01/02'                                      
         END                                                                    
