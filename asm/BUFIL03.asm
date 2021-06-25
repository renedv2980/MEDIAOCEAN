*          DATA SET BUFIL03    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T50203A                                                                  
         TITLE 'T50203 - BUDGET CONTROL LFM - PRODUCT RECORD'                   
T50203   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI03**,RA,RR=R2                                              
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
         LA    R2,PROCLTH          VALIDATE CLIENT                              
         SR    R0,R0                                                            
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+8                                                              
         ICM   R0,8,MISSPARM                                                    
*                                                                               
         GOTO1 VVALCLT,PARAS,(R2),(R0)                                          
         OC    CLTCODE,CLTCODE     TEST FOR 'ALL' CLIENTS                       
         BNZ   VKEY1                                                            
         MVI   ERROR,NOWERR                                                     
         LA    R2,CONWHENH                                                      
         TM    WHEN,X'38'          TEST REQUESTED 'SOON' OR 'ON'                
         BNZ   VKEYX                                                            
         OI    6(R2),X'80'         XMIT WHEN FIELD BACK                         
         B     TRAPERR                                                          
*                                                                               
VKEY1    MVC   SAVCLT,CLTCODE      SAVE AWAY CLIENT CODE                        
         MVC   SAVCLN,CLTNAM       AND NAME                                     
         CLI   ACTNUM,ACTLIST      TEST FOR ACTION LIST                         
         BE    VKEY2               YES-SKIP DISPLAY OF CLIENT NAME              
         XC    PROCLN,PROCLN       CLEAR CLIENT NAME                            
         MVC   PROCLN(L'CLTNAM),CLTNAM                                          
         OI    PROCLNH+6,X'80'     XMIT                                         
         CLI   ACTNUM,ACTREP                                                    
         BE    VKEYX                                                            
*                                                                               
* EDIT AND VALIDATE PRODUCT CODE                                                
*                                                                               
VKEY2    LA    R2,PROCODH                                                       
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BNE   *+8                                                              
         LA    R2,PRLCODH          YES-PROD.CODE IN DIFFERENT PLACE             
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FZERO,YES                                                        
         GOTO1 VFVAL                                                            
*                                                                               
         CLI   FLDH+5,0                                                         
         BNE   VKEY4                                                            
         MVI   ERROR,MISSING                                                    
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX               YES-OK TO HAVE NO PRODUCT                    
         B     TRAPERR                                                          
*                                                                               
VKEY4    MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,2                                                         
         BL    TRAPERR                                                          
         MVC   PRDCODE,FLD                                                      
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX                                                            
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
* NODIO HOOK ROUTINE FOR PRODUCT                                                
*                                                                               
VKEY6    ST    RE,SAVERE                                                        
         CLI   NDLEV,0             TEST FOR MASTER RECORD                       
         BE    VKEY8                                                            
         CLI   NDMODE,NDPROC       TEST FOR RIGHT LEVEL                         
         BNE   VKEY8                                                            
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCPRO     TEST FOR PRODUCT RECORD                      
         BZ    VKEY8                                                            
         CLI   NDLEV,2             TEST NODIO IS IN SYNC                        
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
         MVC   DUB(L'SAVCLT),SAVCLT                                             
         OC    DUB,SPACES                                                       
         MVC   PROCLT,DUB                                                       
         OI    PROCLTH+6,X'80'                                                  
*                                                                               
         MVC   PROCLN,SAVCLN                                                    
         OI    PROCLNH+6,X'80'                                                  
*                                                                               
         MVC   DUB,BUKCODE         DISPLAY PRODUCT CODE                         
         OC    DUB,SPACES                                                       
         MVC   PROCOD,DUB                                                       
         OI    PROCODH+6,X'80'                                                  
*                                                                               
         GOTO1 VTRACE                                                           
         MVC   SVNKEY,NODKEY       READ RECORD THROUGH NODIO FOR CHA            
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE PRODUCT RECORD                                                       
*                                                                               
VREC     L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVI   ELCODE,BUPROELQ                                                  
         GOTO1 REMELEM                                                          
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VREC2                                                            
*                                                                               
         LR    RE,R4               CLEAR IO AREA FOR ADD                        
         L     RF,SIZEIO                                                        
         XCEF                                                                   
*                                                                               
         MVC   BUKEY,NDKEY         INITIALIZE KEY W MASTER                      
         MVC   BURLEN,DATADISP                                                  
         OI    BURCTYP,BUKCPRO     PRODUCT RECORD INDICATOR                     
*                                                                               
VREC2    CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    VREC7                                                            
         CLI   ACTNUM,ACTREST      TEST FOR RESTORE                             
         BE    VREC8                                                            
*                                                                               
         XC    ELEM(BUPROLNQ),ELEM                                              
         LA    R6,ELEM             INITIALIZE PRODUCT DESCRIPTION ELEM          
         USING BUPROD,R6                                                        
         MVI   BUPROEL,BUPROELQ                                                 
         MVI   BUPROLEN,BUPROLNQ                                                
*                                                                               
         LA    R2,PRONAMH          EDIT PRODUCT NAME                            
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   BUPRONAM,FLD                                                     
*                                                                               
VREC4    GOTO1 ADDELEM                                                          
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VREC10              YES-SPECIAL CODE FOR I/O                     
*                                                                               
* I/O ROUTINE FOR PUT                                                           
*                                                                               
VREC6    MVI   IOOPT,YES                                                        
         GOTO1 VCHAACTV                                                         
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',SVNKEY,0                             
         CLI   NDERR,0                                                          
         BE    VRECX                                                            
         LA    R2,PROCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* I/O ROUTINE FOR DELETE                                                        
*                                                                               
VREC7    LA    R2,PROCODH                                                       
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)     POINT TO PLAN LEVEL                          
         MVI   ERROR,DELERR                                                     
         OC    NDLVNOD,NDLVNOD     TEST IF PRODUCT HAS PLANS                    
         BNZ   TRAPERR             YES-SO CANNOT DELETE IT                      
*                                                                               
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'DELETE',SVNKEY,0                          
         CLI   NDERR,0                                                          
         BE    VREC7A                                                           
         GOTO1 VNODERR                                                          
*                                                                               
VREC7A   MVC   CONHEAD(L'DELMSG),DELMSG                                         
         OI    6(R2),X'81'         XMIT MODIFIED TO STOP DOUBLE                 
         B     VREC9               DELETE                                       
*                                                                               
* I/O ROUTINE FOR RESTORE                                                       
*                                                                               
VREC8    LA    R2,PROCODH                                                       
         BAS   RE,REST             RESTORE RECORD                               
         MVC   CONHEAD(L'RESMSG),RESMSG                                         
         B     VREC9                                                            
*                                                                               
* RE-READ DELETED/RESTORED PRODUCT FOR DISPLAY                                  
*                                                                               
VREC9    GOTO1 VSETKEY                                                          
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
         B     DREC                NOW DISPLAY DELETED/RESTORED PRODUCT         
*                                                                               
* I/O ROUTINE FOR ADD (READ TO CHECK FOR DUPLICATES, THEN HIGH TO               
* POSITION ADD)                                                                 
*                                                                               
VREC10   MVI   IOOPT,YES                                                        
         GOTO1 VADDACTV                                                         
         GOTO1 VSETKEY             INITIALIZE NODIO KEY                         
*                                                                               
         XC    NDHOOK,NDHOOK       CHECK FOR DUPLICATE KEYS                     
         MVI   NDDELRSW,YES        RETURN DELETED RECORDS                       
         MVC   NDIOA,AIO2          GET RECORD INTO IO2                          
         MVC   NDIOA2,AIO3                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    VREC12              YES-PROCEED WITH ADD                         
         CLI   NDERR,0                                                          
         BE    VREC11              FOUND A DUPLICATE                            
         LA    R2,PROCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC11   L     R4,NDIOA                                                         
         MVI   ERROR,DELEXIST                                                   
         LA    R2,PROCODH                                                       
         XC    KEY,KEY             READ DIRECTORY POINTER TO                    
         MVC   KEY(L'BUKEY),BUKEY  DISCOVER IF IT IS DELETED                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 READ                                                             
         TM    KEY+(BUKCSTA-BUKEY),X'80'                                        
         BO    *+8                 YES                                          
         MVI   ERROR,RECEXIST                                                   
         B     TRAPERR                                                          
*                                                                               
VREC12   GOTO1 VSETKEY             NOW POSITION FOR ADD                         
         MVI   NDDELRSW,0          DO NOT WANT DELETES                          
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0             TEST FOUND SOMETHING                         
         BE    VREC13              YES-ADD BEFORE RETURNED RECORD               
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    VREC14              YES-ADD RECORD AT END                        
         LA    R2,PROCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC13   GOTO1 VSETKEY             INITIALIZE NODIO KEY                         
         L     R4,NDIOA                                                         
         MVC   DUB,BUKCODE         EXTRACT CODE FROM RETURNED RECORD            
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2         GET RECORD INTO IO2                          
         LA    R0,L'PRDCODE                                                     
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'ADD'),NODKEY,((R0),DUB),   X        
               0,0                                                              
         CLI   NDERR,0                                                          
         BE    VREC15                                                           
         LA    R2,PROCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC14   GOTO1 VSETKEY                                                          
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'ADD'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    VREC15                                                           
         LA    R2,PROCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC15   L     R4,NDIOA                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  EXTRACT KEY FROM RECORD                      
         MVC   CONHEAD(L'ADDMSG),ADDMSG                                         
         MVC   TWAKEYSV,KEY                                                     
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO RESTORE A PRODUCT RECORD AND POSITION TO ITS                   
* ORIGINAL PLACE                                                                
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
         LA    R0,L'PRDCODE                                                     
         MVC   NDIOA,AIO1          RESTORE I/O AREA POINTERS                    
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'RES'),NODKEY,((R0),DUB),0           
         CLI   NDERR,0                                                          
         BE    RESTX                                                            
         GOTO1 VNODERR                                                          
*                                                                               
REST4    GOTO1 VSETKEY                                                          
         MVC   NDIOA,AIO1          RESTORE PRODUCT TO END                       
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'RES'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    RESTX                                                            
         GOTO1 VNODERR                                                          
*                                                                               
RESTX    B     XIT                                                              
         EJECT                                                                  
* DISPLAY PRODUCT RECORD                                                        
*                                                                               
DREC     GOTO1 VCLEARF,DMCB,PRONAMH,PROLAST                                     
         L     R4,AIO                                                           
         GOTO1 VGETVAL             GET PRODUCT RECORD VALUES                    
         MVC   PRONAM,PRDNAM                                                    
*                                                                               
DREC2    CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DRECX                                                            
         MVC   PRODA(3),=C'DA='    DISPLAY DISK ADDRESS                         
         GOTO1 HEXOUT,DMCB,DMDSKADD,PRODA+3,4,=C'TOG'                           
         MVC   PRODA+12(5),=C'NODE='                                            
         GOTO1 (RF),DMCB,BUKNODE,PRODA+17,4,=C'TOG'                             
         OI    PRODAH+6,X'80'      XMIT FIELD                                   
*                                                                               
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
* LIST PRODUCT RECORDS                                                          
*                                                                               
LIST     MVI   NDUPDTSW,NO         DO NOT READ FOR UPDATE                       
         CLI   THISPF,PF7          TEST PF7=PREVIOUS PAGE                       
         BNE   LIST1                                                            
         LR    RE,R7                                                            
         AH    RE,MODLAST          RE=A(LAST MODIFIED FIELD)                    
         LA    R2,CONACTH                                                       
         CR    RE,R2               TEST ANYTHING CHANGED AFTER RECORD           
         BH    LIST1               YES                                          
         LA    R3,NDLVTAB+(2*NDLVTABL)                                          
         USING NDLVTABD,R3                                                      
         OC    NDLVNOD,NDLVNOD     TEST FOR ANY PRODUCTS                        
         BNZ   LIST8               YES-PERFORM BACKWARDS READ                   
*                                                                               
LIST1    OC    KEY,KEY             TEST FOR FIRST TIME                          
         BNZ   LIST4                                                            
*                                                                               
         MVI   SVNPRDS,0                                                        
         XC    SVFSTKEY,SVFSTKEY                                                
         GOTO1 VSETKEY                                                          
         LA    R1,LISTHK           SET IN-LINE HOOK                             
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST2                                                            
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    XIT                 YES-NO MORE RECORDS SO EXIT                  
         LA    R2,PRLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
LIST2    OC    PRDCODE,PRDCODE     TEST FOR START PRODUCT                       
         BZ    LIST6                                                            
         ZIC   RE,NDSQBACK         READ UNTIL END OF CLIENT                     
         BCTR  RE,0                                                             
         STC   RE,NDSQBACK                                                      
         B     LIST6                                                            
*                                                                               
* RE-SET FOR SEQUENTIAL WITH A READ                                             
*                                                                               
LIST4    MVI   SVNPRDS,0                                                        
         XC    SVFSTKEY,SVFSTKEY                                                
         MVC   NODKEY,SVNKEY       GET LAST SAVED NODAL KEY                     
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    LIST5                                                            
         LA    R2,PRLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
LIST5    ZIC   RE,NDSQBACK         READ UNTIL END OF CLIENT                     
         BCTR  RE,0                                                             
         STC   RE,NDSQBACK                                                      
*                                                                               
* READ SEQUENTIAL TO FILL SCREEN OR UP TO EOF                                   
*                                                                               
LIST6    LA    R1,LISTHK                                                        
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'SEQ',NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         LA    R2,PRLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* GO BACK TO PREVIOUS SCREEN                                                    
*                                                                               
LIST8    BAS   RE,BACK             BACK UP TO START OF PREVIOUS SCREEN          
         MVC   NODKEY,NODKEYSV     SET START POINT OF READ                      
         MVI   SVNPRDS,0                                                        
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
         LA    R2,PRLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* HOOK TO PROCESS RECORDS RETURNED BY NODIO                                     
*                                                                               
LISTHK   ST    RE,SAVERE                                                        
         CLI   NDMODE,NDFRST       TEST FOR FIRST TIME AT LEVEL                 
         BNE   LISTHK2                                                          
         CLI   NDLEV,2             TEST FOR RIGHT LEVEL                         
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SET SKIP LOWER LEVELS TO READ                
         B     LISTHKX             ONLY PRODUCTS                                
*                                                                               
LISTHK2  CLI   NDMODE,NDPROC                                                    
         BNE   LISTHKX                                                          
         CLI   NDLEV,2             TEST FOR LEVEL 2 RECORD                      
         BNE   LISTHKX                                                          
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCPRO     MAKE SURE IT IS A PRODUCT                    
         BZ    LISTHKX                                                          
*                                                                               
         GOTO1 VGETVAL                                                          
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),NDLVKEY EXTRACT KEY                                 
         MVC   SVNKEY,NODKEY       EXTRACT NODAL KEY                            
         MVC   DMDSKADD,NDLVDA     AND THE DISK ADDRESS                         
         OC    SVFSTKEY,SVFSTKEY   TEST FOR FIRST KEY                           
         BNZ   *+10                NO                                           
         MVC   SVFSTKEY,NODKEY     YES-SAVE THE KEY                             
         ZIC   R1,SVNPRDS          INCREMENT N'PRDS ON SCREEN                   
         LA    R1,1(R1)                                                         
         STC   R1,SVNPRDS                                                       
*                                                                               
         LA    RF,DISLIST          RF=A(REGULAR LIST ROUTINE)                   
         CLI   TRACEOPT,YES        TEST FOR TRACE                               
         BNE   *+8                                                              
         LA    RF,TRALIST          YES-DISPLAY A TRACE                          
         BASR  RE,RF                                                            
         GOTO1 LISTMON                                                          
*                                                                               
LISTHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
LISTHD   DC    C'CODE   NAME              '                                     
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
         CLI   SVNPRDS,0           TEST ANYTHING ON SCREEN                      
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
         ZIC   R1,NDSQBACK         SUPPRESS CLIENT READ                         
         BCTR  R1,0                                                             
         STC   R1,NDSQBACK                                                      
         MVI   NDSKIP,YES                                                       
         GOTO1 VNODIO,DMCB,NODBLKD,=C'BSEQ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    BACKX                                                            
         CLI   NDERR,NDRNFERR                                                   
         BE    BACKX                                                            
         LA    R2,PRLCLTH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
BACKX    B     XIT                                                              
         SPACE 2                                                                
*  HOOK ROUTINE FOR BACKWARDS READ                                              
*                                                                               
BACKHK   ST    RE,SAVERE                                                        
         CLI   NDMODE,NDFRST                                                    
         BNE   BACKHK2                                                          
         CLI   NDLEV,2                                                          
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SUPPRESS READING PLANS                       
         B     BACKHKX                                                          
*                                                                               
BACKHK2  CLI   NDMODE,NDPROC                                                    
         BNE   BACKHKX                                                          
         CLI   NDLEV,2             CHECK FOR A PRODUCT                          
         BNE   BACKHKX                                                          
         L     R4,NDIOA                                                         
         TM    BURCTYP,BUKCPRO                                                  
         BZ    BACKHKX                                                          
*                                                                               
         MVC   NODKEYSV,NODKEY     SAVE LAST KEY FOUND                          
         ZIC   R1,NPRDS            UPDATE N'PRODUCTS FOUND                      
         LA    R1,1(R1)                                                         
         STC   R1,NPRDS                                                         
         CLC   NPRDS,NLISTS        TEST IF SCREEN FILLED                        
         BL    *+8                 NO                                           
         MVI   NDMODE,NDEND        YES-STOP NODIO READ                          
*                                                                               
BACKHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY LIST DATA LINE (PRECEDE WITH GETVAL CALL)              
*                                                                               
DISLIST  NTR1                                                                   
         XC    PRLHED,PRLHED       DISPLAY HEADLINE                             
         MVC   PRLHED(L'LISTHD),LISTHD                                          
         OI    PRLHEDH+6,X'80'                                                  
         LA    R2,LISTAR                                                        
         USING LSTLIND,R2                                                       
         MVC   LISTAR,SPACES                                                    
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVC   LSTCOD,BUKCODE      EXTRACT ACTUAL PRODUCT CODE                  
         MVC   LSTNAM,PRDNAM                                                    
*                                                                               
DISLISTX B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY A NODAL TRACE FOR A PRODUCT                            
* AT ENTRY, R3=A(LEVEL TABLE ENTRY), R4=A(RECORD)                               
*                                                                               
TRALIST  NTR1                                                                   
         MVC   PRLHED,SPACES       CLEAR LIST HEADLINE                          
         MVC   PRLHED(L'TRACEHD),TRACEHD SET LIST HEADLINE                      
         OI    PRLHEDH+6,X'80'     XMIT                                         
         LA    R2,LISTAR                                                        
         USING TRALIND,R2                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   TRACOD,PRDCODE                                                   
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
* PRINT PRODUCT REPORT                                                          
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
         CLI   NDLEV,2             TEST FOR PRODUCT                             
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SKIP READING PLANS                           
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
         CLI   NDLEV,1             TEST FOR CLIENT                              
         BNE   *+12                                                             
         MVI   FORCEHED,YES        BREADK A PAGE FOR NEW CLIENT                 
         B     PREPX                                                            
*                                                                               
         CLI   NDLEV,2             TEST FOR PRODUCT                             
         BNE   PREPX                                                            
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVC   PRTCODE,PRDCODE                                                  
         OC    PRTCODE,SPACES                                                   
         MVC   PRTNAM,PRDNAM                                                    
         GOTO1 SPOOL,PARAS,(R8)                                                 
         BASR  RE,RF               SKIP A LINE                                  
         B     PREPX                                                            
         DROP  R2                                                               
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(3),CLTCODE                                                 
         OC    H4+10(3),SPACES                                                  
         MVC   H4+15(L'CLTNAM),CLTNAM                                           
*                                                                               
         ICM   R3,15,ABOX                                                       
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
         MVI   BOXROWS+6,C'T'      SET UP FOR BOXES                             
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   40(R2),C'L'                                                      
         MVI   46(R2),C'C'                                                      
         MVI   66(R2),C'R'                                                      
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
ADDMSG   DC    C'PRODUCT RECORD ADDED TO FILE'                                  
DELMSG   DC    C'PRODUCT RECORD DELETED'                                        
RESMSG   DC    C'PRODUCT RECORD RESTORED'                                       
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,48,C'PRODUCT REPORT'                                          
         SSPEC H2,48,C'--------------'                                          
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
         SSPEC H8,42,C'CODE'                                                    
         SSPEC H8,48,C'NAME'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF3D                                                       
         EJECT                                                                  
* DSECT TO COVER LIST SCREEN                                                    
*                                                                               
         ORG   CONTAGH                                                          
*      ++INCLUDE BUFILE3D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
TRACEOPT DS    C                   Y=TRACE OPTION ACTIVE FOR LIST               
NPRDS    DS    X                   N'PRODUCTS FOUND GOING BACKWARDS             
         ORG   TWA1USER                                                         
SAVCLT   DS    CL3                 SAVED CLIENT CODE                            
SAVCLN   DS    CL(L'CLTNAM)        SAVED CLIENT NAME                            
SVNPRDS  DS    X                   N'PRODUCT RECORDS ON LIST SCREEN             
SVFSTKEY DS    CL(L'NODKEY)        1ST REC ON LIST SCREEN KEY                   
         SPACE 2                                                                
* DSECT TO COVER LIST DISPLAY LINE                                              
*                                                                               
LSTLIND  DSECT                                                                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSTCOD   DS    CL3                 PRODUCT CODE                                 
         DS    CL4                 SPARE                                        
LSTNAM   DS    CL20                                                             
         SPACE 2                                                                
* DSECT TO COVER TRACE LIST LINE                                                
*                                                                               
TRALIND  DSECT                                                                  
TRACOD   DS    CL3                 PRODUCT CODE                                 
         DS    CL2                                                              
TRANODE  DS    CL6                 PRODUCT'S NODE                               
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
         DS    CL41                                                             
PRTCODE  DS    CL3                                                              
         DS    CL3                                                              
PRTNAM   DS    CL20                                                             
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
PF7      EQU   X'07'                                                            
PF8      EQU   X'08'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012BUFIL03   05/01/02'                                      
         END                                                                    
