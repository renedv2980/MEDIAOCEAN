*          DATA SET CTSFM0E    AT LEVEL 036 AS OF 08/11/16                      
*PHASE TA0A0EB                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE PUBVAL                                                                 
***********************************************************************         
*        EDICT TRANSACTION REPORT                                               
***********************************************************************         
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - WORK REG                                                          
*        R3 - WORK REG                                                          
*        R4 - WORK REG                                                          
*        R5 - WORK REG                                                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER                              
*        R7 - SECOND BASE REG   *****IMPORTANT*********                         
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
***********************************************************************         
         TITLE 'TA0A0E - EDICT TRANSACTION REPORT'                              
***********************************************************************         
* * * *     - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM    * * * *         
***********************************************************************         
TA0A0E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**EDICT*,R7,RR=R2,CLEAR=YES                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING MYAREAD,R9                                                       
         ST    R2,RELO                                                          
*                                                                               
         L     RE,=A(EDICTBLK)                                                  
         AR    RE,R2               RELO                                         
         ST    RE,AEDCTBLK         STORE ADDRESS OF EDICTBLK                    
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   VUTL,MCUTL          A(UTL)                                       
         MVC   VREMOT,MCVREMOT                                                  
         DROP  R1                                                               
*                                                                               
         USING TWADCOND,R2                                                      
         L     R2,TWADCONS         A(DADDS)                                     
         MVC   VDADDS,TDADDS                                                    
         B     *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         MVC   SETABLBL,=CL16'*SE-TAB**SE-TAB*'                                 
         MVC   DATADISP,=H'28'     FIRST ELEMENT IN CONTROL FILE                
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        EDICT TRANSACTION REPORT                                               
***********************************************************************         
*                                                                               
MAIN     MVI   REPEDI,C'N'         REP OR ADV JCL                               
         CLI   TRNSYST,C'R'                                                     
         BNE   *+8                                                              
         MVI   REPEDI,C'Y'                                                      
*                                                                               
* PUT CORRECT LENGTHS INTO SORTCARD AND RECCARD                                 
*                                                                               
         LA    R0,SRTKEYLN                                                      
         EDIT  (R0),(3,SORTCARD+15),FILL=0                                      
         LA    R0,SRLEN                                                         
         EDIT  (R0),(4,RECCARD+21),FILL=0                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         MVI   FIRSTLIN,C'Y'                                                    
         BRAS  RE,PQOVER           BUILD TABLE OF PQ OVERRIDES                  
         BRAS  RE,EDIREAD          READ EDICT FILE AND PUT TO SORTER            
         BRAS  RE,PROCRECS         GET FORM SORTER AND PRINT REPORTS            
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        BUILD TABLE OF OVERRIDE PRINT QUEUE IDS FROM EDICT RECS (SFM)          
***********************************************************************         
PQOVER   NTR1                                                                   
         MVC   DATADISP,=H'28'     CONTROL FILE                                 
         MVC   LKEY,=H'25'                                                      
*                                                                               
         XC    PQOVTAB,PQOVTAB     OVERRIDE TABLE                               
         LA    R2,PQOVTAB                                                       
*                                                                               
         USING EDIKEYD,R4                                                       
         L     R4,AIO2                                                          
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
*                                                                               
PO05     GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R4),(R4),0               
         CLI   DMCB+8,0                                                         
         BE    PO20                                                             
         DC    H'0'                                                             
PO10     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',0,(R4),0                  
         CLI   DMCB+8,0                                                         
         BE    PO20                                                             
         DC    H'0'                                                             
*                                                                               
PO20     L     R4,AIO2                                                          
         MVC   KEYSAVE(L'EDIKEY),0(R4)                                          
         CLI   0(R4),EDIKSYSQ      EDICT RECORD?                                
         BNE   POX                                                              
         CLI   1(R4),EDITYPEQ                                                   
         BNE   POX                 NO MORE EDICT RECORDS                        
*                                                                               
         USING EDILNKD,R6                                                       
         L     R6,AIO2                                                          
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   EDIOVUID,SPACES     ANY OVERRIDE PQ ID                           
         BNH   PO10                                                             
*                                                                               
         MVC   IDFORM,SPACES       GET ID NUM FOR SENDER                        
         MVC   IDFORM(L'EDINAME),EDINAME                                        
         GOTO1 =A(GETAGYNM),DMCB,(RC),RR=RELO                                   
         MVC   0(2,R2),PQDESTID                                                 
*                                                                               
         MVC   IDFORM,SPACES       GET OVERRIDE ID                              
         MVC   IDFORM,EDIOVUID                                                  
         GOTO1 =A(GETAGYNM),DMCB,(RC),RR=RELO                                   
         MVC   2(2,R2),PQDESTID                                                 
         LA    R2,4(R2)                                                         
         LA    R1,PQOVTABX                                                      
         CR    R2,R1                                                            
         BL    *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         L     R4,AIO2                                                          
         MVC   0(L'EDIKEY,R4),KEYSAVE                                           
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R4),(R4),0               
         CLI   DMCB+8,0                                                         
         BE    PO10                RESTORE SEQUENCE                             
         DC    H'0'                                                             
*                                                                               
POX      B     YES                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        READ EDICT FILE                                                        
*        SERVICE FILES ARE SPECIAL AND TO OPEN READ-ONLY YOU                    
*        HAVE TO FORCE IT BY TURNNING ON THE DTF_RO BIT.                        
*        YOU THINK YOU COULD USE THE WRSVR=N BUT THEN YOU WOULD NOT BE          
*        ABLE TO WRITE OUT TO THE PRINTQ                                        
***********************************************************************         
         USING DTFPHD,R2                                                        
EDIREAD  NTR1                                                                   
         CLI   REPEDI,C'Y'         REP SYSTEM                                   
         BE    ER10                                                             
         GOTO1 DATAMGR,DMCB,(0,=C'DTFADD'),=C'EDCTA'                            
         L     R2,12(R1)           LOAD DTF                                     
         OI    DTFOPEN,DTF_RO      FORCE TO READ-ONLY                           
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'SERVICE',                X        
               =C'NEDCTA  X',IO,0                                               
         B     ER20                                                             
*                                                                               
ER10     GOTO1 DATAMGR,DMCB,(0,=C'DTFADD'),=C'EDCTR'                            
         L     R2,12(R1)                                                        
         OI    DTFOPEN,DTF_RO      FORCE TO READ-ONLY                           
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'SERVICE',                X        
               =C'NEDCTR  X',IO,0                                               
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        READ CONTROL RECORD                                                    
***********************************************************************         
ER20     ST    R2,EDICTFL                                                       
         MVI   EDICTFL,X'00'       CLEAR HIGH ORDER BYTE                        
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,AEDCTBLK,0,                 +        
               EDICTFL,=X'00010100',0                                           
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         USING EDFILD,R1                                                        
         L     R1,AEDCTBLK                                                      
         CLI   EDFMON,EDFMONPQ                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EDCTFTPD,EDFTKPDY         TRACKS/DAY                             
         XC    EDCTFRPT,EDCTFRPT                                                
         MVC   EDCTFRPT+1(1),EDFBKPTK    PHYSICAL RECS (BLKS)/TRACK             
         XC    EDCTRPB,EDCTRPB                                                  
         MVC   EDCTRPB+1(1),EDFRCPBK     LOGICAL RECORDS/BLOCK                  
         MVC   EDCTFRCL,EDFLRECL         LOGICAL RECORD LENGTH                  
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        CALCULATE THE DISK ADDRESS AND GOTO DADDS                              
***********************************************************************         
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(3,DAYSTART)                              
         MVC   BMONTH,DAYSTART+1        SAVE MONTH                              
         ZIC   R2,DAYSTART+2            DAY NUMBER                              
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR TODAY         
         STCM  R2,3,FIRSTRAC                                                    
         LH    R1,EDCTFTPD                                                      
         BCTR  R1,0                                                             
         AR    R1,R2               LAST TRACK NUMBER FOR TODAY                  
         STCM  R1,3,EDCTFLST                                                    
*                                                                               
ER120    STCM  R2,3,EDCTFDSK         TRACK NUMBER                               
         STCM  R2,3,LASTRAC                                                     
*                                                                               
         LA    R2,1                                                             
ER130    STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         STC   R2,LASTBLK                                                       
*                                                                               
         L     R2,EDICTFL          READ BLOCK                                   
         LA    R3,EDCTFDSK                                                      
         L     R4,AEDCTBLK                                                      
         GOTO1 VDADDS,DMCB,RDID,(R4),0,(R2),(R3),0                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        CREATE SORT REC AND GET NEXT RECORD                                    
***********************************************************************         
*                                                                               
         USING EDFILD,R4                                                        
         L     R4,AEDCTBLK         R4-POINTER IN BLOCK                          
         LA    R1,1                                                             
         STH   R1,RECOUNT                                                       
*                                                                               
ER140    CLI   EDFSTAT,EDFNOOP     IGNORE THESE                                 
         BE    ER145                                                            
         CLI   EDFMON,EDFMONPQ     IGNORE THESE                                 
         BE    ER145                                                            
         CLC   EDFMON,BMONTH       IS THIS RECORD FROM THIS MONTH?              
         BNE   ERX                 NO -- WE'VE FOUND THE EOF                    
         BRAS  RE,SORT             MAKE SORT RECORD                             
*                                                                               
ER145    AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LH    R1,RECOUNT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,RECOUNT                                                       
         CH    R1,EDCTRPB          ANY MORE RECORDS IN THIS BLOCK?              
         BNH   ER140                                                            
*                                                                               
         ZIC   R2,LASTBLK                                                       
         LA    R2,1(R2)                                                         
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   ER130               YES                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,LASTRAC                                                     
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
         BNH   ER120                                                            
         DC    H'0'                ALL TRACKS FOR TODAY ARE USED                
ERX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD SORT RECORD                                                      
***********************************************************************         
*                                                                               
         USING SRECD,R3                                                         
         USING EDFILD,R4                                                        
SORT     NTR1                                                                   
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         LA    R3,SREC                                                          
*                                                                               
         OC    EDFPQUID,EDFPQUID   ANY USERID?                                  
         BZ    EXIT                NO - SKIP THIS ONE                           
         CLI   EDFSYS,X'40'        ANY SYSTEM                                   
         BNH   EXIT                NO- THEN SKIP                                
         CLI   EDFSTAT,EDFNOOP     IGNORE THESE                                 
         BE    EXIT                                                             
*                                                                               
         LA    R2,SORTTAB          SORT ROUTINES BY SYS-TYPE                    
SORT10   CLI   0(R2),X'FF'                                                      
         BE    EXIT                                                             
         CLI   0(R2),C' '          NO SYS IN TABLE = ACROSS SYSTEMS             
         BE    SORT15                                                           
         CLC   EDFSYS,0(R2)        SYSTEM                                       
         BNE   SORT20                                                           
SORT15   CLC   EDFTYPE,1(R2)       TYPE                                         
         BE    SORT30                                                           
SORT20   LA    R2,L'SORTTAB(R2)                                                 
         B     SORT10                                                           
*                                                                               
SORT30   ICM   R1,15,2(R2)         SORT ROUTINE                                 
         BR    R1                                                               
         EJECT                                                                  
***********************************************************************         
*        GENERAL FAX TRANSMISSION - SORT REC                                    
***********************************************************************         
*                                                                               
SRGENFAX DS    0H                  GENERAL FAX FEATURE                          
         MVI   SRCODE,C'S'         SENDERS REPORT                               
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRSYS,EDFSYS                                                     
         BRAS  RE,SORTPUT                                                       
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        SA - SPOT ADDS AVAIL REQUESTS - SORT REC                               
***********************************************************************         
*                                                                               
SRSA     DS    0H                  SPOT ADDS AVAILS                             
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,SPAVMED                                                    
         MVC   SRCLT,SPAVCLT                                                    
         MVI   SRSCODE,C'A'        ADDS FIRST                                   
         MVC   SRSATYPE,EDFTYPE                                                 
         MVC   SRSAPRD,SPAVPRD                                                  
         MVC   SRSADEST,EDFDEST                                                 
         MVC   SRSATIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
*                                                                               
***********************************************************************         
*        SO - SPOT ADDS DRAFT ORDERS - SORT REC                                 
***********************************************************************         
*                                                                               
SRSO     DS    0H                SPOT ADDS DRAFT ORDERS                         
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,SPEDMED                                                    
         MVC   SRCLT,SPEDCLT                                                    
         MVI   SRSCODE,C'A'        ADDS ORDERS FIRST                            
         MVC   SRSATYPE,EDFTYPE                                                 
         MVC   SRSAPRD,SPEDPRD                                                  
         MVC   SRSADEST,EDFDEST                                                 
         MVC   SRSATIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        SX - SPOT CONFIRMATION OF PURCHASE - SORT REC                          
***********************************************************************         
*                                                                               
SRSX     MVI   SRCODE,C'S'                                                      
         MVI   SRSYS,C'B'          TRICK IT INTO STARTING NEW REPORT            
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,SPEDMED                                                    
         MVC   SRCLT,SPCPRQST                                                   
         MVI   SRSCODE,C'A'        ADDS ORDERS FIRST                            
         MVC   SRSXTYPE,EDFTYPE                                                 
         MVC   SRSXCLT,SPEDCLT                                                  
         MVC   SRSXPRD,SPEDPRD                                                  
         MVC   SRSXDEST,EDFDEST                                                 
         MVC   SRSXTIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
*                                                                               
***********************************************************************         
*        SV - SPOT INVOICE CONTROL - SORT REC                                   
***********************************************************************         
*                                                                               
SRSV     MVI   SRCODE,C'S'                                                      
         MVI   SRSYS,C'C'          TRICK IT INTO STARTING NEW REPORT            
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,SPNVMED                                                    
         MVI   SRSCODE,C'A'        ADDS ORDERS FIRST                            
         MVC   SRSVTYPE,EDFTYPE                                                 
         MVC   SRSVCLT,SPNVCLT                                                  
         MVC   SRSVDEST,EDFDEST                                                 
         MVC   SRSVTIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
*                                                                               
***********************************************************************         
*        SD - SPOT DMBDE SPECIAL TRANSMISSION - SORT REC                        
***********************************************************************         
*                                                                               
SRSPDMB  DS    0H                  SPOT DMBDE SPECIAL TRANS                     
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,SPDAMED                                                    
         MVC   SRCLT,SPDACLT                                                    
         MVI   SRSCODE,C'A'        ADDS FIRST , TREAT LIKE ADDS                 
         MVC   SRSATYPE,EDFTYPE                                                 
         MVC   SRSAPRD,SPDAPRD                                                  
         MVC   SRSADEST,EDFDEST                                                 
         MVC   SRSATIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        ST/SI/SC/SS/SU - SPOT TRAFFIC - SORT REC                               
***********************************************************************         
*                                                                               
SRSPTRA  DS    0H                                                               
         MVI   SRCODE,C'S'         SPOT TRAFFIC                                 
         MVI   SRSYS,C'T'          FAKE INTO NEW REPORT ID                      
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,EDISTTMD                                                   
         MVC   SRCLT,EDISTTCL                                                   
         MVI   SRSCODE,C'T'        ADDS FIRST / TRAFFIC LAST                    
         MVC   SRSTPRD,EDISTTPR                                                 
         MVC   SRSTPTN,EDISTTP2                                                 
         MVC   SRSTTYPE,EDFTYPE                                                 
         MVC   SRSTDEST,EDFDEST                                                 
         MVC   SRSTTIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
*                                                                               
***********************************************************************         
*        PI/PV/PP - PRINT - SORT RECS                                           
***********************************************************************         
*                                                                               
SRPRINT  DS    0H                  PRINT                                        
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,PPEDMED                                                    
         MVC   SRCLT,PPEDCLT                                                    
         MVC   SRPPRD,PPEDPRD                                                   
         MVC   SRPJOB,PPEDJOB                                                   
         MVC   SRPPUB,PPEDPUB                                                   
         MVC   SRPDEST,EDFDEST                                                  
         MVC   SRPTIME,EDFSNTIM                                                 
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        AO - ACCOUNT ORDERS - SORT REC                                         
***********************************************************************         
*                                                                               
SRACC    DS    0H                  ACC                                          
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,ACFXTYPE      MEDIA NOT USED THIS IS TYPE(P/E)             
         CLI   ACFXTYPE,C'E'                                                    
         BNE   SRACC1                                                           
         MVC   SRAPRD(14),ACFXEXP                                               
         B     SRACC2                                                           
SRACC1   MVC   SRCLT,ACFXCLI                                                    
         MVC   SRAPRD,ACFXPRO                                                   
         MVC   SRAJOB,ACFXJOB                                                   
SRACC2   MVC   SRAONUM,ACFXONUM                                                 
         MVC   SRADEST,EDFDEST                                                  
         MVC   SRATIME,EDFSNTIM                                                 
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        NN/NC - NET TRAFFIC - SORT REC                                         
***********************************************************************         
*                                                                               
SRNETRA  DS    0H                  NET TRAFFIC                                  
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,EDINTNMD                                                   
         MVC   SRCLT,EDINTNCL                                                   
         MVC   SRNPRD,EDINTNPR                                                  
         MVC   SRNTYPE,EDFTYPE                                                  
         MVC   SRNNET,EDINTNET                                                  
         MVC   SRNTIME,EDFSNTIM                                                 
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
*                                                                               
***********************************************************************         
*        NV - NET DUNNING LETTER - SORT REC                                     
***********************************************************************         
*                                                                               
SRNETDL  DS    0H                  NET DUNNING LETTER                           
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,EDINTVMD                                                   
         MVC   SRCLT,EDINTVCL                                                   
         MVC   SRNVPRD,EDINTVPR                                                 
         MVC   SRNVSTN,EDIRTVST                                                 
         MVC   SRNVTIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
*                                                                               
***********************************************************************         
*        NR - NETPAK APPLICATION - SORT REC                                     
***********************************************************************         
*                                                                               
SRNEPAK  DS    0H                  NETPAK                                       
         MVI   SRCODE,C'S'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVI   SRMED,C'N'                                                       
         MVC   SRCLT,NEEDCLT                                                    
         MVC   SRNPPRG,NEEDPRG                                                  
         MVC   SRNPNET,NEEDNET                                                  
         MVC   SRNPEST,NEEDEST                                                  
         MVC   SRNPPAK,NEEDPAK                                                  
         MVC   SRNPTIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        RO/RK/RC - REP APPLICATIONS - SORT REC                                 
***********************************************************************         
*                                                                               
SRREP    DS    0H                  REP                                          
         MVI   SRCODE,C'S'                                                      
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRRTYP,EDFTYPE                                                   
         MVC   SRRTIME,EDFSNTIM                                                 
         CLI   EDFTYPE,EDFTCONQ    CONTRACT ORDERS                              
         BE    SRCON                                                            
         CLI   EDFTYPE,EDFTCCCQ    CONTRACT ORDERS                              
         BNE   SRKWX                                                            
SRCON    MVC   SRROFF,EDIRCNOF                                                  
         MVC   SRRSTN,EDIRCNST                                                  
         MVC   SRRSLP,EDIRCNSP                                                  
         B     REPSORT                                                          
SRKWX    MVC   SRROFF,EDIRKXOF                                                  
         MVC   SRRSTN,EDIRKXST     KWX                                          
         MVC   SRRSLP,EDIRKXBC+2                                                
REPSORT  BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        RD - REP DTR  - SORT REC                                               
***********************************************************************         
*                                                                               
SRREPD   DS    0H                  REP                                          
         MVI   SRCODE,C'S'                                                      
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRMED,EDFTYPE       FORCE TO SORT AFTER OTHER REP TYPES          
         MVC   SRDTYPE,EDFTYPE                                                  
         MVC   SRDTIME,EDFSNTIM                                                 
         MVC   SRDSTN,EDIRDTST                                                  
         MVC   SRDETRN,EDIRDDTS                                                 
         MVC   SRDSTART,EDIRDTSD                                                
         MVC   SRDEND,EDIRDTED                                                  
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        DO/DE/DN/DR - DARE ADV - SORT REC                                      
***********************************************************************         
*                                                                               
SRDAREA  DS    0H                  DARE - ADV                                   
         MVI   SRCODE,C'S'         SENDER'S                                     
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRDABYR,EDIRDRBY                                                 
         TM    EDFSTAT,EDFSTRCV+EDFSTCAN                                        
         BZ    *+10                                                             
         MVC   SRDAREP,EDFDEST     REP IS IN DEST IF DLVD OR CANX               
         MVC   SRDASTN,EDIRDRST                                                 
         MVC   SRDASP,EDIRDRSP                                                  
         MVC   SRDATIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
*                                                                               
***********************************************************************         
*        DA/DR/DC - DARE REP - SORT REC                                         
***********************************************************************         
*                                                                               
SRDARER  DS    0H                  DARE - REP                                   
         CLI   REPEDI,C'Y'                                                      
         BNE   SRDAREA             THEN IT'S DARE FOR ADV                       
*                                                                               
         MVI   SRCODE,C'S'         SENDER'S                                     
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRDRSP,EDIRDRSP                                                  
         MVC   SRDRSTN,EDIRDRST                                                 
         MVC   SRDRAGY,EDIRDRAG                                                 
         MVC   SRDRTIME,EDFSNTIM                                                
         BRAS  RE,SORTPUT                                                       
         B     RSORTPUT                                                         
         EJECT                                                                  
***********************************************************************         
*        CONSOLIDATED REPORT FOR ADV USERS                                      
***********************************************************************         
*                                                                               
RSORTPUT DS    0H                                                               
         CLI   REPEDI,C'Y'         REP?                                         
         BE    CONSREP             NEED CONSOLIDATED REPORT? (REP)              
*                                                                               
         CLI   EDFSYS,EDFDARRQ     NO DARE CONSOLIDATED REPORTS                 
         BE    CKRCV                                                            
         XC    IDFORM,IDFORM                                                    
         MVC   IDFORM+8(2),EDFPQUID                                             
         BRAS  RE,GETPOW              GET POWER CODE                            
         LA    R2,ADVCONTB             WHO NEEDS CONSOLIDATED RPTS(ADV)         
NEXTPOW  CLC   POWCODE,0(R2)           FIND MATCHING POWER CODE                 
         BE    GOTMATCH                                                         
         LA    R2,L'ADVCONTB(R2)   BUMP TO NEXT ROW                             
         CLI   0(R2),X'FF'                                                      
         BNE   NEXTPOW                                                          
         B     CKRCV                                                            
*                                                                               
GOTMATCH LA    R3,SREC                                                          
         USING SRECD,R3                                                         
         LH    R1,EDCTFRCL         LOGICAL RECORD LENGTH                        
* WARNING: IF EDICT RECORD GREATER THAN 256, MUST CHANGE EXECUTED               
* MVC TO A MVCL                                                                 
         CH    R1,=H'256'                                                       
         BH    DIE                                                              
         LTR   R1,R1                                                            
         BZ    DIE                                                              
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SREDIREC(0),0(R4)   MOVE FOR PROPER LENGTH                       
         MVC   SREDIREC+15(2),2(R2)                                             
         MVI   SRCODE,C'Y'         FOR EXTENDED (CONSOLIDATED)                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         B     CKRCV                                                            
         EJECT                                                                  
***********************************************************************         
*        RECEIVER'S REPORT FOR ADV USERS                                        
***********************************************************************         
*                                                                               
CKRCV    DS    0H                                                               
         CLI   EDFDSTTY,EDFDSTEQ   DEST TYPE EDICT= GETS RECEIVER'S             
         BNE   CKKARI                                                           
         CLI   EDFSYS,EDFDARRQ     EXCEPT DARE FOR NOW - NO FORMAT              
         BE    EXIT                                                             
*        CLI   EDFMETH,C'F'        EXCEPT NOT FTP TRANSMISSIONS                 
*        BE    CKKARI                                                           
*                                                                               
         USING RSRECD,R3                                                        
         LA    R3,SREC             MAKE SORT REC                                
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         MVI   RSRCODE,C'R'                                                     
         MVC   RSRDEST,EDFDEST                                                  
         MVC   RSRSEND,EDFPQUID                                                 
         MVC   RSRSYS,EDFSYS                                                    
         MVC   RSRTYPE,EDFTYPE                                                  
         MVC   RSRSENT,EDFSNTIM                                                 
         LH    R1,EDCTFRCL         LOGICAL RECORD LENGTH                        
* WARNING: IF EDICT RECORD GREATER THAN 256, MUST CHANGE EXECUTED               
* MVC TO A MVCL                                                                 
         CH    R1,=H'256'                                                       
         BH    DIE                                                              
         LTR   R1,R1                                                            
         BZ    DIE                                                              
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSREDREC(0),0(R4)   MOVE FOR PROPER LENGTH                       
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         B     CKKARI                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        KARI REPORT - TOTALLING ADDS USAGE                                     
***********************************************************************         
*                                                                               
CKKARI   DS    0H                                                               
         CLI   EDFSYS,EDFSSPTQ     SPOT                                         
         BNE   EXIT                                                             
         CLI   EDFTYPE,EDFTADDQ    SPOT ADDS DRAFT ORDERS                       
         BE    KARIREC                                                          
         CLI   EDFTYPE,EDFTREQQ    SPOT ADDS AVAIL REQUESTS                     
         BNE   EXIT                                                             
*                                                                               
         USING SRECD,R3                                                         
KARIREC  LA    R3,SREC             MAKE SORT REC                                
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         MVI   SRCODE,C'Z'                                                      
         MVC   SRSYS,EDFSYS                                                     
         MVC   SRAGY,EDFPQUID                                                   
         MVC   SRMED,EDFTYPE                                                    
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         TM    KBIT,KDUMMY                                                      
         BO    EXIT                                                             
*                                                                               
         OI    KBIT,KDUMMY                                                      
         LA    R3,SREC             MAKE 1 DUMMY REC TO SIGNAL WHEN              
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               TO COMPILE TOTALS WHEN GETTING FROM          
         USING SRECD,R3            SORTER                                       
         MVI   SRCODE,C'0'         MUST COME AFTER 'Z'                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSOLIDATED REPORT FOR REP USERS                                      
***********************************************************************         
*                                                                               
CONSREP  DS    0H                                                               
         LA    R2,CONTABLE             WHO NEEDS CONSOLIDATED RPTS              
CR10     CLC   EDIRCNRP,0(R2)          FIND MATCHING REP                        
         BE    CR20                                                             
         LA    R2,L'CONTABLE(R2)                                                
         CLI   0(R2),X'FF'                                                      
         BNE   CR10                                                             
         B     EXIT                                                             
*                                                                               
         USING SRECD,R3                                                         
CR20     LA    R3,SREC                                                          
         MVC   EDFPQUID,2(R2)                                                   
         MVI   SRCODE,C'X'             FOR EXTENDED (CONSOLIDATED)              
         BRAS  RE,SORTPUT                                                       
         LA    R0,SREC             SET UP FOR MVCL TO CLEAR SREC                
         LA    R1,L'SREC                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        GET POWER CODE                                                         
***********************************************************************         
*                                                                               
GETPOW   NTR1                                                                   
         MVC   DATADISP,=H'28'     FIRST ELEMENT IN CONTROL FILE                
         MVC   LKEY,=H'25'                                                      
*                                                                               
         USING CTIKEY,R6                                                        
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE 'I'                              
         MVC   CTIKID,IDFORM                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO              KEYSAVE NOT AFFECTED BECAUSE                 
         CLC   CTIKEY(25),KEY                                                   
         BNE   NO                  BETTER BE THERE                              
*                                                                               
         MVI   ELCODE,X'06'        GET POWER CODE                               
         BRAS  RE,GETEL                                                         
         BNE   NO                                                               
         MVC   POWCODE,2(R6)                                                    
GPX      B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PUT SORT RECORD TO SORTER                                              
***********************************************************************         
*                                                                               
SORTPUT  NTR1                                                                   
         USING SRECD,R3                                                         
         LH    R1,EDCTFRCL         LOGICAL RECORD LENGTH                        
* WARNING: IF EDICT RECORD GREATER THAN 256, MUST CHANGE EXECUTED               
* MVC TO A MVCL                                                                 
         CH    R1,=H'256'                                                       
         BH    DIE                                                              
         LTR   R1,R1                                                            
         BZ    DIE                                                              
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SREDIREC(0),0(R4)   MOVE FOR PROPER LENGTH                       
         GOTO1 =V(SORTER),DMCB,=C'PUT',SREC                                     
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        PROCESS SORT RECORDS AND PRINT REPORTS                                 
***********************************************************************         
*                                                                               
PROCRECS NTR1                                                                   
         GOTO1 DATCON,DMCB,(3,DAYSTART),(11,HDATE)                              
         MVI   FIRSTIME,C'Y'                                                    
         MVI   FIRSTSEN,C'Y'                                                    
*                                                                               
GET      GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    PROCX                                                            
         LA    R0,SREC             SET UP FOR MVCL                              
         LA    R1,L'SREC                                                        
         LR    RE,R4                                                            
         LA    RF,L'SREC                                                        
         MVCL  R0,RE               TO SEE IN DUMP                               
         LA    R4,SREC                                                          
*                                                                               
         USING SRECD,R4                                                         
         LA    R4,SREC                                                          
         USING EDFILD,R3                                                        
         LA    R3,SREDIREC                                                      
*                                                                               
         L     R2,=A(REPTABLE)                                                  
*                                                                               
PROC01   CLC   EDFSYS(2),0(R2)          FIND MATCHING REPORT TYPE               
         BE    PROC02                                                           
         LA    R2,L'REPTABLE(R2)   BUMP TO NEXT ROW                             
         CLI   0(R2),X'FF'                                                      
         BNE   PROC01                                                           
         DC    H'0'                                                             
*                                                                               
PROC02   CLC   EDFPQUID,=H'7598'   OMDTOT                                       
         BNE   *+14                                                             
         CLC   =C'STR',10(R2)                                                   
         BE    GET                 SKIP STR REPORT FOR USERID=OMDTOT            
*                                                                               
         CLI   0(R4),C'R'          RECEIVER'S REPORT                            
         BNE   PROC10                                                           
         GOTO1 =A(RCVMAIN),DMCB,(RC),RR=RELO                                    
         B     GET                                                              
*                                                                               
PROC10   CLI   0(R4),C'S'          SENDER'S REPORT                              
         BE    PROC100                                                          
         CLI   0(R4),C'X'          CONSOLIDATED REP RPT                         
         BE    PROC100                                                          
         CLI   0(R4),C'Y'          CONSOLIDATED ADV REPORT                      
         BE    PROC100                                                          
         CLI   0(R4),C'Z'          SPECIAL ADDS REPORT FOR KARI                 
         BE    PROC15                                                           
         CLI   0(R4),C'0'          SPECIAL ADDS REPORT FOR KARI                 
         BNE   GET                                                              
PROC15   GOTO1 =A(KARI),DMCB,(RC),RR=RELO                                       
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORTS - KATZ REPORT                                         
***********************************************************************         
*                                                                               
PROC100  DS    0H                                                               
         B     PROC200             KATZ REPORT DISABLED                         
*                                                                               
         CLI   ANYKATZ,C'Y'        MAKE SURE KATZ GETS TRANS REPORT             
         BE    PROC200             EVEN IF NO INFO FOR THEM                     
         CLI   REPEDI,C'Y'         NOT FOR REP THOUGH                           
         BE    PROC200                                                          
*                                                                               
         MVI   KATTRN,C'Y'                                                      
         MVI   KPRTHDR,C'Y'                                                     
         MVI   KHDR,C'Y'                                                        
         MVI   FORCEHED,C'Y'                                                    
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         XC    IDFORM,IDFORM                                                    
         MVI   PQSW,1              SUPPRESS AUTO PRINTQ OPEN                    
         MVI   RPTCLASS,C'G'                                                    
         MVC   PQDESTID,=X'005A'   DDS                                          
         CLI   TRNPRTQ,C'Y'                                                     
         BNE   PROC110                                                          
         GOTO1 =A(NEWQ),DMCB,(RC),RR=RELO                                       
PROC110  GOTO1 =A(KATZR),DMCB,(RC),RR=RELO                                      
         MVI   ANYKATZ,C'Y'                                                     
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORTS                                                       
***********************************************************************         
*                                                                               
         USING SRECD,R4                                                         
         USING EDFILD,R3                                                        
PROC200  LA    R3,SREDIREC                                                      
         MVC   NEWAGY,SRAGY                                                     
         XC    TOT,TOT                                                          
         CLI   REPEDI,C'Y'         REP SYSTEM                                   
         BNE   PROC250                                                          
*                                                                               
         CLI   EDFSYS,EDFDARRQ     DARE FOR REP                                 
         BNE   PROC210                                                          
         CLC   SVSPCODE,SRDRSP     BREAK ON SALESPERSON CODE                    
         BE    *+8                                                              
         MVI   TOT,C'Y'                                                         
         B     PROC220                                                          
*                                                                               
PROC210  CLC   SVOFF,SRROFF        SAME OFFICE                                  
         BE    *+8                                                              
         MVI   TOT,C'Y'                                                         
PROC220  CLC   SVREP,SRAGY         SAME REP                                     
         BE    *+12                                                             
         MVI   TOT,C'Y'                                                         
         MVI   NEWQUE,C'Y'                                                      
*                                                                               
         CLI   TOT,C'Y'                                                         
         BE    PROC260             PRINT TOTALS AND OPEN NEW QUEUE              
         B     PROC280                                                          
*                                                                               
PROC250  CLC   SRAGY,SVAGYID       NEW AGENCY?                                  
         BNE   PROC260                                                          
         CLC   SRSYS,SVSYSTEM      NEW SYSTEM?                                  
         BE    PROC290                                                          
*                                                                               
PROC260  CLI   FIRSTSEN,C'Y'       FIRST SENDER'S REPORT                        
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XC    IDFORM,IDFORM       READ ID REC                                  
         MVC   IDFORM+8(2),EDFPQUID                                             
         GOTO1 =A(GETAGYNM),DMCB,(RC),RR=RELO                                   
         BNE   GET                                                              
         GOTO1 =A(GETSES),DMCB,(RC),RR=RELO     GET SE #S                       
         BNE   GET                                                              
*                                                                               
         MVI   PQSW,1              SUPPRESS AUTO PRINTQ OPEN                    
         MVI   RPTCLASS,C'A'       ADV USES RPT CLASS 'A'                       
         MVC   PQDESTID,EDFPQUID                                                
         CLI   FIRSTSEN,C'Y'       FIRST SENDER'S REPORT                        
         BE    PROC270                                                          
         MVI   PRCLTOT,C'Y'                                                     
         MVI   PRPRDTOT,C'Y'                                                    
         GOTO1 =A(PRNTOT),DMCB,(RC),RR=RELO                                     
PROC270  MVI   FIRSTLIN,C'Y'                                                    
         CLI   TRNPRTQ,C'Y'        WRITE TO PQ'S                                
         BNE   PROC280                                                          
         CLI   SRCODE,C'X'         CONSOLIDATED REP RPT?                        
         BNE   PROC275                                                          
         CLI   NEWQUE,C'Y'                                                      
         BNE   PROC280                                                          
PROC275  GOTO1 =A(NEWQ),DMCB,(RC),RR=RELO                                       
PROC280  MVI   FIRSTSEN,C'N'                                                    
*                                                                               
PROC290  MVC   SVAGYID,SRAGY       SAVE VALUES                                  
         MVC   SVREP,SRAGY                                                      
         MVC   SVSYSTEM,SRSYS                                                   
         MVC   SVOFF,SRROFF                                                     
         MVC   SVSPCODE,SRDRSP                                                  
         CLI   EDFTYPE,EDFTGENQ    GENERAL FAX FEATURE HAS NO READS             
         BNE   PROC300                                                          
         GOTO1 =A(GENFAX),DMCB,(RC),RR=RELO                                     
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
*        OPEN FILES - NET SYSTEM                                                
***********************************************************************         
PROC300  CLI   EDFSYS,EDFSNETQ     NETWORK SYSTEM                               
         BNE   PROC400                                                          
         L     R1,VUTL             RESET UTL                                    
         MVC   4(1,R1),SELIST      NET SE NUM                                   
         ZIC   R1,SELIST           CHECK IF ALREADY OPENED                      
         LA    R1,SETAB(R1)                                                     
         CLI   0(R1),0                                                          
         BNE   PROC330                                                          
         BRAS  RE,CHKMAXSE                                                      
         MVI   0(R1),C'S'                                                       
         LA    R2,OPENADDS                                                      
PROC310  CLC   0(4,R2),=C'SPOT'    NET=SPOT                                     
         BE    PROC320                                                          
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   PROC310             MUST FIND A MATCH                            
         B     GET                                                              
*                                                                               
PROC320  L     R3,8(R2)                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),(R2),(R3),IO,0                       
PROC330  GOTO1 =A(SPOTREAD),DMCB,(RC),RR=RELO                                   
         GOTO1 =A(NETREAD),DMCB,(RC),RR=RELO                                    
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
*        OPEN FILES - SPOT SYSTEM AND DARE SYSTEM                               
***********************************************************************         
*                                                                               
PROC400  CLI   EDFSYS,EDFSSPTQ     SPOT SYSTEM                                  
         BE    PROC410                                                          
         CLI   EDFSYS,EDFDARRQ     DARE                                         
         BNE   PROC500                                                          
         CLI   REPEDI,C'Y'         SPOT FOR ADV DARE                            
         BE    PROC500                                                          
PROC410  L     R1,VUTL             RESET UTL                                    
         MVC   4(1,R1),SELIST+2    SPOT SE NUM                                  
         ZIC   R1,SELIST+2         CHECK IF ALREADY OPENED                      
         LA    R1,SETAB(R1)                                                     
         CLI   0(R1),0                                                          
         BNE   PROC450                                                          
         BRAS  RE,CHKMAXSE                                                      
         MVI   0(R1),C'S'                                                       
         LA    R2,OPENADDS                                                      
PROC420  CLC   0(4,R2),=C'SPOT'                                                 
         BE    PROC440                                                          
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   PROC420             MUST FIND A MATCH                            
         B     GET                                                              
*                                                                               
PROC440  L     R3,8(R2)                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),(R2),(R3),IO,0                       
PROC450  GOTO1 =A(SPOTREAD),DMCB,(RC),RR=RELO                                   
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
*        OPEN FILES - PRINT SYSTEM                                              
***********************************************************************         
*                                                                               
PROC500  CLI   EDFSYS,EDFSPRTQ     PRINT SYSTEM                                 
         BNE   PROC600                                                          
         L     R1,VUTL             RESET UTL                                    
         MVC   4(1,R1),SELIST+1    PRINT SE NUM                                 
         ZIC   R1,SELIST+1         CHECK IF ALREADY OPENED                      
         LA    R1,SETAB(R1)                                                     
         CLI   0(R1),0                                                          
         BNE   PROC550                                                          
         BRAS  RE,CHKMAXSE                                                      
         MVI   0(R1),C'P'                                                       
         LA    R2,OPENADDS                                                      
PROC510  CLC   0(4,R2),=C'PRINT'                                                
         BE    PROC540                                                          
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   PROC510             MUST FIND A MATCH                            
         B     GET                                                              
*                                                                               
PROC540  L     R3,8(R2)                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),(R2),(R3),IO,0                       
PROC550  GOTO1 =A(PRNTREAD),DMCB,(RC),RR=RELO                                   
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
*        OPEN FILES - ACCOUNT SYSTEM                                            
***********************************************************************         
*                                                                               
PROC600  CLI   EDFSYS,EDFSACCQ     ACC SYSTEM                                   
         BNE   PROC700                                                          
         L     R1,VUTL             RESET UTL                                    
         MVC   4(1,R1),SELIST+4    ACC   SE NUM                                 
         ZIC   R1,SELIST+4         CHECK IF ALREADY OPENED                      
         LA    R1,SETAB(R1)                                                     
         CLI   0(R1),0                                                          
         BNE   PROC650                                                          
         BRAS  RE,CHKMAXSE                                                      
         MVI   0(R1),C'A'                                                       
         LA    R2,OPENADDS                                                      
PROC610  CLC   0(3,R2),=C'ACC'                                                  
         BE    PROC620                                                          
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   PROC610             MUST FIND A MATCH                            
         B     GET                                                              
*                                                                               
PROC620  GOTO1 DATAMGR,DMCB,=C'DTFADD',=C'ACCOUNT'                              
         L     R1,12(R1)                                                        
         TM    ISFTYPE-ISDTF(R1),ISFTEMU                                        
         BNO   PROC630                                                          
         LA    R3,EMULIST                                                       
         OI    EMBITS,X'80'                                                     
         B     PROC640                                                          
PROC630  L     R3,8(R2)                                                         
         NI    EMBITS,X'FF'-X'80'                                               
PROC640  GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),(R2),(R3),IO,0                       
PROC650  GOTO1 =A(ACCREAD),DMCB,(RC),RR=RELO                                    
         B     GET                                                              
         EJECT                                                                  
***********************************************************************         
*        OPEN FILES - REP SYSTEM                                                
***********************************************************************         
*                                                                               
PROC700  CLI   EDFSYS,EDFSREPQ     REP                                          
         BE    PROC710                                                          
         CLI   EDFSYS,EDFDARRQ     DARE                                         
         BNE   GET                                                              
         CLI   REPEDI,C'Y'         REP FOR REP DARE                             
         BNE   GET                                                              
PROC710  L     R1,VUTL             RESET UTL                                    
         MVC   4(1,R1),SELIST+3    REP SE NUM                                   
         ZIC   R1,SELIST+3         CHECK IF ALREADY OPENED                      
         LA    R1,SETAB(R1)                                                     
         CLI   0(R1),0                                                          
         BNE   PROC750                                                          
         BRAS  RE,CHKMAXSE                                                      
         MVI   0(R1),C'R'                                                       
         LA    R2,OPENADDS                                                      
PROC720  CLC   0(4,R2),=C'REP  '                                                
         BE    PROC740                                                          
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   PROC720             MUST FIND A MATCH                            
         B     GET                                                              
*                                                                               
PROC740  L     R3,8(R2)                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),(R2),(R3),IO,0                       
PROC750  GOTO1 =A(REPREAD),DMCB,(RC),RR=RELO                                    
         B     GET                                                              
*                                                                               
PROCX    MVI   PRCLTOT,C'Y'                                                     
         MVI   PRPRDTOT,C'Y'                                                    
         GOTO1 =A(PRNTOT),DMCB,(RC),RR=RELO                                     
         B     EXIT                                                             
DONE     DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        GET SENDER'S ALPHA ID - IN=IDFORM OUT=SNDRID                           
***********************************************************************         
         USING CTIKEY,R6                                                        
SENDID   NTR1                                                                   
         MVC   DATADISP,=H'28'     FIRST ELEMENT IN CONTROL FILE                
         MVC   LKEY,=H'25'                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE 'I'                              
         MVC   CTIKID,IDFORM                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO              KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         CLC   CTIKEY(25),KEY                                                   
         BE    *+8                 BETTER BE THERE                              
         B     NO                                                               
*                                                                               
         MVI   ELCODE,X'02'        GET SENDER                                   
         BRAS  RE,GETEL                                                         
         BNE   NO                                                               
         MVC   SNDRID,2(R6)                                                     
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT DATES FOR PRINTING                                              
*        IN=INDATES  OUT=DATES                                                  
***********************************************************************         
*                                                                               
DATEFORM NTR1                                                                   
         XC    DATES,DATES                                                      
         CLC   INDATES,SPACES          GET DATES                                
         BNH   EXIT                                                             
********TEMP PATCH FOR DATA RANGE ERROR, YYUN, 6/18******************           
         CLI   INDATES,C'|'                                                     
         BNE   *+8                                                              
         MVI   INDATES,C' '                                                     
         CLI   INDATES+6,C'|'                                                   
         BNE   *+8                                                              
         MVI   INDATES+6,C' '                                                   
********TEMP PATCH FOR DATA RANGE ERROR, YYUN, 6/18******************           
         GOTO1 DATCON,DMCB,(0,INDATES),(11,DATES)                               
         MVI   DATES+8,C'-'                                                     
         LA    R2,INDATES                                                       
         LA    R6,DATES                                                         
         GOTO1 DATCON,DMCB,(0,6(R2)),(11,9(R6))                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        FORMAT TIMES FOR PRINTING                                              
*        OUT=SENTIME,RCVTIME                                                    
***********************************************************************         
*                                                                               
TIMEFORM NTR1                                                                   
         XC    SENTIME,SENTIME                                                  
         XC    RCVTIME,RCVTIME                                                  
*                                                                               
         TM    EDFSTAT,EDFSTSNT    WAS REPORT SENT                              
         BNO   EXIT                                                             
         OC    EDFSNTIM,EDFSNTIM   0000 TIME MEANS MIDNIGHT                     
         BNZ   TF10                                                             
         MVC   SENTIME,=C'24:00'                                                
         B     TF20                                                             
*                                                                               
TF10     LA    R2,EDFSNTIM                                                      
         LA    R6,SENTIME                                                       
         GOTO1 HEXOUT,DMCB,(R2),(R6),1                                          
         MVI   2(R6),C':'                                                       
         GOTO1 HEXOUT,DMCB,1(R2),3(R6),1                                        
*                                                                               
TF20     TM    EDFSTAT,EDFSTRCV    WAS REPORT RECIEVED                          
         BNO   EXIT                                                             
         CLC   EDFRCVTM,SPACES                                                  
         BE    EXIT                                                             
         OC    EDFRCVTM,EDFRCVTM   0000 TIME MEANS MIDNIGHT                     
         BNZ   TF30                                                             
         MVC   RCVTIME,=C'24:00'                                                
         B     EXIT                                                             
*                                                                               
TF30     LA    R2,EDFRCVTM                                                      
         LA    R6,RCVTIME                                                       
         GOTO1 HEXOUT,DMCB,(R2),(R6),1                                          
         MVI   2(R6),C':'                                                       
         GOTO1 HEXOUT,DMCB,1(R2),3(R6),1                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT OUT LINE AND KEEP TOTALS                                         
***********************************************************************         
*                                                                               
PRTLINE  NTR1                                                                   
         USING EDFILD,R3                                                        
         CLI   FIRSTLIN,C'Y'                                                    
         BE    PL100                                                            
*                                                                               
         CLC   SVREQST,NEWREQST                                                 
         BE    PL10                                                             
         MVI   TOT,C'Y'                                                         
         MVI   PRCLTOT,C'Y'                                                     
         B     PL50                                                             
*                                                                               
PL10     CLC   SVCLT,NEWCLT                                                     
         BE    *+12                                                             
         MVI   PRCLTOT,C'Y'                                                     
         MVI   TOT,C'Y'                                                         
*                                                                               
         MVI   PRPRDTOT,C'N'                                                    
         CLI   SVPRDHD,C'N'                                                     
         BE    PL50                                                             
         MVI   PRPRDTOT,C'Y'                                                    
         CLC   SVPRD,NEWPRD                                                     
         BE    *+12                                                             
         MVI   PRPRDTOT,C'Y'                                                    
         MVI   TOT,C'Y'                                                         
*                                                                               
         CLC   SVPTN,NEWPTN                                                     
         BE    *+12                                                             
         MVI   PRPRDTOT,C'Y'                                                    
         MVI   TOT,C'Y'                                                         
*                                                                               
PL50     CLC   SVAGY,NEWAGY                                                     
         BE    PL60                                                             
         MVI   PRCLTOT,C'Y'                                                     
         MVI   TOT,C'Y'                                                         
*                                                                               
PL60     CLC   SVMED,NEWMED                                                     
         BE    *+8                                                              
         MVI   TOT,C'Y'                                                         
*                                                                               
         CLI   TOT,C'Y'            PRINT TOTALS                                 
         BNE   PL100                                                            
         MVC   SAVELINE,P                                                       
         MVC   P,SPACES                                                         
         GOTO1 =A(PRNTOT),DMCB,(RC),RR=RELO                                     
         MVC   P,SAVELINE                                                       
*                                                                               
PL100    XC    TOT,TOT                                                          
         TM    EDFSTAT,EDFSTCAN                                                 
         BZ    PL110                                                            
         LH    R1,CANTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,CANTOTAL                                                      
         B     PL200                                                            
*                                                                               
PL110    TM    EDFSTAT,EDFSTRCV                                                 
         BZ    PL120                                                            
         LH    R1,DLVTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,DLVTOTAL                                                      
         B     PL200                                                            
*                                                                               
PL120    LH    R1,MISTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,MISTOTAL                                                      
*                                                                               
PL200    LH    R1,CLTTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,CLTTOTAL                                                      
         LH    R1,PRDTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,PRDTOTAL                                                      
*                                                                               
         BRAS  RE,MYSPOOL                                                       
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         MVC   SVAGY,NEWAGY                                                     
         XC    NEWAGY,NEWAGY                                                    
         MVC   SVMED,NEWMED                                                     
         XC    NEWMED,NEWMED                                                    
         MVC   SVCLT,NEWCLT                                                     
         XC    NEWCLT,NEWCLT                                                    
         MVC   SVPRD,NEWPRD                                                     
         XC    NEWPRD,NEWPRD                                                    
         MVC   SVREQST,NEWREQST                                                 
         XC    NEWREQST,NEWREQST                                                
         MVC   SVPTN,NEWPTN                                                     
         XC    NEWPTN,NEWPTN                                                    
         MVC   SVTYPE,NEWTYPE                                                   
         XC    NEWTYPE,NEWTYPE                                                  
         MVC   SVPRDHD,PRDHEAD                                                  
         XC    PRDHEAD,PRDHEAD                                                  
         MVC   SVPRBIT,PRBIT                                                    
         MVI   PRBIT,0                                                          
         MVI   FIRSTLIN,C'N'                                                    
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*        GETERR - IF ANY ERROR, MOVE ERROR TEXT INTO THE PRINT LINE             
***********************************************************************         
*NTRY:   R1=A(PRINT LINE CL24 ERROR MSG FIELD)                                  
*                                                                               
         USING EDFILD,R3                                                        
GETERR   NTR1                                                                   
         OC    EDFEZERR,EDFEZERR                                                
         BZ    EXIT                                                             
*                                                                               
         L     RF,=A(EZMSGTAB)     EASYLINK ERROR TABLE                         
GER10    CLI   0(RF),X'FF'                                                      
         BNE   *+20                                                             
         MVC   0(14,R1),=CL14'FAX ERROR #'  NOT IN TABLE                        
         MVC   14(4,R1),EDFEZERR            SO DISPLAY NUMBER                   
         B     EXIT                                                             
*                                                                               
         CLC   EDFEZERR,0(RF)      MATCH ON CODE                                
         BE    GER20                                                            
         AHI   RF,28                                                            
         B     GER10                                                            
GER20    MVC   0(24,R1),4(RF)      DISPLAY TEXT                                 
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        SPOOL OUT A LINE                                                       
***********************************************************************         
*                                                                               
MYSPOOL  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT PRODUCT/PARTNER NAMES                                             
*        IN=PRDCODE   OUT=PRDNAME                                               
***********************************************************************         
*                                                                               
GETPRDNM NTR1                                                                   
         XC    PRDNAME,PRDNAME                                                  
         CLC   PRDCODE,SPACES                                                   
         BNH   EXIT                                                             
         XC    KEY,KEY             READ PRODUCT RECORD                          
         LA    R6,KEY                                                           
         USING SP$PKEY,R6                                                       
         MVC   SP$PKEYAM,AMCODE                                                 
         MVC   SP$PKEYCLT,CLTCODE                                               
         MVC   SP$PKEYPRD,PRDCODE                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING SP$PRDHDR,R6                                                     
         MVC   PRDNAME,SP$PNAME                                                 
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SPOT FLIGHT DATES FROM THE ESTIMATE REC                                
*        IN=PRDCODE,BINEST   OUT=INDATES                                        
***********************************************************************         
*                                                                               
SPESTDT  NTR1                                                                   
         XC    INDATES,INDATES                                                  
         XC    KEY,KEY                                                          
         MVC   LKEY,=H'13'                                                      
         LA    R6,KEY                                                           
         USING EKEY,R6                                                          
         MVC   EKEYAM,AMCODE                                                    
         MVC   EKEYCLT,CLTCODE                                                  
         MVC   EKEYPRD,PRDCODE                                                  
         MVC   EKEYEST,BINEST      BINARY ESTIMATE                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         MVC   INDATES,ESTART                                                   
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SPOT BUYER NAME                                                        
*        IN=BYRCODE   OUT=BUYRNAME                                              
***********************************************************************         
*                                                                               
SPBUYER  NTR1                                                                   
         XC    BUYRNAME,BUYRNAME                                                
         CLC   BYRCODE,SPACES                                                   
         BNH   EXIT                                                             
         XC    KEY,KEY             GET BUYER FULL NAME                          
         LA    R6,KEY                                                           
         USING BYRKEY,R6                                                        
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAM,AMCODE                                                    
         MVC   BYRKBYR,BYRCODE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   EXIT                                                             
         USING BYRDSCD,R6                                                       
         MVC   BUYRNAME,BYRFNAME                                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS INVOICE CONTROL REPORT HEADSPECS AND HEADHOOK                
***********************************************************************         
SVHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
SVHDHK   NTR1                                                                   
         MVC   H1(33),=C'NV STATION INVOICE CONTROL REPORT'                     
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H7(7),=C'CLIENT '                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+35(5),=C'STATN'                                               
         MVC   H8+35(5),=C'-----'                                               
         MVC   H7+45(11),=C'DESTINATION'                                        
         MVC   H8+45(11),=C'-----------'                                        
         MVC   H7+75(8),=C'  REF#  '                                            
         MVC   H8+75(8),=C'--------'                                            
         MVC   H7+85(4),=C'SENT'                                                
         MVC   H8+85(5),=C'-----'                                               
         MVC   H7+92(4),=C'DLVD'                                                
         MVC   H8+92(5),=C'-----'                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT DARE   HEADSPECS AND HEADHOOK                                     
***********************************************************************         
ADHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
ADHDHOOK NTR1                                                                   
         MVC   H1(4),=C'DARE'                                                   
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(5),=C'BUYER'                                                  
         MVC   H3+15(3),BYRCODE                                                 
         MVC   H3+20(20),BUYRNAME                                               
         MVC   H7(8),=C'REP/DEST'                                               
         MVC   H8(8),=C'--------'                                               
         MVC   H7+9(8),=C'STATION '                                             
         MVC   H8+9(8),=C'--------'                                             
         MVC   H7+36(3),=C'SP '                                                 
         MVC   H8+36(3),=C'---'                                                 
         MVC   H7+41(17),=C'M CLT PRD-PTN EST'                                  
         MVC   H8+41(17),=C'- --- ------- ---'                                  
         MVC   H7+60(8),=C' ORDER# '                                            
         MVC   H8+60(8),=C'--------'                                            
         MVC   H7+70(8),=C'CONTRCT#'                                            
         MVC   H8+70(8),=C'--------'                                            
         MVC   H7+80(4),=C'TYPE'                                                
         MVC   H8+80(4),=C'----'                                                
         MVC   H7+88(4),=C'SENT'                                                
         MVC   H8+88(5),=C'-----'                                               
         MVC   H7+95(4),=C'DLVD'                                                
         MVC   H8+95(5),=C'-----'                                               
         MVC   H7+102(4),=C'CANX'                                               
         MVC   H8+102(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        EXITS AND STUFF                                                        
***********************************************************************         
*                                                                               
DIE      DC    H'0'                                                             
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
RELO     DS    F                                                                
*                                                                               
         GETEL R6,DATADISP,ELCODE        USED FOR GETEL OPERATIONS              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DMOPEN FILE LISTS                                                      
***********************************************************************         
*                                                                               
OPENADDS DS    0F                                                               
         DC    C'CONTROL ',A(CTFLIST)                                           
         DC    C'SPOT    ',A(SPFLIST)                                           
         DC    C'REP     ',A(REPFLIST)                                          
         DC    C'PRINT   ',A(PRFLIST)                                           
         DC    C'NET     ',A(SPFLIST)                                           
         DC    C'ACCOUNT ',A(ACFLIST)                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
SPFLIST  DS    0F                                                               
         DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
REPFLIST DS    0F                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
ACFLIST  DS    0F                                                               
         DC    CL8'NACCFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
EMULIST  DS    0F                                                               
         DC    CL8'NACCDIR'                                                     
         DC    CL8'NACCMST'                                                     
         DC    CL8'NACCARC'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
PRFLIST  DS    0F                                                               
         DC    CL8'NPRTDIR'                                                     
         DC    CL8'NPRTFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NPUBDIR'                                                     
         DC    CL8'NPUBFILE'                                                    
         DC    C'X'                                                             
         SPACE 1                                                                
CTFLIST  DS    0F                                                               
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
*        CONSOLIDATED REPORTS FOR REP SYSTEM TABLE                              
***********************************************************************         
*                                                                               
CONTABLE DS    0CL4                                                             
*        DC    C'BL',X'0050'       BLRNY                                        
         DC    C'B1',X'05AC'       TELNY                                        
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        CONSOLIDATED REPORTS FOR ADV SYSTEM TABLE                              
***********************************************************************         
*                                                                               
ADVCONTB DS    0CL4                                                             
         DC    C'JW',X'0016'                                                    
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        SPOT HEADS TABLE                                                       
***********************************************************************         
*                                                                               
SPHDTAB  DS    0CL10                                                            
         DC    C'SO',AL4(SOHDSPEC,SOHDHOOK)                                     
         DC    C'SA',AL4(SAHDSPEC,SAHDHOOK)                                     
         DC    C'SD',AL4(SDHDSPEC,SDHDHOOK)                                     
         DC    C'SG',AL4(SGHDSPEC,SGHDHOOK)                                     
         DC    C'SX',AL4(SXHDSPEC,SXHDHOOK)                                     
         DC    C'SY',AL4(SYHDSPEC,SYHDHOOK)                                     
         DC    C'SR',AL4(SRHDSPEC,SYHDHOOK)                                     
         DC    C'SV',AL4(SVHDSPEC,SVHDHK)                                       
         DC    C'ST',AL4(STHDSPEC,STHDHOOK)                                     
         DC    C'SU',AL4(STHDSPEC,STHDHOOK)                                     
         DC    C'SI',AL4(STHDSPEC,STHDHOOK)                                     
         DC    C'SC',AL4(STHDSPEC,STHDHOOK)                                     
         DC    C'SS',AL4(STHDSPEC,STHDHOOK)                                     
         DC    C'SF',AL4(STHDSPEC,STHDHOOK)                                     
         DC    C'SM',AL4(STHDSPEC,STHDHOOK)                                     
         DC    C'DO',AL4(ADHDSPEC,ADHDHOOK)                                     
         DC    C'DE',AL4(ADHDSPEC,ADHDHOOK)                                     
         DC    C'DN',AL4(ADHDSPEC,ADHDHOOK)                                     
         DC    C'DR',AL4(ADHDSPEC,ADHDHOOK)                                     
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        SORT ROUTINE TABLE                                                     
***********************************************************************         
*                                                                               
SORTTAB  DS    0CL6                REPORT SYS-TYPE / SORT ROUTINE               
         DC    C' Z',AL4(SRGENFAX)                                              
         DC    C'PI',AL4(SRPRINT)                                               
         DC    C'PP',AL4(SRPRINT)                                               
         DC    C'PV',AL4(SRPRINT)                                               
         DC    C'SO',AL4(SRSO)                                                  
         DC    C'SA',AL4(SRSA)                                                  
         DC    C'SX',AL4(SRSX)                                                  
         DC    C'SY',AL4(SRSX)                                                  
         DC    C'SR',AL4(SRSX)                                                  
         DC    C'SV',AL4(SRSV)                                                  
         DC    C'SD',AL4(SRSPDMB)                                               
         DC    C'SG',AL4(SRSPDMB)                                               
         DC    C'ST',AL4(SRSPTRA)                                               
         DC    C'SU',AL4(SRSPTRA)                                               
         DC    C'SI',AL4(SRSPTRA)                                               
         DC    C'SC',AL4(SRSPTRA)                                               
         DC    C'SS',AL4(SRSPTRA)                                               
         DC    C'SF',AL4(SRSPTRA)                                               
         DC    C'SM',AL4(SRSPTRA)                                               
         DC    C'NN',AL4(SRNETRA)                                               
         DC    C'NC',AL4(SRNETRA)                                               
         DC    C'NR',AL4(SRNEPAK)                                               
         DC    C'NV',AL4(SRNETDL)                                               
         DC    C'RO',AL4(SRREP)                                                 
         DC    C'RK',AL4(SRREP)                                                 
         DC    C'RC',AL4(SRREP)                                                 
         DC    C'RD',AL4(SRREPD)                                                
         DC    C'AO',AL4(SRACC)                                                 
         DC    C'DO',AL4(SRDAREA)                                               
         DC    C'DE',AL4(SRDAREA)                                               
         DC    C'DN',AL4(SRDAREA)                                               
         DC    C'DR',AL4(SRDARER)                                               
         DC    C'DA',AL4(SRDARER)                                               
         DC    C'DC',AL4(SRDARER)                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        SORT STUFF                                                             
***********************************************************************         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,XXX,A),FORMAT=CH,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XXXX'                                  
         SPACE 3                                                                
***********************************************************************         
*        PRINT QUEUE REPORT ID TABLE                                            
***********************************************************************         
*                                                                               
REPTABLE DS    0CL13                                                            
*              REP SYS-TYP,SYSTEM,SYSTEM NUMBER,JOB ID                          
*                                                                               
         DC    C'NN',C'NETWORK',X'03',C'NTR'                                    
         DC    C'NC',C'NETWORK',X'03',C'NTR'                                    
         DC    C'NP',C'NETWORK',X'03',C'NTR'                                    
         DC    C'NR',C'NETWORK',X'03',C'NTR'                                    
         DC    C'NV',C'NETWORK',X'03',C'NTR'                                    
         DC    C'NZ',C'NETWORK',X'03',C'NTR'                                    
         DC    C'PI',C'PRINT  ',X'04',C'PTR'                                    
         DC    C'PV',C'PRINT  ',X'04',C'PTR'                                    
         DC    C'PP',C'PRINT  ',X'04',C'PTR'                                    
         DC    C'PZ',C'PRINT  ',X'04',C'PTR'                                    
         DC    C'SZ',C'SPOT   ',X'02',C'STR'                                    
         DC    C' Z',C'SPOT   ',X'02',C'STR'     ***                            
         DC    C'ST',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SU',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SI',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SF',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SC',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SM',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SS',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SA',C'SPOT   ',X'02',C'ATR'                                    
         DC    C'SO',C'SPOT   ',X'02',C'ATR'                                    
         DC    C'SX',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SY',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SR',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SV',C'SPOT   ',X'02',C'STR'                                    
         DC    C'SD',C'SPOT   ',X'02',C'ATR'                                    
         DC    C'SG',C'SPOT   ',X'02',C'ATR'                                    
         DC    C'RO',C'REP    ',X'08',C'RTR'                                    
         DC    C'RK',C'REP    ',X'08',C'RTR'                                    
         DC    C'RC',C'REP    ',X'08',C'RTR'                                    
         DC    C'RD',C'REP    ',X'08',C'ETR'                                    
         DC    C'RZ',C'REP    ',X'08',C'RTR'                                    
         DC    C'AO',C'ACC    ',X'06',C'ATA'                                    
         DC    C'AZ',C'ACC    ',X'06',C'ATA'                                    
         DC    C'CZ',C'CONTROL',X'00',C'CTR'                                    
         DC    C'TZ',C'TALENT ',X'00',C'TTR'                                    
         DC    C'DO',C'SPOT   ',X'02',C'DTR'                                    
         DC    C'DR',C'REP    ',X'08',C'DTR'                                    
         DC    C'DA',C'REP    ',X'08',C'DTR'                                    
         DC    C'DC',C'REP    ',X'08',C'DTR'                                    
         DC    C'DE',C'SPOT   ',X'02',C'DTR'                                    
         DC    C'DN',C'SPOT   ',X'02',C'DTR'                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        STUFF                                                                  
***********************************************************************         
*                                                                               
         DS    0D                                                               
EDICTBLK DS    18432X'00'                                                       
*DICTBLK DS    14336X'00'                                                       
         EJECT                                                                  
***********************************************************************         
*        AGENCY NAME, ADDRESS AND POWER CODE                                    
*        IN=IDFORM   OUT=PQDESTID,POWCODE,ALNAME,ALCITY                         
***********************************************************************         
*                                                                               
         USING SRECD,R4                                                         
GETAGYNM NMOD1 0,**GTSE**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
*                                                                               
         MVC   DATADISP,=H'28'     FIRST ELEMENT IN CONTROL FILE                
         MVC   LKEY,=H'25'                                                      
         XC    PQDESTID,PQDESTID                                                
         XC    POWCODE,POWCODE                                                  
         XC    ALCITY,ALCITY                                                    
         XC    ALNAME,ALNAME                                                    
*                                                                               
         USING CTIKEY,R6                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE 'I'                              
         MVC   CTIKID,IDFORM                                                    
GETAGY10 GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO                                                           
         CLC   CTIKEY(25),KEY                                                   
         BNE   NO                  BETTER BE THERE                              
*                                                                               
         MVI   ELCODE,X'02'        GET ID NUM                                   
         BRAS  RE,GETEL                                                         
         BNE   NO                                                               
         MVC   PQDESTID,2(6)                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        GET POWER CODE                               
         BRAS  RE,GETEL                                                         
         BNE   NO                                                               
         MVC   POWCODE,2(R6)                                                    
*                                                                               
         USING CTORGD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'36'        GET ID NAME AND ADDRESS                      
         BRAS  RE,GETEL                                                         
         BNE   GETAGYX                                                          
         MVC   ALNAME,CTORGNAM                                                  
         MVC   SVAGYNM,CTORGNAM                                                 
*                                                                               
         USING CTDSTD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        GET ID NAME AND CITY                         
         BRAS  RE,GETEL                                                         
         BNE   GETAGYX                                                          
         MVC   ALCITY,CTDSTLG2                                                  
*                                                                               
GETAGYX  B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SE NUMBERS  OUT=SELIST                                                 
***********************************************************************         
*                                                                               
         USING SRECD,R4                                                         
GETSES   NMOD1 0,**GTSE**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
         MVC   DATADISP,=H'28'     FIRST ELEMENT IN CONTROL FILE                
         MVC   LKEY,=H'25'                                                      
*                                                                               
         USING CT5REC,R6                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO                                                           
         CLC   CT5KEY(25),KEY                                                   
         BNE   NO                                                               
*                                                                               
         USING CTSYSD,R6                                                        
         MVI   ELCODE,X'21'        GET ID NAME AND ADDRESS                      
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GETSESNX BRAS  RE,NEXTEL                                                        
         BNE   GETSESX                                                          
*                                                                               
         CLI   CTSYSNUM,X'03'      NET??                                        
         BNE   *+14                                                             
         MVC   SELIST(1),CTSYSSE                                                
         B     GETSESNX                                                         
*                                                                               
         CLI   CTSYSNUM,X'04'      PRINT??                                      
         BNE   *+14                                                             
         MVC   SELIST+1(1),CTSYSSE                                              
         B     GETSESNX                                                         
*                                                                               
         CLI   CTSYSNUM,X'02'      SPOT??                                       
         BNE   *+14                                                             
         MVC   SELIST+2(1),CTSYSSE                                              
         B     GETSESNX                                                         
*                                                                               
         CLI   CTSYSNUM,X'08'      REP?                                         
         BNE   *+14                                                             
         MVC   SELIST+3(1),CTSYSSE                                              
         B     GETSESNX                                                         
*                                                                               
         CLI   CTSYSNUM,X'06'      ACC?                                         
         BNE   GETSESNX                                                         
         MVC   SELIST+4(1),CTSYSSE                                              
         B     GETSESNX                                                         
GETSESX  B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        NEW PRINT QUEUE                                                        
***********************************************************************         
*                                                                               
NEWQ     NMOD1 0,**NEWQ**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
*                                                                               
         USING SRECD,R4                                                         
         LA    R4,SREC                                                          
         USING EDFILD,R3                                                        
         LA    R3,SREDIREC                                                      
*                                                                               
         CLI   REPEDI,C'Y'                                                      
         BNE   *+8                                                              
         MVI   RPTCLASS,C'A'       REP WANTS CLASS A REPORTS                    
*                                                                               
         CLI   FIRSTIME,C'Y'                                                    
         BE    *+12                                                             
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         L     R1,VREMOT                                                        
         USING REMOTED,R1                                                       
         MVI   REMOTCPY,1                                                       
         MVC   REMOTCLS,RPTCLASS                                                
*                                                                               
         CLI   KHDR,C'Y'           KATZ                                         
         BE    NEWQ20                                                           
         BRAS  RE,GETRTYPE         GET REPORT TYPE AND ID                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   REMOTJID,REPJID                                                  
         MVC   REMOTSYS,REPSYSN                                                 
         MVC   REMOTDST,PQDESTID                                                
*                                                                               
         MVI   REMOTRET,C' '       RETAIN CLASS DEFAULT = 21/13                 
*                                                                               
         CLC   EDFPQUID,=H'7138'   CARAM  - RETAIN ALL RPTS 48/24               
         BE    NEWQ05                                                           
         CLC   EDFPQUID,=H'1376'   OMNYT                                        
         BE    NEWQ05                                                           
*                                                                               
         CLC   =C'NTR',REMOTJID    FOR NTR REPORTS                              
         BNE   NEWQ10                                                           
         CLC   EDFPQUID,=H'1192'   DFNYT                                        
         BE    NEWQ05                                                           
         CLC   EDFPQUID,=H'4165'   DFZNY                                        
         BE    NEWQ05                                                           
         CLC   EDFPQUID,=H'5070'   ZENYT                                        
         BNE   NEWQ10                                                           
NEWQ05   MVI   REMOTRET,C'U'       RETAIN CLASS U = 48 HR ACT /24 DEAD          
*                                                                               
NEWQ10   CLC   EDFPQUID,=X'0CD2'    DESTINE BWCNY STR AND NTR TO DNNYT          
         BNE   NEWQ30                                                           
         CLC   REMOTJID,=C'STR'                                                 
         BE    *+14                                                             
         CLC   REMOTJID,=C'NTR'                                                 
         BNE   NEWQ30                                                           
         MVC   REMOTDST,=X'0507'   DNNYT                                        
         B     NEWQ30                                                           
*                                                                               
NEWQ20   MVC   REMOTJID,=C'ATR'    STUPID KATZ STUFF                            
         MVI   REMOTSYS,X'02'                                                   
         MVC   REMOTDST,PQDESTID                                                
*                                                                               
NEWQ30   LA    R2,PQOVTAB          CHECK FOR OVERRIDE PRINT QUEUE ID            
NEWQ40   OC    0(4,R2),0(R2)                                                    
         BZ    NEWQ100                                                          
         CLC   PQDESTID,0(R2)      MATCH FIRST ID                               
         BE    NEWQ50                                                           
         LA    R2,4(R2)                                                         
         B     NEWQ40                                                           
NEWQ50   MVC   REMOTDST,2(R2)      REPLACE WITH SECOND ID                       
*                                                                               
NEWQ100  GOTO1 OPENPQ                                                           
         MVI   FIRSTIME,C'N'                                                    
         MVI   NEWQUE,C'N'                                                      
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        GET REPORT TYPE AND SYSTEM NUMBER                                      
***********************************************************************         
*                                                                               
GETRTYPE NTR1                                                                   
         LA    R3,SREDIREC                                                      
         USING EDFILD,R3                                                        
         LA    R2,REPTABLE                                                      
*                                                                               
GETR3    CLC   EDFSYS(2),0(R2)          FIND MATCHING REPORT TYPE               
         BE    GETR5                                                            
         LA    R2,L'REPTABLE(R2)   BUMP TO NEXT ROW                             
         CLI   0(R2),X'FF'                                                      
         BNE   GETR3                                                            
         B     NO                                                               
*                                                                               
GETR5    MVC   REPSYSN,9(R2)       REPORT TYPE SYSTEM NAME                      
         MVC   REPJID,10(R2)       REPORT TYPE JOB ID                           
GETRX    B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SPOT READS                                                             
***********************************************************************         
*                                                                               
SPOTREAD NMOD1 0,**SPRD**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
         USING SRECD,R4                                                         
         USING EDFILD,R3                                                        
         LA    R3,SREDIREC                                                      
*                                                                               
         MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
*                                                                               
         BRAS  RE,SPHEADS          HEADSPECS                                    
         EJECT                                                                  
***********************************************************************         
*        MEDIA AND CLIENT NAME                                                  
***********************************************************************         
*                                                                               
         MVC   MEDCODE,SRMED                                                    
         MVC   NEWMED,SRMED                                                     
         MVC   NEWCLT,SRCLT                                                     
         MVC   CLCLT,SRCLT                                                      
*                                                                               
         CLI   EDFSYS,EDFDARRQ     DARE FOR ADV                                 
         BNE   SPR10                                                            
         MVC   MEDCODE,EDIRDRMD    MEDIA                                        
         MVC   NEWMED,EDIRDRMD                                                  
         MVC   CLCLT,EDIRDRCL                                                   
         MVC   NEWCLT,SRDABYR      ACTUALLY BREAKING ON BUYER CODE              
         OI    PRBIT,PRBYRTOT      PRINT BUYER TOTAL                            
         B     SPR50                                                            
*                                                                               
SPR10    CLI   SRSCODE,C'A'        ADDS?                                        
         BNE   SPR50                                                            
         CLI   SRSATYPE,EDFTXCPQ   CONFIRMATION OF PURCHASE?                    
         BE    SPR12               NOW HAS CLIENT LOWER                         
         CLI   SRSATYPE,EDFTSRYQ   SY                                           
         BE    SPR12               NOW HAS CLIENT LOWER                         
         CLI   SRSATYPE,EDFTSRXQ   SR                                           
         BNE   SPR15               NOW HAS CLIENT LOWER                         
SPR12    MVC   NEWCLT,SRSXCLT                                                   
         MVC   CLCLT,SRSXCLT                                                    
         MVC   NEWREQST,SRCLT      REQUESTOR IS HIGH                            
         OI    PRBIT,PRREQTOT                                                   
         B     SPR50                                                            
*                                                                               
SPR15    CLI   SRSATYPE,EDFTINVQ   INVOICE REPORT                               
         BNE   SPR50               NOW HAS CLIENT LOWER                         
         MVC   NEWCLT,SRSVCLT                                                   
         MVC   CLCLT,SRSVCLT                                                    
         OI    PRBIT,PRNOCLT+PRNOPRD                                            
*                                                                               
SPR50    GOTO1 =A(SPAGYCLT),DMCB,(RC),RR=RELO                                   
*                                                                               
         CLI   EDFSYS,EDFSNETQ     THAT'S ALL FOR NET SYSTEM                    
         BE    EXIT                                                             
         CLI   EDFSYS,EDFDARRQ     DARE FOR ADV                                 
         BE    SPR500                                                           
         CLI   SRSCODE,C'A'        ADDS?                                        
         BE    SPR400                                                           
         CLI   SRSCODE,C'T'        TRAFFIC?                                     
         BNE   EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT TRAFFIC - PRODUCT/PARTNER NAMES                                   
***********************************************************************         
*                                                                               
SPR300   MVC   NEWPTN,SRSTPTN                                                   
         XC    PTNCODE,PTNCODE                                                  
         XC    PTNNAME,PTNNAME                                                  
         MVC   PRDCODE,SRSTPTN                                                  
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
         MVC   PTNCODE,SRSTPTN                                                  
         MVC   PTNNAME,PRDNAME                                                  
         XC    PRDCODE,PRDCODE                                                  
         XC    PRDNAME,PRDNAME                                                  
         MVC   NEWPRD,SRSTPRD                                                   
         MVC   PRDCODE,SRSTPRD                                                  
         BRAS  RE,GETPRDNM         PARTNER NAME                                 
         EJECT                                                                  
***********************************************************************         
*        SPOT TRAFFIC - MARKET NAME                                             
***********************************************************************         
*                                                                               
         CLC   EDFPQSUB,=C'TWX'                                                 
         BE    SPR310                                                           
         CLC   EDFPQSUB,=C'SWX'                                                 
         BE    SPR310                                                           
         CLC   EDFPQSUB,=C'AWX'                                                 
         BE    SPR310                                                           
         CLC   EDFPQSUB,=C'LET'                                                 
         BNE   SPR320                                                           
SPR310   MVC   STACALL,EDISTTST                                                 
         GOTO1 =A(SPMKTRTG),DMCB,(RC),RR=RELO   MARKET NAME                     
         MVC   INDATES,EDISTTDT    FORMAT DATES                                 
         BRAS  RE,DATEFORM                                                      
         MVC   CONTACT,EDISTTCT    CONTACT                                      
         B     SPR390                                                           
*                                                                               
***********************************************************************         
*        SPOT TRAFFIC - HOUSE                                                   
***********************************************************************         
*                                                                               
SPR320   CLI   SRSTTYPE,EDFTCOVQ   SPTRAFFIC COVER LETTER                       
         BE    *+12                                                             
         CLI   SRSTTYPE,EDFTSPMQ   SPTRAFFIC TYPE M                             
         BNE   SPR322                                                           
         MVC   HOUSE,EDISTCHS      GET HOUSE                                    
         GOTO1 =A(GETHOUSE),RR=RELO                                             
         MVC   CONTACT,EDISTCCT    GET CONTACT                                  
         MVC   INDATES,EDISTCDT    FORMAT DATES                                 
         BRAS  RE,DATEFORM                                                      
         B     SPR390                                                           
*                                                                               
SPR322   MVC   HOUSE,EDISTSHS      SPTRAFFIC SHIPPING ORDERS                    
         GOTO1 =A(GETHOUSE),RR=RELO                                             
         MVC   CONTACT,EDISTSCT    CONTACT                                      
         B     SPR390                                                           
         EJECT                                                                  
***********************************************************************         
*        SPOT TRAFFIC - PRINT LINE                                              
***********************************************************************         
*                                                                               
SPR390   MVC   PSTTYPE,EDFPQSUB                                                 
         MVC   PSTDEST(16),EDFDEST                                              
         CLC   EDFPQSUB,=C'TWX'                                                 
         BE    SPR392                                                           
         CLC   EDFPQSUB,=C'SWX'                                                 
         BE    SPR392                                                           
         CLC   EDFPQSUB,=C'AWX'                                                 
         BE    SPR392                                                           
         CLC   EDFPQSUB,=C'LET'                                                 
         BNE   *+14                                                             
SPR392   MVC   PSTDEST+19(24),MKNAME                                            
         B     *+10                                                             
         MVC   PSTDEST+19(24),HOUSENM                                           
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PSTFNUM,EDFEZRLN    PRINT FAX TO #                               
*                                                                               
         LA    R1,PSTERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         CLI   SRSTTYPE,EDFTSHIQ   SHIPPING ORDERS = NO ESTIMATE                
         BE    *+10                                                             
         MVC   PSTEST,EDISTTES                                                  
         MVC   PSTDATES,DATES                                                   
         MVC   PSTCONT,CONTACT                                                  
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PSTEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PSTEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PSTSENT,SENTIME                                                  
         MVC   PSTRCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'Y'                                                     
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS                                                              
***********************************************************************         
*                                                                               
SPR400   MVC   NEWTYPE,SRSATYPE                                                 
         CLC   NEWTYPE,SVTYPE                                                   
         BE    *+8                                                              
         MVI   TOT,C'Y'                                                         
         CLI   SRSATYPE,EDFTADDQ   DRAFT ORDERS OR AVAILS???                    
         BE    SPR410              ORDERS                                       
         CLI   SRSATYPE,EDFTREQQ                                                
         BE    SPR430              AVAILS                                       
         CLI   SRSATYPE,EDFTGOAQ                                                
         BE    SPR450              DMBDE GOAL TRANSFERS                         
         CLI   SRSATYPE,EDFTDMBQ                                                
         BE    SPR460              DMBDE SPECIAL TRANSMISSION                   
*                                                                               
***********************************************************************         
*        SPOT ADDS INVOICE REPORT                                               
***********************************************************************         
*                                                                               
         CLI   SRSATYPE,EDFTINVQ   INVOICE REPORT                               
         BNE   SPR410              DOESN'T HAVE PRODUCT                         
*                                                                               
         MVC   PSVCLT,SRSVCLT      PRINT LINE                                   
         MVI   PSVHYPH,C'-'                                                     
         MVC   PSVCLTNM,CLTNAME                                                 
         MVC   PSVDEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PSVFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PSVERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PSVSTN,SPNVSTA                                                   
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PSVEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PSVEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PSVSENT,SENTIME                                                  
         MVC   PSVRCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS DRAFT ORDERS/CONFIRMATION OF PURCHASE                        
***********************************************************************         
*                                                                               
SPR410   CLC   SRSAPRD,SPACES                                                   
         BNH   SPR416                                                           
         MVC   PRDCODE,SRSAPRD                                                  
*                                                                               
         CLI   SRSATYPE,EDFTXCPQ   CONFIRMATION OF PURCHASE?                    
         BE    SPR411              NOW HAS PRODUCT LOWER                        
         CLI   SRSATYPE,EDFTSRYQ   RY                                           
         BE    SPR411              NOW HAS PRODUCT LOWER                        
         CLI   SRSATYPE,EDFTSRXQ   RR                                           
         BNE   SPR412              NOW HAS PRODUCT LOWER                        
SPR411   CLC   SRSXPRD,SPACES                                                   
         BNH   SPR416                                                           
         MVC   PRDCODE,SRSXPRD                                                  
SPR412   BRAS  RE,GETPRDNM         PRODUCT NAME                                 
*                                                                               
SPR416   MVC   STACALL,SPEDSTA                                                  
         MVC   STACALL+4(1),MEDCODE                                             
         MVC   MKTNUM,SPEDMKT                                                   
         MVC   CLCLT,SPEDCLT                                                    
         GOTO1 =A(SPMKTRTG),DMCB,(RC),RR=RELO   MARKET AND RATING               
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS CONFIRMATION OF PURCHASE - PRINT LINE                        
***********************************************************************         
*                                                                               
         CLI   SRSATYPE,EDFTXCPQ   CONFIRMATION OF PURCHASE?                    
         BE    SPR417                                                           
         CLI   SRSATYPE,EDFTSRYQ   SY                                           
         BE    SPR417                                                           
         CLI   SRSATYPE,EDFTSRXQ   SR                                           
         BNE   SPR418                                                           
SPR417   MVC   PSXCLT,SRSXCLT                                                   
         MVI   PSXHYPH,C'-'                                                     
         MVC   PSXCLTNM,CLTNAME                                                 
         MVC   PSXPRD,SRSXPRD                                                   
         MVI   PSXHYPH2,C'-'                                                    
         MVC   PSXPRDNM,PRDNAME                                                 
         MVC   PSXEST,SPCPEST                                                   
         CLI   SRSATYPE,EDFTXCPQ   CONFIRMATION OF PURCHASE?                    
         BNE   SPR417RY            BRANCH IF SR OR SY                           
         MVC   PSXDEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PSXFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PSXERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PSXSTN,SPCPSTA                                                   
         MVC   PSXMARKT,RSMKT                                                   
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PSXEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PSXEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PSXSENT,SENTIME                                                  
         MVC   PSXRCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
SPR417RY MVC   PSYDATES,SPCDRUSR+15                                             
         MVC   PSYDEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PSYFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PSYERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PSYSTN,SPCPSTA                                                   
         MVC   PSYMARKT,RSMKT                                                   
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PSYEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PSYEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PSYSENT,SENTIME                                                  
         MVC   PSYRCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS DRAFT ORDERS - PRINT LINE                                    
***********************************************************************         
*                                                                               
SPR418   MVC   PSOPRD,SRSAPRD                                                   
         MVI   PSOHYPH,C'-'                                                     
         MVC   PSOPRDNM,PRDNAME                                                 
         MVC   PSOEST,SPEDEST                                                   
         PACK  DUB2(8),SPEDEST(3)                                               
         CVB   R1,DUB2                                                          
         STC   R1,BINEST                                                        
         MVC   INDATES,SPEDFLST                                                 
         CLC   INDATES(3),SPACES                                                
         BNE   *+8                                                              
         BRAS  RE,SPESTDT                                                       
         BRAS  RE,DATEFORM                                                      
         MVC   PSODATES,DATES                                                   
         MVC   PSODEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PSOFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PSOERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PSOBUYER,SPEDBYR                                                 
         MVC   PSOCAMPN,SPEDCAM                                                 
         MVC   PSOSTN,SPEDSTA                                                   
         MVC   PSOMARKT,RSMKT                                                   
         MVC   PSOREQ,SPEDQUES                                                  
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PSOEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PSOEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PSOSENT,SENTIME                                                  
         MVC   PSORCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS AVAIL REQUESTS                                               
***********************************************************************         
*                                                                               
SPR430   CLC   SRSAPRD,SPACES                                                   
         BE    SPR432                                                           
         MVC   PRDCODE,SRSAPRD                                                  
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
SPR432   MVC   BYRCODE,SPAVBUYR                                                 
         BRAS  RE,SPBUYER          GET BUYER NAME                               
*                                                                               
         MVC   PSAPRD,SRSAPRD      PRINT LINE                                   
         MVI   PSAHYPH,C'-'                                                     
         MVC   PSAPRDNM,PRDNAME                                                 
         MVC   PSAEST,SPAVEST                                                   
         MVC   PSADATES,SPAVFLST                                                
         MVC   PSADEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PSAFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PSAERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PSAREFN,SPAVREFN                                                 
         MVC   PSABUYER,BUYRNAME                                                
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PSAEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PSAEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PSASENT,SENTIME                                                  
         MVC   PSARCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS DMBDE GOAL TRANSFERS                                         
***********************************************************************         
*                                                                               
SPR450   MVC   PRDNAME,SPACES                                                   
         CLC   SRSAPRD,SPACES                                                   
         BNH   *+14                                                             
         MVC   PRDCODE,SRSAPRD                                                  
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
*                                                                               
         MVC   PSGPRD,SPGTPRD      PRINT LINE                                   
         MVI   PSGHYPH,C'-'                                                     
         MVC   PSGPRDNM,PRDNAME                                                 
         MVC   PSGEST,SPGTEST                                                   
         MVC   PSGDEST,EDFDEST                                                  
*                                                                               
         EDIT  (B4,EDFEZLED),(5,PSDFTP)                                         
         BRAS  RE,TIMEFORM                                                      
         MVC   PSGSENT,SENTIME                                                  
         MVC   PSGRCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS DMBDE SPECIAL TRANSMISSIONS                                  
***********************************************************************         
*                                                                               
SPR460   MVC   PRDNAME,SPACES                                                   
         CLC   SRSAPRD,SPACES                                                   
         BNH   *+14                                                             
         MVC   PRDCODE,SRSAPRD                                                  
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
*                                                                               
         MVC   PSDPRD,SPDAPRD      PRINT LINE                                   
         MVI   PSDHYPH,C'-'                                                     
         MVC   PSDPRDNM,PRDNAME                                                 
         MVC   PSDEST,SPDAEST                                                   
         MVC   PSDDEST,EDFDEST                                                  
*                                                                               
         MVC   PSDBUYER,SPDABYR                                                 
         MVC   PSDCAMPN,SPDACAM                                                 
         MVC   PSDMARKT,SPDAMKT                                                 
         MVC   PSDSTATN,SPDASTA                                                 
         EDIT  (B4,EDFEZLED),(5,PSDFTP)                                         
         BRAS  RE,TIMEFORM                                                      
         MVC   PSDSENT,SENTIME                                                  
         MVC   PSDRCVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT DARE                                                              
***********************************************************************         
*                                                                               
SPR500   MVC   BYRCODE,EDIRDRBY                                                 
         BRAS  RE,SPBUYER          GET BUYER NAME                               
         MVC   STACALL,EDIRDRST                                                 
         MVC   STACALL+4(1),EDIRDRMD                                            
         GOTO1 =A(SPMKTRTG),DMCB,(RC),RR=RELO   MARKET NAME                     
*                                                                               
         MVC   PRDAREP,SRDAREP     FILL IN PRINT LINE                           
         MVC   PRDASTN,EDIRDRST                                                 
         MVI   PRDAHYPH,C'-'                                                    
         MVC   PRDASTNM,MKNAME                                                  
         MVC   PRDASAL,EDIRDRSP                                                 
         MVC   PRDAMED,EDIRDRMD                                                 
         MVC   PRDACLT,EDIRDRCL                                                 
         MVC   PRDAPRD,EDIRDRP1                                                 
         CLC   EDIRDRP2,SPACES                                                  
         BNH   *+14                                                             
         MVI   PRDAHYP2,C'-'                                                    
         MVC   PRDAPTN,EDIRDRP2                                                 
         MVC   PRDAEST,EDIRDRES                                                 
         MVC   PRDAORD#,EDIRDRAN                                                
         MVC   PRDACON#,EDIRDRCN                                                
         MVC   PRDATYPE,=C'ORD'                                                 
         CLI   EDFTYPE,EDFTDORQ    ORDER                                        
         BE    SPR520                                                           
         MVC   PRDATYPE,=C'ERR'                                                 
         CLI   EDFTYPE,EDFTDERQ    ERROR                                        
         BE    SPR520                                                           
         MVC   PRDATYPE,=C'RCL'                                                 
         CLI   EDFTYPE,EDFTDRCQ    RECALL                                       
         BE    SPR520                                                           
         MVC   PRDATYPE,=C'NOT'                                                 
SPR520   BRAS  RE,TIMEFORM                                                      
         MVC   PRDASENT,SENTIME                                                 
         TM    EDFSTAT,EDFSTCAN                                                 
         BNZ   *+14                                                             
         MVC   PRDADLVD,RCVTIME                                                 
         B     *+10                                                             
         MVC   PRDACANX,RCVTIME                                                 
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT HEADSPECS AND HEADHOOKS                                           
***********************************************************************         
*                                                                               
         USING SRECD,R4                                                         
         USING EDFILD,R3                                                        
SPHEADS  NTR1                                                                   
         LA    R3,SREDIREC                                                      
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         LA    R2,SPHDTAB          SPOT HEAD SPECS TABLE                        
SPHD10   CLC   EDFSYS,0(R2)        SYSTEM                                       
         BNE   SPHD20                                                           
         CLC   EDFTYPE,1(R2)                                                    
         BE    SPHD50                                                           
SPHD20   LA    R2,L'SPHDTAB(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   SPHD10                                                           
         B     SPHDX                                                            
*                                                                               
SPHD50   ICM   R1,15,2(R2)                                                      
         ST    R1,SPECS                                                         
         ICM   R1,15,6(R2)                                                      
         ST    R1,HEADHOOK                                                      
SPHDX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT TRAFFIC HEADSPECS AND HEADHOOK                                    
***********************************************************************         
*                                                                               
STHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
STHDHOOK NTR1                                                                   
         MVC   H1(12),=C'SPOT TRAFFIC'                                          
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H4(9),=C'PRODUCT  '                                              
         MVC   H4+10(3),PRDCODE                                                 
         MVC   H4+14(20),PRDNAME                                                
         MVC   H5(9),=C'PARTNER  '                                              
         MVC   H5+10(3),PTNCODE                                                 
         MVC   H5+14(20),PTNNAME                                                
         MVC   H7(3),=C'TYP'                                                    
         MVC   H8(3),=C'---'                                                    
         MVC   H7+4(11),=C'DESTINATION'                                         
         MVC   H8+4(11),=C'-----------'                                         
         MVC   H7+34(12),=C'MARKET/HOUSE'                                       
         MVC   H8+34(12),=C'------------'                                       
         MVC   H7+60(3),=C'EST'                                                 
         MVC   H8+60(3),=C'---'                                                 
         MVC   H7+67(12),=C'FLIGHT DATES'                                       
         MVC   H8+67(12),=C'------------'                                       
         MVC   H7+84(7),=C'CONTACT'                                             
         MVC   H8+84(7),=C'-------'                                             
         MVC   H7+109(8),=C'  REF#  '                                           
         MVC   H8+109(8),=C'--------'                                           
         MVC   H7+119(4),=C'SENT'                                               
         MVC   H8+119(5),=C'-----'                                              
         MVC   H7+126(4),=C'DLVD'                                               
         MVC   H8+126(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS DRAFT ORDERS HEADSPECS AND HEADHOOK                          
***********************************************************************         
*                                                                               
SOHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
SOHDHOOK NTR1                                                                   
         MVC   H1(22),=C'SPOT ADDS DRAFT ORDERS'                                
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(7),=C'PRODUCT'                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(3),=C'EST'                                                 
         MVC   H8+25(3),=C'---'                                                 
         MVC   H7+32(12),=C'FLIGHT DATES'                                       
         MVC   H8+32(12),=C'------------'                                       
         MVC   H7+49(11),=C'DESTINATION'                                        
         MVC   H8+49(11),=C'-----------'                                        
         MVC   H7+77(3),=C'BYR'                                                 
         MVC   H8+77(3),=C'---'                                                 
         MVC   H7+81(5),=C'CMPGN'                                               
         MVC   H8+81(5),=C'-----'                                               
         MVC   H7+87(5),=C'STATN'                                               
         MVC   H8+87(5),=C'-----'                                               
         MVC   H7+93(4),=C'MKT '                                                
         MVC   H8+93(4),=C'----'                                                
         MVC   H7+98(9),=C'REQUESTOR'                                           
         MVC   H8+98(9),=C'---------'                                           
         MVC   H7+111(8),=C'  REF#  '                                           
         MVC   H8+111(8),=C'--------'                                           
         MVC   H7+120(4),=C'SENT'                                               
         MVC   H8+120(5),=C'-----'                                              
         MVC   H7+126(4),=C'DLVD'                                               
         MVC   H8+126(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS AVAIL REQUESTS HEADSPECS AND HEADHOOK                        
***********************************************************************         
*                                                                               
SAHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
SAHDHOOK NTR1                                                                   
         MVC   H1(24),=C'SPOT ADDS AVAIL REQUESTS'                              
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(7),=C'PRODUCT'                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(3),=C'EST'                                                 
         MVC   H8+25(3),=C'---'                                                 
         MVC   H7+32(12),=C'FLIGHT DATES'                                       
         MVC   H8+32(12),=C'------------'                                       
         MVC   H7+49(11),=C'DESTINATION'                                        
         MVC   H8+49(11),=C'-----------'                                        
         MVC   H7+77(5),=C'REF #'                                               
         MVC   H8+77(5),=C'-----'                                               
         MVC   H7+85(5),=C'BUYER'                                               
         MVC   H8+85(5),=C'-----'                                               
         MVC   H7+110(8),=C'  REF#  '                                           
         MVC   H8+110(8),=C'--------'                                           
         MVC   H7+120(4),=C'SENT'                                               
         MVC   H8+120(5),=C'-----'                                              
         MVC   H7+126(4),=C'DLVD'                                               
         MVC   H8+126(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT DMBDE SPECIAL TRANSMISSION HEADSPECS AND HEADHOOK                 
***********************************************************************         
*                                                                               
SDHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
SDHDHOOK NTR1                                                                   
         MVC   H1(8),=C'SPOT DMB'                                               
         MVI   H1+8,X'50'          C'&'                                         
         MVC   H1+9(15),=C'B FILE TRANSFER'                                     
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(7),=C'PRODUCT'                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(3),=C'EST'                                                 
         MVC   H8+25(3),=C'---'                                                 
         MVC   H7+30(4),=C'MKT '                                                
         MVC   H8+30(4),=C'----'                                                
         MVC   H7+36(5),=C'STATN'                                               
         MVC   H8+36(5),=C'-----'                                               
         MVC   H7+43(3),=C'BYR'                                                 
         MVC   H8+43(3),=C'---'                                                 
         MVC   H7+48(5),=C'CMPGN'                                               
         MVC   H8+48(5),=C'-----'                                               
         MVC   H7+55(11),=C'DESTINATION'                                        
         MVC   H8+55(11),=C'-----------'                                        
         MVC   H7+83(5),=C'FTP #'                                               
         MVC   H8+83(5),=C'-----'                                               
         MVC   H7+90(4),=C'SENT'                                                
         MVC   H8+90(5),=C'-----'                                               
         MVC   H7+97(4),=C'DLVD'                                                
         MVC   H8+97(5),=C'-----'                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT DMBDE GOAL TRANSFER HEADSPECS AND HEADHOOK                        
***********************************************************************         
*                                                                               
SGHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
SGHDHOOK NTR1                                                                   
         MVC   H1(8),=C'SPOT DMB'                                               
         MVI   H1+8,X'50'          C'&'                                         
         MVC   H1+9(15),=C'B GOAL TRANSFER'                                     
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(7),=C'PRODUCT'                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(3),=C'EST'                                                 
         MVC   H8+25(3),=C'---'                                                 
         MVC   H7+30(11),=C'DESTINATION'                                        
         MVC   H8+30(11),=C'-----------'                                        
         MVC   H7+58(5),=C'FTP #'                                               
         MVC   H8+58(5),=C'-----'                                               
         MVC   H7+65(4),=C'SENT'                                                
         MVC   H8+65(5),=C'-----'                                               
         MVC   H7+72(4),=C'DLVD'                                                
         MVC   H8+72(5),=C'-----'                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SPOT ADDS CONFIRMATION OF PURCHASE HEADSPECS AND HEADHOOK              
***********************************************************************         
SXHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         SSPEC H1,1,C'SPOT ADDS CONFIRMATION OF PURCHASE'                       
         DC    X'00'                                                            
SYHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         SSPEC H1,1,C'STATION ROTATION SCHEDULE'                                
         DC    X'00'                                                            
SRHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         SSPEC H1,1,C'NETWORK ROTATION SCHEDULE'                                
         DC    X'00'                                                            
*                                                                               
SXHDHOOK NTR1  BASE=*                                                           
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'REQUESTOR'                                              
         MVC   H3+10(3),NEWREQST                                                
         MVC   H7(7),=C'CLIENT '                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(7),=C'PRODUCT'                                             
         MVC   H8+25(7),=C'-------'                                             
         MVC   H7+50(3),=C'EST'                                                 
         MVC   H8+50(3),=C'---'                                                 
         MVC   H7+55(11),=C'DESTINATION'                                        
         MVC   H8+55(11),=C'-----------'                                        
         MVC   H7+85(5),=C'STATN'                                               
         MVC   H8+85(5),=C'-----'                                               
         MVC   H7+92(4),=C'MKT '                                                
         MVC   H8+92(4),=C'----'                                                
         MVC   H7+98(8),=C'  REF#  '                                            
         MVC   H8+98(8),=C'--------'                                            
         MVC   H7+108(4),=C'SENT'                                               
         MVC   H8+108(5),=C'-----'                                              
         MVC   H7+115(4),=C'DLVD'                                               
         MVC   H8+115(5),=C'-----'                                              
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
SYHDHOOK NTR1  BASE=*                                                           
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'REQUESTOR'                                              
         MVC   H3+10(3),NEWREQST                                                
         MVC   H7(7),=C'CLIENT '                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(7),=C'PRODUCT'                                             
         MVC   H8+25(7),=C'-------'                                             
         MVC   H7+50(3),=C'EST'                                                 
         MVC   H8+50(3),=C'---'                                                 
         MVC   H7+55(8),=C'   DATES'                                            
         MVC   H8+55(11),=C'-----------'                                        
         MVC   H7+68(11),=C'DESTINATION'                                        
         MVC   H8+68(11),=C'-----------'                                        
         MVC   H7+96(5),=C'STATN'                                               
         MVC   H8+96(5),=C'-----'                                               
         MVC   H7+103(4),=C'MKT '                                               
         MVC   H8+103(4),=C'----'                                               
         MVC   H7+109(8),=C'  REF#  '                                           
         MVC   H8+109(8),=C'--------'                                           
         MVC   H7+119(4),=C'SENT'                                               
         MVC   H8+119(5),=C'-----'                                              
         MVC   H7+126(4),=C'DLVD'                                               
         MVC   H8+126(5),=C'-----'                                              
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SOME BUG WE HAVE NOT BEEN ABLE TO SOLVE WHEN WE OPEN OVER 15 FILES  *         
* CLOSE ONE TO GET AROUND THIS ISSUE                                  *         
***********************************************************************         
CHKMAXSE NTR1  BASE=*,LABEL=*                                                   
         CLI   MAXSE#,15           MAX SYSTEMS TO OPEN AT ONE TIME              
         BE    CHKMX10                                                          
         LLC   RF,MAXSE#                                                        
         AHI   RF,1                                                             
         STC   RF,MAXSE#           UP MAX OPEN                                  
         B     CHKMXIT                                                          
*                                                                               
CHKMX10  L     R5,VUTL                                                          
         MVC   SVSE#,4(R5)         SAVE SE                                      
*                                                                               
         LA    R3,SETAB                                                         
         LA    RF,0                                                             
CHKMX12  CLI   0(R3),0             IS THIS FILE OPEN?                           
         BNE   CHKMX18             YES, THEN CLOSE IT                           
         AHI   R3,1                NO, CHECK NEXT SE                            
         AHI   RF,1                KEEP TRACK OF SE TO CLOSE                    
         B     CHKMX12                                                          
*                                                                               
CHKMX18  LA    R2,OPENADDS         TABLE OF SYSTEMS                             
         STC   RF,4(,R5)           SET SE IN UTL TO CLOSE                       
*                                                                               
CHKMX20  CLC   0(1,R2),0(R3)       MATCH FOR SYSTEM                             
         BE    CHKMX30                                                          
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   CHKMX20             MUST FIND A MATCH                            
         DC    H'00'                                                            
*                                                                               
CHKMX30  MVI   0(R3),0                                                          
         L     R3,8(,R2)                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMCLSE'),(R2),(R3),IO,0                       
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   4(1,R5),SVSE#       RESTORE SE                                   
                                                                                
CHKMXIT  XIT1                                                                   
SVSE#    DC    X'00'                                                            
MAXSE#   DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        SPOT HOUSE NAME                                                        
***********************************************************************         
*                                                                               
GETHOUSE NTR1  BASE=*,LABEL=*                                                   
         MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XC    HOUSENM,HOUSENM                                                  
*                                                                               
         USING PRHKEY,R6                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   PRHKID,=X'0A29'                                                  
         MVC   PRHKAM,AMCODE                                                    
         MVC   PRHKPRH,HOUSE                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   HEXIT                                                            
         GOTO1 GETREC                                                           
*                                                                               
         USING PRHDTAEL,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   HEXIT                                                            
         MVC   HOUSENM,PRHLINE1                                                 
HEXIT    XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SPOT AGENCY NAME AND ADDRESS, A/M CODE AND MEDIA NAME                  
***********************************************************************         
* * * *     - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM    * * * *         
***********************************************************************         
SPAGYCLT NMOD1 0,**SPAC**                                                       
         L     RC,0(R1)                                                         
         XC    AMCODE,AMCODE                                                    
         XC    MEDNAME,MEDNAME                                                  
         XC    CLTNAME,CLTNAME                                                  
         XC    RTGSRV,RTGSRV                                                    
         XC    CLTCODE,CLTCODE                                                  
*                                                                               
         USING AGYKEY,R6                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,POWCODE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BNE   EXIT                                                             
*                                                                               
         USING AGYEL,R6                                                         
GO200    MVI   ELCODE,X'01'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   EXIT                                                             
         MVC   ANAME,AGYNAME       AGENCY NAME                                  
         MVC   AADDR,AGYADDR       AGENCY ADDRESS                               
*                                                                               
         MVI   ELCODE,X'02'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   EXIT                                                             
         B     *+8                                                              
SAC10    BRAS  RE,NEXTEL                                                        
         CLI   0(R6),X'02'                                                      
         BNE   EXIT                                                             
         CLC   2(1,R6),MEDCODE     GET AGENCY/MEDIA CODE AND MEDIA              
         BNE   SAC10                                         NAME               
         MVC   AMCODE,3(R6)                                                     
         MVC   MEDNAME,4(R6)                                                    
         EJECT                                                                  
***********************************************************************         
*        SPOT CLIENT NAME AND RATING SERVICE                                    
***********************************************************************         
         GOTO1 =V(CLPACK),DMCB,CLCLT,CLTCODE                                    
         CLI   0(R1),0                                                          
         BNE   EXIT                                                             
*                                                                               
         USING CKEY,R6                                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   CKEYTYPE,X'00'                                                   
         MVC   CKEYAM,AMCODE                                                    
         MVC   CKEYCLT,CLTCODE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         GOTO1 GETREC                                                           
         USING CLTHDR,R6                                                        
         L     R6,AIO                                                           
         MVC   CLTNAME(L'CNAME),CNAME                                           
         MVC   RTGSRV,CPROF+3                                                   
         MVI   RS,C'N'                                                          
         CLI   RTGSRV,C'0'         NEILSON RATING SERVICE                       
         BE    EXIT                                                             
         MVI   RS,C'A'                                                          
         CLI   RTGSRV,C'1'         ARBITRON RATING SERVICE                      
         BE    EXIT                                                             
         XC    RS,RS                                                            
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SPOT MARKET NAME AND RATING                                            
***********************************************************************         
*                                                                               
SPMKTRTG NMOD1 0,**SPMK**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
*                                                                               
         XC    MKNAME,MKNAME                                                    
         MVC   MKNAME(8),=C'**************'                                     
         MVC   RSMKT,=C'****'                                                   
         MVI   MRTG,C'*'                                                        
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'                                                       
         MVC   LKEY,=H'17'                                                      
*                                                                               
         USING STAKEY,R6                                                        
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,MEDCODE                                                  
         MVC   STAKCALL,STACALL                                                 
*        MVC   STAKCALL+4(1),MEDCODE                                            
         MVC   STAKAGY,POWCODE                                                  
         MVC   STAKCLT,CLCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   SPMKX                                                            
         USING STAREC,R6                                                        
         L     R6,AIO                                                           
         MVC   MKTNUM,SMKT                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),MEDCODE                                                 
         MVC   KEY+2(4),MKTNUM                                                  
         MVC   KEY+6(2),POWCODE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   SPMKX                                                            
*                                                                               
         USING MKTREC,R6                                                        
         L     R6,AIO                                                           
         MVC   MKNAME,MKTNAME      MARKET NAME                                  
         MVC   MRTG,MKTRTG         RATING SERVICE OVERRIDE                      
*                                                                               
         CLC   MKTRS1,RTGSRV                                                    
         BNE   SPMR30                                                           
         EDIT  (B2,MKTRSM1),(4,RSMKT),FILL=0                                    
         B     SPMKX                                                            
SPMR30   CLC   MKTRS2,RTGSRV                                                    
         BNE   SPMKX                                                            
         EDIT  (B2,MKTRSM2),(4,RSMKT),FILL=0                                    
SPMKX    XC    FILENAME,FILENAME                                                
         XC    MKTNUM,MKTNUM                                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        NET SYSTEM READS                                                       
***********************************************************************         
*                                                                               
NETREAD  NMOD1 0,**NETR**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
*                                                                               
         MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
*                                                                               
         USING SRECD,R4                                                         
         USING EDFILD,R3                                                        
         LA    R3,SREDIREC                                                      
*                                                                               
         LA    R1,NTHDSPEC         HEADLINES                                    
         LA    R2,NTHDHOOK                                                      
         CLI   EDFTYPE,EDFTPAKQ    NEW NETPAK APPLICATION/                      
         BNE   NR20                                                             
         LA    R1,NPHDSPEC                                                      
         LA    R2,NPHDHOOK                                                      
         B     NR20X                                                            
*                                                                               
NR20     CLI   EDFTYPE,EDFTNDLQ    NET DUNNING LETTERS                          
         BNE   NR20X                                                            
         LA    R1,NVHDSPEC                                                      
         LA    R2,NVHDHOOK                                                      
*                                                                               
NR20X    ST    R1,SPECS                                                         
         ST    R2,HEADHOOK                                                      
*                                                                               
         CLI   EDFTYPE,EDFTPAKQ    NEW NETPAK APPLICATION/                      
         BE    NR200                                                            
         MVC   PRDCODE,SRNPRD                                                   
         MVC   NEWPRD,SRNPRD                                                    
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
         CLI   EDFTYPE,EDFTNDLQ    NET DUNNING LETTERS                          
         BE    NR300                                                            
         CLI   SRNTYPE,EDFTNINQ    NINS?                                        
         BE    NR30                                                             
         MVC   INDATES,EDINTCDT                                                 
         BRAS  RE,DATEFORM                                                      
         B     NR100                                                            
NR30     MVC   INDATES,EDINTNDT    NET TRAFFIC NINS                             
         BRAS  RE,DATEFORM                                                      
         EJECT                                                                  
***********************************************************************         
*        PROGRAM NAME                                                           
***********************************************************************         
*                                                                               
         XC    PROGNAME,PROGNAME                                                
         MVC   PROGNAME,=CL16'********'                                         
*                                                                               
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'                                                       
         MVC   LKEY,=H'17'                                                      
*                                                                               
         USING STAKEY,R6                                                        
         XC    KEY,KEY             FIRST HAVE TO GET MARKET CODE                
         LA    R6,KEY                                                           
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,SRMED                                                    
         MVC   STAKCALL,SRNNET                                                  
         MVC   STAKCALL+4(1),SRMED                                              
         MVC   STAKAGY,POWCODE                                                  
         MVC   STAKCLT,SRCLT                                                    
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         CLC   KEY(5),KEYSAVE                                                   
         BNE   NR100                                                            
         USING STAREC,R6                                                        
         L     R6,AIO                                                           
         MVC   MKTNUM,SMKT                                                      
         PACK  DUB(8),MKTNUM(4)                                                 
         CVB   R1,DUB                                                           
         STH   R1,MSNUM                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,EDINTNDT),(2,COMPDATE)                            
*                                                                               
         MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
*                                                                               
         USING NPGRECD,R6                                                       
         XC    KEY,KEY             PROGRAM RECORD                               
         LA    R6,KEY                                                           
         MVC   KEY(2),=X'0D20'                                                  
         MVC   NPGKAM,AMCODE                                                    
         MVC   NPGKNET,MSNUM                                                    
         MVC   NPGKPROG,EDINTNPG                                                
         GOTO1 HIGH                                                             
         B     NR40                                                             
NR40SQ   GOTO1 SEQ                                                              
NR40     CLC   KEY(11),KEYSAVE                                                  
         BNE   NR100                                                            
         CLC   COMPDATE,NPGKEND    CHECK END DATE                               
         BH    NR40SQ                                                           
         GOTO1 GETREC                                                           
         BNE   NR100                                                            
*                                                                               
         USING NPGEL92,R6                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,GETEL                                                         
         MVC   PROGNAME,NPGNAME                                                 
         B     NR100                                                            
         EJECT                                                                  
***********************************************************************         
*        FORMAT PRINT LINES                                                     
***********************************************************************         
*                                                                               
NR100    MVC   PNTYPE,EDFPQSUB                                                  
         MVC   PNNET,SRNNET                                                     
         CLI   SRNTYPE,EDFTNINQ    NINS?                                        
         BNE   NR120                                                            
         MVC   PNPRGCD,EDINTNPG    ONLY NINS HAS PROGRAMS                       
         MVC   PNPRGCDS,EDINTNPM                                                
         MVC   PNPRGNM,PROGNAME                                                 
         CLC   EDINTNRV,=C'000'                                                 
         BNE   *+10                                                             
         MVC   EDINTNRV,=C'ORG'                                                 
         MVC   PNREV,EDINTNRV      AND REVISIONS                                
NR120    MVC   PNDEST,EDFDEST                                                   
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PNFNUM,EDFEZRLN     PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PNERR                                                         
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PNDATES,DATES                                                    
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PNEZNUM,EDFEZLED                                                 
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PNEZNUM,EDFECNTN                                                 
         BRAS  RE,TIMEFORM                                                      
         MVC   PNSENT,SENTIME                                                   
         MVC   PNDLVD,RCVTIME                                                   
         MVI   PRDHEAD,C'Y'                                                     
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
*                                                                               
NR200    MVC   PNPPRG,NEEDPRG                                                   
         MVC   PNPNET,NEEDNET                                                   
         MVC   PNPMED,NEEDMED                                                   
         MVC   PNPEST,NEEDEST                                                   
         MVC   PNPPAK,NEEDPAK                                                   
         MVC   PNPREQ,NEEDREQ                                                   
         MVC   PNPDEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PNPFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PNPERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         BRAS  RE,TIMEFORM                                                      
         MVC   PNPSENT,SENTIME                                                  
         MVC   PNPDLVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
*                                                                               
NR300    MVC   PVEST,EDISTVES                                                   
         MVC   PVSTN,EDIRTVST                                                   
         MVC   PVDATES,EDINTVDT                                                 
         MVC   PVDEST,EDFDEST                                                   
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PVFNUM,EDFEZRLN     PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PVERR                                                         
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PVEZNUM,EDFEZLED                                                 
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PVEZNUM,EDFECNTN                                                 
         BRAS  RE,TIMEFORM                                                      
         MVC   PVSENT,SENTIME                                                   
         MVC   PVDLVD,RCVTIME                                                   
         MVI   PRDHEAD,C'Y'                                                     
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        NET HEADSPECS AND HEADHOOK                                             
***********************************************************************         
*                                                                               
NTHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
NTHDHOOK NTR1                                                                   
         MVC   H1(7),=C'NETWORK'                                                
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H4(9),=C'PRODUCT  '                                              
         MVC   H4+10(3),PRDCODE                                                 
         MVC   H4+14(20),PRDNAME                                                
         MVC   H7(3),=C'TYP'                                                    
         MVC   H8(3),=C'---'                                                    
         MVC   H7+4(7),=C'NETWORK'                                              
         MVC   H8+4(7),=C'-------'                                              
         MVC   H7+12(11),=C'DESTINATION'                                        
         MVC   H8+12(11),=C'-----------'                                        
         MVC   H7+40(6),=C'PGM CD'                                              
         MVC   H8+40(6),=C'------'                                              
         MVC   H7+48(12),=C'PROGRAM NAME'                                       
         MVC   H8+48(12),=C'------------'                                       
         MVC   H7+65(3),=C'REV'                                                 
         MVC   H8+65(3),=C'---'                                                 
         MVC   H7+73(12),=C'FLIGHT DATES'                                       
         MVC   H8+73(12),=C'------------'                                       
         MVC   H7+91(8),=C'  REF#  '                                            
         MVC   H8+91(8),=C'--------'                                            
         MVC   H7+102(4),=C'SENT'                                               
         MVC   H8+102(5),=C'-----'                                              
         MVC   H7+110(4),=C'DLVD'                                               
         MVC   H8+110(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        NEW NETPAK HEADSPECS AND HEADHOOK                                      
***********************************************************************         
*                                                                               
NPHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
NPHDHOOK NTR1                                                                   
         MVC   H1(6),=C'NETPAK'                                                 
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(3),=C'PRG'                                                    
         MVC   H8(3),=C'---'                                                    
         MVC   H7+5(7),=C'NETWORK'                                              
         MVC   H8+5(7),=C'-------'                                              
         MVC   H7+14(3),=C'MED'                                                 
         MVC   H8+14(3),=C'---'                                                 
         MVC   H7+19(3),=C'EST'                                                 
         MVC   H8+19(3),=C'---'                                                 
         MVC   H7+24(3),=C'PAK'                                                 
         MVC   H8+24(3),=C'---'                                                 
         MVC   H7+29(3),=C'REQ'                                                 
         MVC   H8+29(3),=C'---'                                                 
         MVC   H7+34(11),=C'DESTINATION'                                        
         MVC   H8+34(11),=C'-----------'                                        
         MVC   H7+62(8),=C'  REF#  '                                            
         MVC   H8+62(8),=C'--------'                                            
         MVC   H7+73(4),=C'SENT'                                                
         MVC   H8+73(5),=C'-----'                                               
         MVC   H7+81(4),=C'DLVD'                                                
         MVC   H8+81(5),=C'-----'                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        NET DUNNING LETTER HEADSPECS AND HEADHOOK                              
***********************************************************************         
*                                                                               
NVHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
NVHDHOOK NTR1                                                                   
         MVC   H1(7),=C'NETWORK'                                                
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H4(9),=C'PRODUCT  '                                              
         MVC   H4+10(3),PRDCODE                                                 
         MVC   H4+14(20),PRDNAME                                                
         MVC   H7+PVSTN-P(7),=C'STATION'                                        
         MVC   H8+PVSTN-P(7),=C'-------'                                        
         MVC   H7+PVDEST-P(11),=C'DESTINATION'                                  
         MVC   H8+PVDEST-P(11),=C'-----------'                                  
         MVC   H7+PVEST-P(8),=C'ESTIMATE'                                       
         MVC   H8+PVEST-P(8),=C'--------'                                       
         MVC   H7+PVDATES-P+5(5),=C'DATES'                                      
         MVC   H8+PVDATES-P+5(5),=C'-----'                                      
         MVC   H7+PVEZNUM-P(8),=C'  REF#  '                                     
         MVC   H8+PVEZNUM-P(8),=C'--------'                                     
         MVC   H7+PVSENT-P(4),=C'SENT'                                          
         MVC   H8+PVSENT-P(5),=C'-----'                                         
         MVC   H7+PVDLVD-P(4),=C'DLVD'                                          
         MVC   H8+PVDLVD-P(5),=C'-----'                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT READS                                                            
***********************************************************************         
*        - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                       
PRNTREAD NMOD1 0,**PRTR**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING SRECD,R4                                                         
         USING EDFILD,R3                                                        
         LA    R3,SREDIREC                                                      
*                                                                               
         MVC   LKEY,=H'25'                                                      
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'33'                                                  
         MVC   SYSFIL,=C'PRTFIL  '                                              
         MVC   SYSDIR,=C'PRTDIR  '                                              
         XC    FILENAME,FILENAME                                                
*                                                                               
         LA    R1,PRHDSPEC                                                      
         LA    R2,PRHDHOOK                                                      
         CLI   EDFTYPE,EDFTPINQ    INVOICE REPORT                               
         BNE   *+12                                                             
         LA    R1,PVHDSPEC                                                      
         LA    R2,PVHDHOOK                                                      
         CLI   EDFTYPE,EDFTPPPQ    PRINT REPORTS                                
         BNE   *+12                                                             
         LA    R1,PPHDSPEC                                                      
         LA    R2,PPHDHOOK                                                      
         ST    R1,SPECS                                                         
         ST    R2,HEADHOOK                                                      
*                                                                               
         MVC   NEWMED,SRMED                                                     
         MVC   NEWCLT,SRCLT                                                     
         EJECT                                                                  
***********************************************************************         
*        AGENCY RECORD                                                          
***********************************************************************         
*                                                                               
         USING PAGYKEY,R6                                                       
         LA    R6,KEY              AGENCY RECORD                                
         XC    KEY,KEY                                                          
         MVC   PAGYKAGY,POWCODE                                                 
         MVC   PAGYKMED,SRMED                                                   
         MVI   PAGYKRCD,X'01'                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PAGYELEM,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
*                                                                               
         MVC   ANAME,PAGYNAME                                                   
         MVC   AADDR,PAGYADDR                                                   
         MVC   MEDNAME,PAGYMED                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CLIENT RECORD                                                          
***********************************************************************         
*                                                                               
         MVC   CLTNAME,SPACES                                                   
         USING PCLTKEY,R6                                                       
         LA    R6,KEY              CLIENT RECORD                                
         XC    KEY,KEY                                                          
         MVC   PCLTKAGY,POWCODE                                                 
         MVC   PCLTKMED,SRMED                                                   
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SRCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   PR30                                                             
         GOTO1 GETREC                                                           
         USING PCLTELEM,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PR30                                                             
         MVC   CLTNAME,PCLTNAME                                                 
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PRODUCT RECORD                                                         
***********************************************************************         
*                                                                               
PR30     MVC   PRDNAME,SPACES                                                   
         USING PPRDKEY,R6                                                       
         LA    R6,KEY              PRODUCT RECORD                               
         XC    KEY,KEY                                                          
         MVC   PPRDKAGY,POWCODE                                                 
         MVC   PPRDKMED,SRMED                                                   
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,SRCLT                                                   
         MVC   PPRDKPRD,SRPPRD                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   PR40                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'06'                                                     
         USING PPRDELEM,R6                                                      
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         MVC   PRDNAME,PPRDNAME                                                 
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PUB RECORD                                                             
***********************************************************************         
*                                                                               
PR40     MVI   USEIO,0                                                          
         XC    PUBNM,PUBNM                                                      
         XC    PACKPUB,PACKPUB                                                  
*                                                                               
         SR    R6,R6               PACK PUB                                     
         LA    R1,PPEDPUB                                                       
PR42     CLI   0(R1),X'40'                                                      
         BE    PR44                                                             
         LA    R6,1(R6)                                                         
         LA    R1,1(R1)                                                         
         CH    R6,=H'15'                                                        
         BL    PR42                                                             
PR44     GOTO1 =V(PUBVAL),DMCB,((R6),PPEDPUB),PACKPUB                           
         CLI   0(R1),X'FF'                                                      
         BE    PR100                                                            
*                                                                               
         USING PUBREC,R6                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY              PUB RECORD                                   
         MVC   PUBKMED,SRMED                                                    
         MVC   PUBKPUB(6),PACKPUB                                               
         MVC   PUBKAGY,POWCODE                                                  
         MVI   PUBKCOD,X'81'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PR100                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
         USING PUBNAMEL,R6                                                      
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'        PUB NAME ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   PR100                                                            
         MVC   PUBNM,PUBNAME                                                    
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE                                                             
***********************************************************************         
*                                                                               
PR100    MVC   PPIPRD,SRPPRD       PRODUCT                                      
         MVI   PPIHYPH,C'-'                                                     
         MVC   PPIPRDNM,PRDNAME                                                 
*                                                                               
         CLI   EDFTYPE,EDFTPINQ    INVOICE REPORT  - PRINT ESTIMATE             
         BE    PR102                                                            
         CLI   EDFTYPE,EDFTPPPQ    PRINT REPORTS - REP TYPE AND CONT            
         BE    PR104                                                            
         MVC   PPIJOB,SRPJOB       JOB                                          
         B     PR110                                                            
PR102    MVC   PPIEST,PPEDEST      ESTIMATE                                     
         B     PR110                                                            
PR104    MVC   PPIREP,PPEDRPT      REPORT TYPE                                  
         MVC   PPICON,PPEDCON      CONTRACT NUMBER                              
*                                                                               
PR110    MVC   PPIPUB,SRPPUB       PUB                                          
         MVC   PPIPUBNM,PUBNM      PUB NAME                                     
         MVC   PPIREQID,PPEDREQ    REQUESTOR                                    
         MVC   PPIDEST,EDFDEST     DESTINATION                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PPIFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PPIERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PPIEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PPIEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PPISENT,SENTIME                                                  
         MVC   PPIDLVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
*                                                                               
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT HEADSPECS AND HEADHOOK                                           
***********************************************************************         
*                                                                               
PRHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
PRHDHOOK NTR1                                                                   
         MVC   H1(5),=C'PRINT'                                                  
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(7),=C'PRODUCT'                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(7),=C'AD CODE'                                             
         MVC   H8+25(7),=C'-------'                                             
         MVC   H7+34(10),=C'PUB NUMBER'                                         
         MVC   H8+34(10),=C'----------'                                         
         MVC   H7+52(8),=C'PUB NAME'                                            
         MVC   H8+52(8),=C'--------'                                            
         MVC   H7+73(3),=C'REQ'                                                 
         MVC   H8+73(3),=C'---'                                                 
         MVC   H7+79(11),=C'DESTINATION'                                        
         MVC   H8+79(11),=C'-----------'                                        
         MVC   H7+107(8),=C'  REF#  '                                           
         MVC   H8+107(8),=C'--------'                                           
         MVC   H7+119(4),=C'SENT'                                               
         MVC   H8+119(5),=C'-----'                                              
         MVC   H7+127(4),=C'DLVD'                                               
         MVC   H8+127(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT NV INVOICE HEADLINES                                             
***********************************************************************         
*                                                                               
PVHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
PVHDHOOK NTR1                                                                   
         MVC   H1(31),=C'PRINT NV INVOICE CONTROL REPORT'                       
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(7),=C'PRODUCT'                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(7),=C' EST   '                                             
         MVC   H8+25(7),=C' ---   '                                             
         MVC   H7+34(10),=C'PUB NUMBER'                                         
         MVC   H8+34(10),=C'----------'                                         
         MVC   H7+52(8),=C'PUB NAME'                                            
         MVC   H8+52(8),=C'--------'                                            
         MVC   H7+73(3),=C'REQ'                                                 
         MVC   H8+73(3),=C'---'                                                 
         MVC   H7+79(11),=C'DESTINATION'                                        
         MVC   H8+79(11),=C'-----------'                                        
         MVC   H7+107(8),=C'  REF#  '                                           
         MVC   H8+107(8),=C'--------'                                           
         MVC   H7+119(4),=C'SENT'                                               
         MVC   H8+119(5),=C'-----'                                              
         MVC   H7+127(4),=C'DLVD'                                               
         MVC   H8+127(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT TYPE P HEADSPECS AND HEADHOOK                                    
***********************************************************************         
*                                                                               
PPHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
PPHDHOOK NTR1                                                                   
         MVC   H1(31),=C'PRINT REPORTS                  '                       
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H2(10),MEDNAME                                                   
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'CLIENT   '                                              
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(20),CLTNAME                                                
         MVC   H7(7),=C'PRODUCT'                                                
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(8),=C' RPT CON'                                            
         MVC   H8+25(8),=C' --- ---'                                            
         MVC   H7+34(10),=C'PUB NUMBER'                                         
         MVC   H8+34(10),=C'----------'                                         
         MVC   H7+52(8),=C'PUB NAME'                                            
         MVC   H8+52(8),=C'--------'                                            
         MVC   H7+73(3),=C'REQ'                                                 
         MVC   H8+73(3),=C'---'                                                 
         MVC   H7+79(11),=C'DESTINATION'                                        
         MVC   H8+79(11),=C'-----------'                                        
         MVC   H7+107(8),=C'  REF#  '                                           
         MVC   H8+107(8),=C'--------'                                           
         MVC   H7+119(4),=C'SENT'                                               
         MVC   H8+119(5),=C'-----'                                              
         MVC   H7+127(4),=C'DLVD'                                               
         MVC   H8+127(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ACCOUNT READS                                                          
***********************************************************************         
*                                                                               
ACCREAD  NMOD1 0,**ACCR**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
         USING SRECD,R4                                                         
         USING EDFILD,R3                                                        
         LA    R3,SREDIREC                                                      
*                                                                               
         MVC   LKEY,=Y(L'ACCKEY)                                                
         MVC   LSTATUS,=Y(L'ACCRSTA)                                            
         MVC   DATADISP,=Y(ACCORFST)     ACCOUNT USES THIS                      
         MVC   SYSFIL,=C'ACCOUNT '                                              
         MVC   SYSDIR,=C'ACCOUNT '                                              
         XC    FILENAME,FILENAME                                                
*                                                                               
         LA    R1,ACHDSPEC                                                      
         ST    R1,SPECS                                                         
         LA    R1,ACHDHOOK                                                      
         ST    R1,HEADHOOK                                                      
         MVC   NEWMED,SRMED                                                     
*                                                                               
         CLI   ACFXTYPE,C'P'       PRODUCTION                                   
         BE    AR10                                                             
         XC    NEWCLT,NEWCLT                                                    
         XC    CLTCODE,CLTCODE                                                  
         XC    CLTNAME,CLTNAME                                                  
         XC    PRDCODE,PRDCODE                                                  
         XC    PRDNAME,PRDNAME                                                  
         MVC   PRDEXP,=C'EXPENSE'                                               
         XC    CLIWORD,CLIWORD                                                  
         XC    JOBWORD,JOBWORD                                                  
         XC    JOBUNDER,JOBUNDER                                                
         B     AR60                                                             
*                                                                               
AR10     MVC   PRDEXP,=C'PRODUCT'                                               
         MVC   CLIWORD,=C'CLIENT   '                                            
         MVC   JOBWORD,=C'JOB CODE'                                             
         MVC   JOBUNDER,=C'--------'                                            
         MVC   NEWCLT,SRCLT                                                     
         MVC   NEWPRD,SRAPRD                                                    
         EJECT                                                                  
***********************************************************************         
*        CLIENT                                                                 
***********************************************************************         
*                                                                               
         CLC   NEWCLT,SVCLT        NEW CLIENT NAME                              
         BE    AR20                                                             
         XC    CLTNAME,CLTNAME                                                  
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY              CLIENT NAME                                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA(6),ACFXCOMP UP TO CLIENT                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',KEY,AIO                  
         L     R6,AIO                                                           
         CLC   KEY(42),0(R6)                                                    
         BNE   AR20                                                             
         BRAS  RE,ACGTNAME                                                      
         BNE   AR20                                                             
         MVC   CLTNAME,WORK                                                     
*                                                                               
***********************************************************************         
*        PRODUCT                                                                
***********************************************************************         
*                                                                               
AR20     CLC   NEWPRD,SVPRD        NEW PRODUCT CODE                             
         BE    AR40                NO, THEN SKIP TO JOB                         
         XC    PRDNAME,PRDNAME                                                  
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY              PRODUCT NAME                                 
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA(9),ACFXCOMP   UP TO PRODUCT                             
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',KEY,AIO                  
         L     R6,AIO                                                           
         CLC   KEY(42),0(R6)                                                    
         BNE   AR40                                                             
         BRAS  RE,ACGTNAME                                                      
         BNE   AR40                                                             
         MVC   PRDNAME,WORK                                                     
         EJECT                                                                  
***********************************************************************         
*        JOB                                                                    
***********************************************************************         
*                                                                               
AR40     CLC   ACFXJOB,SVJOB       NEW JOB CODE                                 
         BE    AR60                NO, THEN SKIP TO VENDOR                      
         XC    JOBNAME,JOBNAME                                                  
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY              JOB NAME                                     
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA(15),ACFXCOMP   UP TO JOB                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',KEY,AIO                  
         L     R6,AIO                                                           
         CLC   KEY(42),0(R6)                                                    
         BNE   AR60                                                             
         BRAS  RE,ACGTNAME                                                      
         BNE   AR60                                                             
         MVC   JOBNAME,WORK                                                     
         MVC   SVJOB,ACFXJOB                                                    
*                                                                               
***********************************************************************         
*        VENDOR                                                                 
***********************************************************************         
*                                                                               
AR60     CLC   ACFXVEND,SVVEND     NEW VENDOR                                   
         BE    AR80                NO, THEN PRINT OUT LINE                      
         XC    VENDNAME,VENDNAME                                                
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY              VENDOR NAME                                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,ACFXCOMP                                                 
         MVC   ACTKULA(14),ACFXVEND                                             
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',KEY,AIO                  
         L     R6,AIO                                                           
         CLC   KEY(42),0(R6)                                                    
         BNE   AR80                                                             
         BRAS  RE,ACGTNAME                                                      
         BNE   AR80                                                             
         MVC   VENDNAME,WORK                                                    
         MVC   SVVEND,ACFXVEND                                                  
         EJECT                                                                  
***********************************************************************         
*        FORMAT PRINT LINE                                                      
***********************************************************************         
*                                                                               
AR80     CLI   ACFXTYPE,C'P'                                                    
         BNE   AR100                                                            
         MVC   PACPRD,SRPPRD                                                    
         MVC   PACPRDNM,PRDNAME                                                 
         MVC   PACJOB,SRPJOB                                                    
         MVC   PACJBNM,JOBNAME                                                  
         MVC   PACPO#,ACFXONUM                                                  
         MVC   PACVEND,ACFXVEND+2                                               
         MVC   PACVDNM,VENDNAME                                                 
         MVC   PACDEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PACFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PACERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PACEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PACEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PACSENT,SENTIME                                                  
         MVC   PACDLVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
         B     ARX                                                              
*                                                                               
AR100    MVC   PACPRD(14),ACFXEXP                                               
         MVC   PACPO#,ACFXONUM                                                  
         MVC   PACVEND,ACFXVEND+2                                               
         MVC   PACVDNM,VENDNAME                                                 
         MVC   PACDEST,EDFDEST                                                  
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PACFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PACERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PACEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PACEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PACSENT,SENTIME                                                  
         MVC   PACDLVD,RCVTIME                                                  
         MVI   PRDHEAD,C'N'                                                     
         OI    PRBIT,PRNOPRD                                                    
         BRAS  RE,PRTLINE                                                       
ARX      XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        GET NAME FROM X'20' ELEM AND PUT IN WORK                               
***********************************************************************         
*                                                                               
ACGTNAME NTR1                                                                   
         USING NAMELD,R6                                                        
         L     R6,AIO                                                           
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,NAMELQ       SET ELEMENT CODE                             
         BRAS  RE,GETEL                                                         
         BNE   NO                  NOT FOUND                                    
         SR    R1,R1               MOVE NAME TO WORK                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q)      MINUS ELCODE AND LENGTH                      
         SH    R1,=H'1'                                                         
         BM    NO                                                               
         CH    R1,=H'64'                                                        
         BH    NO                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),NAMEREC                                                  
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ACC HEADSPECS AND HEADHOOK                                             
***********************************************************************         
ACHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
ACHDHOOK NTR1                                                                   
         MVC   H1(10),=C'REPORT ATA'                                            
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),CLIWORD                                                    
         MVC   H3+10(3),NEWCLT                                                  
         MVC   H3+14(30),CLTNAME                                                
         MVC   H7(7),PRDEXP                                                     
         MVC   H8(7),=C'-------'                                                
         MVC   H7+25(8),JOBWORD                                                 
         MVC   H8+25(8),JOBUNDER                                                
         MVC   H7+53(6),=C'PO NUM'                                              
         MVC   H8+53(6),=C'------'                                              
         MVC   H7+61(6),=C'VENDOR'                                              
         MVC   H8+61(6),=C'------'                                              
         MVC   H7+95(11),=C'DESTINATION'                                        
         MVC   H8+95(11),=C'-----------'                                        
         MVC   H7+110(8),=C'  REF#  '                                           
         MVC   H8+110(8),=C'--------'                                           
         MVC   H7+120(4),=C'SENT'                                               
         MVC   H8+120(5),=C'-----'                                              
         MVC   H7+127(4),=C'DLVD'                                               
         MVC   H8+127(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        REP READS                                                              
***********************************************************************         
*                                                                               
REPREAD  NMOD1 0,**REPR**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
*                                                                               
         USING SRECD,R4                                                         
         LA    R4,SREC                                                          
         USING EDFILD,R3                                                        
         LA    R3,SREDIREC                                                      
*                                                                               
         MVC   SVAGY,EDFPQUID                                                   
         MVC   LKEY,=H'27'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         XC    FILENAME,FILENAME                                                
*                                                                               
         LA    R1,RPHDSPEC         NON DARE REP                                 
         ST    R1,SPECS                                                         
         LA    R1,RPHDHOOK                                                      
         ST    R1,HEADHOOK                                                      
         MVI   PROFFTOT,C'Y'                                                    
*                                                                               
         CLI   EDFSYS,EDFDARRQ     DARE                                         
         BNE   RR05                                                             
         LA    R1,RDHDSPEC                                                      
         ST    R1,SPECS                                                         
         LA    R1,RDHDHOOK                                                      
         ST    R1,HEADHOOK                                                      
         B     RR10                GO PRINT DARE REP                            
*                                                                               
RR05     CLI   EDFTYPE,EDFTDTRQ    REP TYPE D                                   
         BNE   RR100               ELSE OTHER REP TYPE                          
         LA    R1,R2HDSPEC                                                      
         ST    R1,SPECS                                                         
         LA    R1,R2HDHOOK                                                      
         ST    R1,HEADHOOK                                                      
         MVI   PROFFTOT,C'N'                                                    
         B     RR200                                                            
         EJECT                                                                  
***********************************************************************         
*        SALESPERSON REC                                                        
***********************************************************************         
*                                                                               
RR10     MVC   SPCODE,EDIRDRSP                                                  
         XC    SALNAME,SALNAME                                                  
         CLC   SPCODE,SPACES                                                    
         BNH   RR20                                                             
*                                                                               
         USING RSALKEY,R6                                                       
         LA    R6,KEY              SALESPERSON RECORD                           
         XC    KEY,KEY                                                          
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,EDIRDRRP                                                
         MVC   RSALKSAL,EDIRDRSP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RR20                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RSALELEM,R6                                                      
         MVC   SALNAME,RSALNAME                                                 
         EJECT                                                                  
***********************************************************************         
*        STATION MARKET NAME                                                    
***********************************************************************         
*                                                                               
RR20     XC    MKNAME,MKNAME                                                    
         USING RSTAKEY,R6                                                       
         LA    R6,KEY              GET STATION MARKET NAME                      
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,EDIRDRRP                                                
         MVC   RSTAKSTA,EDIRDRST                                                
         MVI   RSTAKSTA+4,C' '                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RR30                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RSTAELEM,R6                                                      
         MVC   MKNAME,RSTAMKT                                                   
         EJECT                                                                  
***********************************************************************         
*        REP DARE PRINT LINE                                                    
***********************************************************************         
*                                                                               
RR30     MVC   PRDRSTN,EDIRDRST                                                 
         MVC   PRDRSTNM,MKNAME                                                  
         MVC   PRDRAGY,EDIRDRAG                                                 
         MVC   PRDRMED,EDIRDRMD                                                 
         MVC   PRDRCLT,EDIRDRCL                                                 
         MVC   PRDRPRD,EDIRDRP1                                                 
         CLC   EDIRDRP2,SPACES                                                  
         BNH   RR32                                                             
         MVI   PRDRHYPH,C'-'                                                    
         MVC   PRDRPTN,EDIRDRP2                                                 
RR32     MVC   PRDREST,EDIRDRES                                                 
         MVC   PRDRBYR,EDIRDRBY                                                 
         MVC   PRDRORD#,EDIRDRAN                                                
         MVC   PRDRCON#,EDIRDRCN                                                
         MVC   PRDRTYPE,=C'APP'                                                 
         CLI   EDFTYPE,EDFTDAPQ    APPROVAL                                     
         BE    RR34                                                             
         MVC   PRDRTYPE,=C'CNF'                                                 
         CLI   EDFTYPE,EDFTDCFQ    CONFIRM                                      
         BE    RR34                                                             
         MVC   PRDRTYPE,=C'REJ'    REJECTION                                    
RR34     BRAS  RE,TIMEFORM                                                      
         MVC   PRDRSENT,SENTIME                                                 
         MVC   PRDRDLVD,RCVTIME                                                 
         BRAS  RE,REPLINE                                                       
         B     RRX                                                              
         EJECT                                                                  
***********************************************************************         
*        OFFICE REC                                                             
***********************************************************************         
*                                                                               
RR100    XC    OFFNM,OFFNM                                                      
         MVC   OFFCD,EDIRCNOF                                                   
         MVC   NEWOFF,EDIRCNOF                                                  
         CLC   EDIRCNOF,SPACES                                                  
         BNH   RR110                                                            
*                                                                               
         USING ROFFKEY,R6                                                       
         LA    R6,KEY              OFFICE RECORD                                
         XC    KEY,KEY                                                          
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,EDIRCNRP                                                
         MVC   ROFFKOFF,EDIRCNOF                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RR110                                                            
         GOTO1 GETREC                                                           
         USING ROFFELEM,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         MVC   OFFNM,ROFFNAME                                                   
         EJECT                                                                  
***********************************************************************         
*        SALESPERSON                                                            
***********************************************************************         
*                                                                               
RR110    CLI   EDFTYPE,EDFTKWXQ    KWX?                                         
         BE    RR400               THEN PRINT LINE, ELSE MORE READS             
*                                                                               
         XC    SALNAME,SALNAME                                                  
         USING RSALKEY,R6                                                       
         LA    R6,KEY              SALESPERSON RECORD                           
         XC    KEY,KEY                                                          
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,EDIRCNRP                                                
         MVC   RSALKSAL,EDIRCNSP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RR130                                                            
         GOTO1 GETREC                                                           
         USING RSALELEM,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         MVC   SALNAME,RSALNAME                                                 
         EJECT                                                                  
***********************************************************************         
*        AGENCY REC                                                             
***********************************************************************         
*                                                                               
RR130    XC    RAGYNM,RAGYNM                                                    
         USING RAGYKEY,R6                                                       
         LA    R6,KEY              AGENCY  RECORD                               
         XC    KEY,KEY                                                          
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,EDIRCNAG                                                
         MVC   RAGYKAOF,EDIRCNAO                                                
         MVC   RAGYKREP,EDIRCNRP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    RR132                                                            
         MVC   RAGYKAOF,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RR140                                                            
RR132    GOTO1 GETREC                                                           
         USING RAGYELEM,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         MVC   RAGYNM,RAGYNAM1                                                  
         EJECT                                                                  
***********************************************************************         
*        ADVERTISER REC                                                         
***********************************************************************         
*                                                                               
RR140    XC    ADVNAME,ADVNAME                                                  
         XC    KEY,KEY             ADVERTISER RECORD                            
         USING RADVREC,R6                                                       
         LA    R6,KEY                                                           
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,EDIRCNAD                                                
         MVC   RADVKREP,EDIRCNRP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RR500                                                            
         GOTO1 GETREC                                                           
         USING RADVELEM,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         MVC   ADVNAME,RADVNAME                                                 
         B     RR500                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT PRINT LINES                                                     
***********************************************************************         
*                                                                               
RR200    MVC   PRDSTA,EDIRDTST                                                  
         MVC   PRDETRAN,EDIRDDTS                                                
         CLC   EDIRDTSD,SPACES                                                  
         BNH   RR210                                                            
********TEMP PATCH FOR DATA RANGE ERROR, YYUN, 6/18******************           
         CLI   EDIRDTSD,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDIRDTSD,C' '                                                    
         CLI   EDIRDTED,C'|'                                                    
         BNE   *+8                                                              
         MVI   EDIRDTED,C' '                                                    
********TEMP PATCH FOR DATA RANGE ERROR, YYUN, 6/18******************           
         GOTO1 DATCON,DMCB,(0,EDIRDTSD),(11,PRDSTART)                           
RR210    CLC   EDIRDTED,SPACES                                                  
         BNH   RR220                                                            
         GOTO1 DATCON,DMCB,(0,EDIRDTED),(11,PRDEND)                             
RR220    EDIT  (B4,EDFEZLED),(5,PRDFTP)                                         
         BRAS  RE,TIMEFORM                                                      
         MVC   PRDSENT,SENTIME                                                  
         MVC   PRDDLVD,RCVTIME                                                  
         BRAS  RE,REPLINE                                                       
         B     RRX                                                              
*                                                                               
RR400    MVC   PRSTATN,EDIRKXST                                                 
         MVC   PRTYP,EDFTYPE                                                    
         MVC   PRSLPBK(3),EDIRKXBC+2                                            
         MVC   PRCKNUM,EDIRKXNM                                                 
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PREZNUM,EDFEZLED                                                 
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PREZNUM,EDFECNTN                                                 
         BRAS  RE,TIMEFORM                                                      
         MVC   PRSENT,SENTIME                                                   
         MVC   PRDLVD,RCVTIME                                                   
         BRAS  RE,REPLINE                                                       
         B     RRX                                                              
*                                                                               
RR500    MVC   PRSTATN,EDIRCNST                                                 
         MVC   PRTYP,EDFTYPE                                                    
         MVC   PRSLPBK,SALNAME                                                  
         MVC   PRCKNUM,EDIRCNHN                                                 
         MVC   PRVER,EDIRCNVN                                                   
         MVC   PRAGYCD,EDIRCNAG                                                 
         MVC   PRCTYCD,EDIRCNAO                                                 
         MVC   PRADVCD,EDIRCNAD                                                 
         MVC   PRADV,ADVNAME                                                    
         MVC   PRDATES(8),EDIRCNFS                                              
         MVI   PRDATES+8,C'-'                                                   
         MVC   PRDATES+9(8),EDIRCNFE                                            
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PREZNUM,EDFEZLED                                                 
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PREZNUM,EDFECNTN                                                 
         BRAS  RE,TIMEFORM                                                      
         MVC   PRSENT,SENTIME                                                   
         MVC   PRDLVD,RCVTIME                                                   
         BRAS  RE,REPLINE                                                       
*                                                                               
RRX      XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE AND RUN TOTALS                                              
***********************************************************************         
*                                                                               
REPLINE  NTR1                                                                   
         TM    EDFSTAT,EDFSTCAN                                                 
         BZ    RL10                                                             
         LH    R1,CANTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,CANTOTAL                                                      
         B     RL100                                                            
*                                                                               
RL10     TM    EDFSTAT,EDFSTRCV                                                 
         BZ    RL20                                                             
         LH    R1,DLVTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,DLVTOTAL                                                      
         B     RL100                                                            
*                                                                               
RL20     LH    R1,MISTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,MISTOTAL                                                      
*                                                                               
RL100    CLI   EDFSYS,EDFDARRQ     DARE                                         
         BNE   RL110                                                            
         LH    R1,SPTOTAL          SALESPERSON TOTAL                            
         LA    R1,1(R1)                                                         
         STH   R1,SPTOTAL                                                       
         B     RL120                                                            
*                                                                               
RL110    LH    R1,OFFTOTAL         OFFICE TOTAL                                 
         LA    R1,1(R1)                                                         
         STH   R1,OFFTOTAL                                                      
*                                                                               
RL120    GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   SVAGY,NEWAGY                                                     
         XC    NEWAGY,NEWAGY                                                    
         MVC   SVOFF,NEWOFF                                                     
         XC    NEWOFF,NEWOFF                                                    
         MVC   SVSPCODE,SPCODE                                                  
         XC    SPCODE,SPCODE                                                    
         MVI   FIRSTLIN,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        REP HEADSPECS AND HEADHOOK                                             
***********************************************************************         
RPHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
RPHDHOOK NTR1                                                                   
         MVC   H1(3),=C'REP'                                                    
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(9),=C'OFFICE   '                                              
         MVC   H3+10(2),OFFCD                                                   
         MVC   H3+14(20),OFFNM                                                  
         MVC   H7(5),=C'STATN'                                                  
         MVC   H8(5),=C'-----'                                                  
         MVC   H7+7(1),=C'T'                                                    
         MVC   H8+7(1),=C'-'                                                    
         MVC   H7+10(20),=C'BOOK CD./SALESPERSON'                               
         MVC   H8+10(20),=C'--------------------'                               
         MVC   H7+32(8),=C'KWX/CON#'                                            
         MVC   H8+32(8),=C'--------'                                            
         MVC   H7+42(3),=C'VER'                                                 
         MVC   H8+42(3),=C'---'                                                 
         MVC   H7+47(6),=C'AGY CD'                                              
         MVC   H8+47(6),=C'------'                                              
         MVC   H7+55(26),=C'      ADVERTISER          '                         
         MVC   H8+55(26),=C'--------------------------'                         
         MVC   H7+83(15),=C'  FLIGHT DATES '                                    
         MVC   H8+83(17),=C'-----------------'                                  
         MVC   H7+102(8),=C'  REF#  '                                           
         MVC   H8+102(8),=C'--------'                                           
         MVC   H7+112(4),=C'SENT'                                               
         MVC   H8+112(5),=C'-----'                                              
         MVC   H7+119(4),=C'DLVD'                                               
         MVC   H8+119(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        REP HEADSPECS AND HEADHOOK - TYPE D ONLY                               
***********************************************************************         
R2HDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
R2HDHOOK NTR1                                                                   
         MVC   H1(3),=C'REP'                                                    
         MVC   H2(23),=C'INVENTORY STATION RECAP'                               
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),USERNAME                                               
         MVC   H2+97(33),USERADDR                                               
         MVC   H3+51(8),HDATE                                                   
         MVC   H7(5),=C'STATN'                                                  
         MVC   H8(5),=C'-----'                                                  
         MVC   H7+10(12),=C'ETRAN NUMBER'                                       
         MVC   H8+10(12),=C'------------'                                       
         MVC   H7+25(8),=C'ST DATE '                                            
         MVC   H8+25(8),=C'--------'                                            
         MVC   H7+35(8),=C'END DATE'                                            
         MVC   H8+35(8),=C'--------'                                            
         MVC   H7+50(5),=C'FTP #'                                               
         MVC   H8+50(5),=C'-----'                                               
         MVC   H7+60(4),=C'SENT'                                                
         MVC   H8+60(5),=C'-----'                                               
         MVC   H7+68(4),=C'DLVD'                                                
         MVC   H8+68(5),=C'-----'                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        REP - DARE HEADSPECS AND HEADHOOK                                      
***********************************************************************         
RDHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
RDHDHOOK NTR1                                                                   
         MVC   H1(4),=C'DARE'                                                   
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H3+51(8),HDATE                                                   
         MVC   H3(11),=C'SALESPERSON'                                           
         MVC   H3+15(3),SPCODE                                                  
         MVC   H3+20(20),SALNAME                                                
         MVC   H7(8),=C'STATION '                                               
         MVC   H8(8),=C'--------'                                               
         MVC   H7+27(8),=C'DARE AGY'                                            
         MVC   H8+27(8),=C'--------'                                            
         MVC   H7+37(17),=C'M CLT PRD-PTN EST'                                  
         MVC   H8+37(17),=C'- --- ------- ---'                                  
         MVC   H7+55(3),=C'BYR'                                                 
         MVC   H8+55(3),=C'---'                                                 
         MVC   H7+60(8),=C' ORDER# '                                            
         MVC   H8+60(8),=C'--------'                                            
         MVC   H7+70(8),=C'CONTRCT#'                                            
         MVC   H8+70(8),=C'--------'                                            
         MVC   H7+80(4),=C'TYPE'                                                
         MVC   H8+80(4),=C'----'                                                
         MVC   H7+88(4),=C'SENT'                                                
         MVC   H8+88(5),=C'-----'                                               
         MVC   H7+95(4),=C'DLVD'                                                
         MVC   H8+95(5),=C'-----'                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GENERAL FAX FEATURE REPORT - TYPE Z                                    
***********************************************************************         
*                                                                               
GENFAX   NMOD1 0,*GENFAX*                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
         USING SRECD,R4                                                         
         LA    R3,SREDIREC                                                      
         USING EDFILD,R3                                                        
*                                                                               
         LA    R1,ZZHDSPEC         HEADLINES                                    
         ST    R1,SPECS                                                         
         LA    R1,ZZHDHOOK                                                      
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVC   NEWTYPE,EDFTYPE                                                  
         CLC   NEWTYPE,SVTYPE                                                   
         BE    *+8                                                              
         MVI   TOT,C'Y'            TOTALS IF THIS IS NEW TYPE                   
*                                                                               
         XC    SVPRD,SVPRD         DON'T PRINT CLIENT/PROD TOTALS               
         XC    SVCLT,SVCLT                                                      
         OI    PRBIT,PRNOCLT+PRNOPRD                                            
*                                                                               
         MVC   SYSNAME,SPACES                                                   
         LA    R2,REPTABLE         GET SYSTEM NAME                              
GF10     CLC   EDFSYS(2),0(R2)     MATCH ON REPORT TYPE                         
         BE    GF20                                                             
         LA    R2,L'REPTABLE(R2)   BUMP TO NEXT ROW                             
         CLI   0(R2),X'FF'                                                      
         BNE   GF10                                                             
         B     GF25                                                             
*                                                                               
GF20     MVC   SYSNAME,2(R2)       GET REPORT TYPE SYSTEM NAME                  
GF25     MVC   PZZDEST,EDFDEST     PRINT LINE                                   
*                                                                               
         LA    R1,EDFDEST                                                       
         BRAS  RE,ISFAX#                                                        
         BE    *+10                                                             
         MVC   PZZFNUM,EDFEZRLN    PRINT FAX TO # ALSO                          
*                                                                               
         LA    R1,PZZERR                                                        
         BRAS  RE,GETERR                                                        
*                                                                               
         MVC   PZZINFO,EDFAPPL                                                  
         CLI   EDFMETH,C'F'                                                     
         BE    *+10                                                             
         MVC   PZZEZNUM,EDFEZLED                                                
         CLI   EDFMETH,C'I'                                                     
         BNE   *+10                                                             
         MVC   PZZEZNUM,EDFECNTN                                                
         BRAS  RE,TIMEFORM                                                      
         MVC   PZZSENT,SENTIME                                                  
         MVC   PZZDLVD,RCVTIME                                                  
         BRAS  RE,PRTLINE                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        TYPE Z FAX REPORT  HEADSPECS AND HEADHOOK                              
***********************************************************************         
ZZHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
ZZHDHOOK NTR1                                                                   
         MVC   H1(10),=C'FAX REPORT'                                            
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),SVAGYNM                                                
         MVC   H2+97(33),SVAGYNM+33                                             
         MVC   H3+51(8),HDATE                                                   
         MVC   H2(7),SYSNAME                                                    
         MVC   H7(11),=C'DESTINATION'                                           
         MVC   H8(11),=C'-----------'                                           
         MVC   H7+29(18),=C'REPORT INFORMATION'                                 
         MVC   H8+29(18),=C'------------------'                                 
         MVC   H7+89(8),=C'  REF#  '                                            
         MVC   H8+89(8),=C'--------'                                            
         MVC   H7+99(5),=C'SENT '                                               
         MVC   H8+99(5),=C'-----'                                               
         MVC   H7+106(5),=C'DLVD '                                              
         MVC   H8+106(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TOTALS                                                                 
***********************************************************************         
*                                                                               
         USING EDFILD,R3                                                        
         USING SRECD,R4                                                         
PRNTOT   NMOD1 0,**PTOT**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
         LA    R3,SREDIREC                                                      
*                                                                               
         MVC   SVHDHOOK,HEADHOOK                                                
         MVC   SVSPECS,SPECS                                                    
         XC    HEADHOOK,HEADHOOK                                                
         XC    SPECS,SPECS                                                      
*                                                                               
         MVC   P1+41(22),=C'DELIVERIES RECEIVED - '                             
         LA    R3,P1                                                            
         EDIT  (2,DLVTOTAL),(5,66(R3)),ZERO=NOBLANK                             
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         OC    MISTOTAL,MISTOTAL                                                
         BZ    PT10                                                             
         MVC   P1+41(21),=C'NO NOTICE RECEIVED - '                              
         LA    R3,P1                                                            
         EDIT  (2,MISTOTAL),(5,66(R3))                                          
         BRAS  RE,MYSPOOL                                                       
*                                                                               
PT10     OC    CANTOTAL,CANTOTAL                                                
         BZ    PT20                                                             
         MVC   P1+41(12),=C'CANCELLED - '                                       
         LA    R3,P1                                                            
         EDIT  (2,CANTOTAL),(5,66(R3))                                          
         BRAS  RE,MYSPOOL                                                       
*                                                                               
PT20     OC    OFFTOTAL,OFFTOTAL   FOR REP                                      
         BZ    PT30                                                             
         CLI   PROFFTOT,C'N'                                                    
         BE    PRNTOTX                                                          
         MVC   P1+41(15),=C'OFFICE TOTAL - '                                    
         LA    R3,P1                                                            
         EDIT  (2,OFFTOTAL),(5,66(R3))                                          
         BRAS  RE,MYSPOOL                                                       
         B     PRNTOTX                                                          
*                                                                               
PT30     OC    SPTOTAL,SPTOTAL     FOR DARE ON REP                              
         BZ    PT40                                                             
         MVC   P1+41(19),=C'SALESPERSON TOTAL -'                                
         LA    R3,P1                                                            
         EDIT  (2,SPTOTAL),(5,66(R3))                                           
         BRAS  RE,MYSPOOL                                                       
         B     PRNTOTX                                                          
*                                                                               
PT40     TM    SVPRBIT,PRNOPRD     OPTION TO SKIP PRODUCT TOTAL                 
         BO    PT50                                                             
         CLI   PRPRDTOT,C'Y'                                                    
         BNE   PT50                                                             
         MVC   P1+41(16),=C'PRODUCT TOTAL - '                                   
         LA    R3,P1                                                            
         EDIT  (2,PRDTOTAL),(5,66(R3))                                          
         BRAS  RE,MYSPOOL                                                       
*                                                                               
PT50     TM    SVPRBIT,PRNOCLT     OPTION TO SKIP CLIENT TOT                    
         BO    PRNTOTX                                                          
         CLI   PRCLTOT,C'Y'                                                     
         BNE   PRNTOTX                                                          
*                                                                               
         TM    SVPRBIT,PRREQTOT    PRINT REQUESTOR TOTAL                        
         BNO   *+14                                                             
         MVC   P1+41(17),=C'REQUESTOR TOTAL -'                                  
         B     PT60                                                             
*                                                                               
         TM    SVPRBIT,PRBYRTOT    PRINT BUYER TOTAL                            
         BNO   *+14                                                             
         MVC   P1+41(13),=C'BUYER TOTAL -'                                      
         B     PT60                                                             
*                                                                               
         MVC   P1+41(15),=C'CLIENT TOTAL - '                                    
PT60     LA    R3,P1                                                            
         EDIT  (2,CLTTOTAL),(5,66(R3))                                          
         BRAS  RE,MYSPOOL                                                       
         XC    CLTTOTAL,CLTTOTAL                                                
*                                                                               
PRNTOTX  DS    0H                                                               
         TM    SVPRBIT,PRNOCLT     OPTION TO SKIP CLIENT TOT                    
         BNO   PRNTOTXX                                                         
         XC    CLTTOTAL,CLTTOTAL                                                
PRNTOTXX XC    PRCLTOT,PRCLTOT                                                  
         XC    PRPRDTOT,PRPRDTOT                                                
         XC    MISTOTAL,MISTOTAL                                                
         XC    DLVTOTAL,DLVTOTAL                                                
         XC    CANTOTAL,CANTOTAL                                                
         XC    PRDTOTAL,PRDTOTAL                                                
         XC    OFFTOTAL,OFFTOTAL                                                
         XC    SPTOTAL,SPTOTAL                                                  
         MVI   SVPRBIT,0                                                        
*        MVI   PRBIT,0                                                          
         MVC   HEADHOOK,SVHDHOOK                                                
         MVC   SPECS,SVSPECS                                                    
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        RECEIVER'S REPORTS                                                     
***********************************************************************         
*                                                                               
RCVMAIN  NMOD1 0,**RCVM**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
*                                                                               
         B     RCV100                   KATZ REPORTS DISABLED                   
*                                                                               
         USING RSRECD,R4                                                        
         CLC   =C'KATNY  ',RSRDEST      KATZ RECEIVER REPORT IS SPECIAL         
         BE    RCVKATZ                                                          
         CLC   =C'KATDE  ',RSRDEST                                              
         BE    RCVKATZ                                                          
         CLC   =C'KATCH  ',RSRDEST                                              
         BE    RCVKATZ                                                          
         CLC   =C'KATSF  ',RSRDEST                                              
         BE    RCVKATZ                                                          
         CLC   =C'KATLA  ',RSRDEST                                              
         BE    RCVKATZ                                                          
         CLC   =C'KATDA  ',RSRDEST                                              
         BE    RCVKATZ                                                          
         CLC   =C'KATAT  ',RSRDEST                                              
         BNE   RCV100              RECEIVER'S PRINTED REPORT                    
*                                                                               
RCVKATZ  XC    SVSEND,SVSEND                                                    
         MVI   ANYKATZ,C'Y'                                                     
         CLC   RSRDEST(3),SVDEST                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         XC    IDFORM,IDFORM                                                    
         MVC   IDFORM+8(2),RSRSEND                                              
         BRAS  RE,SENDID           GET SENDER'S NAME                            
         GOTO1 =A(GETAGYNM),DMCB,(RC),RR=RELO                                   
         BNE   EXIT                                                             
         MVC   SNDRNAME,ALNAME                                                  
         MVC   SENDCITY,ALCITY                                                  
         MVC   SVDEST,RSRDEST                                                   
         GOTO1 =A(GETSES),DMCB,(RC),RR=RELO                                     
         BNE   EXIT                                                             
         MVI   PQSW,1              SUPPRESS AUTO PRINTQ OPEN                    
*                                                                               
         CLI   FRSTKATZ,C'N'                                                    
         BE    RCVK10                                                           
         MVI   RPTCLASS,C'G'                                                    
         MVC   PQDESTID,=X'005A'   DDS                                          
         CLI   TRNPRTQ,C'Y'                                                     
         BNE   RCVK10                                                           
         GOTO1 =A(NEWQ),DMCB,(RC),RR=RELO                                       
RCVK10   MVI   KATTRN,C'Y'                                                      
         GOTO1 =A(KATZR),DMCB,(RC),RR=RELO                                      
         MVI   FRSTKATZ,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        RECEIVER'S REPORTS - NOT KATZ                                          
***********************************************************************         
*                                                                               
         USING EDFILD,R3                                                        
         USING RSRECD,R4                                                        
RCV100   LA    R3,RSREDREC                                                      
         CLC   RSRDEST,SVDEST      SAME RECEIVER?                               
         BE    RCV110                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   PRHDR,C'Y'          PRINT A NEW HEADER                           
         XC    IDFORM,IDFORM                                                    
         MVC   IDFORM,RSRDEST                                                   
         GOTO1 =A(GETAGYNM),DMCB,(RC),RR=RELO                                   
         BNE   EXIT                                                             
         MVC   SVPQID,PQDESTID             OF RECEIVER                          
         MVC   RCVRADR,SVAGYNM                                                  
*                                                                               
RCV110   CLC   RSRSEND,SVSEND      SAME SENDER?                                 
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XC    IDFORM,IDFORM                                                    
         MVC   IDFORM+8(2),RSRSEND                                              
         BRAS  RE,SENDID           GET SENDER'S ID                              
         XC    IDFORM,IDFORM                                                    
         MVC   IDFORM+8(2),RSRSEND                                              
         GOTO1 =A(GETAGYNM),DMCB,(RC),RR=RELO                                   
         BNE   EXIT                                                             
         MVC   SNDRNAME,ALNAME                                                  
         MVC   SENDCITY,ALCITY                                                  
*                                  GET AGENCY SE NUMBERS (SENDER'S)             
         GOTO1 =A(GETSES),DMCB,(RC),RR=RELO                                     
         BNE   EXIT                                                             
*                                                                               
         CLI   PRHDR,C'N'          PRINT HEADER?                                
         BE    RCV130                                                           
         MVI   PQSW,1              SUPPRESS AUTO PRINTQ OPEN                    
         MVC   PQDESTID,SVPQID                                                  
         MVI   RPTCLASS,C'G'                                                    
*                                                                               
         CLC   =C'DMB',EDFDEST     SPECIAL REPORT FOR DDS                       
         BNE   *+18                FOR ALL DMB ACTIVITY                         
         MVI   RPTCLASS,C'K'                                                    
         MVC   PQDESTID,=X'0011'   SJR                                          
         MVI   PRHDR,C'N'          DON'T NEED HEADER                            
*                                                                               
         CLI   TRNPRTQ,C'Y'                                                     
         BNE   RCV120                                                           
         GOTO1 =A(NEWQ),DMCB,(RC),RR=RELO                                       
*                                                                               
RCV120   CLI   PRHDR,C'N'          PRINT HEADER?                                
         BE    RCV130                                                           
         MVI   PRHDR,C'N'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P+4(5),=C'*HDR*'                                                 
         MVI   P+35,C'P'                                                        
         MVC   P+9(6),=C'EDICT='                                                
         MVC   P+15(10),EDFDEST                                                 
         MVC   P+39(10),EDFDEST                                                 
         BRAS  RE,MYSPOOL                                                       
         MVC   P(15),=CL15'++DDS EDICTTRN '                                     
         BRAS  RE,MYSPOOL                                                       
         MVI   FORCEHED,C'Y'                                                    
RCV130   MVC   SVSEND,RSRSEND                                                   
         MVC   SVDEST,RSRDEST                                                   
         GOTO1 =A(RCVPRINT),DMCB,(RC),RR=RELO                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        RECEIVER'S REPORT                                                      
***********************************************************************         
*                                                                               
RCVPRINT NMOD1 0,**RCVR**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
*                                                                               
         USING EDFILD,R3                                                        
         USING RSRECD,R4                                                        
         LA    R4,SREC                                                          
         LA    R3,RSREDREC                                                      
*                                                                               
         LA    R1,RCHDSPEC         HEADLINES                                    
         ST    R1,SPECS                                                         
         LA    R1,RCHDHOOK                                                      
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLI   RSRSYS,EDFSSPTQ     SPOT SYSTEM                                  
         BNE   EXIT                                                             
         CLC   SVSENUM,SELIST+2    SAME AS BEFORE?                              
         BE    RP50                SKIP OPEN                                    
*                                                                               
         L     R1,VUTL             RESET UTL                                    
         MVC   4(1,R1),SELIST+2    SPOT SE NUM                                  
         MVC   SVSENUM,4(R1)       SAVE LAST SE NUMBER                          
         LA    R2,OPENADDS                                                      
RP10     CLC   0(4,R2),=C'SPOT'                                                 
         BE    RP20                                                             
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   RP10                MUST FIND A MATCH                            
         B     NO                                                               
RP20     L     R3,8(R2)                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),(R2),(R3),IO,0                       
*                                                                               
RP50     MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         LA    R3,RSREDREC                                                      
*                                                                               
         MVC   TITLE,SPACES                                                     
         CLI   RSRTYPE,EDFTADDQ    DRAFT ORDER   ********                       
         BE    RCVDRAFT                                                         
         CLI   RSRTYPE,EDFTREQQ    AVAIL REQUEST ********                       
         BE    RCVAVAIL                                                         
         CLI   RSRTYPE,EDFTDMBQ    DMB&B SCHEDULE TRANSFER                      
         BE    RCVTRANS                                                         
         CLI   RSRTYPE,EDFTGOAQ    DMB&B GOAL TRANSFER                          
         BE    RCVTRANS                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SPOT TRANSFERS                                                         
***********************************************************************         
*                                                                               
RCVTRANS MVC   TITLE(14),=C'SPOT TRANSFERS'                                     
         MVI   RPTYPE,EDFTGOAQ                                                  
         MVC   MEDCODE,SPDAMED                                                  
         MVC   CLCLT,SPDACLT                                                    
         MVC   PRDCODE,SPDAPRD                                                  
         GOTO1 =A(SPAGYCLT),DMCB,(RC),RR=RELO                                   
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
         CLI   RSRTYPE,EDFTGOAQ    DMB&B GOAL TRANSFER                          
         BE    RTRANS10                                                         
         MVC   STACALL,SPDASTA                                                  
         MVC   BYRCODE,SPDABYR                                                  
         BRAS  RE,SPBUYER          GET BUYER NAME                               
         MVC   RPBUYER,BUYRNAME                                                 
         MVI   RPTYPE,EDFTDMBQ                                                  
         MVC   RPCMPGN,SPDACAM                                                  
         MVC   RPSTN,SPDASTA                                                    
RTRANS10 MVC   RPCLTNM,CLTNAME                                                  
         MVC   RPPRDNM,PRDNAME                                                  
         MVC   RPEST,SPDAEST                                                    
         CLC   SPDAEST,SPACES                                                   
         BNH   RTRANS20                                                         
         PACK  DUB2(8),SPDAEST(3)                                               
         CVB   R1,DUB2                                                          
         STC   R1,BINEST                                                        
         BRAS  RE,SPESTDT                                                       
         BRAS  RE,DATEFORM                                                      
         MVC   RPDATES,DATES                                                    
RTRANS20 BRAS  RE,TIMEFORM                                                      
         MVC   RPSENT,SENTIME                                                   
         MVC   RPDLVD,RCVTIME                                                   
         B     RX                                                               
         EJECT                                                                  
***********************************************************************         
*        SPOT DRAFT ORDERS                                                      
***********************************************************************         
*                                                                               
RCVDRAFT MVC   TITLE(9),=C'SPOT ADDS'                                           
         MVC   MEDCODE,SPEDMED                                                  
         MVC   CLCLT,SPEDCLT                                                    
         MVC   PRDCODE,SPEDPRD                                                  
         MVC   BYRCODE,SPEDBYR                                                  
         MVC   STACALL,SPEDSTA                                                  
         GOTO1 =A(SPAGYCLT),DMCB,(RC),RR=RELO                                   
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
         BRAS  RE,SPBUYER          GET BUYER NAME                               
         MVI   RPTYPE,EDFTADDQ                                                  
         MVC   RPCLTNM,CLTNAME                                                  
         MVC   RPPRDNM,PRDNAME                                                  
         MVC   RPEST,SPEDEST                                                    
         PACK  DUB2(8),SPEDEST(3)                                               
         CVB   R1,DUB2                                                          
         STC   R1,BINEST                                                        
         MVC   INDATES,SPEDFLST                                                 
         CLC   INDATES(3),SPACES                                                
         BNE   *+8                                                              
         BRAS  RE,SPESTDT                                                       
         BRAS  RE,DATEFORM                                                      
         MVC   RPDATES,DATES                                                    
         MVC   RPBUYER,BUYRNAME                                                 
         MVC   RPCMPGN,SPEDCAM                                                  
         MVC   RPSTN,SPEDSTA                                                    
         MVC   RPREQ,SPEDQUES                                                   
         BRAS  RE,TIMEFORM                                                      
         MVC   RPSENT,SENTIME                                                   
         MVC   RPDLVD,RCVTIME                                                   
         B     RX                                                               
         EJECT                                                                  
***********************************************************************         
*        SPOT AVAILS                                                            
***********************************************************************         
*                                                                               
RCVAVAIL MVC   TITLE(9),=C'SPOT ADDS'                                           
         MVC   MEDCODE,SPAVMED                                                  
         MVC   CLCLT,SPAVCLT                                                    
         MVC   PRDCODE,SPAVPRD                                                  
         MVC   BYRCODE,SPAVBUYR                                                 
         GOTO1 =A(SPAGYCLT),DMCB,(RC),RR=RELO                                   
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
         BRAS  RE,SPBUYER          GET BUYER NAME                               
         MVI   RPTYPE,EDFTREQQ                                                  
         MVC   RPCLTNM,CLTNAME                                                  
         MVC   RPPRDNM,PRDNAME                                                  
         MVC   RPEST,SPAVEST                                                    
         MVC   RPDATES,SPAVFLST                                                 
         MVC   RPBUYER,BUYRNAME                                                 
         MVC   RPREFN,SPAVREFN                                                  
         BRAS  RE,TIMEFORM                                                      
         MVC   RPSENT,SENTIME                                                   
         MVC   RPDLVD,RCVTIME                                                   
RX       BRAS  RE,MYSPOOL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        RECEIVER'S REPORT  HEADSPECS AND HEADHOOK                              
***********************************************************************         
*                                                                               
RCHDSPEC SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
RCHDHOOK NTR1                                                                   
         MVC   H1(17),=C'RECEIVER''S REPORT'                                    
         MVC   H1+41(29),=C'ELECTRONIC TRANSACTION REPORT'                      
         MVC   H2+41(29),=C'-----------------------------'                      
         MVC   H1+97(33),RCVRADR                                                
         MVC   H2+97(33),RCVRADR+33                                             
         MVC   H2(20),TITLE                                                     
         MVC   H3+51(8),HDATE                                                   
         MVC   H4(9),=C'SENDER   '                                              
         MVC   H4+10(6),SNDRID                                                  
         MVC   H4+18(33),SNDRNAME                                               
         MVC   H5+18(7),SENDCITY                                                
         MVC   H7(7),=C'CLIENT '                                                
         MVC   H8(7),=C'------ '                                                
         MVC   H7+21(7),=C'PRODUCT'                                             
         MVC   H8+21(7),=C'-------'                                             
         MVC   H7+42(3),=C'EST'                                                 
         MVC   H8+42(3),=C'---'                                                 
         MVC   H7+48(12),=C'FLIGHT DATES'                                       
         MVC   H8+46(17),=C'-----------------'                                  
         MVC   H7+64(1),=C'T'                                                   
         MVC   H8+64(1),=C'-'                                                   
         MVC   H7+67(5),=C'BUYER'                                               
         MVC   H8+67(5),=C'-----'                                               
         MVC   H7+92(7),=C'AV.REF#'                                             
         MVC   H8+92(7),=C'-------'                                             
         MVC   H7+100(5),=C'CMPGN'                                              
         MVC   H8+100(5),=C'-----'                                              
         MVC   H7+107(5),=C'STATN'                                              
         MVC   H8+107(5),=C'-----'                                              
         MVC   H7+114(3),=C'REQ'                                                
         MVC   H8+114(3),=C'---'                                                
         MVC   H7+119(5),=C'SENT '                                              
         MVC   H8+119(5),=C'-----'                                              
         MVC   H7+125(5),=C'DLVD '                                              
         MVC   H8+125(5),=C'-----'                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        KATZ' RECEIVER REPORT                                                  
***********************************************************************         
*                                                                               
KATZR    NMOD1 0,**KATZ**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
         USING RSRECD,R4                                                        
         LA    R3,RSREDREC                                                      
         USING EDFILD,R3                                                        
*                                                                               
         B     EXIT                KATZ REPORTS DISABLED                        
*                                                                               
         CLI   KHDR,C'Y'                                                        
         BE    KATZ100                                                          
         CLI   RSRSYS,EDFSSPTQ     SPOT SYSTEM                                  
         BNE   EXIT                                                             
         CLC   SVSENUM,SELIST+2    SAME AS BEFORE?                              
         BE    KATZ50              SKIP OPEN                                    
         L     R1,VUTL             RESET UTL                                    
         MVC   4(1,R1),SELIST+2    SPOT SE NUM                                  
         MVC   SVSENUM,4(R1)       SAVE LAST SE NUMBER                          
         LA    R2,OPENADDS                                                      
KATZ20   CLC   0(4,R2),=C'SPOT'                                                 
         BE    KATZ30                                                           
         LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BNE   KATZ20              MUST FIND A MATCH                            
         B     NO                                                               
KATZ30   L     R3,8(R2)                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),(R2),(R3),IO,0                       
KATZ50   LA    R3,RSREDREC                                                      
         MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XC    FILENAME,FILENAME                                                
         EJECT                                                                  
***********************************************************************         
*        KATZ' RECEIVER REPORT - HEADER PAGE                                    
***********************************************************************         
*                                                                               
KATZ100  CLI   KPRTHDR,C'N'                                                     
         BE    KATZ200                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+4(5),=C'*HDR*'                                                 
         MVI   P+35,C'P'                                                        
         MVC   P+9(6),=C'EDICT='                                                
         MVC   P+15(10),=C'KATNY     '                                          
         MVC   P+39(10),=C'KATNY     '                                          
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         MVC   P(15),=CL15'++DDS EDICTTRN '                                     
         BRAS  RE,MYSPOOL                                                       
         MVC   P(15),=CL15'++DDS SPADDHDR '                                     
         MVC   P+15(2),=C'TR'                                                   
         BRAS  RE,MYSPOOL                                                       
         MVC   P(15),=CL15'++DDS SPADDTLR '                                     
         BRAS  RE,MYSPOOL                                                       
*                                                                               
***********************************************************************         
*        KATZ' RECEIVER REPORT - PRINT LINES                                    
***********************************************************************         
*                                                                               
KATZ200  CLI   KATTRN,C'Y'                                                      
         BNE   KATZ210                                                          
         CLI   KPRTHDR,C'N'                                                     
         BE    KATZ210                                                          
         MVC   KDATE,HDATE                                                      
         MVI   KTYPE,C'T'          TRANS REPORT                                 
         MVC   KSENDER(3),=C'DDS'                                               
         CLI   KHDR,C'Y'                                                        
         BE    KATZX                                                            
         BRAS  RE,MYSPOOL                                                       
*                                                                               
KATZ210  MVC   KDATE,HDATE                                                      
         BRAS  RE,TIMEFORM                                                      
         MVC   KSENTIME,SENTIME                                                 
         MVI   KPRTHDR,C'N'                                                     
         CLI   RSRTYPE,EDFTADDQ    DRAFT ORDER                                  
         BE    KATZ300                                                          
         CLI   RSRTYPE,EDFTREQQ    AVAIL REQUEST                                
         BE    KATZ400                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        KATZ' RECEIVER REPORT - PRINT LINES - DRAFT ORDERS                     
***********************************************************************         
*                                                                               
KATZ300  CLC   SPEDCITY,SPACES     FORMAT SENDER'S ID (SPLIT CITY)              
         BNH   KATZ310                                                          
         MVC   KSENDER,SNDRID                                                   
         MVC   KSENDERC,SPEDCITY                                                
         B     KATZ350                                                          
*                                                                               
KATZ310  LA    R2,SNDRID                                                        
KATZ320  CLI   0(R2),X'40'                                                      
         BE    KATZ330                                                          
         LA    R2,1(R2)                                                         
         B     KATZ320                                                          
KATZ330  SH    R2,=H'2'                                                         
         MVC   KSENDERC,0(R2)                                                   
         MVC   0(2,R2),SPACES                                                   
         MVC   KSENDER,SNDRID                                                   
*                                                                               
KATZ350  MVC   MEDCODE,SPEDMED                                                  
         MVC   CLCLT,SPEDCLT                                                    
         MVC   PRDCODE,SPEDPRD                                                  
         MVC   BYRCODE,SPEDBYR                                                  
         MVC   STACALL,SPEDSTA                                                  
         MVC   STACALL+4(1),MEDCODE                                             
         MVC   MKTNUM,SPEDMKT                                                   
         GOTO1 =A(SPAGYCLT),DMCB,(RC),RR=RELO                                   
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
         GOTO1 =A(SPMKTRTG),DMCB,(RC),RR=RELO                                   
         MVC   LKEY,=H'13'         RESET                                        
         BRAS  RE,SPBUYER          GET BUYER NAME                               
*                                                                               
         MVI   KTYPE,EDFTADDQ                                                   
         MVC   KCLTNM,CLTNAME                                                   
         MVC   KPRDNM,PRDNAME                                                   
         MVC   KDESTCY,EDFDEST+3                                                
         MVC   KAGYREF,SPACES                                                   
         MVC   KAGYREF(10),SPEDMED                                              
         MVC   KSTATION,SPEDSTA                                                 
         MVC   KMARKET,RSMKT                                                    
         MVC   KRATING,RS                                                       
         PACK  DUB2(8),SPEDEST(3)                                               
         CVB   R1,DUB2                                                          
         STC   R1,BINEST                                                        
         MVC   INDATES,SPEDFLST                                                 
         CLC   INDATES(3),SPACES                                                
         BNE   *+8                                                              
         BRAS  RE,SPESTDT                                                       
         BRAS  RE,DATEFORM                                                      
         MVC   KDATES,DATES                                                     
         MVC   KBUYER,BUYRNAME                                                  
         B     KATZX                                                            
         EJECT                                                                  
***********************************************************************         
*        KATZ' RECEIVER REPORT - PRINT LINES - AVAILS                           
***********************************************************************         
*                                                                               
KATZ400  CLC   SPAVCITY,SPACES     FORMAT SENDER'S ID (SPLIT CITY)              
         BNH   KATZ410                                                          
         MVC   KSENDER,SNDRID                                                   
         MVC   KSENDERC,SPAVCITY                                                
         B     KATZ450                                                          
*                                                                               
KATZ410  LA    R2,SNDRID                                                        
KATZ420  CLI   0(R2),X'40'                                                      
         BE    KATZ430                                                          
         LA    R2,1(R2)                                                         
         B     KATZ420                                                          
KATZ430  SH    R2,=H'2'                                                         
         MVC   KSENDERC,0(R2)                                                   
         MVC   0(2,R2),SPACES                                                   
         MVC   KSENDER,SNDRID                                                   
*                                                                               
KATZ450  MVC   MEDCODE,SPAVMED                                                  
         MVC   CLCLT,SPAVCLT                                                    
         MVC   PRDCODE,SPAVPRD                                                  
         MVC   BYRCODE,SPAVBUYR                                                 
         GOTO1 =A(SPAGYCLT),DMCB,(RC),RR=RELO                                   
         BRAS  RE,GETPRDNM         PRODUCT NAME                                 
         BRAS  RE,SPBUYER          GET BUYER NAME                               
         MVI   KTYPE,EDFTREQQ      AVAIL REQUEST                                
         MVC   KCLTNM,CLTNAME                                                   
         MVC   KPRDNM,PRDNAME                                                   
         MVC   KDESTCY,EDFDEST+3   SKIP THE 'KAT'                               
         MVC   KAGYREF,SPACES                                                   
         MVC   KAGYREF(10),SPAVMED                                              
         MVC   KAVREF,SPAVREFN                                                  
         MVC   KDATES,SPAVFLST                                                  
         MVC   KBUYER,BUYRNAME                                                  
*                                                                               
KATZX    MVI   KHDR,C'N'                                                        
         BRAS  RE,MYSPOOL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADDS USAGE REPORT FOR KARI                                             
***********************************************************************         
*                                                                               
KARI     NMOD1 0,**KARI**                                                       
*           - NEVER USE R7 IN ANY NMOD IN THIS WHOLE PROGRAM                    
         L     RC,0(R1)                                                         
         USING SRECD,R4                                                         
*                                                                               
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         TM    KBIT,PQOPEN         PRINT QUEUE ALREADY OPEN                     
         BO    K20                                                              
*                                                                               
         MVI   PRCLTOT,C'Y'        PRINT TOTALS OF PREV REPORT                  
         GOTO1 =A(PRNTOT),DMCB,(RC),RR=RELO                                     
         MVC   SVAGY,SRAGY                                                      
         CLI   TRNPRTQ,C'Y'                                                     
         BNE   K10                                                              
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         BRAS  RE,MYSPOOL                                                       
*                                                                               
         MVI   PQSW,1              SUPPRESS AUTO PRINTQ OPEN                    
         MVI   RPTCLASS,C'K'                                                    
         MVC   PQDESTID,=X'0011'   SJR                                          
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK         EXTENDED SPOOLKEY (128 BYTES)                
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVC   PLDESC,=CL11'XACT REP'                                           
         OI    GENSTAT3,NOCLRSPK   GENCON DON'T CLEAR SPOOLKEY                  
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'12'                                                   
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,SPUINIT    USER VALUES PRESENT                          
         DROP  R1                                                               
*                                                                               
         L     R1,VREMOT                                                        
         USING REMOTED,R1                                                       
         MVI   REMOTCPY,1                                                       
         MVC   REMOTCLS,RPTCLASS                                                
         MVC   REMOTJID,=C'ITR'    DDS TRANSACTION REPORT                       
         MVI   REMOTSYS,X'02'                                                   
         MVC   REMOTDST,PQDESTID                                                
         DROP  R1                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
K10      OI    KBIT,PQOPEN                                                      
         EJECT                                                                  
***********************************************************************         
*        HEADLINES                                                              
***********************************************************************         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+30(19),=C'ADDS SUMMARY REPORT'                                 
         MVC   P2+36(8),HDATE                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+20(37),=C'AGENCY          ORDERS         AVAILS'               
         MVC   P2+20(37),=C'-------------------------------------'              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
***********************************************************************         
*        PRINT LINE                                                             
***********************************************************************         
*                                                                               
K20      CLC   SRAGY,SVAGY                                                      
         BE    K30                 EQUAL THEN JUST UPDATE TOTALS                
*                                                                               
K25      XC    IDFORM,IDFORM                                                    
         MVC   IDFORM+8(2),SVAGY                                                
         BRAS  RE,SENDID           GET AGENCY ID                                
         MVC   KAGY,SNDRID                                                      
         MVC   SVAGY,SRAGY                                                      
*                                                                               
         EDIT  (2,ORDCNT),(5,KORDERS),ZERO=NOBLANK                              
         EDIT  (2,AVACNT),(5,KAVAILS),ZERO=NOBLANK                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LH    R1,ORDCNT           UPDATE AND RESET TOTALS                      
         LH    R2,ORDCNTOT                                                      
         AR    R2,R1                                                            
         STH   R2,ORDCNTOT                                                      
         LH    R1,AVACNT                                                        
         LH    R2,AVACNTOT                                                      
         AR    R2,R1                                                            
         STH   R2,AVACNTOT                                                      
         XC    ORDCNT,ORDCNT                                                    
         XC    AVACNT,AVACNT                                                    
         CLI   SRCODE,C'0'         END OF ADDS RECS                             
         BE    K100                                                             
         EJECT                                                                  
***********************************************************************         
*        UPDATE TOTALS                                                          
***********************************************************************         
*                                                                               
K30      CLI   SRMED,EDFTADDQ      SRMED IS ACTUALLY TYPE                       
         BE    K40                 ORDERS                                       
         CLI   SRMED,EDFTREQQ                                                   
         BNE   EXIT                                                             
*                                                                               
         LH    R1,AVACNT           AVAILS                                       
         LA    R1,1(R1)                                                         
         STH   R1,AVACNT                                                        
         B     EXIT                                                             
*                                                                               
K40      LH    R1,ORDCNT           ORDERS                                       
         LA    R1,1(R1)                                                         
         STH   R1,ORDCNT                                                        
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        FINAL TOTALS                                                           
***********************************************************************         
*                                                                               
K100     MVC   P1+20(37),=C'-------------------------------------'              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   KAGY,=C'TOTALS'                                                  
         EDIT  (2,ORDCNTOT),(5,KORDERS),ZERO=NOBLANK                            
         EDIT  (2,AVACNTOT),(5,KAVAILS),ZERO=NOBLANK                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ISFAX#                                                                 
***********************************************************************         
*NTRY:   R1=A(FAX#)                                                             
*EXIT:   SET CC = EQ/NE                                                         
*                                                                               
ISFAX#   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,10                                                            
ISFX20   CLI   0(R1),C'A'                                                       
         BL    *+12                                                             
         CLI   0(R1),C'Z'                                                       
         BNH   ISFXN                                                            
*                                                                               
         LA    R1,1(R1)                                                         
         BCT   RE,ISFX20                                                        
*                                                                               
ISFXY    J     YES                                                              
ISFXN    J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*DDEDIEZERR {EZMSGTAB}                                                          
         PRINT OFF                                                              
       ++INCLUDE DDEDIEZERR                                                     
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
*PREFIX=SP$                                                                     
       ++INCLUDE SPGENPRD                                                       
*PREFIX=                                                                        
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPADBUYER                                                      
         EJECT                                                                  
       ++INCLUDE SPTRPRH                                                        
         EJECT                                                                  
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
         EJECT                                                                  
       ++INCLUDE ACGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC3D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SYSD STORAGE                                                           
***********************************************************************         
*                                                                               
MYAREAD  DSECT                                                                  
AMASTD   DS    A                   A(MASTER)                                    
VUTL     DS    A                   A(UTL)                                       
VREMOT   DS    A                   A(REMOTEC)                                   
VDADDS   DS    A                   A(DADDS)                                     
VOPENPQ  DS    A                   A(OPENPQ)                                    
*                                                                               
EDICTFL  DS    A                   A(EDICT FILE)                                
DUMMY    DS    F                                                                
DUB2     DS    D                                                                
AEDCTBLK DS    A                   A(EDICT BLOCK)                               
EDCTFDSK DC    F'0'                EDICTFIL DISK ADDRESS                        
EDCTFRPT DS    H                   EDICTFIL PHYSICAL RECORDS PER TRACK          
EDCTFTPD DS    H                   EDICTFIL TRACKS PER DAY                      
EDCTRPB  DS    H                   EDICTFIL LOGICAL RECORDS PER BLOCK           
EDCTFRCL DS    H                   EDICTFIL LOGICAL RECORD LENGTH               
*                                                                               
EDCTFLST DS    H                   LAST TRACK FOR GIVEN DAY                     
LASTRAC  DS    H                                                                
FIRSTRAC DS    H                   FIRST TRAC FOR GIVEN DAY                     
RECLEN   DS    H                                                                
RECOUNT  DS    H                                                                
*                                                                               
SVHDHOOK DS    F                                                                
SVSPECS  DS    F                                                                
*                                                                               
BINEST   DS    XL1                 BINARY ESTIMATE                              
BMONTH   DS    XL1                 MONTH IN BINARY                              
SYSID    DS    XL1                 SYSTEM ID (REP/ADV/TEST/MEL)                 
LASTBLK  DS    XL1                 LAST BLOCK NUMBER                            
FLAGS    DS    XL1                                                              
DAYSTART DS    XL3                 WHAT DAY?                                    
NAME     DS    CL8                 USER ID NAME                                 
IDNUM    DS    H                   USER ID NUM                                  
SELIST   DS    XL5                 LIST OF SE NUMBERS                           
SREC     DS    CL(SRLEN)           SORT RECORD (58+256)                         
DATE     DS    CL8                                                              
STDATE   DS    CL8                                                              
ENDATE   DS    CL8                                                              
STDATEN  DS    CL6                                                              
ENDATEN  DS    CL6                                                              
HDATE    DS    CL8                 HEADLINE DATE                                
COMPDATE DS    CL2                 COMPRESSED DATE                              
TITLE    DS    CL20                                                             
*                                                                               
SVSENUM  DS    CL1                 SAVED SE NUM                                 
SVAGYID  DS    XL2                 SAVED AGENCY ID                              
SVAGYNM  DS    CL66                AGENCY FULL NAME AND ADDRESS                 
RCVRADR  DS    CL66                AGENCY FULL NAME AND ADDRESS                 
SVAGY    DS    CL2                 SAVED AGENCY                                 
SVMED    DS    CL1                 SAVED MEDIA                                  
SVSYSTEM DS    CL1                 SAVED SYSTEM                                 
SVCLT    DS    CL3                 SAVED CLIENT                                 
SVPRD    DS    CL3                 SAVED PRODUCT                                
SVPTN    DS    CL3                 SAVED PARTNER                                
SVREQST  DS    CL3                 SAVED REQUESTOR                              
SVTYPE   DS    CL1                 SAVED TYPE                                   
SVJOB    DS    CL6                                                              
SVVEND   DS    CL14                                                             
SVSEND   DS    CL2                 SAVED SENDER ID NUM                          
*                                                                               
NEWAGY   DS    CL2                 NEW AGENCY                                   
NEWMED   DS    CL1                 NEW MEDIA                                    
NEWCLT   DS    CL3                 NEW CLIENT                                   
NEWPRD   DS    CL3                 NEW PRODUCT                                  
NEWPTN   DS    CL3                 NEW PARTNER                                  
NEWREQST DS    CL3                 NEW REQUESTOR                                
NEWTYPE  DS    CL1                 NEW TYPE                                     
*                                                                               
FIRSTLIN DS    CL1                 FIRSTLINE  Y/N                               
POWCODE  DS    H                   AGENCY POWER CODE                            
AMCODE   DS    CL1                 A/M CODE                                     
SYSNAME  DS    CL8                 SYSTEM NAME                                  
MEDNAME  DS    CL10                MEDIA NAME                                   
CLTCODE  DS    CL2                 PACKED CLIENT CODE                           
CLTNAME  DS    CL36                CLIENT NAME                                  
PRDCODE  DS    CL3                 PRODUCT CODE                                 
PRDNAME  DS    CL20                PRODUCT NAME                                 
PUBNM    DS    CL20                PUB NAME                                     
PTNCODE  DS    CL3                 PARTNER CODE                                 
PTNNAME  DS    CL20                PARTNER NAME                                 
BUYRNAME DS    CL24                BUYER NAME                                   
JOBNAME  DS    CL20                JOB NAME                                     
VENDNAME DS    CL20                VENDOR NAME                                  
PRDEXP   DS    CL7                                                              
CLIWORD  DS    CL9                                                              
JOBWORD  DS    CL8                                                              
JOBUNDER DS    CL8                                                              
ANAME    DS    CL33                AGENCY NAME                                  
AADDR    DS    CL33                AGENCY ADDRESS                               
MKTNUM   DS    CL4                 MARKET NUMBER                                
MKNAME   DS    CL24                MARKET NAME                                  
MSNUM    DS    H                   MARKET STATION NUMBER                        
MRTG     DS    CL1                 RATING SERVICE OVERRIDE                      
HOUSE    DS    CL6                 HOUSE                                        
HOUSENM  DS    CL24                HOUSE NAME                                   
PROGNAME DS    CL16                PROGRAM NAME                                 
PACKPUB  DS    CL6                 PACKED PUB/ZN/ED                             
DATES    DS    CL17                FLIGHT DATES                                 
INDATES  DS    CL12                DATES TO BE PASSED TO ROUTINE                
CONTACT  DS    CL24                CONTACT                                      
SENTIME  DS    CL5                 SENT TIME                                    
RCVTIME  DS    CL5                 RECEIVED TIME                                
RECCT    DS    H                   RECS READ FROM DAVID E'S FILE                
SRTCT    DS    H                   SORTED RECS                                  
MISTOTAL DS    H                   MISSING NOTICES                              
DLVTOTAL DS    H                   DELIVERED NOTICES                            
CANTOTAL DS    H                   CANCELLED NOTICES                            
TRNTOTAL DS    H                   TOTAL TRANSACTIONS                           
CLTTOTAL DS    H                   CLIENT TOTAL                                 
PRDTOTAL DS    H                   PRODUCT TOTAL                                
OFFTOTAL DS    H                   OFFICE  TOTAL                                
SPTOTAL  DS    H                   SALESPERSON TOTAL                            
BYRTOTAL DS    H                   BUYER TOTAL                                  
TOT      DS    CL1                 Y/N                                          
*                                                                               
PROFFTOT DS    CL1                                                              
PRCLTOT  DS    CL1                                                              
PRPRDTOT DS    CL1                 PRINT PRODUCT TOTAL  Y/N                     
SVPRBIT  DS    XL1                 PRINT OPTIONS                                
PRBIT    DS    XL1                 PRINT OPTIONS                                
PRNOCLT  EQU   X'80'               NO CLIENT TOTAL                              
PRNOPRD  EQU   X'40'               NO PRODUCT TOTAL                             
PRREQTOT EQU   X'20'               PRINT REQUESTOR TOTAL                        
PRBYRTOT EQU   X'10'               PRINT BUYER TOTAL                            
*                                                                               
NEWQUE   DS    CL1                 NW PRINT QUEUE  Y/N                          
FIRSTIME DS    CL1                 FIRST TIME  Y/N                              
KPRTHDR  DS    CL1                 KATZ PRINT HEADER  Y/N                       
KHDR     DS    CL1                 KATZ  Y/N                                    
KATTRN   DS    CL1                 KATZ TRANSACTION RPT  Y/N                    
ANYKATZ  DS    CL1                 ANY KATZ REPORT YET  Y/N                     
REPSYSN  DS    XL1                 REPORT TYPE SYSTEM NUMBER                    
REPJID   DS    CL3                 REPORT JOB ID                                
IDFORM   DS    CL10                PASSED TO ID RECORD KEY                      
SVDEST   DS    CL16                SAVED DEST FOR RECEIVER'S REPORT             
PQDESTID DS    XL2                 FOR PRINT QUEUE DESTINATION                  
SVPQID   DS    XL2                 FOR PRINT QUEUE DESTINATION                  
MEDCODE  DS    CL1                 MEDIA                                        
CLCLT    DS    CL3                 CLIENT                                       
STACALL  DS    CL5                 STATION CALL LETTERS                         
BYRCODE  DS    CL3                 BUYER CODE                                   
RPTCLASS DS    CL1                 REPORT CLASS                                 
SNDRID   DS    CL6                 SENDERS ID                                   
SENDCITY DS    CL7                 SENDER'S CITY                                
ALCITY   DS    CL7                 ALPHA CITY                                   
SNDRNAME DS    CL33                SENDERS NAME                                 
ALNAME   DS    CL33                ALPHA NAME                                   
FIRSTSEN DS    CL1                 FLAG FOR FIRST SENDERS REPORT                
RTGSRV   DS    CL1                 RATING SERVICE                               
RSMKT    DS    CL4                 RATING SERVICE MARKET                        
RS       DS    CL1                 RATING SERVICE (A/N)                         
REPEDI   DS    CL1                 READING REP EDICT FILE (Y/N)                 
*                                                                               
ORDCNT   DS    H                                                                
AVACNT   DS    H                                                                
ORDCNTOT DS    H                                                                
AVACNTOT DS    H                                                                
*                                                                               
EMBITS   DS    X                                                                
KBIT     DS    X                                                                
KDUMMY   EQU   X'80'                                                            
PQOPEN   EQU   X'40'                                                            
*                                                                               
PRDHEAD  DS    CL1                 IS PRODUCT IN HEADLINES  Y/N                 
SVPRDHD  DS    CL1                 SAVED                                        
FRSTKATZ DS    CL1                 FIRST KATZ LINE  Y/N                         
PRHDR    DS    CL1                 PRINT HEADER                                 
SPCODE   DS    CL3                 SALESPERSON CODE                             
SALNAME  DS    CL20                REP SALEPERSON NAME                          
OFFNM    DS    CL20                REP OFFICE NAME                              
OFFCD    DS    CL2                 REP OFFICE CODE                              
NEWOFF   DS    CL2                 REP OFFICE CODE                              
SVOFF    DS    CL2                 REP OFFICE CODE                              
SVSPCODE DS    CL3                 REP SAVED SALESPERSON CODE                   
SVREP    DS    CL2                 REP REP CODE                                 
RAGYNM   DS    CL20                REP AGENCY NAME                              
ADVNAME  DS    CL20                REP ADVERTISER NAME                          
SAVELINE DS    CL132               SAVED PRINT LINE                             
SCANBLK  DS    CL160               SPACE FOR SCANNER                            
*                                                                               
PQOVTAB  DS    CL100               PQ OVERRIDE TABLE                            
PQOVTABX DS    0C                                                               
         DS    CL100                                                            
*                                                                               
DMWK     DS    12D                                                              
SETABLBL DS    CL16                                                             
SETAB    DC    XL256'00'           TABLE OF OPENED SE NUMS                      
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT SORT RECORD DSECT                                      
***********************************************************************         
*                                                                               
*****WARNING!!!! IF THIS VALUE IS INCREASED ABOVE 256 YOU MUST CHANGE           
*****CERTAIN EXECUTED MVC'S IN THIS PROGRAM TO MVCL'S--DO A FIND ON             
*****THE WORD 'WARNING'                                                         
EDILEN   EQU   256                 MAXIMUM EDICT RECORD LENGTH                  
*                                                                               
SRECD    DSECT                                                                  
SRCODE   DS    CL1                 S=SENDERS  R=RECEIVERS REPORT                
SRAGY    DS    XL2                 AGENCY NUMBER OR REP FFOR SENDERS            
SRSYS    DS    CL1                 SYSTEM                                       
SRMED    DS    CL1                 MEDIA                                        
SRCLT    DS    CL3                 CLIENT                                       
SRAPPL   DS    CL50                DEPENDS ON TYPE                              
SRTKEYLN EQU   *-SRECD             SORT KEY LENGTH                              
SREDIREC DS    CL(EDILEN)          EDICT RECORD                                 
SRLEN    EQU   *-SRECD             SORT RECORD LENGTH                           
*                                                                               
         ORG   SRAPPL              SPOT                                         
SRSCODE  DS    CL1                 SO ADD ARE SEPARATE FROM TRAFFIC             
SRADDS   DS    0C                                                               
         ORG   SRADDS              SPOT ADDS                                    
SRSATYPE DS    CL1                 TYPE                                         
SRSAPRD  DS    CL3                 PRODUCT                                      
SRSADEST DS    CL17                DESTINATION                                  
SRSATIME DS    XL2                 SENT TIME PWOS                               
*                                                                               
         ORG   SRADDS              SPOT ADDS CONFIMATION OF PURCHASE            
SRSXTYPE DS    CL1                 TYPE                                         
SRSXCLT  DS    CL3                 CLIENT                                       
SRSXPRD  DS    CL3                 PRODUCT                                      
SRSXDEST DS    CL17                DESTINATION                                  
SRSXTIME DS    XL2                 SENT TIME PWOS                               
*                                                                               
         ORG   SRADDS              SPOT ADDS INVOICE CONTROL REPORT             
SRSVTYPE DS    CL1                 TYPE                                         
SRSVCLT  DS    CL3                 CLIENT                                       
SRSVDEST DS    CL17                DESTINATION                                  
SRSVTIME DS    XL2                 SENT TIME PWOS                               
*                                                                               
         ORG   SRADDS              SPOT TRAFFIC                                 
SRSTPRD  DS    CL3                 PRODUCT                                      
SRSTPTN  DS    CL3                 PARTNER                                      
SRSTTYPE DS    CL1                 TYPE                                         
SRSTDEST DS    CL17                DESTINATION                                  
SRSTTIME DS    XL2                 SENT TIME PWOS                               
*                                                                               
         ORG   SRAPPL              PRINT                                        
SRPPRD   DS    CL3                 PRODUCT                                      
SRPJOB   DS    CL6                 JOB CODE                                     
SRPPUB   DS    CL17                PUB NUMBER                                   
SRPDEST  DS    CL17                DESTINATION                                  
SRPTIME  DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              ACC                                          
SRAPRD   DS    CL3                 PRODUCT                                      
SRAJOB   DS    CL6                 JOB CODE                                     
SRAONUM  DS    CL6                 ORDER NUMBER                                 
SRADEST  DS    CL17                DESTINATION                                  
SRATIME  DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              NET                                          
SRNPRD   DS    CL3                 PRODUCT                                      
SRNTYPE  DS    CL1                 TYPE                                         
SRNNET   DS    CL4                 NETWORK                                      
SRNTIME  DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              NET DUNNING LETTER                           
SRNVPRD  DS    CL11                PRODUCT                                      
SRNVSTN  DS    CL5                 STATION                                      
SRNVTIME DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              NET                                          
SRNPPRG  DS    CL2                 PROGRAM REQUESTED                            
SRNPNET  DS    CL4                 NETWORK                                      
SRNPEST  DS    CL3                 ESTIMATE                                     
SRNPPAK  DS    CL3                 PACKAGE                                      
SRNPTIME DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              REP                                          
SRROFF   DS    CL2                 OFFICE                                       
SRRSTN   DS    CL5                 STATION                                      
SRRTYP   DS    CL1                 TYPE                                         
SRRSLP   DS    CL3                 SALESPERSON                                  
SRRTIME  DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              REP - TYPE D                                 
SRDOFF   DS    CL2                 OFFICE - BLANK                               
SRDTYPE  DS    CL1                 EDICT TYPE                                   
SRDSTN   DS    CL5                 STATION                                      
SRDETRN  DS    CL12                ETRAN NUMBER                                 
SRDSTART DS    CL6                 START DATE                                   
SRDEND   DS    CL6                 END DATE                                     
SRDTIME  DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              ADV DARE                                     
SRDABYR  DS    CL3                 BUYER                                        
SRDAREP  DS    CL8                 REP DESTINATION                              
SRDASTN  DS    CL5                 STATION                                      
SRDASP   DS    CL3                 SALESPERSON                                  
SRDATIME DS    XL2                 SENT TIME                                    
*                                                                               
         ORG   SRAPPL              REP DARE                                     
SRDRSP   DS    CL3                 SALESPERSON                                  
SRDRSTN  DS    CL5                 STATION                                      
SRDRAGY  DS    CL5                 DARE AGENCY                                  
SRDRTIME DS    XL2                 SENT TIME                                    
*                                                                               
*                                                                               
***********************************************************************         
*        RECEIVER'S REPORT SORT RECORD DSECT                                    
***********************************************************************         
*                                                                               
RSRECD   DSECT                                                                  
RSRCODE  DS    CL1                 R=RECEIVER'S                                 
RSRDEST  DS    CL16                THE RECEIVER                                 
RSRSEND  DS    CL2                 SENDER'S USER ID                             
RSRSYS   DS    CL1                 SYSTEM                                       
RSRTYPE  DS    CL1                 TYPE                                         
RSRSENT  DS    CL2                                                              
         DS    CL35                TO PUT EDICT REC IN SAME SPOT                
RSREDREC DS    CL(EDILEN)          THE EDICT RECORD                             
         EJECT                                                                  
***********************************************************************         
*        DDEDICTFIL                                                             
***********************************************************************         
*                                                                               
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE DSECTS                                                      
***********************************************************************         
*                                                                               
SPOOLD   DSECT                                                                  
***********************************************************************         
*        SPECIAL ADDS SUMMARY REPORT FOR KARI                                   
***********************************************************************         
         ORG   P                                                                
         DS    CL20                                                             
KAGY     DS    CL6                 AGENCY ID                                    
         DS    CL10                                                             
KORDERS  DS    CL5                 NUMBER OF ORDERS                             
         DS    CL10                                                             
KAVAILS  DS    CL5                 NUMBER OF AVAILS                             
                                                                                
***********************************************************************         
*        KATZ RECEIVER'S REPORT - PRINT LINE                                    
***********************************************************************         
         ORG   P                                                                
KATRCV   DS    0CL132              KATZ' PRINT LINE #1 (LEN=82)                 
KTYPE    DS    CL1                 A=AVAIL                                      
*                                  O=ORDER                                      
*                                  T=TRANSACTION REPORT                         
*                                  D=DISCREPANCY REPORT (UNWRITTEN)             
KDATE    DS    CL8                 DATE (MMMDD/YY)                              
KSENTIME DS    CL5                 SENT TIME (HH:MM)                            
KSENDER  DS    CL6                 SENDER'S ID                                  
KSENDERC DS    CL2                 SENDER'S ID CITY CODE                        
KDESTCY  DS    CL2                 DESTINATION ID CITY CODE                     
KSTATION DS    CL5                 STATION                                      
KMARKET  DS    CL4                 MARKET CODE                                  
KRATING  DS    CL1                 RATING SERVICE (ARB OR NSI)                  
KAVREF   DS    CL7                 AVAIL REFERENCE NUMBER                       
KDATES   DS    CL17                FLIGHT DATES (MMMDD/YY-MMMDD/YY)             
KBUYER   DS    CL24                BUYER NAME                                   
         ORG   P2                                                               
KATRCV2  DS    0CL132              KATZ' PRINT LINE #2 (LEN=43)                 
KCLTNM   DS    CL20                CLIENT NAME                                  
KPRDNM   DS    CL20                PRODUCT NAME                                 
KAGYREF  DS    CL12                AGENCY REFERENCE NUMBER                      
         EJECT                                                                  
***********************************************************************         
*        RECEIVER'S REPORT - PRINT LINE                                         
***********************************************************************         
*                                                                               
         ORG   P                                                                
RPCLTNM  DS    CL20                                                             
         DS    CL1                                                              
RPPRDNM  DS    CL20                                                             
         DS    CL1                                                              
RPEST    DS    CL3                                                              
         DS    CL1                                                              
RPDATES  DS    CL17                                                             
         DS    CL1                                                              
RPTYPE   DS    CL1                                                              
         DS    CL2                                                              
RPBUYER  DS    CL24                                                             
         DS    CL1                                                              
RPREFN   DS    CL7                                                              
         DS    CL1                                                              
RPCMPGN  DS    CL5                                                              
         DS    CL2                                                              
RPSTN    DS    CL5                                                              
         DS    CL2                                                              
RPREQ    DS    CL3                                                              
         DS    CL2                                                              
RPSENT   DS    CL5                                                              
         DS    CL1                                                              
RPDLVD   DS    CL5                                                              
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT TRAFFIC                            
***********************************************************************         
*                                                                               
         ORG   P                                                                
PSTTYPE  DS    CL3                 SPOT TRAFFIC                                 
         DS    CL1                                                              
PSTDEST  DS    CL43                                                             
         DS    CL2                                                              
PSTFNUM  DS    CL10                                                             
         DS    CL1                                                              
PSTEST   DS    CL3                                                              
         DS    CL2                                                              
PSTDATES DS    CL17                                                             
         DS    CL2                                                              
PSTCONT  DS    CL24                                                             
         DS    CL1                                                              
PSTEZNUM DS    CL8                                                              
         DS    CL2                                                              
PSTSENT  DS    CL5                                                              
         DS    CL2                                                              
PSTRCVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PSTDEST-P)                                                    
PSTERR   DS    CL24                                                             
*                                                                               
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT ADDS AVAILS                        
***********************************************************************         
*                                                                               
         ORG   P                                                                
PSAPRD   DS    CL3                 SPOT ADDS AVAILS                             
PSAHYPH  DS    CL1                                                              
PSAPRDNM DS    CL20                                                             
         DS    CL1                                                              
PSAEST   DS    CL3                                                              
         DS    CL2                                                              
PSADATES DS    CL17                                                             
         DS    CL2                                                              
PSADEST  DS    CL16                                                             
         DS    CL1                                                              
PSAFNUM  DS    CL10                                                             
         DS    CL1                                                              
PSAREFN  DS    CL7                                                              
         DS    CL1                                                              
PSABUYER DS    CL24                                                             
         DS    CL1                                                              
PSAEZNUM DS    CL8                                                              
         DS    CL2                                                              
PSASENT  DS    CL5                                                              
         DS    CL2                                                              
PSARCVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PSADEST-P)                                                    
PSAERR   DS    CL24                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT ADDS ORDERS                        
***********************************************************************         
*                                                                               
         ORG   P                                                                
PSOPRD   DS    CL3                 SPOT ADDS ORDERS                             
PSOHYPH  DS    CL1                                                              
PSOPRDNM DS    CL20                                                             
         DS    CL1                                                              
PSOEST   DS    CL3                                                              
         DS    CL2                                                              
PSODATES DS    CL17                                                             
         DS    CL2                                                              
PSODEST  DS    CL16                                                             
         DS    CL1                                                              
PSOFNUM  DS    CL10                                                             
         DS    CL1                                                              
PSOBUYER DS    CL3                                                              
         DS    CL1                                                              
PSOCAMPN DS    CL5                                                              
         DS    CL1                                                              
PSOSTN   DS    CL5                                                              
         DS    CL1                                                              
PSOMARKT DS    CL4                                                              
         DS    CL1                                                              
PSOREQ   DS    CL12                                                             
         DS    CL1                                                              
PSOEZNUM DS    CL8                                                              
         DS    CL1                                                              
PSOSENT  DS    CL5                                                              
         DS    CL1                                                              
PSORCVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PSODEST-P)                                                    
PSOERR   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT ADDS CONIRMATION OF PURCH          
***********************************************************************         
*                                                                               
         ORG   P                                                                
PSXCLT   DS    CL3                 SPOT ADDS CONFIRMATION OF PURCHASE           
PSXHYPH  DS    CL1                                                              
PSXCLTNM DS    CL20                                                             
         DS    CL1                                                              
PSXPRD   DS    CL3                                                              
PSXHYPH2 DS    CL1                                                              
PSXPRDNM DS    CL20                                                             
         DS    CL1                                                              
PSXEST   DS    CL3                                                              
         DS    CL2                                                              
PSXDEST  DS    CL16                                                             
         DS    CL1                                                              
PSXFNUM  DS    CL10                                                             
         DS    CL3                                                              
PSXSTN   DS    CL5                                                              
         DS    CL2                                                              
PSXMARKT DS    CL4                                                              
         DS    CL2                                                              
PSXEZNUM DS    CL8                                                              
         DS    CL2                                                              
PSXSENT  DS    CL5                                                              
         DS    CL2                                                              
PSXRCVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PSXDEST-P)                                                    
PSXERR   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT RX AND RY REPORTS                  
***********************************************************************         
*                                                                               
         ORG   P                                                                
PSYCLT   DS    CL3                 SPOT ADDS CONFIRMATION OF PURCHASE           
PSYHYPH  DS    CL1                                                              
PSYCLTNM DS    CL20                                                             
         DS    CL1                                                              
PSYPRD   DS    CL3                                                              
PSYHYPH2 DS    CL1                                                              
PSYPRDNM DS    CL20                                                             
         DS    CL1                                                              
PSYEST   DS    CL3                                                              
         DS    CL2                                                              
PSYDATES DS    CL11                                                             
         DS    CL2                                                              
PSYDEST  DS    CL16                                                             
         DS    CL1                                                              
PSYFNUM  DS    CL10                                                             
         DS    CL1                                                              
PSYSTN   DS    CL5                                                              
         DS    CL2                                                              
PSYMARKT DS    CL4                                                              
         DS    CL2                                                              
PSYEZNUM DS    CL8                                                              
         DS    CL2                                                              
PSYSENT  DS    CL5                                                              
         DS    CL2                                                              
PSYRCVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PSYDEST-P)                                                    
PSYERR   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT ADDS INVOICE CONTROL               
***********************************************************************         
*                                                                               
         ORG   P                                                                
PSVCLT   DS    CL3                 SPOT ADDS INVOICE CONTROL REPORTS            
PSVHYPH  DS    CL1                                                              
PSVCLTNM DS    CL30                                                             
         DS    CL1                                                              
PSVSTN   DS    CL8                                                              
         DS    CL2                                                              
PSVDEST  DS    CL16                                                             
         DS    CL1                                                              
PSVFNUM  DS    CL10                                                             
         DS    CL3                                                              
PSVEZNUM DS    CL8                                                              
         DS    CL2                                                              
PSVSENT  DS    CL5                                                              
         DS    CL2                                                              
PSVRCVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PSVDEST-P)                                                    
PSVERR   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT DMBDE SPECIAL XMISSION             
***********************************************************************         
         ORG   P                                                                
PSDPRD   DS    CL3                 SPOT DMBDE SPECIAL TRANS                     
PSDHYPH  DS    CL1                                                              
PSDPRDNM DS    CL20                                                             
         DS    CL1                                                              
PSDEST   DS    CL3                                                              
         DS    CL2                                                              
PSDMARKT DS    CL4                                                              
         DS    CL2                                                              
PSDSTATN DS    CL5                                                              
         DS    CL2                                                              
PSDBUYER DS    CL3                                                              
         DS    CL2                                                              
PSDCAMPN DS    CL5                                                              
         DS    CL2                                                              
PSDDEST  DS    CL16                                                             
         DS    CL1                                                              
PSDFTP   DS    CL5                                                              
         DS    CL2                                                              
PSDSENT  DS    CL5                                                              
         DS    CL2                                                              
PSDRCVD  DS    CL5                                                              
                                                                                
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - SPOT DMBDE GOAL TRANSFER                
***********************************************************************         
         ORG   P                                                                
PSGPRD   DS    CL3                 SPOT DMBDE GOAL TRANSFER                     
PSGHYPH  DS    CL1                                                              
PSGPRDNM DS    CL20                                                             
         DS    CL1                                                              
PSGEST   DS    CL3                                                              
         DS    CL2                                                              
PSGDEST  DS    CL16                                                             
         DS    CL1                                                              
PSGFTP   DS    CL5                                                              
         DS    CL2                                                              
PSGSENT  DS    CL5                                                              
         DS    CL2                                                              
PSGRCVD  DS    CL5                                                              
         ORG   P                                                                
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - PRINT                                   
***********************************************************************         
*                                                                               
PPIPRD   DS    CL3                 PRINT                                        
PPIHYPH  DS    CL1                                                              
PPIPRDNM DS    CL20                                                             
         DS    CL1                                                              
PPIJOB   DS    CL6                                                              
         ORG   PPIJOB                                                           
         DS    CL1                                                              
PPIEST   DS    CL3                                                              
         DS    CL5                                                              
         ORG   PPIJOB                                                           
         DS    CL1                                                              
PPIREP   DS    CL3                                                              
         DS    CL1                                                              
PPICON   DS    CL3                                                              
         DS    CL1                                                              
PPIPUB   DS    CL17                                                             
         DS    CL1                                                              
PPIPUBNM DS    CL20                                                             
         DS    CL1                                                              
PPIREQID DS    CL3                                                              
         DS    CL3                                                              
PPIDEST  DS    CL16                                                             
         DS    CL1                                                              
PPIFNUM  DS    CL10                                                             
         DS    CL1                                                              
PPIEZNUM DS    CL8                                                              
         DS    CL4                                                              
PPISENT  DS    CL5                                                              
         DS    CL3                                                              
PPIDLVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PPIDEST-P)                                                    
PPIERR   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - ACCOUNT                                 
***********************************************************************         
*                                                                               
         ORG   P                                                                
PACPRD   DS    CL3                 ACC                                          
PACHYPH  DS    CL1                                                              
PACPRDNM DS    CL19                                                             
         DS    CL2                                                              
PACJOB   DS    CL6                                                              
PACJHYP  DS    CL1                                                              
PACJBNM  DS    CL19                                                             
         DS    CL2                                                              
PACPO#   DS    CL6                                                              
         DS    CL2                                                              
PACVEND  DS    CL12                                                             
PACVHYP  DS    CL1                                                              
PACVDNM  DS    CL19                                                             
         DS    CL2                                                              
PACDEST  DS    0CL13                                                            
PACFNUM  DS    CL10                                                             
         DS    CL3                                                              
         DS    CL2                                                              
PACEZNUM DS    CL8                                                              
         DS    CL2                                                              
PACSENT  DS    CL5                                                              
         DS    CL2                                                              
PACDLVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PACDEST-P)                                                    
PACERR   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - NET TRAFFIC                             
***********************************************************************         
*                                                                               
         ORG   P                                                                
PNTYPE   DS    CL3                 NET TRAFFIC                                  
         DS    CL1                                                              
PNNET    DS    CL4                                                              
         DS    CL4                                                              
PNDEST   DS    CL16                                                             
         DS    CL1                                                              
PNFNUM   DS    CL10                                                             
         DS    CL1                                                              
PNPRGCD  DS    CL6                                                              
PNPRGCDS DS    CL1                                                              
         DS    CL1                                                              
PNPRGNM  DS    CL16                                                             
         DS    CL1                                                              
PNREV    DS    CL3                                                              
         DS    CL3                                                              
PNDATES  DS    CL17                                                             
         DS    CL3                                                              
PNEZNUM  DS    CL8                                                              
         DS    CL3                                                              
PNSENT   DS    CL5                                                              
         DS    CL3                                                              
PNDLVD   DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PNDEST-P)                                                     
PNERR    DS    CL24                                                             
                                                                                
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - NETPAK                                  
***********************************************************************         
         ORG   P                                                                
PNPPRG   DS    CL2                 NET                                          
         DS    CL3                                                              
PNPNET   DS    CL4                                                              
         DS    CL6                                                              
PNPMED   DS    CL1                                                              
         DS    CL3                                                              
PNPEST   DS    CL3                                                              
         DS    CL2                                                              
PNPPAK   DS    CL3                                                              
         DS    CL2                                                              
PNPREQ   DS    CL3                                                              
         DS    CL2                                                              
PNPDEST  DS    CL16                                                             
         DS    CL1                                                              
PNPFNUM  DS    CL10                                                             
         DS    CL1                                                              
PNPEZNUM DS    CL8                                                              
         DS    CL3                                                              
PNPSENT  DS    CL5                                                              
         DS    CL3                                                              
PNPDLVD  DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PNPDEST-P)                                                    
PNPERR   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - NET DUNNING LETTER                      
***********************************************************************         
         ORG   P                                                                
PVSTN    DS    CL5                 NET DUNNING LETTER                           
         DS    CL6                                                              
PVDEST   DS    CL16                                                             
         DS    CL1                                                              
PVFNUM   DS    CL10                                                             
         DS    CL1                                                              
PVEST    DS    CL8                                                              
         DS    CL3                                                              
PVDATES  DS    CL17                                                             
         DS    CL3                                                              
PVEZNUM  DS    CL8                                                              
         DS    CL3                                                              
PVSENT   DS    CL5                                                              
         DS    CL3                                                              
PVDLVD   DS    CL5                                                              
         ORG   P2                                                               
         DS    CL(PVDEST-P)                                                     
PVERR    DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - REPPAK                                  
***********************************************************************         
*                                                                               
         ORG   P                                                                
PRSTATN  DS    CL5                 REP                                          
         DS    CL2                                                              
PRTYP    DS    CL1                                                              
         DS    CL2                                                              
PRSLPBK  DS    CL20                                                             
         DS    CL2                                                              
PRCKNUM  DS    CL8                                                              
         DS    CL2                                                              
PRVER    DS    CL3                                                              
         DS    CL2                                                              
PRAGYCD  DS    CL4                                                              
PRCTYCD  DS    CL2                                                              
         DS    CL2                                                              
PRADVCD  DS    CL4                                                              
         DS    CL2                                                              
PRADV    DS    CL20                                                             
         DS    CL2                                                              
PRDATES  DS    CL17                                                             
         DS    CL2                                                              
PREZNUM  DS    CL8                                                              
         DS    CL2                                                              
PRSENT   DS    CL5                                                              
         DS    CL2                                                              
PRDLVD   DS    CL5                                                              
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - REPPAK TYPE D                           
***********************************************************************         
*                                                                               
         ORG   P                                                                
PRDSTA   DS    CL5                                                              
         DS    CL5                                                              
PRDETRAN DS    CL12                                                             
         DS    CL3                                                              
PRDSTART DS    CL8                                                              
         DS    CL2                                                              
PRDEND   DS    CL8                                                              
         DS    CL7                                                              
PRDFTP   DS    CL5                                                              
         DS    CL5                                                              
PRDSENT  DS    CL5                                                              
         DS    CL1                                                              
PRDSAME  DS    CL1                                                              
PRDDLVD  DS    CL5                                                              
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - DARE FOR REP                            
***********************************************************************         
*                                                                               
         ORG   P                                                                
PRDRSTN  DS    CL5                 DARE  REP                                    
PRDRSTHY DS    CL1                                                              
PRDRSTNM DS    CL20                                                             
         DS    CL1                                                              
PRDRAGY  DS    CL5                                                              
         DS    CL5                                                              
PRDRMED  DS    CL1                                                              
         DS    CL1                                                              
PRDRCLT  DS    CL3                                                              
         DS    CL1                                                              
PRDRPRD  DS    CL3                                                              
PRDRHYPH DS    CL1                                                              
PRDRPTN  DS    CL3                                                              
         DS    CL1                                                              
PRDREST  DS    CL3                                                              
         DS    CL1                                                              
PRDRBYR  DS    CL3                                                              
         DS    CL2                                                              
PRDRORD# DS    CL8                                                              
         DS    CL2                                                              
PRDRCON# DS    CL8                                                              
         DS    CL2                                                              
PRDRTYPE DS    CL3                                                              
         DS    CL5                                                              
PRDRSENT DS    CL5                                                              
         DS    CL2                                                              
PRDRDLVD DS    CL5                                                              
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - DARE FOR ADV                            
***********************************************************************         
*                                                                               
         ORG   P                                                                
PRDAREP  DS    CL8                 DARE  ADV                                    
         DS    CL1                                                              
PRDASTN  DS    CL5                                                              
PRDAHYPH DS    CL1                                                              
PRDASTNM DS    CL20                                                             
         DS    CL1                                                              
PRDASAL  DS    CL3                                                              
         DS    CL2                                                              
PRDAMED  DS    CL1                                                              
         DS    CL1                                                              
PRDACLT  DS    CL3                                                              
         DS    CL1                                                              
PRDAPRD  DS    CL3                                                              
PRDAHYP2 DS    CL1                                                              
PRDAPTN  DS    CL3                                                              
         DS    CL1                                                              
PRDAEST  DS    CL3                                                              
         DS    CL2                                                              
PRDAORD# DS    CL8                                                              
         DS    CL2                                                              
PRDACON# DS    CL8                                                              
         DS    CL2                                                              
PRDATYPE DS    CL3                                                              
         DS    CL5                                                              
PRDASENT DS    CL5                                                              
         DS    CL2                                                              
PRDADLVD DS    CL5                                                              
         DS    CL2                                                              
PRDACANX DS    CL5                                                              
         EJECT                                                                  
***********************************************************************         
*        SENDER'S REPORT - PRINT LINE - GENERAL FAX FEATURE                     
***********************************************************************         
*                                                                               
         ORG   P                                                                
PZZDEST  DS    CL16                GENERAL FAX FEATURE REPORT                   
         DS    CL1                                                              
PZZFNUM  DS    CL10                                                             
         DS    CL2                                                              
PZZINFO  DS    CL58                                                             
         DS    CL2                                                              
PZZEZNUM DS    CL8                                                              
         DS    CL2                                                              
PZZSENT  DS    CL5                                                              
         DS    CL2                                                              
PZZDLVD  DS    CL5                                                              
         ORG   P2                                                               
*        DS    CL(PZZDEST-P)       COMMENT OUT B/C LENGTH=0                     
PZZERR   DS    CL24                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036CTSFM0E   08/11/16'                                      
         END                                                                    
