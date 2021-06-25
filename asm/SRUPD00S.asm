*          DATA SET SRUPD00S   AT LEVEL 003 AS OF 08/01/00                      
*PHASE T10D00A                                                                  
         TITLE '$UPD - FACWRK RECOVERY UPDATE FACILITY'                         
UPD      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**$UPD**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         MVC   SRPARS,0(R1)        SAVE S/R PARAMETER LIST                      
         L     R9,SRPAR1                                                        
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   ATIA,SRPAR2                                                      
         MVC   AUTL,SRPAR3                                                      
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         LA    RF,MYPGMLST                                                      
         STCM  RF,7,TASVC          SET DUMMY PGMLST ENTRY ALSO                  
         DROP  R1                                                               
*                                                                               
         L     R1,SRPAR4                                                        
         USING COMFACSD,R1                                                      
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VCALLOVL,CCALLOV                                                 
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VHEXOUT,CHEXOUT                                                  
         DROP  R1                                                               
*                                                                               
         LH    RF,=Y(IOA2-WORKD)                                                
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOA2                                                         
         LH    RF,=Y(IOA3-WORKD)                                                
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOA3                                                         
*                                                                               
         MVI   SENUM,X'01'         SET CONNECTED TO SE1                         
         EJECT                                                                  
***********************************************************************         
* SCAN FACWRK FOR HELD RECOVERY FILES FOR UPDATE PENDING              *         
***********************************************************************         
         SPACE 1                                                                
WKR001   LA    R2,FINDEX           READ FACWRK INDEX                            
         USING UKRECD,R2                                                        
         XC    UKINDEX,UKINDEX                                                  
         LA    RE,FACWRK           SET SPECIAL DMCB FOR FACWRK I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         LH    R3,=Y(IOA-WORKD)                                                 
         LA    R3,WORKD(R3)                                                     
         ST    R3,FWAREC                                                        
         USING FWRECD,R3                                                        
         LH    R4,=Y(WBUFF-WORKD)                                               
         LA    R4,WORKD(R4)                                                     
         ST    R4,FWABUF                                                        
         ST    R4,ACIREC                                                        
         USING WKRECD,R4                                                        
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'BUF')                                  
*                                                                               
WKR010   GOTO1 VDATAMGR,FWDMCB,(X'08',=C'IND')                                  
         CLI   8(R1),0                                                          
         BE    WKR020                                                           
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BO    WKRX                                                             
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
WKR020   TM    UKSTAT,WKSTHO       FILE MUST BE HOLD                            
         BZ    WKR010                                                           
         TM    UKSTAT,WKSTKE       AND NOT KEEP                                 
         BO    WKR010                                                           
         CLI   UKCLASS,C'R'        MUST BE RECOVERY CLASS                       
         BNE   WKR010                                                           
         CLI   UKSUBPRG,C' '       TEST FACPAK ID PRESENT                       
         BNH   *+14                NO                                           
         CLC   UKSUBPRG,SSBSYSN1   ELSE MUST MATCH                              
         BNE   WKR010                                                           
         MVC   RECCOUNT,WKRECS     SAVE NUMBER OR RECORDS                       
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'KEE')                                  
*                                                                               
         MVC   WORK(2),UKUSRID     PUT USERID INTO WORK                         
         BAS   RE,GETALPH          GET AGYALPH IN WORK+2                        
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   TAGY,WORK+2         SAVE IT IN UTL0 FOR DATAMGR                  
         MVC   TUSER,WORK                                                       
         DROP  R1                                                               
*                                                                               
WKR030   GOTO1 VDATAMGR,FWDMCB,(X'00',=C'REA')                                  
         CLI   8(R1),0                                                          
         BE    WKR040                                                           
         TM    8(R1),X'80'         TEST END OF FILE                             
         BO    WKR200                                                           
         DC    H'0'                DIE IF DISK/FORMAT ERROR                     
*                                                                               
WKR040   CLC   FWRHDR,=C'SOON'                                                  
         BE    WKR050                                                           
         CLC   FWRHDR,=C'TSO '                                                  
         BE    WKR050                                                           
         CLC   FWRUSER,=C'USER='                                                
         BE    WKR052                                                           
         CLC   FWRUSER,=C'LAST='                                                
         BE    WKR053                                                           
         CLI   RFILTY,X'A1'        TEST FOR CTFILE                              
         BE    *+12                                                             
         CLI   RFILTY,X'AD'        TEST CTUSER                                  
         BNE   WKR045                                                           
         CLI   IOA1,C'8'           CT8REC (LOCKET) TYPE                         
         BNE   WKR045                                                           
         B     WKR060                                                           
*                                                                               
WKR045   TM    FLAG,FLFIRSTQ       TEST FIRST TIME FLAG                         
         BO    WRK046                                                           
         GOTO1 FIRSTIME            ANYTHING TO DO BEFORE FIRST UPDATE           
WRK046   GOTO1 UPDATE              PROCESS RECORD IN IOA                        
         B     WKR030                                                           
*                                                                               
WKR050   MVC   HEADER,0(R3)        SAVE HEADER INFO                             
         B     WKR030                                                           
*                                                                               
WKR052   MVC   USID,FWRUSID        SAVE USER ID                                 
         MVC   LUID,FWRLUID        LUID                                         
         MVC   PQID,FWRPQID        PQ ID                                        
         MVC   WKID,FWRWKID        WORKER FILE ID                               
         B     WKR030                                                           
*                                                                               
WKR053   MVC   PQIDL,FWRPQID       LAST PQ ID                                   
         B     WKR030                                                           
*                                                                               
WKR060   CLI   RRECTY,2            IF CT8REC CHANGE (UNLOCK)                    
         BNE   WKR030                                                           
         MVC   WORK(15),IOA1+4                                                  
         GOTO1 VLOCKET,DMCB,(C'U',WORK),SRPAR4                                  
         B     WKR030                                                           
*                                                                               
WKR200   GOTO1 EXHOOK                                                           
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'DEL')                                  
         GOTO1 UNLOCK                                                           
         GOTO1 LASTIME             ANYTHING TO DO BEFORE I GO                   
         BRAS  RE,BACKUP           SEE IF ANY MORE UNPROCESSED AROUND           
*                                                                               
WKRX     XMOD1                                                                  
*                                                                               
SYSNOP   DC    H'0'                FOR NOW                                      
         EJECT                                                                  
***********************************************************************         
* FACWK RECOVERY RECORD IS IN IOA. ATTEMPT TO DUP CHANGES IN FILE     *         
***********************************************************************         
         SPACE 1                                                                
UPDATE   NTR1                                                                   
         MVC   RACTN,RRECTY        SAVE RECORD TYPE                             
*                                                                               
         CLI   RACTN,CHANGEQ       FOR CHANGE ACTION                            
         BNE   UPD005                                                           
         CLI   LASTACTN,COPYQ      LAST ACTION MUST BE COPY                     
         BNE   UPD001                                                           
         CLC   SENUM,RSYS          SAME SYSTEM                                  
         BNE   UPD001                                                           
         CLC   LASTFILE,RFILTY     SAME FILE                                    
         BE    UPD005                                                           
UPD001   DC    H'0'                ELSE SEQUENCE ERROR                          
*                                                                               
UPD005   CLC   SENUM,RSYS          IS SWITCH REQUIRED                           
         BE    UPD010                                                           
         GOTO1 VSWITCH,DMCB,(RSYS,X'FFFFFFFF'),0                                
         MVC   SENUM,RSYS                                                       
         CLI   4(R1),0             SWITCHED OK ?                                
         BE    UPD010                                                           
         CLI   4(R1),2             TEST FOR SYSTEM NOP                          
         BE    SYSNOP                                                           
         DC    H'0'                UNKNOWN SWITCH ERROR                         
*                                                                               
UPD010   SR    R1,R1                                                            
         IC    R1,RFILTY           USE EXTERNAL NUMBER                          
         SLL   R1,5                TO INDEX INTO FILTAB                         
         LA    R5,FILTAB(R1)                                                    
         USING FILTABD,R5                                                       
         MVC   LASTFILE,DMFLNUM    SAVE FILE NUMBER                             
         MVC   FILENAME,DMFLNAME   AND FILE NAME                                
*                                                                               
UPD020   MVC   LASTACTN,RACTN      SAVE ACTION VALUE                            
         SR    R1,R1                                                            
         ICM   R1,1,RACTN                                                       
         BNZ   *+6                                                              
         DC    H'0'                RACTN MUST BE BETWEEN 1 - 3                  
         CLI   RACTN,3                                                          
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         EX    0,ACTTAB(R1)        PROCESS RECOVERY RECORD                      
*                                                                               
ACTTAB   B     COPY                                                             
         B     CHANGE                                                           
         B     ADD                                                              
         EJECT                                                                  
***********************************************************************         
* COPY RECORDS MUST BE IDENTICAL TO RECORDS ON FILE. IF RECOVERY COPY *         
* IS DIFFERENT THEN PROTECTION DURING UPDATE MUST HAVE FAILED         *         
***********************************************************************         
         SPACE 1                                                                
COPY     CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BNE   COPY10                                                           
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),FILENAME,IOA1,AIOA2                 
         TM    8(R1),X'FD'                                                      
         BZ    COPY20                                                           
         DC    H'0'                                                             
COPY10   CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS                           
         BE    *+10                                                             
         BAS   RE,DEATH                                                         
         DC    H'0'                UNKNOWN FILE TYPE                            
         MVC   DDA,RVCHR                                                        
         GOTO1 VDATAMGR,DMCB,(X'88',GETREC),FILENAME,DDA,AIOA2,IOWORK           
         TM    8(R1),X'FD'                                                      
         BZ    COPY20                                                           
         DC    H'0'                                                             
*                                                                               
COPY20   TM    DMFLSTYP,DMFLFIX    TEST FIXED LENGTH RECORD                     
         BNO   COPY21                                                           
         MVC   RECLEN,DMFLMINI     IF SO MINIMUM LEN IS LEN                     
         B     COPY25                                                           
COPY21   TM    DMFLFLG1,DMFLRLEN   V/L RECORDS LEN MUST BE IN RECORD            
         BO    *+10                                                             
         BAS   RE,DEATH                                                         
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,DMFLLEND                                                      
         L     R0,AIOA2                                                         
         AR    R1,R0               INDEX TO RECORD LENGTH                       
         MVC   RECLEN,0(R1)                                                     
*                                                                               
COPY25   L     R0,AIOA3            MOVE FACWK RECORD TO IOA3                    
         LA    RE,IOA1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AIOA3            USE NEW BUFFER FOR COMPARE                   
         L     RE,AIOA2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    COPYX                                                            
*                                                                               
         BAS   RE,CPYHOOK          NOT A MATCH                                  
         BE    COPYX               IF HOOK RETURNS EQU THEN OK                  
         BAS   RE,DEATH                                                         
         DC    H'0'                ELSE COPY RECORD DID NOT MATCH               
*                                                                               
COPYX    B     UPDATEX                                                          
         EJECT                                                                  
***********************************************************************         
* COPY RECORD HAS BEEN READ FOR UPDATE NOW REPLACE WITH CHANGE RECORD *         
***********************************************************************         
         SPACE 1                                                                
CHANGE   BAS   RE,CHGHOOK          SEE IF WE HAVE A HOOK                        
         CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BNE   CHNG10                                                           
         GOTO1 VDATAMGR,DMCB,DMWRT,FILENAME,IOA1,IOA1                           
         CLI   8(R1),0                                                          
         BE    CHNG20                                                           
         DC    H'0'                                                             
CHNG10   CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN FILE TYPE                            
         GOTO1 VDATAMGR,DMCB,PUTREC,FILENAME,DDA,IOA1,IOWORK                    
         CLI   8(R1),0                                                          
         BE    CHNG20                                                           
         DC    H'0'                                                             
CHNG20   GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    UPDATEX                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TO ADD DA RECORDS DO ADDREC                                         *         
* TO ADD IS RECORDS FIRST CHECK IF IS/DA PAIR IF NOT DO DMADD         *         
* IF SO CHECK DISK ADDR. IF NON ZERO USE IT AS IT MUST BE A PASSIVE   *         
* IF IT IS ZERO TEST KEY WITH PREVIOUS ADDREC KEY AND IF ITS IDENTICAL*         
* IGNORE THE RECORD AS ADDREC HAS ALREADY ADDED IT                    *         
* IF NOT INSERT LAST DA KEY AND ADD AS IT MUST BE A NEW PASSIVE       *         
***********************************************************************         
         SPACE 1                                                                
ADD      CLI   DMFLTYP,DMFLIS      TEST INDEX SEQUENTIAL                        
         BNE   ADD10                                                            
         TM    DMFLFLG1,DMFLISDA   DO WE NEED A DISK ADDRESS                    
         BNO   ADD1                                                             
         SR    R1,R1                                                            
         IC    R1,DMFLDAD                                                       
         LA    R1,IOA1(R1)                                                      
         OC    0(4,R1),0(R1)       DISK ADDRESS MUST BE A PASSIVE               
         BNZ   ADD1                                                             
         CLC   DMFLPAIR,DALAST     LAST DA ADD MUST BE PAIRED DA FILE           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R1),DALINK      USE DA FROM LAST ADDREC                      
         SR    R1,R1                                                            
         IC    R1,DMFLKEYL         GET KEY LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DKEY(0),IOA1        TEST WITH LAST DA KEY ADDED                  
         BE    ADD20               ADDREC HAS ALREADY DONE THIS                 
ADD1     MVC   KEY,IOA1                                                         
         GOTO1 VDATAMGR,DMCB,DMADD,FILENAME,KEY,IOA1                            
         CLI   8(R1),0                                                          
         BE    ADD20                                                            
         DC    H'0'                                                             
*                                                                               
ADD10    CLI   DMFLTYP,DMFLDA      TEST DIRECT ACCESS                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN FILE TYPE                            
         CLI   DMFLSTYP,DMFLREQ    TEST REQUEST FILE                            
         BE    ADD100                                                           
*                                                                               
         MVC   DALAST,DMFLNUM                                                   
         MVC   DKEY,IOA1                                                        
         GOTO1 VDATAMGR,DMCB,ADDREC,FILENAME,DKEY,IOA1,IOWORK                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DALINK,DKEY         SAVE DISK ADDRESS                            
         MVC   DKEY,IOA1           SAVE KEY                                     
         B     ADD20                                                            
ADD20    GOTO1 VDATAMGR,DMCB,DMUNLK,FILENAME  UNLOCK BLOCK                      
         CLI   8(R1),0                                                          
         BE    UPDATEX                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ADD REQUEST RECORDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADD100   DS    0H                                                               
         LH    R1,IOA              GET RECORD LENGTH                            
         SH    R1,=H'28'           ADJUST FOR LEN/RECOVERY HDR                  
         SR    R0,R0                                                            
         D     R0,=F'80'           GIVES NUMBER OF CARDS IN R1                  
         LTR   R0,R0               TEST FOR REMAINDER                           
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,IOA1                                                          
         USING RQHHDRD,RE                                                       
         BCTR  R1,0                ADJUST FOR 80 BYTE OVERHEAD                  
         BCTR  R1,0                AND NEED TO SET N'CARDS-1                    
         SLL   R1,4                NUMBER TO LEFT NIBBLE                        
         STC   R1,RQHFLAG          SET NUMBER IN REQHDR                         
         SRL   R1,4                RESTORE COUNT                                
         DROP  RE                                                               
*                                                                               
* MUST NOT ADD DUPLICATE SPOOL-TYPE REQUESTS WITH                               
* SAME SIN BECAUSE IT CAUSES PROBLEMS IN END OF DAY.                            
*                                                                               
         OC    NEXTSIN,NEXTSIN     TEST FIRST TIME THIS WKFILE                  
         BNZ   ADD102                                                           
         MVC   SAVESIN,RSIN                                                     
         MVC   NEXTSIN,RSIN                                                     
*                                                                               
* NO EASY WAY TO KNOW IF IT IS A SPOOL REQUEST, SO                              
* LOOK FOR SYSTEM INPUT NUMBER IN ALL REQUEST CARDS                             
*                                                                               
ADD102   L     RF,RSIN             GET SIN IN REQUEST CARD FORMAT               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSIN,DUB                                                         
*                                                                               
         LA    R0,1(R1)            SET TO NUMBER OF CARDS - 1                   
         LA    RE,IOA1+80                                                       
*                                                                               
ADD104   CLC   QSIN,5(RE)                                                       
         BNE   ADD110                                                           
         LA    RE,80(RE)                                                        
         BCT   R0,ADD104                                                        
*                                                                               
* ALL MATCH - REPLACE SIN IN REQUESTS WITH NEXTSIN                              
*                                                                               
         L     RF,NEXTSIN          GET SIN IN REQUEST CARD FORMAT               
         LA    RF,1(RF)            BUMP FOR NEXT TIME                           
         ST    RF,NEXTSIN                                                       
         BCTR  RF,0                                                             
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSIN,DUB                                                         
*                                                                               
         LA    R0,1(R1)                                                         
         LA    RE,IOA1+80                                                       
*                                                                               
ADD106   MVC   5(6,RE),QSIN                                                     
         LA    RE,80(RE)                                                        
         BCT   R0,ADD106                                                        
*                                                                               
ADD110   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'20',DMADD),=C'REQUEST',DKEY,IOA1                
         CLI   8(R1),0                                                          
         BE    UPDATEX                                                          
         DC    H'0'                                                             
*                                                                               
UPDATEX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FIRSTIME (CALLED BEFORE 1ST UPDATE)                                 *         
***********************************************************************         
         SPACE 1                                                                
FIRSTIME NTR1                                                                   
         OI    FLAG,FLFIRSTQ       MAKE SURE ONLY RUNS ONCE                     
*&&UK                                                                           
         CLI   RSYS,X'14'          IS THIS MEDZ UPDATE                          
         BNE   FIRSTX                                                           
         CLC   RECCOUNT,=F'1000'   IS IT > 1000 RECS                            
         BL    FIRSTX                                                           
         MVC   OPMSG(L'ENQZMSG),ENQZMSG     TELL OPS MVS ABOUT IT               
         OC    OPMSG,SPACES                                                     
         MVC   OPFACID+4(3),SYSNAME                                             
         BAS   RE,WTO                                                           
         OI    FLAG,FLENQZQ        FLAG MESSAGE OUTPUT                          
*&&                                                                             
FIRSTX   B     UPDATEX                                                          
         EJECT                                                                  
***********************************************************************         
* LASTIME (CALLED AFTER LAST UPDATE)                                  *         
***********************************************************************         
         SPACE 1                                                                
LASTIME  NTR1                                                                   
*&&UK                                                                           
         TM    FLAG,FLENQZQ        DID WE OUTPUT A MESSAGE                      
         BNO   UPDATEX                                                          
         MVC   OPMSG(L'ENQXMS),ENQXMSG   TELL OPS MVS WE'VE FINISHED            
         OC    OPMSG,SPACES                                                     
         MVC   OPFACID+4(3),SYSNAME                                             
         BAS   RE,WTO                                                           
*&&                                                                             
         OC    WKID,WKID           TEST WORKER FILE KEY                         
         BZ    LAST09                                                           
         LA    R2,FINDEX           READ WORKER INDEX                            
         USING UKRECD,R2                                                        
         XC    FINDEX,FINDEX                                                    
         MVC   UKINDEX,WKID                                                     
         LA    RE,WKFILE           SET SPECIAL DMCB FOR WORKER I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         LH    R3,=Y(IOA-WORKD)                                                 
         LA    R3,WORKD(R3)                                                     
         ST    R3,FWAREC                                                        
         USING FWRECD,R3                                                        
         LH    R4,=Y(WBUFF-WORKD)                                               
         LA    R4,WORKD(R4)                                                     
         ST    R4,FWABUF                                                        
         LR    R0,R4               CLEAR BUFFER                                 
         LH    R1,=Y(L'WBUFF)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
LAST03   GOTO1 VDATAMGR,FWDMCB,(X'08',INDEX)                                    
         CLI   8(R1),0                                                          
         BE    LAST05                                                           
         BAS   RE,DEATH                                                         
         DC    H'0'                CAN'T FIND WORKER FILE                       
*                                                                               
LAST05   LA    R4,WKID             SEARCH ID                                    
         USING WKRECD,R4                                                        
         CLC   UKKEY,WKINDEX       MUST BE SAME KEY                             
         BNE   LAST03                                                           
         CLC   UKFILNO,WKFILNO     FILE NUMBER                                  
         BNE   LAST03                                                           
         GOTO1 VDATAMGR,FWDMCB,(X'00',UNKEEP)                                   
*                                                                               
LAST09   OC    PQID,PQID           TEST PRTQUE KEY                              
         BZ    LASTX                                                            
         L     R0,ACIREC           CLEAR BUFFER                                 
         LH    R1,=Y(L'WBUFF)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NDX,NDX             FIND PRTQ ID FOR USER REPORT                 
         L     R5,ACIREC                                                        
         MVC   NDX(2),PQID                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',GFILE),PRTQUE,NDX,SAVE,(R5)                 
         MVC   PRTQID,NDX+(PKUSRINF-PKINDEX)                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5),0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEBUFF,0(R5)      SAVE RETURN DATA IN BUFFER                   
*                                                                               
         LA    R5,NDX              SEARCH PRTQUE INDEX FOR REPORT               
         XC    NDX,NDX             FIND PRTQ ID FOR USER REPORT                 
         USING PKRECD,R5                                                        
         MVC   PKKEY,PQID          SET REPORT ID IN USER INDEX                  
         OI    PKFLAG,X'0C'        PASS TEMPS/DIRECT LOCATE                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'08',INDEX),PRTQID,NDX,SAVE,ACIREC,0             
         CLI   8(R1),0                                                          
         BE    LAST11                                                           
         BAS   RE,DEATH                                                         
         DC    H'0'                ERR - DIE ON INDEX READ DISK ERROR           
*                                                                               
*                                   MAKE REPORT VISIBLE                         
LAST11   GOTO1 VDATAMGR,DMCB,(X'00',VISIBL),PRTQID,NDX,SAVE,ACIREC,0            
         CLI   8(R1),0                                                          
         BE    LAST20                                                           
         BAS   RE,DEATH                                                         
         DC    H'0'                DIE IF CANT READ                             
*                                                                               
LAST20   DS    0H                                                               
         OC    PQIDL,PQIDL         TEST SECOND REPORT                           
         BZ    LASTX                                                            
         MVC   PQID,PQIDL                                                       
         XC    PQIDL,PQIDL                                                      
         B     LAST09                                                           
*                                                                               
LASTX    B     UPDATEX                                                          
         EJECT                                                                  
***********************************************************************         
* DEATH THREATS                                                       *         
* IF YOU WANT TO SEND AN E-MAIL DUMP NOTIFICATION YOU HAVE TO         *         
* BEGIN THE MESSAGE WITH '*SOON DUMP*' ELSE IT WILL NOT BE RECOGNIZED *         
***********************************************************************         
         SPACE 1                                                                
DEATH    ST    RE,DUB              SAVE WHERE WE CAME FROM                      
         NTR1  ,                                                                
         OC    USID,USID           DID WE FIND ID YET?                          
         BNZ   DEATH1              YES                                          
         BAS   RE,FINDID           NO, LOOK FOR ONE                             
         OC    USID,USID           DID WE FIND AN ID?                           
         BZ    DEATHX              NO                                           
*                                                                               
DEATH1   MVI   OPMSG,C' '                                                       
         MVC   OPMSG+1(L'OPMSG-1),OPMSG                                         
         MVC   OPSOON,SOONMSG                                                   
         MVC   OPUSID,USID         USER                                         
         MVC   OPPRGM,WKID+2       PROGRAM AND SUB/PROGRAM                      
         MVC   OPLUID,LUID                                                      
         SR    RE,RE                                                            
         ICM   RE,7,DUB+1          GET RE                                       
         LA    RF,0(RB)                                                         
         SR    RE,RF               GET DISPLACEMENT                             
         ST    RE,DUB                                                           
         GOTO1 VHEXOUT,PARML,DUB+1,OPDLOC,3,0,0                                 
         BAS   RE,WTO              WRITE THE MESSAGE                            
*                                                                               
DEATHX   B     UPDATEX                                                          
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
***********************************************************************         
         SPACE 1                                                                
WTO      NTR1                                                                   
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,PARML,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                      
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     UPDATEX                                                          
         EJECT                                                                  
**********************************************************************          
* READ TO END OF FACWK FILE TO FIND USID                             *          
**********************************************************************          
         SPACE 1                                                                
FINDID   NTR1                                                                   
         TM    FWDMCB+8,X'80'      ARE WE ALREADY AT END?                       
         BO    FINDX               YES, JUST GO BACK                            
*                                                                               
FIND02   GOTO1 VDATAMGR,FWDMCB,(X'00',=C'REA')                                  
         CLI   8(R1),0                                                          
         BE    FIND04                                                           
         TM    8(R1),X'80'         TEST END OF FILE                             
         BO    FINDX                                                            
         DC    H'0'                DIE IF DISK/FORMAT ERROR                     
*                                                                               
FIND04   CLC   FWRUSER,=C'USER='                                                
         BE    FIND06                                                           
         CLC   FWRUSER,=C'LAST='                                                
         BNE   FIND02                                                           
*                                                                               
         MVC   PQIDL,FWRPQID       LAST PQ ID                                   
         B     FIND02                                                           
*                                                                               
FIND06   MVC   USID,FWRUSID        SAVE USER ID                                 
         MVC   LUID,FWRLUID        LUID                                         
         MVC   PQID,FWRPQID        PQ ID                                        
         MVC   WKID,FWRWKID        WORKER FILE ID                               
         B     FIND02                                                           
*                                                                               
FINDX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET AGYALPH INTO WORK+2 FROM USERID IN WORK                         *         
***********************************************************************         
         SPACE 1                                                                
GETALPH  NTR1                                                                   
         LA    R5,KEY              READ ID RECORD                               
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,WORK                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,IOA1                             
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,IOA1+CTIDATA-CTIREC                                           
         USING CTAGYD,R5                                                        
         XC    WORK+2(2),WORK+2    CLEAR OUTPUT AREA                            
GETAL01  CLI   CTAGYEL,CTAGYELQ    SCAN FOR ALPHA ELEMENT                       
         BE    GETAL02                                                          
         CLI   CTAGYEL,0                                                        
         BE    GETALX              NOT FOUND                                    
         SR    R0,R0                                                            
         IC    R0,CTAGYLEN         TRY NEXT ELEMENT                             
         AR    R5,R0                                                            
         B     GETAL01                                                          
*                                                                               
GETAL02  MVC   WORK+2(2),CTAGYID   SAVE ALPHAID IN WORK+2                       
GETALX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REMOVE FAUPDTAB ENTRY IF THERE                                      *         
***********************************************************************         
         SPACE 1                                                                
UNLOCK   NTR1                                                                   
         OC    HEADER,HEADER                                                    
         BZ    UNLOCKX                                                          
         LA    R3,HEADER                                                        
         USING FWRECD,R3                                                        
*                                                                               
         L     R6,VUPDTAB                                                       
         LH    R4,0(R6)                                                         
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING UPDTABD,R6                                                       
UNLK010  CLI   UPDTABT,UPDTSQ      TEST UPDATIVE SOON ENTRY                     
         BNE   *+14                                                             
         CLC   FWRPQK,UPDTSOON     TEST KEY                                     
         BE    UNLK020                                                          
         BXLE  R6,R4,UNLK010                                                    
         B     UNLOCKX             NO LOCK FOUND                                
*                                                                               
UNLK020  MVC   BYTE,UPDCHAIN       SAVE CHAIN BYTE                              
*                                                                               
         LR    R0,R6               R0=DEST                                      
         LA    R1,1(R5)                                                         
         SR    R1,R6               R1=L'DEST                                    
         LR    RE,R6                                                            
         AR    RE,R4               RE=SOURCE                                    
         LR    RF,R1                                                            
         S     RF,R4               RF=L'SOURCE                                  
         MVCL  R0,RE               MOVE & PAD LAST ENTRY WITH ZERO              
UNLOCKX  XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CALL EXIT ROUTINE                                                   *         
* NOTE: UTL IS SWITCHED TO THE LAST SYSTEM UPDATED                    *         
* R9=SYSFACS R8=SSB R2=UKRECD                                         *         
***********************************************************************         
         SPACE 1                                                                
EXHOOK   NTR1                                                                   
         LA    R1,KEYTAB           TABLE OF EXITS                               
EXHK1    L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         CLC   UKSYSPRG,0(R1)                                                   
         BER   RF                  EXECUTE USER EXIT                            
         LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   EXHK1                                                            
EXHOOKX  XIT1                                                                   
*                                                                               
         DS    0F                                                               
KEYTAB   DC    C'TCK',X'00',A(TALEXIT)                                          
         DC    C'TDC',X'00',A(TALEXIT)                                          
         DC    C'A27',X'00',A(ACCEXIT)                                          
         DC    C'A29',X'00',A(ACCEXIT)                                          
         DC    C'A55',X'00',A(ACCEXIT)                                          
         DC    C'A56',X'00',A(ACCEXIT)                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL EXIT ROUTINE FOR COPY NOT EQUAL                                *         
* NOTE: UTL IS SWITCHED TO THE LAST SYSTEM UPDATED                    *         
* R9=SYSFACS R8=SSB R2=UKRECD                                         *         
* IOA1=COPY RECORD FROM FACWRK                                        *         
* IOA2=CURRENT FILE RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
CPYHOOK  NTR1                                                                   
         LA    R1,KEYTAB1          TABLE OF COPY EXITS                          
CPYHK1   L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         CLC   UKSYSPRG,0(R1)                                                   
         BER   RF                  EXECUTE USER EXIT                            
         LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   CPYHK1                                                           
         LTR   RB,RB               SET NEQ TO CAUSE DUMP                        
CPYHK1X  XIT1                                                                   
*                                                                               
         DS    0F                                                               
KEYTAB1  DC    C'DEM',X'00',A(DEMEXIT)                                          
*&&US*&& DC    C'SAI',X'00',A(SAIEXIT)  TRAFFIC PATTERNS                        
*&&US*&& DC    C'SAT',X'00',A(SAIEXIT)  TRAFFIC PATTERNS                        
*&&US*&& DC    C'SSH',X'00',A(SAIEXIT)  TRAFFIC SHIPPING RECAP PATTERNS         
*&&US*&& DC    C'A27',X'00',A(BILEXIT)  BILLING EXCEPTIONS                      
*&&US*&& DC    C'A29',X'00',A(BILEXIT)  BILLING EXCEPTIONS                      
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL EXIT ROUTINE FOR CHANGE                                        *         
* NOTE: UTL IS SWITCHED TO THE LAST SYSTEM UPDATED                    *         
* R9=SYSFACS R8=SSB R2=UKRECD                                         *         
* IOA1=CURRENT RECORD TO BE WRITTEN                                   *         
***********************************************************************         
         SPACE 1                                                                
CHGHOOK  NTR1                                                                   
         LA    R1,KEYTAB2          TABLE OF CHANGE EXITS                        
CHGHK1   L     RF,4(R1)                                                         
         A     RF,RELO                                                          
         CLC   UKSYSPRG,0(R1)                                                   
         BER   RF                  EXECUTE USER EXIT                            
         LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   CHGHK1                                                           
CHGHK1X  XIT1                                                                   
*                                                                               
         DS    0F                                                               
KEYTAB2  DC    C'DEM',X'00',A(DEMEXIT2)                                         
         DC    C'A27',X'00',A(BILEXIT2)                                         
         DC    C'A29',X'00',A(BILEXIT2)                                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT ROUTINES                                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMEXIT  EQU   *                                                                
*&&UK                                                                           
         LA    R4,IOA1             COPY RECORD                                  
         L     R5,AIOA2            CURRENT RECORD                               
         USING DEMFILED,R4                                                      
         CLI   DEKCNTR,DEKCNTRQ    IS IT A CONTROL REC                          
         BNE   DEMEXDIE                                                         
         XC    DEMSAVE,DEMSAVE                                                  
*                                  COPY CHANGED DATA                            
         CLC   DECNTPRO(6),DECNTPRO-DEMFILED(R5)                                
         BE    *+16                                                             
         MVC   DEMSAVE,DECNTPRO-DEMFILED(R5)                                    
         MVC   DECNTPRO(6),DEMSAVE                                              
         MVC   DECNTUCI(8),DECNTUCI-DEMFILED(R5)                                
*                                                                               
         LA    R0,IOA1             THEN RECOMPARE THEM                          
         L     RE,AIOA2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN         RECLEN WAS SET PREVIOUSLY                    
         LR    RF,R1                                                            
         CLCL  R0,RE               ARE RECORDS NOW THE SAME ?                   
         BNE   DEMEXDIE                                                         
*&&                                                                             
DEMEXOK  CR    RB,RB               SET EQU AND RETURN                           
         B     CPYHK1X                                                          
DEMEXDIE LTR   RB,RB               SET NEQ FOR DUMP                             
         B     CPYHK1X                                                          
*                                                                               
DEMEXIT2 EQU   *                                                                
*&&UK                                                                           
         LA    R4,IOA1             COPY RECORD                                  
         USING DEMFILED,R4                                                      
         CLI   DEKCNTR,DEKCNTRQ    IS IT A CONTROL REC                          
         BNE   DEMXX2                                                           
         OC    DEMSAVE,DEMSAVE                                                  
         BZ    *+10                                                             
         MVC   DECNTPRO(6),DEMSAVE                                              
*&&                                                                             
DEMXX2   B     CHGHK1X             RETURN                                       
         EJECT                                                                  
TALEXIT  EQU   *                   TALENT URGENT CHECK RUN                      
         GOTO1 VCALLOVL,DUB,0,X'D9000A88'  GET A(TAL SYSTEM TABLES)             
         L     R1,0(R1)                                                         
         A     R1,TGACKLK-TGTABLES(R1)     R1=A(LOCKOUT STATUS BYTE)            
         MVI   0(R1),CKLKOK                SET OK STATUS                        
         B     EXHOOKX                                                          
         EJECT                                                                  
ACCEXIT  EQU    *                ACCPAK - SOON CHECKS/A27-A29 BILLING           
         GOTO1 VCALLOVL,DUB,0,X'D9000A66'  GET A(ACSETLOCK)                     
         L     RF,0(R1)                                                         
         GOTO1 (RF),DUB,(C'U',AIOA2),SRPAR4 ISSUE UNLOCK                        
         CLI   4(R1),0                                                          
         BE    EXHOOKX                                                          
         DC    H'0'                ERROR TRYING TO UNLOCK                       
         EJECT                                                                  
* CODE FOR TRAFFIC PATTERN RECORDS                                              
*&&US                                                                           
SAIEXIT  DS    0H                                                               
         CLC   =X'0A22',IOA1       THIS A PATTERN RECORD                        
         BNE   SAIEXITE                                                         
         LA    R4,IOA1+24          COPY                                         
         L     R5,AIOA2            CURRENT REC                                  
         LA    R5,24(R5)                                                        
         MVC   PATUSED-PATDTAEL(3,R5),PATUSED-PATDTAEL(R4)                      
         SPACE                                                                  
         LA    R5,IOA1+24          COPY                                         
         SPACE                                                                  
         MVI   ELCODE,X'F1'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R5                                                            
         SPACE                                                                  
         L     R5,AIOA2            CURRENT                                      
         LA    R5,24(R5)                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE TO CURRENT FROM COPY                    
         SPACE                                                                  
         LA    R0,IOA1             CHECK COPY RECORD IS SAME                    
         L     RE,AIOA2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    SAIEXITE                                                         
         DC    H'0'                                                             
         SPACE                                                                  
SAIEXITE CR    RB,RB               SET EQU AND RETURN                           
         B     CPYHK1X                                                          
*&&                                                                             
         EJECT                                                                  
*&&US                                                                           
         USING FILTABD,R5                                                       
BILEXIT  DS    0H                                                               
         XC    SAVEJOB,SAVEJOB                                                  
         XC    SAVEDT,SAVEDT       CLEAR IN CASE WE HAVE TO CHANGE              
         XC    SAVE31,SAVE31                                                    
         XC    SAVE32,SAVE32                                                    
*                                                                               
         LA    R4,IOA1                                                          
         CLI   9(R4),C' '          MUST NOT BE BLANK TO BE A JOB                
         BE    BILEXITX                                                         
         CLC   15(17,R4),SPACES    MUST  BE BLANK TO BE A JOB                   
         BNE   BILEXITX                                                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,DMFLELD        DISPLACEMENT TO 1ST ELEMENT                  
         BZ    BILEXITX            NONE, MUST BE DIRECTORY                      
         MVC   SAVEJOB,0(R4)       SAVE THE JOB FOR CHANGE                      
*                                                                               
         LA    R4,IOA1(R1)         R4 = A(FACWK COPY)                           
         L     RE,AIOA2                                                         
         LA    R6,0(R1,RE)         R6 = A(CURRENT FILE)                         
*                                                                               
         MVI   ELCODE,RSTELQ       RECORD STATUS ELEMENT X'30'                  
         LR    R5,R4                                                            
         BAS   RE,FIRSTEL                                                       
         BNE   BILEXITA                                                         
         LR    R4,R5                                                            
*                                                                               
         LR    R5,R6                                                            
         BAS   RE,FIRSTEL                                                       
         BNE   BILEXITA                                                         
         LR    R6,R5                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),0(R4)       ARE STATUS ELEMENTS THE SAME?                
         BE    BILEXITA            YES                                          
*                                                                               
* UPDATE COPY WITH RSTTDATE                                                     
*                                                                               
         CLC   RSTTDATE-RSTELD(L'RSTTDATE,R6),RSTTDATE-RSTELD(R4)               
         BE    *+10                UPDATE RSTTDATE IN COPY                      
         MVC   RSTTDATE-RSTELD(L'RSTTDATE,R4),RSTTDATE-RSTELD(R6)               
         MVC   SAVEDT,RSTTDATE-RSTELD(R6)                                       
*                                                                               
BILEXITA LA    R5,IOA1(R1)         ACCOUNT STATUS ELEMENT X'31'                 
         MVI   ELCODE,ASTELQ                                                    
         BAS   RE,FIRSTEL                                                       
         BNE   BILEXITX                                                         
         LR    R4,R5               R4 = A(FACWK COPY)                           
*                                                                               
         L     RE,AIOA2                                                         
         LA    R5,0(R1,RE)         R5 = A(CURRENT FILE)                         
         BAS   RE,FIRSTEL                                                       
         BNE   BILEXITX                                                         
*                                                                               
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R5)       ARE ELEMENTS THE SAME?                       
         BE    BILEXITB            YES                                          
         EX    RF,*+8              NO, SAVE THE CURRENT ONE FOR CHANGE          
         B     *+10                                                             
         MVC   SAVE31(0),0(R5)                                                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE FACWK COPY TO CURRENT                   
*                                                                               
BILEXITB LA    R5,IOA1(R1)         ACCOUNT BALANCE ELEMENT X'32'                
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,FIRSTEL                                                       
         BNE   BILEXITX                                                         
         LR    R4,R5               R4 = A(FACWK COPY)                           
*                                                                               
         L     RE,AIOA2                                                         
         LA    R5,0(R1,RE)         R5 = A(CURRENT FILE)                         
         BAS   RE,FIRSTEL                                                       
         BNE   BILEXITX                                                         
*                                                                               
         SR    RF,RF               GET ELEMENT LENGTH                           
         IC    RF,1(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R5)       ARE ELEMENTS THE SAME?                       
         BE    BILEXITC            YES                                          
         EX    RF,*+8              NO, SAVE THE CURRENT ONE FOR CHANGE          
         B     *+10                                                             
         MVC   SAVE32(0),0(R5)                                                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE FACWK COPY TO CURRENT                   
*                                                                               
BILEXITC LA    R0,IOA1             CHECK COPY RECORD IS SAME                    
         L     RE,AIOA2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,RECLEN                                                      
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    CPYHK1X                                                          
*                                                                               
BILEXITX LTR   RB,RB               SET NOT EQUAL & RETURN                       
         B     CPYHK1X                                                          
*&&                                                                             
*                                                                               
BILEXIT2 EQU   *                                                                
*&&US                                                                           
         LA    R4,IOA1             R4 = A(CHANGE RECORD)                        
         CLC   0(ACCORLEN,R4),SAVEJOB                                           
         BNE   BILXXX                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,DMFLELD        DISPLACEMENT TO 1ST ELEMENT                  
         BZ    BILXXX              NONE, MUST BE DIRECTORY                      
         OC    SAVEDT,SAVEDT       DO WE HAVE A DATE FROM CURRENT?              
         BZ    BILXX2              NO                                           
*                                                                               
         LA    R5,IOA1(R1)                                                      
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ELEMENT                            
         MVC   RSTTDATE-RSTELD(L'RSTTDATE,R5),SAVEDT                            
*                                                                               
BILXX2   OC    SAVE31,SAVE31       DO WE HAVE A NEW STATUS ELEMENT?             
         BZ    BILXX4              NO                                           
         LA    R5,IOA1(R1)                                                      
         MVI   ELCODE,ASTELQ                                                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ELEMENT                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SAVE31                                                   
*                                                                               
BILXX4   OC    SAVE32,SAVE32       DO WE HAVE A NEW BALANCE ELEMENT?            
         BZ    BILXXX              NO, DONE                                     
         LA    R5,IOA1(R1)                                                      
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE BALANCE                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SAVE32                                                   
*&&                                                                             
BILXXX   B     CHGHK1X             RETURN                                       
         EJECT                                                                  
FIRSTEL  CLI   0(R5),0                                                          
         BNE   *+10                                                             
         CLI   0(R5),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R5)                                                     
         BCR   8,RE                                                             
NEXTEL   SR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R5,RF                                                            
         B     FIRSTEL                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
ENQZMSG  DC    CL40'+ENQDEQ+ MEDZ LONG UPDATE     (FACPAK)'                     
ENQXMSG  DC    CL40'+ENQDEQ+ MEDZ UPDATE ENDED    (FACPAK)'                     
SOONMSG  DC    C'*SOON DUMP* IN SRUPD00 AT'                                     
INVSMSG  DC    C'*SOON DUMP* INVISIBLE REPORT '                                 
SPACES   DC    CL60' '                                                          
         SPACE 1                                                                
DMREAD   DC    C'DMREAD '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READ     DC    CL8'READ'                                                        
PRTQUE   DC    CL8'PRTQUE'                                                      
ACTI     DC    CL8'ACTI'                                                        
UNKEEP   DC    CL8'UNKEEP'                                                      
VISIBL   DC    CL8'VISIBLE'                                                     
*                                                                               
CTFILE   DC    C'CTFILE '                                                       
FACWRK   DC    C'FACWRK '                                                       
WKFILE   DC    C'WKFILE '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
*                                                                               
MYPGMLST DC    CL7'SRUPD00',X'14',X'0D',X'00',AL1(000),9X'00'                   
         EJECT                                                                  
***********************************************************************         
* SEE IF THERE ARE ANY MORE WKR FILES THAT REQUIRE $UPD TO BE RE-RUN  *         
***********************************************************************         
         SPACE 1                                                                
BACKUP   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',=C'DTFAD'),=C'FACWRK'                       
         L     RE,DMCB+12          A(DTF)                                       
         TM    36(RE),X'40'        IS FACWRK FILE NO-OP?                        
         BO    BACKUPX             YES -- CAN'T READ IT                         
*                                                                               
         LA    R2,FINDEX           READ FACWRK INDEX                            
         USING UKRECD,R2                                                        
         XC    UKINDEX,UKINDEX                                                  
         LA    RE,FACWRK           SET SPECIAL DMCB FOR FACWRK I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         LH    R3,=Y(IOA-WORKD)                                                 
         LA    R3,WORKD(R3)                                                     
         ST    R3,FWAREC                                                        
         USING FWRECD,R3                                                        
         LHI   R4,WBUFF-WORKD                                                   
         LA    R4,WORKD(R4)                                                     
         ST    R4,FWABUF                                                        
         ST    R4,ACIREC                                                        
         USING WKRECD,R4                                                        
*                                                                               
         GOTO1 VDATAMGR,FWDMCB,(X'00',=C'BUF')                                  
*                                                                               
BACK02   GOTO1 VDATAMGR,FWDMCB,(X'08',=C'IND')                                  
         CLI   8(R1),0                                                          
         BE    BACK04                                                           
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BO    BACKUPX                                                          
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
BACK04   TM    UKSTAT,WKSTAC       FILE MUST BE ACTIVE                          
         BZ    BACK06                                                           
         TM    UKSTAT,WKSTKE       AND NOT KEEP                                 
         BO    BACK02                                                           
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   BACK02                                                           
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   BACK08              NO                                           
         CLC   UKSUBPRG,SSBSYSN1                                                
         BNE   BACK02                                                           
         B     BACK08                                                           
*                                                                               
BACK06   TM    UKSTAT,WKSTHO       LOOK FOR HOLD FILES THAT DIDNT GET           
         BZ    BACK02              PROCESSED. WE DONT KNOW WHY !!               
         TM    UKSTAT,WKSTKE       TEST IF HOLD AND NOT KEEP                    
         BO    BACK02                                                           
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   BACK02                                                           
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   BACK02              NO                                           
         CLC   UKSUBPRG,SSBSYSN1                                                
         BNE   BACK02                                                           
         OI    SSBJFLAG,SSBJFWKR                                                
         B     BACKUPX             EXIT WITH FLAG SET TO GET GOING              
*                                                                               
BACK08   GOTO1 VDATAMGR,FWDMCB,(X'00',=C'HOL')                                  
         OI    SSBJFLAG,SSBJFWKR                                                
*                                                                               
BACKUPX  XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*DMFILTAB                                                                       
         PRINT ON                                                               
       ++INCLUDE DMFILTAB                                                       
         EJECT                                                                  
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    C                                                                
FLFIRSTQ EQU   X'80'               FIRST TIME FLAG (SET WHEN DONE)              
FLENQZQ  EQU   X'40'               ENQ MEDZ MESSAGE HAS BEEN SENT               
DMCB     DS    6F                                                               
*                                                                               
FWDMCB   DS    0XL24               FACWRK DMCB                                  
FWAACTN  DS    A                                                                
FWAFILE  DS    A                                                                
FWANDX   DS    A                                                                
FWAREC   DS    A                                                                
FWABUF   DS    A                                                                
         DS    A                                                                
*                                                                               
ACIREC   DS    A                                                                
*                                                                               
AIOA2    DS    A                   A(IOA2)                                      
AIOA3    DS    A                   A(IOA3)                                      
PARML    DS    6F                                                               
*                                                                               
FINDEX   DS    XL32                FACWRK INDEX                                 
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
DKEY     DS    XL64                KEY OF LAST DA RECORD ADDED                  
HEADER   DS    XL32                                                             
USID     DS    CL(L'FWRUSID)                                                    
LUID     DS    CL(L'FWRLUID)                                                    
PQID     DS    CL(L'FWRPQID)                                                    
PQIDL    DS    CL(L'FWRPQID)                                                    
WKID     DS    CL(L'FWRWKID)                                                    
IOWORK   DS    12D                                                              
SAVE     DS    50F                                                              
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
LASTFILE DS    CL1                 PREVIOUS FILE NUMBER                         
DALINK   DS    CL4                 DISK ADDRESS OF LAST ADDREC                  
DDA      DS    CL4                 DISK ADDRESS OF GETREC                       
DALAST   DS    CL1                 LAST DA FILE ADDED TO                        
SENUM    DS    CL1                 SWITCHED SYSTEM                              
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
LASTACTN DS    CL1                 LAST TYPE (COPY CHNG ADD)                    
ELCODE   DS    XL1                                                              
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
RECLEN   DS    XL2                 RECORD LENGTH                                
RECCOUNT DS    F                   RECORD COUNT                                 
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
*                                                                               
DEMSAVE  DS    XL6                 DEMO EXIT SAVE AREA FOR DEMPROS              
SAVEDT   DS    CL(L'RSTTDATE)      DATE FROM RECORD STATUS ELEMENT              
SAVE31   DS    CL(ASTLN1Q)         ACCOUNT STATUS ELEMENT                       
SAVE32   DS    CL(ABLLN2Q)         ACCOUNT BALANCE ELEMENT                      
SAVEJOB  DS    CL(ACCORLEN)        JOB KEY                                      
*                                                                               
RELO     DS    A                                                                
VSWITCH  DS    V                                                                
VHELLO   DS    V                                                                
VDATCON  DS    V                                                                
VCALLOVL DS    V                                                                
VLOCKET  DS    V                                                                
VHEXOUT  DS    V                                                                
*                                                                               
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ASELIST  DS    A                                                                
*                                                                               
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
SAVESIN  DS    F                                                                
NEXTSIN  DS    F                                                                
QSIN     DS    CL6                                                              
PRTQID   DS    CL8                                                              
NDX      DS    XL40                                                             
*                                                                               
OPMSG    DS    0CL60                                                            
OPENQID  DS    CL8                 *ENQUEUE                                     
         DS    CL1                                                              
OPSYSID  DS    CL4                 SESYS (MEDZ)                                 
         DS    CL1                                                              
OPCMSG   DS    CL15                MESSAGE                                      
         DS    CL1                                                              
OPFACID  DS    CL8                 FACPAKID                                     
         DS    CL2                                                              
*                                                                               
         ORG   OPMSG                                                            
OPSOON   DS    CL(L'SOONMSG)       *SOON DUMP*                                  
         DS    CL1                                                              
OPDLOC   DS    CL6                 LOCATION OF DEATH                            
         DS    CL1                                                              
OPUSID   DS    CL7                 USER                                         
         DS    CL1                                                              
OPPRGM   DS    CL4                 PROGRAM/SUB                                  
         DS    CL1                                                              
OPLUID   DS    CL8                 LUID                                         
         DS    CL1                                                              
         ORG   OPMSG+L'OPMSG                                                    
OPMSGL   EQU   *-OPMSG                                                          
*                                                                               
         ORG   OPMSG                                                            
OPINVS   DS    CL(L'INVSMSG)       UNABLE TO UNINVISIBLE A REPORT               
         DS    CL1                                                              
OPIUSID  DS    CL7                 USER                                         
         DS    CL1                                                              
OPIPRGM  DS    CL4                 PROGRAM/SUB                                  
         DS    CL1                                                              
OPILUID  DS    CL8                 LUID                                         
         DS    CL1                                                              
OPPQNAM  DS    CL3                 LUID                                         
OPCOMMA  DS    CL1                                                              
OPPQNUM  DS    CL4                                                              
         ORG   OPMSG+L'OPMSG                                                    
*                                                                               
*                                                                               
IOA      DS    XL28                FACWRK RECORD LEN AND RECOVERY HDR           
IOA1     DS    XL4096              FACWRK RECORD (MAX 4K)                       
IOA2     DS    XL4096              FILE RECORD                                  
IOA3     DS    XL4096              SAVE IOA1 FOR COMPARE                        
WBUFF    DS    XL14336             FACWRK & PQ BUFFER                           
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SRUPDD                                                         
         ORG   FWRHDR                                                           
       ++INCLUDE DMRCVRHDR                                                      
FWRDATA  DS    X                                                                
         EJECT                                                                  
PKRECD   DSECT                                                                  
*                                                                               
PKINDEX  DS    0CL24               PRTQ INDEX ENTRY                             
*                                                                               
PKKEY    DS    0CL7                                                             
PKSRCID  DS    XL2                 USER ID NUMBER                               
PKSUBID  DS    CL3                 REPORT ID                                    
PKREPNO  DS    XL2                 FILE REPORT NUMBER WITHIN USERID             
*                                                                               
PKCLASS  DS    XL1                 CLASS                                        
PKTYPE   DS    XL1                 TYPE                                         
PKATTB   DS    XL1                 ATTRIBUTES                                   
PKSTAT   DS    XL1                 FILE STATUS                                  
PKSEQ    DS    XL1                 CI SEQ NUM                                   
PKAGES   DS    XL1                 NUMBER OF CONTROL INTERVALS                  
PKAGELD  DS    XL2                 LIVE DATE                                    
PKAGEDD  DS    XL2                 DEAD DATE                                    
PKAGERD  DS    XL2                 RETN DATE                                    
         DS    XL1                                                              
PKAGELT  DS    XL2                 LIVE TIME (SECS*3)/4                         
         DS    XL2                                                              
*                                                                               
PKINFO   DS    XL2                 INFO PASSING FIELD                           
PKREPNOX DS    XL2                 UPPER LIMIT                                  
PKCIADDR DS    XL2                 TTTT OF FIRST CI                             
PKFLAG   DS    XL1                 FLAG VALUES                                  
PKFLDAT  EQU   X'80'               PASS BACK DATA                               
PKFLDSW  EQU   X'40'               SWITCH FROM INDEX TO DATA                    
PKFLUSR  EQU   X'20'               USER INFO SUPPLIED IN UKUSRINF               
PKFLHRS  EQU   X'10'               HOURS PASSED IN UKINFO                       
         DS    XL1                 N/D                                          
*                                                                               
PKUSRINF DS    XL8                 USER INFO                                    
         EJECT                                                                  
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DMFILTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMPRTQS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQS                                                        
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* TASYSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*&&US                                                                           
* SPTRPAT                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRPAT                                                        
         PRINT ON                                                               
* SPTRSHIP                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPTRSHIP                                                       
*&&                                                                             
*                                                                               
*&&UK                                                                           
* MEDEMFILED                                                                    
         PRINT OFF                                                              
       ++INCLUDE MEDEMFILED                                                     
*&&                                                                             
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRUPD00S  08/01/00'                                      
         END                                                                    
