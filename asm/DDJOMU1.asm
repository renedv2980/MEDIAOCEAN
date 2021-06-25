*          DATA SET DDJOMU1    AT LEVEL 012 AS OF 05/01/02                      
*          DATA SET SPADINT    AT LEVEL 024 AS OF 12/13/93                      
*PHASE T00A5E                                                                   
SPADINT  TITLE 'SPADINT - SPOT ADDS INTERFACE'                                  
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* SPADINT - SPOT ADDS INTERFACE MODULE                                *         
*                                                                     *         
* PARM1 = A(SPADINTD BLOCK)                                           *         
*                                                                     *         
* THIS MODULE MAINTAINS THE ADDS ELEMENTS IN THE STATUS RECORD FOR    *         
* THE REQUESTED MEDIA/CLIENT/PRODUCT/ESTIMATE/STATION.                *         
* THE ACTION IS PASSED IN ADACTN:                                     *         
* CHANGE  TURNS OFF THE SENT STATUS FLAG IN THE STATUS RECORD AND     *         
*         TURNS OFF ALL THE MATCHED STATUS FLAGS.                     *         
* MATCH   TURNS ON THE MATCHED STATUS FLAG FOR THE REQUESTED          *         
*         YEAR/MONTH.                                                 *         
* UNMATCH TURNS OFF THE MATCHED STATUS FLAG FOR THE REQUESTED         *         
*         YEAR/MONTH.                                                 *         
* SEND    SENDS A SOON JOB CONFIRMATION OF PURCHASE (SPCP) TO THE REP *         
*         FOR EACH STATION IN THE REQUESTED MARKET OR FOR JUST THE    *         
*         REQUESTED STATION. IF A SOON PROCESSING ERROR IS            *         
*         ENCOUNTERED, AN ERROR MESSAGE IS FORMATTED IN THE MESSAGE   *         
*         AREA.                                                       *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
SPADINT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**ADIN**,RA,RR=RE,CLEAR=YES                          
         USING WORKD,RC            RC=A(LOCAL WORKING STORAGE)                  
         ST    RE,RELO                                                          
         L     R9,0(R1)                                                         
         USING SPADINTD,R9         R9=A(SPADINT BLOCK)                          
         L     R8,ADACOMFC                                                      
         USING COMFACSD,R8         R8=A(COMFACS)                                
*                                                                               
         OC    CCALLOV,CCALLOV     TEST A(CALLOV) IS AVAILABLE                  
         BZ    INIT3               NO                                           
         OC    ADACLUNP,ADACLUNP                                                
         BNZ   INIT1                                                            
         GOTO1 CCALLOV,PARM,0,X'D9000A15'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADACLUNP,0(R1)                                                   
*                                                                               
INIT1    OC    ADAMSUNP,ADAMSUNP                                                
         BNZ   INIT2                                                            
         GOTO1 CCALLOV,PARM,0,X'D9000A1C'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADAMSUNP,0(R1)                                                   
*                                                                               
INIT2    OC    ADASQUAS,ADASQUAS                                                
         BNZ   INIT3                                                            
         GOTO1 CCALLOV,PARM,0,X'D9000A0D'                                       
         CLI   4(R1),X'FF'                                                      
         BE    INIT3                                                            
         MVC   ADASQUAS,0(R1)                                                   
*                                                                               
INIT3    MVI   ADERRS,0                                                         
         MVI   ADINDS,0                                                         
         MVC   BPROD1,ADQPRD       SET THE PRODUCTS                             
         MVC   BPROD2,ADQPRD2                                                   
         MVC   PROD1,ADQAPRD                                                    
         MVC   PROD2,ADQAPRD2                                                   
         OC    PROD2,BLANKS                                                     
         CLI   ADQPRD2,0           TEST 2 ALPHA PRODUCTS                        
         BE    INIT4                                                            
         CLC   ADQAPRD2,BLANKS                                                  
         BNH   INIT4                                                            
         CLC   ADQAPRD,BLANKS                                                   
         BNH   INIT4                                                            
         CLC   ADQAPRD,ADQAPRD2    YES-KEEP THEM IN APLHA ORDER                 
         BNH   INIT4                                                            
         MVC   BPROD1,ADQPRD2                                                   
         MVC   BPROD2,ADQPRD                                                    
         MVC   PROD1,ADQAPRD2                                                   
         MVC   PROD2,ADQAPRD                                                    
*                                                                               
INIT4    CLI   ADACTN,ADACHG       ACTION CHANGE                                
         BE    CHG                                                              
         CLI   ADACTN,ADAMATCH            MATCH                                 
         BE    MAT                                                              
         CLI   ADACTN,ADAUNMAT            UNMATCH                               
         BE    MAT                                                              
         CLI   ADACTN,ADASEND             SEND                                  
         BE    SEND                                                             
         CLI   ADACTN,ADARESEN            RESEND                                
         BE    SEND                                                             
         MVI   ADERRS,ADERACT                                                   
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE                                                              *         
* ------                                                              *         
* RESETS THE STATUS RECORD ACTIVITY FLAG.                             *         
* THIS ANNOUNCES THAT A BUY RECORD CHANGE HAS OCCURRED AND THAT THE   *         
* STATION IS READY FOR A SEND.                                        *         
***********************************************************************         
         SPACE 1                                                                
CHG      DS    0H                                                               
         BAS   RE,RDSTAT           READ THE STATUS RECORD                       
         BNE   CHGX                                                             
         USING ASTELEM,R3                                                       
         ICM   R3,15,AASTELEM      IS THERE A STATUS ELEMENT?                   
         BNZ   CHG2                                                             
         LA    R3,ELEM             NO-BUILD ONE                                 
         XC    ELEM,ELEM                                                        
         MVI   ASTCODE,ASTCODEQ                                                 
         MVI   ASTLEN,ASTLENQ                                                   
*                                                                               
CHG2     NI    ASTSTAT,255-ASTSENT   TURN OFF 'NO SEND REQUIRED'                
         OC    AASTELEM,AASTELEM   ADD THE ELEMENT?                             
         BNZ   CHG4                                                             
         OI    ADINDS,ADINEWEL     YES-INDICATE NEW STATUS ELEMENT              
         GOTO1 CHELLO,PARM,(C'P',FIL),IOAREA,ELEM                               
         CLI   12(R1),0                                                         
         BE    CHG4                                                             
         DC    H'0'                                                             
*                                                                               
CHG4     ICM   R3,15,AAMSELEM      TEST FOR MATCHING STATUS ELEMENTS            
         BZ    CHG8                                                             
*                                                                               
         USING AMSELEM,R3                                                       
CHG6     NI    AMSSTAT,255-AMSSMAT   YES-TURN OFF ALL MATCHED BITS              
*                                                                               
CHG7     SR    R0,R0                                                            
         ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    CHG8                                                             
         CLI   0(R3),AMSCODEQ                                                   
         BE    CHG6                                                             
         B     CHG7                                                             
*                                                                               
CHG8     BAS   RE,WRTSTAT          WRITE BACK THE STATUS RECORD                 
         BNE   CHGX                                                             
*                                                                               
CHGX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MATCH AND UNMATCH                                                   *         
* -----------------                                                   *         
* TURNS ON OR OFF THE MATCHED FLAG IN THE MATCHING STATUS ELEMENT FOR *         
* THE REQUESTED YEAR/MONTH IN THE STATUS RECORD.                      *         
***********************************************************************         
         SPACE 1                                                                
MAT      DS    0H                                                               
         BAS   RE,RDSTAT                                                        
         BNE   MATX                                                             
         USING AMSELEM,R3                                                       
         ICM   R3,15,AAMSELEM      IS THERE A MATCHING ELEMENT?                 
         BNZ   MAT6                                                             
*                                                                               
MAT2     CLI   ADACTN,ADAUNMAT     NO-EXIT NOW IF ACTION=UNMATCH                
         BE    MATX                                                             
         OI    ADINDS,ADINEWMS     INDICATE NEW MATCHING ELEMENT                
         LA    R3,ELEM             BUILD ONE                                    
         XC    ELEM,ELEM                                                        
         MVI   AMSCODE,AMSCODEQ                                                 
         MVI   AMSLEN,AMSLENQ                                                   
         MVC   AMSYM,ADQYM         MATCH YEAR/MONTH                             
         B     MAT10                                                            
*                                                                               
MAT4     CLI   0(R3),0                                                          
         BE    MAT2                                                             
         CLI   0(R3),AMSCODEQ                                                   
         BNE   MAT8                                                             
*                                                                               
MAT6     CLC   AMSYM,ADQYM         GET THE RIGHT YEAR/MONTH                     
         BE    MAT10                                                            
*                                                                               
MAT8     ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     MAT4                                                             
*                                                                               
MAT10    CLI   ADACTN,ADAUNMAT     TURN MATCHED FLAG ON OR OFF                  
         BE    *+12                                                             
         OI    AMSSTAT,AMSSMAT                                                  
         B     *+8                                                              
         NI    AMSSTAT,255-AMSSMAT                                              
         TM    ADINDS,ADINEWMS     ADD THE ELEMENT?                             
         BZ    MAT12                                                            
         GOTO1 CHELLO,PARM,(C'P',FIL),IOAREA,ELEM                               
         CLI   12(R1),0                                                         
         BE    MAT12                                                            
         DC    H'0'                                                             
*                                                                               
MAT12    BAS   RE,WRTSTAT          WRITE BACK THE STATUS RECORD                 
         BNE   MATX                                                             
*                                                                               
MATX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SEND                                                                *         
* ----                                                                *         
***********************************************************************         
         SPACE 1                                                                
SEND     DS    0H                                                               
         LA    R5,ADSTAB           R5=A(STATION TABLE)                          
         XC    0(ADSTABL,R5),0(R5)                                              
         OC    ADQSTA,ADQSTA       TEST SINGLE STATION                          
         BZ    SEND1                                                            
         BAS   RE,RDSTAT           YES-READ THE STATUS RECORD                   
         BNE   SENDX                                                            
         LA    R2,IOAREA                                                        
         USING STATD,R2                                                         
         ICM   R3,15,AASTELEM      IS THERE A STATUS ELEMENT?                   
         BNZ   SEND5                                                            
         OI    ADINDS,ADINEWEL     NO-ADD ONE                                   
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING ASTELEM,R3                                                       
         MVI   ASTCODE,ASTCODEQ                                                 
         MVI   ASTLEN,ASTLENQ                                                   
         B     SEND5                                                            
*                                                                               
SEND1    XC    KY,KY                                                            
         LA    R2,KY               BUILD STATUS RECORD KEY                      
         USING STATD,R2                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD,ADQAGYMD                                                 
         MVC   STKCLT,ADQCLT                                                    
         MVC   STKPRD,BPROD1                                                    
         MVC   STKPRD2,BPROD2                                                   
         MVC   STKEST,ADQEST                                                    
         MVC   STKMKT,ADQMKT                                                    
         LA    R6,RDHI                                                          
         B     SEND3                                                            
*                                                                               
SEND2    OC    ADQSTA,ADQSTA       TEST SINGLE STATION                          
         BNZ   SENDX                                                            
         LA    R6,RDSQ             NO-READ SEQ FOR NEXT                         
*                                                                               
SEND3    MVI   ADRECTYP,ADQSTAT    READ STATUS RECORD                           
         MVC   KYSAVE,KY                                                        
         LA    R2,KYSAVE                                                        
         GOTO1 CDATAMGR,PARM,(R6),DIR,KYSAVE,KY,(0,DWORK)                       
         CLI   8(R1),0                                                          
         BNE   SEND99                                                           
         CLC   KY(STKSTA-STATKEY),KYSAVE                                        
         BNE   SENDX                                                            
         GOTO1 CDATAMGR,PARM,(UPD,GETR),FIL,KY+14,IOAREA,(0,DWORK)              
         CLI   8(R1),0                                                          
         BNE   SEND99                                                           
         LA    R2,IOAREA                                                        
         LA    R3,STELEMS          LOOK FOR ADDS VERSION ELEMENT                
         SR    R0,R0                                                            
*                                                                               
SEND4    CLI   0(R3),0                                                          
         BE    SEND2               NOT FOUND-IGNORE THIS STATION                
         CLI   0(R3),ASTCODEQ                                                   
         BE    SEND5                                                            
         ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         B     SEND4                                                            
*                                                                               
         USING ASTELEM,R3          FOUND-                                       
SEND5    CLI   ADACTN,ADARESEN     RESEND ALWAYS SENDS                          
         BE    SEND6                                                            
         CLC   ADQBUYER,BLANKS     TEST BUYER'S WORKSHEET CALL                  
         BNH   SEND6               NO-BUY PROGRAM CAN ALWAYS RESEND             
         TM    ASTSTAT,ASTSENT     YES-SEND ONLY IF NOT ALREADY SENT            
         BO    SEND2                                                            
*                                                                               
SEND6    OI    ASTSTAT,ASTSENT     UPDATE THE STATUS                            
         CLI   ADACTN,ADARESEN     TEST RESEND                                  
         BE    SEND7               YES-KEEP VERSIONS THE SAME                   
         MVC   ASTDATE2,ASTDATE1   SHUFFLE VERSIONS DOWN                        
         MVC   ASTVER2,ASTVER1                                                  
         MVC   ASTDATE1,ASTDATE                                                 
         MVC   ASTVER1,ASTVER                                                   
         ZIC   RF,ASTVER           INCREMENT CURRENT VERSION NUMBER             
         LA    RF,1(RF)                                                         
         STC   RF,ASTVER                                                        
         CLI   ASTVER,0                                                         
         BNE   *+8                                                              
         MVI   ASTVER,1                                                         
*                                                                               
SEND7    GOTO1 CDATCON,PARM,(5,0),(2,ASTDATE)  VERSION DATE = TODAY             
*                                                                               
         TM    ADINDS,ADINEWEL     TEST NEW STATUS ELEMENT                      
         BZ    SEND8                                                            
         GOTO1 CHELLO,PARM,(C'P',FIL),IOAREA,ELEM   YES-ADD IT NOW              
         CLI   12(R1),0                                                         
         BE    SEND8                                                            
         DC    H'0'                                                             
*                                                                               
         USING ADSTAB,R5                                                        
SEND8    MVC   ADSTA,STKSTA        BUILD STATION TABLE ENTRY                    
         MVC   ADSVER,ASTVER                                                    
         MVC   ADSPVER,ASTVER1                                                  
         MVC   ADSPDATE,ASTDATE1                                                
*                                                                               
         MVC   MYDUB(2),ADQMKT                                                  
         MVC   MYDUB+2(3),STKSTA                                                
         GOTO1 ADAMSUNP,PARM,MYDUB,MKTSV,STASV                                  
*                                                                               
         BAS   RE,GETID            GET REP'S RECEIVING ID                       
         BE    SEND9                                                            
         OI    ADSERR,ADSEREPI     ERROR                                        
         OC    ADQSTA,ADQSTA       TEST SINGLE STATION REQUEST                  
         BNZ   SENDX               YES-EXIT NOW                                 
         MVI   ADERRS,0            NO-ONLY MARK THIS STATION WITH ERROR         
         B     SEND10                 AND ADVANCE TO NEXT STATION               
*                                                                               
SEND9    BAS   RE,SOON             ADD A SOON JOB                               
         BNE   SENDX                                                            
         MVC   ADSRPTID,RPTID      SAVE THE REPORT ID AND NUMBER                
         MVC   ADSRPTNO,RPTNUM                                                  
         MVC   ADSREPID,REPSV      SAVE REP ID                                  
         BAS   RE,WRTSTAT          WRITE BACK THE STATUS RECORD                 
         BNE   SENDX                                                            
*                                                                               
SEND10   LA    R5,L'ADSTAB(R5)     POINT TO NEXT                                
         XC    KY,KY                                                            
         MVC   KY(L'STATKEY),STATKEY  RE-ESTABLISH READ SEQUENCE                
         GOTO1 CDATAMGR,PARM,RDHI,DIR,KY,KY,(0,DWORK)                           
         B     SEND2               READ NEXT STATION                            
*                                                                               
SEND99   OI    ADERRS,ADERDM       DATAMGR ERROR                                
         ST    R2,ADKEYERR         SET ERROR ARG FOR USER                       
*                                                                               
SENDX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ THE STATUS RECORD                                   *         
* RETURNS STATUS RECORD IN IOAREA                                     *         
* ADINDS=ADINEWST IF RECORD DOES NOT EXIST                            *         
* AASTELEM=A(STATUS ELEMENT)                                          *         
* AAMSELEM=A(MATCHING ELEMENT)                                        *         
***********************************************************************         
         SPACE 1                                                                
RDSTAT   NTR1  ,                                                                
         XC    AASTELEM,AASTELEM   CLEAR A(STATUS ELEMENT)                      
         XC    AAMSELEM,AAMSELEM   CLEAR A(MATCHING ELEMENT)                    
         XC    KY,KY                                                            
         LA    R2,KY               BUILD STATUS RECORD KEY                      
         USING STATD,R2                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD,ADQAGYMD                                                 
         MVC   STKCLT,ADQCLT                                                    
         MVC   STKPRD,BPROD1                                                    
         MVC   STKPRD2,BPROD2                                                   
         MVC   STKEST,ADQEST                                                    
         MVC   STKMKT,ADQMKT                                                    
         MVC   STKSTA,ADQSTA                                                    
         MVC   KYSAVE,KY                                                        
         GOTO1 CDATAMGR,PARM,RDHI,DIR,KY,KY,(0,DWORK)                           
         CLI   8(R1),0                                                          
         BNE   RDSTAT9                                                          
         CLC   STATKEY,KYSAVE      TEST RECORD FOUND                            
         BE    RDSTAT2                                                          
         OI    ADINDS,ADINEWST     NO-INDICATE NEW RECORD                       
         LA    R2,IOAREA                                                        
         XC    0(256,R2),0(R2)     BUILD SKELETON RECORD                        
         MVC   STATKEY,KYSAVE                                                   
         MVC   STLEN,=Y(STELEMS-STATKEY)                                        
         MVC   STALPHA,ADQAGY                                                   
         B     RDSTAT8                                                          
*                                                                               
RDSTAT2  GOTO1 CDATAMGR,PARM,(UPD,GETR),FIL,KY+14,IOAREA,(0,DWORK)              
         CLI   8(R1),0                                                          
         BNE   RDSTAT9                                                          
         LA    R2,IOAREA                                                        
         LA    R3,STELEMS          SEARCH RECORD FOR ELEMENTS                   
         SR    R0,R0                                                            
*                                                                               
RDSTAT4  CLI   0(R3),0                                                          
         BE    RDSTAT8                                                          
         CLI   0(R3),ASTCODEQ                                                   
         BNE   *+12                                                             
         ST    R3,AASTELEM                                                      
         B     RDSTAT6                                                          
         CLI   0(R3),AMSCODEQ                                                   
         BNE   RDSTAT6                                                          
         OC    AAMSELEM,AAMSELEM                                                
         BNZ   RDSTAT6                                                          
         ST    R3,AAMSELEM                                                      
*                                                                               
RDSTAT6  ICM   R0,1,1(R3)                                                       
         BZ    RDSTAT8                                                          
         AR    R3,R0                                                            
         B     RDSTAT4                                                          
*                                                                               
RDSTAT8  B     RDSTATX                                                          
*                                                                               
RDSTAT9  OI    ADERRS,ADERDM       DATAMGR ERROR                                
*                                                                               
RDSTATX  CLI   ADERRS,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE THE STATUS RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
WRTSTAT  NTR1  ,                                                                
         TM    ADINDS,ADINEWST     TEST NEW RECORD                              
         BZ    WRTSTAT2                                                         
         GOTO1 CDATAMGR,PARM,ADDR,FIL,KY,IOAREA,(0,DWORK)                       
         B     WRTSTAT4                                                         
*                                                                               
WRTSTAT2 GOTO1 CDATAMGR,PARM,PUTR,FIL,KY+14,IOAREA,(0,DWORK)                    
*                                                                               
WRTSTAT4 CLI   8(R1),0             TEST DATAMGR ERROR                           
         BE    WRTSTATX                                                         
         OI    ADERRS,ADERDM                                                    
*                                                                               
WRTSTATX CLI   ADERRS,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET REP'S RECEIVING ID                                              *         
* ON EXIT,  REPSV=REP ID                                              *         
*           CITY=LOCATION OVERRIDE                                    *         
***********************************************************************         
         SPACE 1                                                                
GETID    NTR1  ,                                                                
         MVC   REPSV,BLANKS                                                     
         MVC   CITY,BLANKS                                                      
         MVI   ADRECTYP,ADQSTATN   READ ADDS STATION RECORD                     
         XC    KY,KY                                                            
         LA    R2,KY                                                            
         USING STTNRECD,R2                                                      
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYPQ                                                 
         MVC   STTNCALL,STASV                                                   
         CLI   STTNCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STTNCALL+4,C'T'                                                  
         GOTO1 CDATAMGR,PARM,DMRD,CTFIL,KY,CTIO,0                               
         CLI   8(R1),0                                                          
         BNE   GETID9                                                           
         LA    R2,CTIO                                                          
         LA    R3,28(R2)           FIND DESCRIPTION ELEMENT                     
         SR    R0,R0                                                            
         GOTO1 CDATCON,PARM,(5,0),(3,TODAYSDT) GET TODAY'S DATE                 
*                                                                               
GETID2   CLI   0(R3),0                                                          
         BE    GETID9                                                           
         CLI   0(R3),STTNDCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETID2                                                           
         USING STTNDSCD,R3                                                      
         MVC   REPSV(3),STTNDREP   EXTRACT THE STATION'S REP CODE               
         CLC   STTNDEFF,TODAYSDT   TEST EFFECTIVE DATE AFTER TODAY              
         BNH   *+10                                                             
         MVC   REPSV(3),STTNDPRP   YES-USE PREVIOUS REP                         
*                                                                               
         MVI   ADRECTYP,ADQDIRAD   READ ADDS DIR RECORD                         
         XC    KY,KY                                                            
         LA    R2,KY                                                            
         USING DIRKEYD,R2                                                       
         MVI   DIRKSYS,DIRKSYSQ                                                 
         MVI   DIRTYPE,DIRTYPEQ                                                 
         MVC   DIRMED,ADQMED                                                    
         MVC   DIRID,ADUSRID                                                    
*                                                                               
         GOTO1 CDATAMGR,PARM,DMRD,CTFIL,KY,CTIO,0                               
         CLI   8(R1),0                                                          
         BNE   GETID9                                                           
         LA    R2,CTIO                                                          
         LA    R3,28(R2)                                                        
         SR    R0,R0               FIND REP ELEMENT                             
         LA    RE,2                                                             
         CLI   REPSV+2,C' '                                                     
         BH    GETID3                                                           
         BCTR  RE,0                                                             
*                                                                               
GETID3   CLI   0(R3),0                                                          
         BE    GETID6                                                           
         CLI   0(R3),DIRREPEQ                                                   
         BNE   GETID4                                                           
         USING DIRREPD,R3                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DIRREPID(0),REPSV   COMPARE FOR STATION'S REP                    
         BNE   GETID5                                                           
         MVC   REPSV,DIRREPID      YES-SAVE THE REP OFFICE                      
         B     GETID5                                                           
*                                                                               
GETID4   CLI   0(R3),DIRLOVEQ      PICK UP LOCATION OVERRIDE IF ANY             
         BNE   GETID5                                                           
         USING DIRLOVD,R3                                                       
         MVC   CITY,DIRLOVER                                                    
*                                                                               
GETID5   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETID3                                                           
*                                                                               
GETID6   CLC   REPSV+3(7),BLANKS   TEST REP OFFICE FOUND                        
         BE    GETID9              NO-ERROR                                     
*                                                                               
         B     GETIDX              *** REP'S ID CODE NO LONGER NEEDED           
*                                                                               
         MVI   ADRECTYP,ADQID      READ ID RECORD FOR REP OFFICE                
         XC    KY,KY                                                            
         LA    R2,KY                                                            
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,REPSV                                                     
         GOTO1 CDATAMGR,PARM,DMRD,CTFIL,KY,CTIO,0                               
         CLI   8(R1),0                                                          
         BNE   GETID9                                                           
         LA    R2,CTIO                                                          
         LA    R3,28(R2)                                                        
         SR    R0,R0               FIND DESCRIPTION ELEMENT                     
*                                                                               
GETID7   CLI   0(R3),0                                                          
         BE    GETID9                                                           
         CLI   0(R3),2                                                          
         BE    *+14                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETID7                                                           
         MVC   DESTID,2(R3)        EXTRACT THE REP'S ID                         
         B     GETIDX                                                           
*                                                                               
GETID9   OI    ADERRS,ADERDM       DATAMGR ERROR                                
         ST    R2,ADKEYERR         SET KEY/REC ADDRESS FOR USER                 
*                                                                               
GETIDX   CLI   ADERRS,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SEND A SOON JOB                                                     *         
* ON ENTRY, R5=A(STATION TABLE ENTRY)                                 *         
* ON EXIT,  CC EQ IF SOON JOB SENT AND RPTID AND RPTNUM ARE SET       *         
*           CC NE IF SOON ERROR ENCOUNTERED AND ERROR MESSAGE WILL BE *         
*                 FORMATTED TO THE MESSAGE FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
SOON     NTR1  ,                                                                
         USING ADSTAB,R5                                                        
         LA    R4,SPOOKWRK                                                      
         USING SPOOK,R4                                                         
         XC    SPOOK(SPOOKL),SPOOK                                              
         MVC   SPOOKUID,ADUSRID                                                 
         MVC   SPOOKDES,ADUSRID                                                 
         MVC   SPOOKAGY,ADQAGY                                                  
         MVC   SPOOKDID,=C'QXX'                                                 
         MVC   RPTID,SPOOKDID                                                   
         MVC   SPOOKSYS,=C'SP'                                                  
         MVC   SPOOKEOD,=C'DO'                                                  
         MVC   SPOOKJCL,=C'DO'                                                  
         MVI   SPOOKWEN,2          SOON                                         
         MVC   SPOOKRLD(3),=C'LD='                                              
         MVC   SPOOKRLD+3(2),=H'48'                                             
         MVC   SPOOKRLD+5(2),=H'24'                                             
         DROP  R4                                                               
*                                                                               
         XC    REQHDR(REQEOH-REQHDR),REQHDR                                     
         MVC   REQDEST,ADUSRID                                                  
         MVI   REQUEST,C' '                                                     
         MVC   REQUEST+1(L'REQUEST-1),REQUEST                                   
         MVC   REQUEST2,REQUEST                                                 
         MVC   REQUEST3,REQUEST                                                 
         MVC   DESTCARD,REQUEST                                                 
         MVC   TRNCARD,REQUEST                                                  
         MVC   HDRCARD,REQUEST                                                  
         MVC   STACARD,REQUEST                                                  
         MVC   TLRCARD,REQUEST                                                  
*                                                                               
         LA    R4,DESTCARD         DESTINATION CONTROL CARD                     
         MVC   0(7,R4),=C'=EZCTL='                                              
         MVC   11(5,R4),=C'*HDR*'  COL 5 OF EDICT'S HEADER CARD                 
         MVC   16(6,R4),=C'EDICT=' COL 10                                       
         MVC   22(8,R4),REPSV      COL 16 - REP ID                              
         MVI   41(R4),C'W'         COL 35 - REPORT IS 132 CHARS WIDE            
         MVI   42(R4),C'P'         COL 36 - PAGE BREAK                          
         MVC   45(8,R4),REPSV      COL 39 - FORMATTED DEST ID                   
         MVC   61(1,R4),ADQMED     COL 55 - BILLING INFO                        
         GOTO1 ADACLUNP,PARM,ADQCLT,QCLIENT                                     
         MVC   62(3,R4),QCLIENT                                                 
*                                                                               
         LA    R4,TRNCARD          TRANSACTION REPORT CONTROL CARD              
         MVC   0(7,R4),=C'=EZCTL='                                              
         MVC   7(14,R4),=C'++DDS SPORDTRN'   COL 1 FOR EDICT                    
         LA    R4,22(R4)           COL 16 - APPLICATION AREA                    
         USING SPEDICTD,R4                                                      
         MVI   SPEDTYPE,SPEDTYDO   TYPE = DRAFT OREDER                          
         MVC   SPEDMED,ADQMED      MEDIA                                        
         MVC   SPEDCLT,QCLIENT     CLIENT                                       
         MVC   SPEDPRD,PROD1       PRODUCT                                      
         MVC   SPEDPRD2,PROD2      PARTNER PRODUCT                              
         ZIC   RF,ADQEST           ESTIMATE                                     
         CVD   RF,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  QESTIM,MYDUB                                                     
         MVC   SPEDEST,QESTIM                                                   
         MVC   SPEDMKT,MKTSV       MARKET                                       
         MVC   SPEDSTA,STASV       STATION                                      
         MVC   SPEDQUES,ADQUEST    REQUESTOR                                    
         MVC   SPEDCITY,CITY       LOCATION OVERRIDE                            
         MVC   SPEDBYR,ADQBUYER    BUYER                                        
         SR    R1,R1                                                            
         ICM   R1,3,ADQCAMP        CAMPAIGN                                     
         BZ    SOON1                                                            
         CVD   R1,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  QCAMP,MYDUB                                                      
         MVC   SPEDCAM,QCAMP                                                    
*                                                                               
SOON1    OC    ADQSTART,ADQSTART   TEST FLIGHT DATES PASSED                     
         BZ    SOON2                                                            
         GOTO1 CDATCON,PARM,(2,ADQSTART),(0,RQSTART)                            
         GOTO1 (RF),(R1),(2,ADQEND),(0,RQEND)                                   
         MVC   SPEDFLST,RQSTART                                                 
         MVC   SPEDFLND,RQEND                                                   
         DROP  R4                                                               
*                                                                               
SOON2    LA    R4,HDRCARD          'HDRHDR' CARD                                
         MVC   0(18,R4),TRNCARD                                                 
         MVC   15(6,R4),=C'ADDHDR'                                              
         MVC   22(2,R4),=C'DO'     COL 16 - DRAFT ORDER                         
         MVC   24(1,R4),ADQMED     COL 18 - AGENCY REFERENCE                    
         MVC   25(3,R4),QCLIENT                                                 
         MVC   28(3,R4),PROD1                                                   
         MVC   31(3,R4),QESTIM                                                  
         MVC   48(1,R4),ADRTGSVC   RATING SERVICE                               
         MVC   56(2,R4),CITY       COL 50 - LOCATION OVERRIDE FOR EDICT         
*                                                                               
         LA    R4,STACARD          'STASTA' CARD                                
         MVC   0(18,R4),HDRCARD                                                 
         MVC   18(3,R4),=C'STA'                                                 
         SR    RE,RE               RATING SERVICE MARKET NUMBER                 
         ICM   RE,3,ADRSMKT                                                     
         CVD   RE,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  22(4,R4),MYDUB                                                   
         MVC   26(5,R4),STASV      STATION CALL LETTERS                         
*                                                                               
         LA    R4,TLRCARD          'TLRTLR' CARD                                
         MVC   0(18,R4),HDRCARD                                                 
         MVC   18(3,R4),=C'TLR'                                                 
*                                                                               
         MVC   RQHDR2,RQHDR        FORMAT SPOOF REQUEST CARDS                   
         MVC   RQAGY,ADQAGY                                                     
*                                                                               
         MVC   RQAREA2(RQAREAL),RQAREA                                          
         MVC   RQBYR,ADQBUYER                                                   
         CLC   ADQBUYER,BLANKS                                                  
         BH    *+10                                                             
         MVC   RQBYR,=C'QXX'                                                    
         MVC   RQMED,ADQMED                                                     
         MVC   RQCLT,QCLIENT                                                    
         OC    RQCLT,BLANKS                                                     
         MVC   RQPRD(3),PROD1                                                   
         CLC   PROD2,BLANKS                                                     
         BNH   SOON2A                                                           
         LA    RE,7                                                             
         LA    R1,RQPRD+3                                                       
         CLI   RQPRD+2,C' '                                                     
         BH    *+8                                                              
         BCTR  RE,0                                                             
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(3,R1),PROD2                                                    
         CLI   PROD2+2,C' '                                                     
         BH    *+6                                                              
         BCTR  RE,0                                                             
         CVD   RE,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  RQPRDHDR+3(2),MYDUB                                              
*                                                                               
SOON2A   MVC   RQEST,QESTIM                                                     
         MVC   RQMKT,MKTSV                                                      
         LA    RE,4                STATION                                      
         MVC   RQSTA(4),STASV                                                   
         CLI   STASV+3,C' '                                                     
         BH    *+6                                                              
         BCTR  RE,0                                                             
         CLI   STASV+4,C'T'                                                     
         BE    SOON2B                                                           
         CLI   STASV+4,C' '                                                     
         BE    SOON2B                                                           
         LA    RE,6                                                             
         LA    R1,RQSTA+4                                                       
         CLI   STASV+3,C' '                                                     
         BH    *+8                                                              
         BCTR  R1,0                                                             
         BCTR  RE,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),STASV+4                                                  
*                                                                               
SOON2B   CVD   RE,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  RQSTAHDR+3(2),MYDUB                                              
         OC    ADQSTART,ADQSTART                                                
         BNZ   *+14                                                             
         MVC   RQPERHDR,BLANKS                                                  
         B     SOON3                                                            
         GOTO1 CDATCON,PARM,(2,ADQSTART),(8,RQPER)                              
         MVI   RQPER+8,C'-'                                                     
         GOTO1 (RF),(R1),(2,ADQEND),(8,RQPER+9)                                 
*                                                                               
SOON3    CLC   PROD2,BLANKS        DON'T SPLIT PIGGYBACKS                       
         BH    SOON3A              IF PIGGYBACK REQUEST                         
         CLC   PROD1,=C'POL'       OR POL REQUEST                               
         BE    SOON3A                                                           
         MVC   RQPIGOPT,BLANKS                                                  
         MVC   RQOPTHDR+3(2),=C'16'                                             
*                                                                               
SOON3A   MVC   RQBYRCD,ADQBUYER                                                 
         OC    RQBYRCD,BLANKS                                                   
         CLC   RQBYRCD,BLANKS                                                   
         BH    *+10                                                             
         MVC   RQBYRHDR,BLANKS                                                  
         MVC   RQBYRNM,ADQUEST                                                  
         OC    RQBYRNM,BLANKS                                                   
         CLC   RQBYRNM,BLANKS                                                   
         BH    *+14                                                             
         MVC   RQBNMHDR,BLANKS                                                  
         B     SOON4                                                            
         LA    RE,L'RQBYRNM                                                     
         LA    R1,RQBYRNM+L'RQBYRNM-1                                           
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   RE,*-10                                                          
         DC    H'0'                                                             
         CVD   RE,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  RQBNMHDR+3(2),MYDUB                                              
*                                                                               
SOON4    OC    ADQCAMP,ADQCAMP                                                  
         BNZ   *+14                                                             
         MVC   RQCAMHDR,BLANKS                                                  
         B     *+10                                                             
         MVC   RQCAM,QCAMP                                                      
         ZIC   RE,ADSVER                                                        
         CVD   RE,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  RQCURVER,MYDUB                                                   
         ZIC   RE,ADSPVER                                                       
         CVD   RE,MYDUB                                                         
         OI    MYDUB+7,X'0F'                                                    
         UNPK  RQPREVER,MYDUB                                                   
         CLI   ADSPVER,0                                                        
         BNE   *+14                                                             
         MVC   RQPREHDR,BLANKS                                                  
         B     SOON5                                                            
         GOTO1 CDATCON,PARM,(2,ADSPDATE),(8,RQPREDT)                            
*                                                                               
SOON5    CLC   REPSV(3),=C'KAT'    TEST DESTINATION = KATZ                      
         BE    *+14                                                             
         MVC   RQLPPHDR,BLANKS                                                  
         B     *+10                                                             
         MVC   RQLPP,=C'45'        YES-RESTRICT RPT TO 45 LPP                   
*                                                                               
         TM    ADQINDS,ADQIDLY     TEST DAILY SCHEDULE                          
         BZ    *+8                                                              
         MVI   RQDAILY,C'Y'                                                     
*                                                                               
         LA    R1,RQAREAL          GET RID OF UNNECESSARY BLANKS                
         ST    R1,PARM+4                                                        
         L     RF,ADASQUAS                                                      
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),PARM,RQAREA2                                                
         ICM   R7,15,4(R1)         R7=TOTAL LENGTH                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R2,RQAREA2          MOVE TO REQUEST CARDS                        
         LA    R3,REQUEST                                                       
         SR    R6,R6               R6=REQUEST CARD COUNT                        
*                                                                               
SOON6    MVC   0(12,R3),RQHDR2                                                  
         MVC   12(68,R3),0(R2)                                                  
         LA    R6,1(R6)                                                         
         SH    R7,=H'68'                                                        
         BNP   SOON9                                                            
         LA    R2,67(R2)                                                        
         CLI   0(R2),C' '                                                       
         BNE   *+12                                                             
         BAS   RE,CHKSTART                                                      
         BE    SOON8                                                            
         LA    R1,79(R3)                                                        
*                                                                               
SOON7    CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BAS   RE,CHKSTART                                                      
         BE    SOON8                                                            
         MVI   0(R1),C' '                                                       
         LA    R7,1(R7)                                                         
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         B     SOON7                                                            
*                                                                               
SOON8    LTR   R7,R7               TEST MORE                                    
         BNP   SOON9                                                            
         LA    R2,1(R2)            YES-                                         
         LA    R3,80(R3)           NEXT REQUEST CATD                            
         B     SOON6                                                            
*                                                                               
SOON9    LA    R6,4(R6)            SET TOTAL N'CARDS - 1                        
         SLL   R6,4                                                             
         STC   R6,REQFLAG                                                       
*                                                                               
         GOTO1 CREQTWA,PARM,(5,ADATWA),REQHDR,CDATAMGR,ADACOMFC,       X        
               SPOOKWRK                                                         
*                                                                               
         LA    R4,GETTXTWK         BUILD GETTXT BLOCK                           
         USING GETTXTD,R4                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         CLI   8(R1),X'FE'         TEST FOR ERRORS                              
         BNE   *+14                                                             
         MVC   GTMSGNO,ERTFULL     TERMINAL QUEUE FULL                          
         B     SOON10                                                           
         CLI   8(R1),X'FF'                                                      
         BNE   *+14                                                             
         MVC   GTMSGNO,ERQFULL     PRINT QUEUE FULL                             
         B     SOON10                                                           
         L     RE,8(R1)                                                         
         OC    0(7,RE),0(RE)                                                    
         BZ    *+14                                                             
         MVC   RPTNUM,6(RE)        JOB SENT OK - RETURN THE RPT NUMBER          
         B     SOONX                                                            
         MVC   GTMSGNO,ERNOJCL     JCL ERROR                                    
*                                                                               
SOON10   OI    ADERRS,ADERREQ      ERROR MESSAGE                                
         MVI   GTMSYS,X'FF'                                                     
         GOTO1 CGETTXT,GETTXTD                                                  
         ICM   RF,15,ADATWA                                                     
         BZ    SOONX                                                            
         OI    64+6(RF),X'80'      TRANSMIT HEADER MESSAGE                      
*                                                                               
SOONX    CLI   ADERRS,0                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 2                                                                
CHKSTART CLI   1(R2),C'0'                                                       
         BL    CHKSTNO                                                          
         CLI   2(R2),C'0'                                                       
         BL    CHKSTNO                                                          
         CLC   3(2,R2),=C'**'                                                   
         BE    CHKSTYES                                                         
         CLI   3(R2),C'0'                                                       
         BL    CHKSTNO                                                          
         CLI   4(R2),C'0'                                                       
         BNL   CHKSTYES                                                         
CHKSTNO  LTR   RE,RE                                                            
         BR    RE                                                               
CHKSTYES CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
DIR      DC    CL8'SPTDIR'                                                      
FIL      DC    CL8'SPTFILE'                                                     
CTFIL    DC    CL8'CTFILE'                                                      
RDHI     DC    CL8'DMRDHI'                                                      
RDSQ     DC    CL8'DMRSEQ'                                                      
DMRD     DC    CL8'DMREAD'                                                      
GETR     DC    CL8'GETREC'                                                      
PUTR     DC    CL8'PUTREC'                                                      
ADDR     DC    CL8'ADDREC'                                                      
*                                                                               
ERTFULL  DC    H'225'                                                           
ERQFULL  DC    H'226'                                                           
ERNOJCL  DC    H'227'                                                           
UPD      DC    X'80'                                                            
*                                                                               
BLANKS   DC    CL16' '                                                          
*                                                                               
RQHDR    DS    0CL12                                                            
         DC    CL2'DO'                                                          
         DC    CL2'  '                                                          
         DC    CL8' 000001 '                                                    
*                                                                               
RQAREA   DC    CL24'0202DO 0303REP 05**SOON,'                                   
         DC    CL3' '                                                           
         DC    CL5' 1201'                                                       
         DC    CL1' '                                                           
         DC    CL5' 13**'                                                       
         DC    CL3' '                                                           
         DC    CL5' 14**'                                                       
         DC    CL7' '                                                           
         DC    CL5' 1503'                                                       
         DC    CL3' '                                                           
         DC    CL5' 1604'                                                       
         DC    CL4' '                                                           
         DC    CL5' 17**'                                                       
         DC    CL6' '                                                           
         DC    CL5' 1817'                                                       
         DC    CL17' '                                                          
         DC    CL21' 2026BOX=N,LEFT,SOLID'                                      
         DC    CL10',PBSPLIT=N'                                                 
         DC    CL5' 22**'                                                       
         DC    CL3' '                                                           
         DC    CL5' 23**'                                                       
         DC    CL12' '                                                          
         DC    CL5' 2405'                                                       
         DC    CL5' '                                                           
         DC    CL5' 2503'                                                       
         DC    CL3' '                                                           
         DC    CL5' 2603'                                                       
         DC    CL3' '                                                           
         DC    CL5' 2708'                                                       
         DC    CL8' '                                                           
         DC    CL5' 2802'                                                       
         DC    CL2' '                                                           
         DC    CL5' 2901'                                                       
         DC    CL1'N'                                                           
         DC    CL17' 3011DRAFT ORDER*'                                          
RQAREAL  EQU   *-RQAREA                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER WORKING STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
DWORK    DS    12D                                                              
MYDUB    DS    D                                                                
RELO     DS    F                                                                
PARM     DS    6F                                                               
MYWORK   DS    CL16                                                             
*                                                                               
AASTELEM DS    A                                                                
AAMSELEM DS    A                                                                
*                                                                               
GETTXTWK DS    XL(L'GTBLOCK)                                                    
KY       DS    XL32                                                             
KYSAVE   DS    XL32                                                             
ELEM     DS    XL256                                                            
DESTID   DS    XL2                                                              
BPROD1   DS    XL1                                                              
BPROD2   DS    XL1                                                              
PROD1    DS    CL3                                                              
PROD2    DS    CL3                                                              
STASV    DS    CL5                                                              
MKTSV    DS    CL4                                                              
REPSV    DS    CL10                                                             
CITY     DS    CL2                                                              
RPTID    DS    CL3                                                              
RPTNUM   DS    XL2                                                              
TODAYSDT DS    XL3                                                              
QCLIENT  DS    CL3                                                              
QESTIM   DS    CL3                                                              
QCAMP    DS    CL5                                                              
RQSTART  DS    CL6                                                              
RQEND    DS    CL6                                                              
*                                                                               
RQHDR2   DS    0CL12                                                            
         DS    CL2                                                              
RQAGY    DS    CL2                                                              
         DS    CL8                                                              
*                                                                               
RQAREA2  DS    0CL(RQAREAL)                                                     
         DS    CL24                                                             
RQBYR    DS    CL3                                                              
         DS    CL5                                                              
RQMED    DS    CL1                                                              
         DS    CL5                                                              
RQCLT    DS    CL3                                                              
RQPRDHDR DS    CL5                                                              
RQPRD    DS    CL7                                                              
         DS    CL5                                                              
RQEST    DS    CL3                                                              
         DS    CL5                                                              
RQMKT    DS    CL4                                                              
RQSTAHDR DS    CL5                                                              
RQSTA    DS    CL6                                                              
RQPERHDR DS    CL5                                                              
RQPER    DS    CL17                                                             
RQOPTHDR DS    CL21                                                             
RQPIGOPT DS    CL10                                                             
RQBYRHDR DS    CL5                                                              
RQBYRCD  DS    CL3                                                              
RQBNMHDR DS    CL5                                                              
RQBYRNM  DS    CL12                                                             
RQCAMHDR DS    CL5                                                              
RQCAM    DS    CL5                                                              
         DS    CL5                                                              
RQCURVER DS    CL3                                                              
         DS    CL5                                                              
RQPREVER DS    CL3                                                              
RQPREHDR DS    CL5                                                              
RQPREDT  DS    CL8                                                              
RQLPPHDR DS    CL5                                                              
RQLPP    DS    CL2                                                              
         DS    CL5                                                              
RQDAILY  DS    CL1                                                              
         DS    CL17                                                             
*                                                                               
SPOOKWRK DS    XL(SPOOKL)                                                       
*                                                                               
REQHDR   DS    0F                                                               
       ++INCLUDE DMREQHDR                                                       
DESTCARD DS    CL80                DESTINATION CONTROL CARD FOR MASTER          
TRNCARD  DS    CL80                TRANSACTION REPORT CONTROL CARD              
HDRCARD  DS    CL80                HEADER CONTROL CARD                          
STACARD  DS    CL80                STATION CONTROL CARD                         
TLRCARD  DS    CL80                TRAILER CONTROL CARD                         
REQUEST  DS    CL80                REQUEST CARD                                 
REQUEST2 DS    CL80                2ND REQUEST CARD                             
REQUEST3 DS    CL80                3RD REQUEST CARD                             
*                                                                               
CTIO     DS    XL1000                                                           
*                                                                               
IOAREA   DS    XL2000                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPADINTD                                                       
         EJECT                                                                  
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
         EJECT                                                                  
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENADSTA                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENADSTA                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENADDIR                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENADDIR                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSPOOK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DDJOMU1   05/01/02'                                      
         END                                                                    
