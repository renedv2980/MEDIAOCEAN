*          DATA SET SPGETTBA   AT LEVEL 001 AS OF 04/24/95                      
*PHASE SPGETCTA                                                                 
SPGETCTA TITLE 'CALCULATE DOLLARS FOR SPOT CONTRACT ANALYSIS'                   
SPGETCTA CSECT                                                                  
         NMOD1 WORKX-WORKD,SPGETCTA,CLEAR=YES                                   
         USING WORKD,RC                                                         
         LR    RA,R1                                                            
         USING CIBBLKD,RA                                                       
         MVI   CIBERR,0                                                         
         MVC   CIBBLKID,=C'*CIBBLK*'  SET EYECATCHERS                           
         MVC   BUCKETID,=C'*BUCKETS'                                            
         MVC   KEYID,=C'*KEY'                                                   
         MVC   KEYSVID,=C'KYSV'                                                 
*                                                                               
         L     R8,CIBABUY          GET BUY RECORD ADDRESS                       
         USING BUYRECD,R8                                                       
*                                                                               
         BAS   RE,BUYVALS          EXTRACT BUY VALUES INTO BUCKETS              
         CLI   CIBERR,0                                                         
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,MAKELS           TRANSFORM BUCKETS TO ELEMENTS                
         CLI   CIBERR,0                                                         
         BNE   EXIT                                                             
*                                                                               
         TM    CIBFLAGS,CIBNUPDQ   TEST 'DO NOT UPDATE'                         
         BO    EXIT                                                             
         CLI   CIBACT,CIBCPYQ      TEST PROCESS COPY                            
         BE    EXIT                YES - DEFER UPDATE TILL CHANGE               
*                                                                               
         BAS   RE,UPDATE           UPDATE CTA RECORD                            
         CLI   CIBERR,0                                                         
         BNE   EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===========================================================*                   
* BUILD BROADCAST MONTH DATES FROM ESTART/EEND              *                   
* IF A(BRDWK) HAS X'80' ON, THIS IS OFFLINE CALL AND        *                   
*  ADDRESS POINTS TO BUCKET AREA IN FOLLOWING FORMAT        *                   
*                                                           *                   
*    Y/M  MNST  MNEND  GORD  GPAID                          *                   
*    +0   +2    +4     +6    +10                            *                   
*===========================================================*                   
         SPACE                                                                  
BUYVALS  NTR1                                                                   
         USING BUYRECD,R8                                                       
         USING BUCKETSD,R7                                                      
*                                                                               
         TM    BUYREC+15,X'80'     TEST BUYREC DELETED                          
         BO    EXIT                YES - NO NEW VALUES                          
*                                                                               
         L     R7,CIBABUCK                                                      
         TM    CIBESTDT,CIBBUCKQ   TEST A(BUCKETS PASSED)                       
         BO    BV10                                                             
*                                                                               
         GOTO1 CIBABRWK,DMCB,(X'80',CIBESTDT),(X'02',WORK)                      
*                                                                               
         LA    R4,WORK                                                          
         LA    R7,BUCKETS                                                       
*                                                                               
BV4      MVC   BUCKSTDT(4),0(R4)   MOVE MONTH START/END (PACKED)                
*                                                                               
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDATCON                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB,(2,(R4)),(3,DUB)                                       
*                                                                               
         MVC   BUCKYM,DUB          MOVE YEAR/MONTH (BINARY)                     
         LA    R7,L'BUCKETS(R7)    NEXT ELEMENT                                 
         LA    R4,4(R4)            NEXT MONTH                                   
*                                                                               
         CLI   0(R4),0             TEST FOR EOL                                 
         BNE   BV4                                                              
*                                                                               
* NOW PROCESS BUY RECORD                                                        
*                                                                               
BV10     LA    R6,BDELEM                                                        
*                                                                               
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BE    BV12                                                             
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
*                                                                               
BV12     DS    0H                  SET UP FOR MULTIPLE GETRATE CALLS            
         ICM   RF,15,CIBAGTRT      GET A(GETRATE)                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DMCB                                                          
         LA    RE,SPOTS                                                         
         ST    RE,0(R1)                                                         
         MVC   0(1,R1),BUYKPRD                                                  
         ST    R8,4(R1)            SET A(BUYREC)                                
*                                                                               
* POINT TO FIRST BUCKET NOW BECAUSE REGELS ARE IN DATE ORDER                    
*                                                                               
         L     R7,CIBABUCK                                                      
         TM    CIBESTDT,CIBBUCKQ   TEST A(BUCKETS PASSED)                       
         BO    *+8                                                              
         LA    R7,BUCKETS          SEARCH BUCKETS TO POST REGEL                 
*                                                                               
BV40     BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         USING REGEL,R6                                                         
*                                                                               
         CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BNE   *+12                                                             
         CLI   1(R6),10            TEST ALLOCATED                               
         BNH   BV40                                                             
* CALL GETRATE -- NOTE RF, R1 ASSUMED TO BE INTACT                              
         GOTO1 (RF),(R1),,,(R6)                                                 
*                                                                               
BV42     CLC   RDATE,BUCKSTDT      REGEL DATE TO BUCKET START                   
         BL    BV44                                                             
         CLC   RDATE,BUCKNDDT      REGEL DATE TO BUCKET END                     
         BNH   BV46                                                             
*                                                                               
BV44     LA    R7,L'BUCKETS(R7)                                                 
         CLI   0(R7),0                                                          
         BNE   BV42                                                             
         DC    H'0'                                                             
*                                                                               
BV46     L     RE,GROSS                                                         
         CLI   CIBACT,CIBCPYQ      COPY BUCKETS ARE NEGATIVE !                  
         BNE   *+6                                                              
         LCR   RE,RE                                                            
*                                                                               
         ICM   R0,15,BUCKGORD                                                   
         AR    R0,RE                                                            
         STCM  R0,15,BUCKGORD                                                   
*                                                                               
         OC    RPAY,RPAY           TEST REGEL PAID                              
         BZ    BV40                NO                                           
*                                                                               
         ICM   R0,15,BUCKGPD                                                    
         AR    R0,RE                                                            
         STCM  R0,15,BUCKGPD                                                    
         B     BV40                                                             
         DROP  R6,R7,R8                                                         
         EJECT                                                                  
*==========================================================*                    
* TRANSFORM NON-ZERO BUCKET DATA INTO ELEMENTS             *                    
* SAVE ELEMENTS IN CIB BUFFER (CIBABUFF)                   *                    
*==========================================================*                    
         SPACE                                                                  
MAKELS   NTR1                                                                   
         USING BUYRECD,R8                                                       
*                                                                               
         L     RF,CIBABUFF         GET BUFFER ADDRESS                           
         CLI   CIBACT,CIBCHGQ      TEST PROCESSING CHANGE                       
         BE    *+10                YES - DON'T CLEAR COPY DATA                  
         XC    0(256,RF),0(RF)     ELSE CLEAR BUFFER NOW                        
*                                                                               
         L     R7,CIBABUCK                                                      
         TM    CIBESTDT,CIBBUCKQ   TEST A(BUCKETS PASSED)                       
         BO    *+8                                                              
         LA    R7,BUCKETS                                                       
         USING BUCKETSD,R7                                                      
*                                                                               
         LA    R6,ELEM                                                          
         USING CTAUSELD,R6                                                      
*                                                                               
         OC    BUCKGORD(8),BUCKGORD   TEST FOR DOLLARS                          
         BZ    MAKEL20                                                          
*                                                                               
MAKEL2   XC    ELEM,ELEM                                                        
         MVI   CTAUSEL,CTAUSELQ                                                 
         MVI   CTAUSELN,CTAUSLNQ                                                
         MVC   CTAUSCLT,BUYKCLT                                                 
         MVC   CTAUSPRD,BUYKPRD                                                 
         MVC   CTAUSEST,BUYKEST                                                 
         MVC   CTAUSYM,BUCKYM                                                   
         MVC   CTAUSOGR,BUCKGORD                                                
         MVC   CTAUSPGR,BUCKGPD                                                 
*                                                                               
         L     RF,CIBABUFF         SEARCH BUFFER FOR ELEMENT                    
*                                                                               
MAKEL10  CLI   0(RF),0                                                          
         BE    MAKEL12                                                          
         CLC   CTAUSEL(8),0(RF)    EL/LEN/CLT/PRD/EST/YM                        
         BE    MAKEL15                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(RF)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,R0                                                            
         B     MAKEL10                                                          
*                                                                               
MAKEL12  LA    RE,CTAUSLNQ-1       MOVE NEW ELEMENT TO BUFFER                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R6)  *EXECUTED*                                        
         B     MAKEL20                                                          
*                                                                               
MAKEL15  ICM   R0,15,CTAUSOGR              ORD FROM NEW ELEMENT                 
         ICM   R1,15,CTAUSOGR-CTAUSEL(RF)  ORD FROM OLD ELEMENT                 
         AR    R1,R0                                                            
         STCM  R1,15,CTAUSOGR-CTAUSEL(RF)  SAVE                                 
*                                                                               
         ICM   R0,15,CTAUSPGR              PAID FROM NEW ELEMENT                
         ICM   R1,15,CTAUSPGR-CTAUSEL(RF)  PAID FROM OLD ELEMENT                
         AR    R1,R0                                                            
         STCM  R1,15,CTAUSPGR-CTAUSEL(RF)  SAVE                                 
*                                                                               
MAKEL20  LA    R7,L'BUCKETS(R7)                                                 
         CLI   0(R7),0             TEST END                                     
         BNE   MAKEL2              NO - CONTINUE                                
         B     EXIT                                                             
         DROP  R6,R7,R8                                                         
         EJECT                                                                  
*==========================================================*                    
* MOVE BUCKET DATA TO CTA RECORD                           *                    
*==========================================================*                    
         SPACE                                                                  
UPDATE   NTR1                                                                   
         USING BUYRECD,R8                                                       
*                                                                               
         MVI   ELCDLO,X'70'        FIND CONTRACT ID ELEMENT                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R8                                                               
* NEED TO MAKE SURE CONTRACT IS NUMERIC                                         
         LA    R1,3(R6)                                                         
         LA    R0,5                <==== IS THIS RIGHT LENGTH ???               
*                                                                               
UPD2     CLI   0(R1),C'0'                                                       
         BL    BADCON                                                           
         CLI   0(R1),C'9'                                                       
         BH    BADCON                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,UPD2                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CTAKEY,R1                                                        
         XC    KEY,KEY                                                          
         MVI   CTAKTYP,CTAKTYPQ                                                 
         MVI   CTAKSUB,CTAKSUBQ                                                 
         MVC   CTAKAGMD,BUYKAM                                                  
*                                                                               
         PACK  DUB,3(2,R6)         PACK YEAR DIGIT + 1 EXTRA BYTE               
         MVC   CTAKYR,DUB+6        MOVE YEAR DIGIT                              
*                                                                               
         ZAP   DUB(4),=P'9999'                                                  
         PACK  DUB+4(4),4(4,R6)    PACK LAST 4 DIGITS                           
         SP    DUB(4),DUB+4(4)     GIVES 9'S COMPLEMENT                         
         L     R0,DUB                                                           
         SRL   R0,4                                                             
         STM   R0,3,CTAKNUM        STORE PWOS CONTRACT NUM                      
         DROP  R1                                                               
         EJECT                                                                  
         MVC   KEYSAVE,KEY                                                      
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDATAMGR                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                <=== TEST ONLY                               
*                                                                               
         GOTO1 (RF),DMCB,=C'GETREC',=C'SPTFILE',KEY+14,CIBAIO,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R8,CIBAIO                                                        
         USING CTARECD,R8                                                       
*                                                                               
         L     R7,CIBABUFF         POINT TO ELEMENT BUFFER                      
         B     UPD22                                                            
*                                                                               
* SEARCH RECORD FOR MATCHING ELEMENT                                            
*                                                                               
UPD10    LA    R6,CTAEL                                                         
         USING CTAUSELD,R6                                                      
         SR    R0,R0                                                            
*                                                                               
UPD12    ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(8,R7),0(R6)       NEW ELEMENT TO RECORD ELEM                   
         BL    UPD12               IF LOW, KEEP GOING                           
         BH    UPD15               HIGH - INSERT ELEM BEFORE                    
*                                                                               
* ADD VALUES IN CURRENT ELEMENT TO EXISTING ELEMENT                             
*                                                                               
         ICM   R0,15,CTAUSOGR              ORD FROM NEW ELEMENT                 
         ICM   R1,15,CTAUSOGR-CTAUSEL(R7)  ORD FROM OLD ELEMENT                 
         AR    R1,R0                                                            
         STCM  R1,15,CTAUSOGR-CTAUSEL(R7)  SAVE                                 
*                                                                               
         ICM   R0,15,CTAUSPGR              PAID FROM NEW ELEMENT                
         ICM   R1,15,CTAUSPGR-CTAUSEL(R7)  PAID FROM OLD ELEMENT                
         AR    R1,R0                                                            
         STCM  R1,15,CTAUSPGR-CTAUSEL(R7)  SAVE                                 
*                                                                               
         OC    CTAUSOGR(8),CTAUSOGR        TEST ANY DOLLARS LEFT                
         BNZ   UPD20                                                            
<<<<< REMOVE ELEMENT FROM RECORD                                                
         B     UPD20                                                            
*                                                                               
UPD15    DS    0H                  ADD ELEMENT TO RECORD                        
<<<<< USE RECUP ?? HELLO ??                                                     
         EJECT                                                                  
UPD20    DS    0H                  FIND NEXT ELEMENT IN BUFFER                  
         SR    R0,R0                                                            
         ICM   R0,1,1(R7)                                                       
         AR    R7,R0                                                            
*                                                                               
UPD22    CLI   0(R7),0             TEST ANY MORE ELEMENTS IN BUFFER             
         BNE   UPD10               YES - CONTINUE                               
*                                                                               
* ALL ELEMENTS PROCESSED -- WRITE RECORD                                        
*                                                                               
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDATAMGR                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,CIBAIO,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R6,R7,R8                                                         
         EJECT                                                                  
BADCON   MVI   CIBERR,CIBCONTQ     SET INVALID CONTRACT                         
         B     EXIT                                                             
         SPACE                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLC   0(1,R6),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6A                                                               
DMWORK   DS    16F                                                              
*                                                                               
KEYID    DS    CL4                                                              
KEY      DS    XL20                                                             
KEYSVID  DS    CL4                                                              
KEYSAVE  DS    XL20                                                             
*                                                                               
SPOTS    DS    F                                                                
GROSS    DS    F                                                                
NET      DS    F                                                                
ADJ      DS    F                                                                
*                                                                               
ELEM     DS    XL64                                                             
WORK     DS    XL64                                                             
ELCDLO   DS    C                                                                
ELCDHI   DS    C                                                                
*                                                                               
         DS    0D                                                               
BUCKETID DS    D                                                                
BUCKETS  DS    0XL16                                                            
         DS    XL256                                                            
WORKX    EQU   *                                                                
*                                                                               
*  NOTE - FOLLOWING DSECT IS ALSO INCLUDED IN SPREPTB02 !!!                     
*                                                                               
BUCKETS  DSECT                                                                  
BUCKYM   DS    XL2                 Y/M (BINARY)                                 
         DS    XL2                 SPARE                                        
BUCKSTDT DS    XL2                 BUCKET START DATE (PACKED)                   
BUCKNDDT DS    XL2                 BUCKET END DATE (PACKED                      
BUCKGORD DS    XL4                 GROSS ORDERED                                
BUCKGPD  DS    XL4                 GROSS PAID                                   
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE DDCOMFACS                                                      
****************************                                                    
<<< THIS GOES IN SPGENCTA !                                                     
****************************                                                    
CTAUSELD DSECT      *********      CTA USAGE ELEMENT                            
CTAUSEL  DS    AL1(06)             ELEMENT CODE                                 
CTAUSELQ EQU   6                                                                
CTAUSELN DS    AL1(16)             ELEMENT LENGTH                               
CTAUSCLT DS    XL2                 CLIENT CODE                                  
CTAUSPRD DS    XL1                 PRODUCT CODE                                 
CTAUSEST DS    XL1                 ESTIMATE                                     
CTAUSYM  DS    XL2                 BROADCAST YEAR/MONTH                         
CTAUSOGR DS    XL4                 GROSS ORDERED DOLLARS                        
CTAUSPGR DS    XL4                 GROSS PAID DOLLARS                           
CTAUSLNQ EQU   *-CTAUSEL                                                        
                                                                                
***********************************                                             
                                                                                
CIBBLKD  DSECT    ******           CTA INTERFACE BLOCK                          
CIBBLKID DS    CL8'*CIBBLK*'       INITIALIZED BY SPGETCTA                      
*                                                                               
CIBACT   DS    XL1                 ACTION CODE                                  
CIBCPYQ  EQU   X'01'               PROCESS COPY                                 
CIBCHGQ  EQU   X'02'               PROCESS CHANGE                               
CIBADDQ  EQU   X'03'               PROCESS ADD                                  
*                                                                               
CIBERR   DS    XL1                 ERROR RETURN CODE                            
CIBCONTQ EQU   X'01'               INVALID CONTRACT IN BUYREC                   
*                                                                               
CIBFLAGS DS    XL1                 FLAGS                                        
CIBNUPDQ EQU   X'80'               'DO NOT UPDATE' FLAG                         
         DS    XL1                 SPARE                                        
*                                                                               
CIBAGYA  DS    CL2                 AGENCY ALPHA                                 
         DS    XL2                 SPARE                                        
*                                                                               
CIBACOMF DS    A                   A(COMFACS)                                   
CIBAGTRT DS    A                   A(GETRATE)                                   
*                                                                               
CIBABRWK DS    A                   A(BRDWK OR 0 FOR OFFLINE CALL)               
*                                                                               
CIBABUY  DS    A                   A(BUYREC)                                    
CIBABUFF DS    A                   A(256 BYTE BUFFER)                           
CIBAIO   DS    A                   A(I/O AREA FOR CTA UPDATE)                   
CIBESTDT DS    A                   A(ESTIMATE START/END DATES)                  
         ORG   CIBESTDT                                                         
CIBBUCKQ EQU   X'80'               THIS IS A(BUCKET AREA)                       
CIBABUCK DS    A                                                                
