*          DATA SET REREPP203  AT LEVEL 109 AS OF 02/13/96                      
*PHASE REP202A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
         TITLE 'REREPP102  (REP102A) --- PETRY CONVERSION'                      
*                                                                               
********************************************************************            
*                                                                  *            
*   GETJDSEL ROUTINE TESTER -                                      *            
*                                                                  *            
*                                                                  *            
*        REREPP102  -- PETRY CONVERSION: CONTRACT FILE CONVERSION. *            
*                      ACCEPT PETRY TAPE, CONVERT CONTRACT RECORDS *            
*                      TO CONTRACTS AND BUYLINES IN DDS FORMAT     *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* MAY24/95 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                  *            
*  TO DISPLAY A RANGE OF CONTRACTS BEING CONVERTED (SERIAL COUNT   *            
*     FROM X TO Y)                                                 *            
*                                                                  *            
*     QRECORD+20-25  =  SIX=DIGIT LOW-COUNTER VALUE FOR DISPLAYS   *            
*     QRECORD+26-31  =  SIX=DIGIT HI -COUNTER VALUE FOR DISPLAYS   *            
*                       BOTH VALUES MUST BE ENTERED FOR DISPLAYS   *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =                                                *            
*     QUESTOR+1   =                                                *            
*     QUESTOR+2   =                                                *            
*     QUESTOR+3   =                                                *            
*     QUESTOR+4   =                                                *            
*                 =                                                *            
*     QUESTOR+5   =                                                *            
*     QUESTOR+6   =                                                *            
*     QUESTOR+7   =                                                *            
*     QUESTOR+8   =                                                *            
*     QUESTOR+9   =                                                *            
*     QUESTOR+10  =                                                *            
*     QUESTOR+11  =                                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REP102   CSECT                                                                  
         NMOD1 0,**REP1**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    MAIN0040                                                         
*                                                                               
GETT0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
MAIN0040 DS    0H                                                               
         MVC   RECORD2(LTESTREC),TESTRECD                                       
*                                  LOAD TEST RECORD TO WORK FILE AREA           
MAIN0080 EQU   *                                                                
         MVI   JDSELT,X'40'        GET DESCRIPTOR ELEMENT                       
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     MAIN0180                                                         
MAIN0160 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
MAIN0180 EQU   *                                                                
         BNZ   MAIN0360            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         TM    JDSELT,X'F0'        COMPRESSED ELT?                              
         BNZ   MAIN0240            YES                                          
*                                  MOVE FROM UNCOMPRESSED ELEMENT               
         EX    RE,MAIN0200         MOVE ELEMENT DATA BY LENGTH                  
         B     MAIN0320                                                         
MAIN0200 EQU   *                                                                
         MVC   P+1(0),2(RF)        MOVE ELEMENT BY LENGTH                       
MAIN0240 EQU   *                   MOVE FROM COMPRESSED ELEMENT                 
         EX    RE,MAIN0280         MOVE ELEMENT DATA BY LENGTH                  
         B     MAIN0320                                                         
MAIN0280 EQU   *                                                                
         MVC   P+1(0),1(RF)        MOVE ELEMENT BY LENGTH                       
MAIN0320 EQU   *                                                                
         LA    RE,1(RE)            INCREASE LENGTH FOR EX                       
         MVC   P+20(18),=C'CHARACTER LENGTH ='                                  
         EDIT  (RE),(3,P+40),FILL=0                                             
         GOTO1 REPORT                                                           
         B     MAIN0160                                                         
MAIN0360 EQU   *                                                                
         MVC   P+1(17),=C'ELEMENT NOT FOUND'                                    
         GOTO1 REPORT                                                           
MAIN0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GETJDSEL:  RETRIEVE A(ELEMENT SOUGHT) BASED ON ELEMENT TYPE                 
*   GETJDSNX:  RETRIEVE NEXT ELEMENT OF TYPE.  JDSELT MUST BE SET,              
*        AND JDSADDR MUST CONTAIN ADDRESS OF LAST ELEMENT FOUND.                
*                                                                               
GETJDSNX NTR1                                                                   
         LA    R1,RECORD2+24       SET A(RECORD IN PROCESS: LENGTH)             
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         L     R1,JDSADDR          SET A(LAST ELEMENT)                          
         TM    0(R1),X'F0'         DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0400            COMPRESSED: SKIP THIS ELEMENT                
         B     GJDS0100            UNCOMPRESSED: SKIP THIS ELEMENT              
GETJDSEL NTR1                                                                   
         LA    R1,RECORD2+24       SET A(RECORD LENGTH)                         
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         LA    R1,RECORD2+66       SET A(DESCRIPTOR ELEMENT)                    
         TM    0(R1),X'F0'         DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0200            COMPRESSED:                                  
GJDS0020 EQU   *                                                                
         CR    R1,R2               END OF RECORD REACHED?                       
         BNL   GJDS0800            YES - NO ELT: RETURN CC NOT ZERO             
*                                                                               
*   TEST                                                                        
         MVC   P+1(04),=C'ELT='                                                 
         MVC   P+5(20),0(R1)                                                    
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         TM    0(R1),X'F0'         ELT COMPRESSED/UNCOMPRESSED?                 
         BNZ   GJDS0200            COMPRESSED                                   
*                                  UNCOMPRESSED                                 
         CLC   JDSELT,0(R1)        COMPARE ELEMENT TYPE                         
         BNE   GJDS0100            NOT EQUAL - GO TO NEXT                       
         ST    R1,JDSADDR          SAVE A(ELEMENT FOUND)                        
         ZIC   RF,1(R1)            GET ELEMENT LENGTH                           
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         SH    RF,=H'2'            SUBTRACT TWO BYTES FOR CONTROL               
         ST    RF,JDSLEN           SAVE ELEMENT LENGTH                          
         B     GJDS1000            EXIT: RETURN CC = ZERO                       
GJDS0100 EQU   *                   BUMP PAST UNCOMPRESSED ELT                   
         ZIC   RF,1(R1)            RETRIEVE ELEMENT LENGTH                      
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         AR    R1,RF               ADD TO ELEMENT ADDR                          
         B     GJDS0020            GO BACK FOR NEXT ELEMENT                     
GJDS0200 EQU   *                                                                
         ZIC   RE,0(R1)            UNLOAD ELT/LENGTH                            
         SRL   RE,4                DROP LENGTH NYBBLE                           
         SLL   RE,4                RESTORE TO ORIG PLACE                        
         STC   RE,JDSELT2                                                       
         CLC   JDSELT,JDSELT2      COMPARE ELT SOUGHT VS THIS ELT               
         BNE   GJDS0400            NOT FOUND -                                  
         ST    R1,JDSADDR          SAVE A(ELEMENT FOUND)                        
         MVC   JDSELT2,0(R1)       RETRIEVE ELT CODE/LNGTH AGAIN                
         NI    JDSELT2,X'FF'-X'F0' TURN OFF ELT CODE, LEAVING LENGTH            
         ZIC   RF,JDSELT2          PROCESS LENGTH                               
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         BCTR  RF,0                SUBTRACT 1 BYTE FOR CONTROL                  
         ST    RF,JDSLEN           SAVE ELEMENT LENGTH                          
         B     GJDS1000            EXIT: RETURN CC = ZERO                       
GJDS0400 EQU   *                                                                
         MVC   JDSELT2,0(R1)       RETRIEVE ELT CODE/LNGTH AGAIN                
         NI    JDSELT2,X'FF'-X'F0' TURN OFF ELT CODE, LEAVING LENGTH            
         ZIC   RF,JDSELT2          PROCESS LENGTH                               
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     GJDS0020            GO BACK FOR NEXT ELEMENT                     
GJDS0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     GJDS1200                                                         
GJDS1000 EQU   *                                                                
         SR    R0,R0                                                            
GJDS1200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         DS    2400C               DUMMY FOR ADDRESSABILITY                     
*        REMOVE WHEN PROGRAM CODE PASSES FIRST BASE REGISTER                    
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
ASTAAREA DS    A                   STATION/GROUP-SUBGROUP AREA                  
ASTAEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ASALAREA DS    A                   SALESPERSON CONVERSION AREA                  
ASALEND  DS    A                   A(LAST ENTRY IN TABLE)                       
AMISAREA DS    A                   A(STATION MISSING AREA)                      
ANEXTMIS DS    A                                                                
AAGYAREA DS    A                   AGENCY CONVERSION AREA                       
AAGYEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ANEWSALS DS    A                                                                
ANXTSAL  DS    A                                                                
ANEWPPRS DS    A                                                                
ANXTPPR  DS    A                                                                
ARECORD4 DS    A                                                                
NEWPPRCT DS    F                                                                
LBLDAREA DS    F                                                                
TOTCTR   DS    F                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
ADVCTR   DS    F                                                                
AGYCTR   DS    F                                                                
AGYCTR2  DS    F                                                                
TSPTCTR  DS    F                                                                
TCOSCTR  DS    F                                                                
TWEEKCTR DS    F                                                                
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DS    F                                                                
STACTR   DS    F                                                                
SALCTR   DS    F                                                                
SALMISS  DS    F                                                                
AIOAREA  DS    F                                                                
BUCKWORK DS    4F                  BUCKET UPDATE ARGUMENTS                      
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
ELTBILD1 DS    CL128                                                            
ELTBILD2 DS    CL128                                                            
ELTBILD3 DS    CL128                                                            
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
WORK2    DS    CL256                                                            
*                                                                               
DATEFLAG DS    CL1                                                              
ACEGRAPH DS    XL1                 ACE/GRAPHNET INDICATOR                       
CONVERT# DS    CL1                                                              
JDSELT   DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
JDSELT2  DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
BLKFLAG  DS    CL1                 BLOCK EMPTY FLAG                             
JDSADDR  DS    F                                                                
JDSLEN   DS    F                                                                
BLKADDR  DS    F                   ADDRESS OF CURRENT RECORD IN BLOCK           
BLKEND   DS    F                   ADDRESS OF CURRENT RECORD EOR                
BASEYEAR DC    F'92'               BASE YEAR IS UNDETERMINED!!                  
BUYREC4  DS    CL1                 RECORD IN BUYREC4 AREA FLAG                  
*                                                                               
TESTRECD DS    CL24                OFFSET                                       
         DC    X'0080'             LENGTH                                       
         DC    X'0000'             CONTROL BYTE                                 
         DC    X'0000'             COMPRESSION                                  
         DC    X'0020'             KEY TYPE                                     
         DC    26X'00'             FILLER                                       
         DC    C'V1'               REP CODE                                     
         DC    X'12345678'         CONTRACT NUMBER                              
         DC    X'0101'             LINE NUMBERS                                 
         DC    X'18'               DESCRIPTOR ELEMENT                           
         DC    C'ABCDEFGHIJKLMNO'                                               
         DC    X'24'               D/T ELEMENT # 1                              
         DC    C'1234567'                                                       
         DC    X'24'               D/T ELEMENT # 2                              
         DC    C'8901234'                                                       
         DC    X'24'               D/T ELEMENT # 3                              
         DC    C'5678901'                                                       
         DC    X'32'               EFF DATE ELT # 1                             
         DC    C'A12'                                                           
         DC    X'32'               EFF DATE ELT # 2                             
         DC    C'B12'                                                           
         DC    X'0105'             COMMENT ELT # 1                              
         DC    C'COMMENT1'                                                      
         DC    X'0105'             COMMENT ELT # 2                              
         DC    C'COMMENT2'                                                      
         DC    X'0209'             FILLER PADDING                               
         DC    16X'00'                                                          
LTESTREC EQU   *-TESTRECD                                                       
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=16300,            X        
               BLKSIZE=32600,MACRF=GM,EODAD=GETT0100                            
*                                                                               
         SPACE 3                                                                
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL1024                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERT      RECORD                           
         EJECT                                                                  
         ORG                                                                    
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCLS          CLASS RECORD                                 
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCTG          CATEGORY RECORD                              
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSADV          ADVERTISER RECORD                            
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSAGY          AGENCY RECORD                                
         EJECT                                                                  
*  OTHER JDS RECORD DSECTS GET ORG'D HERE                                       
         EJECT                                                                  
         ORG                                                                    
RECORD3  DS    CL1024                                                           
         ORG                                                                    
RECORD4  DS    CL1024                                                           
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         EJECT                                                                  
*********************************************************************           
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109REREPP203 02/13/96'                                      
         END                                                                    
