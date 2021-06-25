*          DATA SET ACNV02     AT LEVEL 012 AS OF 08/16/00                      
*PHASE ACNV02A                                                                  
         TITLE 'ACCPAK CONVERSION - KEY SORT'                                   
         PRINT NOGEN                                                            
ACNVKS   CSECT                                                                  
         NMOD1 0,*KSORT,RA                                                      
         USING ACNVD,R9                                                         
         BAS   RE,SINF             SORT THE INPUT FILE                          
         BAS   RE,ATSR             ASSIGN TRANSACTION SUB-REF                   
         BAS   RE,SOUT             SORT THE OUTPUT FILE                         
         BAS   RE,FBEL             FIX BATCH ELEMENTS                           
         BAS   RE,SOLK             SORT OLD KEYS                                
         BAS   RE,UBTR             UPDATE BATCH AND TRANSACTION RECORDS         
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SORT KEYS BY TYPE/NEW/OLD                                           *         
***********************************************************************         
                                                                                
SINF     NTR1  ,                                                                
         GOTO1 SORTER,DMCB,SORTCRD1,RECCRD1,0                                   
         OPEN  (TKEY,(INPUT))                                                   
         L     R4,AINP                                                          
*                                                                               
SINF3    GET   TKEY,(R4)                                                        
         GOTO1 SORTER,DMCB,=C'PUT',(R4)                                         
         B     SINF3                                                            
*                                                                               
SINFX    CLOSE (TKEY)                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET KEYS IN NEW TRANSACTION SEQUENCE NUMBER - ASSIGN NEW SUB-REF    *         
***********************************************************************         
                                                                                
ATSR     NTR1  ,                                                                
         OPEN  (WKEY,(OUTPUT))                                                  
         XC    NEWKY,NEWKY                                                      
         SR    R7,R7                                                            
         LA    R4,KEYWRK                                                        
         USING KEYCD,R4                                                         
*                                                                               
ATSR3    GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RE,15,4(R1)         TEST EOF                                     
         BZ    ATSR15                                                           
         MVC   KEYWRK,0(RE)                                                     
         CLI   KEYTYP,KEYTTRN      TEST TRANSACTION                             
         BNE   ATSR9                                                            
         LA    R2,KEYNEW                                                        
         USING TRNRECD,R2                                                       
         OC    NEWKY,NEWKY        FIRST TIME                                    
         BZ    ATSR5                                                            
         CLC   TRNKEY(41),NEWKY   TEST SAME KEY                                 
         BE    ATSR5                                                            
         SR    R7,R7                                                            
         MVI   NXTREF,0                                                         
*                                                                               
ATSR5    STC   R7,TRNKSBR                                                       
         CH    R7,=H'255'                                                       
         BNH   ATSR7                                                            
         SR    R1,R1                                                            
         IC    R1,NXTREF                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TRNKREF+4(2),DUB                                                 
         MVI   TRNKREF+3,C'*'                                                   
         AH    R1,=H'1'                                                         
         STC   R1,NXTREF                                                        
ATSR7    MVC   NEWKY,TRNKEY                                                     
         AH    R7,=H'1'                                                         
ATSR9    PUT   WKEY,(R4)           PUT LAST TRANSACTION                         
         B     ATSR3                                                            
*                                                                               
ATSR15   CLOSE (WKEY)                                                           
         B     XIT                                                              
NXTREF   DC     X'0'                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SORT OUTPUT KEY FILE BY OLD/TYPE/NEW                                *         
***********************************************************************         
                                                                                
SOUT     NTR1  ,                                                                
         GOTO1 SORTER,DMCB,SORTCRD2,RECCRD2,0                                   
         OPEN  (WKEY,(INPUT))                                                   
         L     R4,AINP                                                          
*                                                                               
SOUT3    GET   WKEY,(R4)                                                        
         GOTO1 SORTER,DMCB,=C'PUT',(R4)                                         
         B     SOUT3                                                            
*                                                                               
SOUTX    CLOSE (WKEY)                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD NEW TRANSACACTION KEYS TO BATCH ITEMS                           *         
***********************************************************************         
                                                                                
FBEL     NTR1  ,                                                                
         OPEN  (WKEY,(OUTPUT))                                                  
         LA    R4,KEYWRK                                                        
         USING KEYCD,R4                                                         
*                                                                               
FBEL3    GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RE,15,4(R1)         TEST EOF                                     
         BZ    FBEL15                                                           
         MVC   KEYWRK,0(RE)                                                     
         CLI   KEYTYP,KEYTTRN      TEST TRANSACTION                             
         BNE   FBEL5                                                            
         MVC   OLDKY,KEYOLD                                                     
         MVC   NEWKY,KEYNEW                                                     
         B     FBEL7                                                            
*                                                                               
FBEL5    CLI   KEYTYP,KEYTBAT      TEST BATCH                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEYTBN,KEYTBO       MOVE OLD TO NEW                              
         CLC   KEYTBO,OLDKY        TEST BATCH ELEMENT VS. OLD KEY               
         BNE   *+10                                                             
         MVC   KEYTBN,NEWKY                                                     
*                                                                               
FBEL7    PUT   WKEY,(R4)                                                        
         B     FBEL3                                                            
*                                                                               
FBEL15   CLOSE (WKEY)                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SORT BY BATCH KEY OR OLD ACCOUNT CODE                               *         
***********************************************************************         
                                                                                
SOLK     NTR1  ,                                                                
         GOTO1 SORTER,DMCB,SORTCRD3,RECCRD3,0                                   
         OPEN  (WKEY,(INPUT))                                                   
         L     R4,AINP                                                          
*                                                                               
SOLK3    GET   WKEY,(R4)                                                        
         GOTO1 SORTER,DMCB,=C'PUT',(R4)                                         
         B     SOLK3                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE BATCH ITEMS AND TRANSACTTIONS                                *         
***********************************************************************         
                                                                                
UBTR     NTR1  ,                                                                
         OPEN  (TTRN,(INPUT))                                                   
         OPEN  (TOUT,(OUTPUT))                                                  
         BAS   RE,BLDT             BUILD INITIAL KEY CHANGE TABLE               
         L     R2,AINP                                                          
         USING ACCRECD,R2                                                       
*                                                                               
UBTR3    L     R4,AINPL            SORT INPUT TAPE                              
         GET   TTRN,(R4)                                                        
UBTR5    ICM   RF,15,NTAB          TEST NUMBER IN TABLE                         
         BZ    UBTR15                                                           
         L     R4,AKEYT                                                         
         USING KEYCD,R4                                                         
         LA    R1,L'ACCKEY         SET LENGTH FOR COMPARE                       
         BCTR  R1,0                                                             
         CLI   ACCKEY,C' '         TEST TRANSACTION RECORD                      
         BNH   *+6                                                              
         BCTR  R1,0                DON'T COMPARE ON SUBREF                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACCKEY(0),KEYREC    TEST UP TO CHANGE FILE                       
         BL    UBTR15                                                           
         BNH   UBTR9                                                            
         BAS   RE,BLDT             BUILD NEW KEY CHANGE TABLE                   
         B     UBTR5                                                            
*                                                                               
UBTR9    CLI   ACCKEY,TBAKTYPQ     TEST TRANSACTION BATCH RECORD                
         BNE   UBTR11                                                           
         BAS   RE,TBAR             BATCH RECORD FIX                             
         B     UBTR15                                                           
*                                                                               
         USING TRNRECD,R2                                                       
UBTR11   CLC   TRNKREF,=C'*TIME*'  TEST TIME RECORD                             
         BE    UBTR12                                                           
         CLI   TRNRFST,TRNELQ      TEST TRANSACTION RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
UBTR12   BAS   RE,TRNR             TRANSACTION RECORD                           
*                                                                               
UBTR15   L     R4,AINPL            PUT OUTPUT RECORD                            
         PUT   TOUT,(R4)                                                        
         B     UBTR3                                                            
*                                                                               
UBTRX    CLOSE (TTRN)                                                           
         CLOSE (TOUT)                                                           
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CHANGE TRANSACTION BATCH RECORDS                                    *         
***********************************************************************         
                                                                                
         USING TBARECD,R2                                                       
TBAR     NTR1  ,                                                                
         LA    R3,TBARFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING ASKELD,R3                                                        
TBAR3    CLI   ASKEL,0             FIND KEY ELEMENT                             
         BE    XIT                                                              
         CLI   ASKEL,ASKELQ                                                     
         BE    TBAR7                                                            
TBAR5    IC    R0,ASKLN                                                         
         AR    R3,R0                                                            
         B     TBAR3                                                            
*                                                                               
TBAR7    ICM   RF,15,NTAB          TEST NUMBER IN TABLE                         
         L     R4,AKEYT                                                         
         USING KEYCD,R4                                                         
TBAR9    CLC   KEYOLD,ASKKEY       FIND MATCH IN TABLE                          
         BE    TBAR11                                                           
         LA    R4,KEYCLNQ(R4)                                                   
         BCT   RF,TBAR9                                                         
         B     TBAR5                                                            
*                                                                               
TBAR11   MVC   ASKKEY,KEYNEW       FIX BATCH KEY                                
         B     TBAR5                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* CHANGE TRANSACTION RECORDS                                          *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
TRNR     NTR1  ,                                                                
         XC    ORGKY,ORGKY                                                      
         MVC   ORGKY(41),TRNKEY                                                 
         ICM   RF,15,NTAB          TEST NUMBER IN TABLE                         
         L     R4,AKEYT                                                         
         USING KEYCD,R4                                                         
TRNR3    CLC   KEYOLD,TRNKEY       FIND MATCH IN TABLE                          
         BE    TRNR5                                                            
         LA    R4,KEYCLNQ(R4)                                                   
         BCT   RF,TRNR3                                                         
         DC    H'0'                                                             
*                                                                               
TRNR5    MVC   TRNKEY,KEYNEW       FIX TRANSACTION KEY                          
         CLC   TRNKREF,=C'*TIME*'  TEST TIME RECORD                             
         BE    XIT                                                              
*                                                                               
         USING TRNELD,R3                                                        
TRNR6    LA    R3,TRNRFST          UPDATE TRNELD SUBREF NUMBER WITH             
         CLI   TRNEL,TRNELQ        NEW SEQUENCE NUMBER ASSINED TO               
         BE    *+6                 THE TRANSACTION KEY                          
         DC    H'0'                                                             
         MVC   TRNSUB,TRNKSBR                                                   
         MVC   TRNREF,TRNKREF                                                   
         DROP  R3                                                               
*                                                                               
TRNR7    LA    R3,TRNRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING MPYELD,R3                                                        
TRNR9    CLI   MPYEL,0             FIND MANUAL PAYMENT ELEMENT                  
         BE    XIT                                                              
         CLI   MPYEL,MPYELQ                                                     
         BE    TRNR13                                                           
         IC    R0,MPYLN                                                         
         AR    R3,R0                                                            
         B     TRNR9                                                            
*                                                                               
TRNR13   CLI   MPYLN,MPYLN2Q       TEST NEW LENGTH                              
         BL    XIT                                                              
         MVC   ORGKY+(TRNKSBR-TRNKEY)(1),MPYSUB                                 
         ICM   RF,15,NTAB          TEST NUMBER IN TABLE                         
         L     R4,AKEYT                                                         
*                                                                               
         USING KEYCD,R4                                                         
TRNR15   CLC   KEYOLD,ORGKY                                                     
         BE    TRNR19                                                           
         LA    R4,KEYCLNQ(R4)                                                   
         BCT   RF,TRNR15                                                        
         B     XIT                 CAN'T FIND PROPER KEY                        
*                                                                               
TRNR19   MVC   MPYSUB,KEYNEW+(TRNKSBR-TRNKEY)   REPLACE SUB-REF                 
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD A TABLE OF KEY CHANGES                                        *         
***********************************************************************         
                                                                                
BLDT     NTR1  ,                                                                
         SR    R0,R0                                                            
         CLI   NXTSORT,EOT         END OF SORT FILE                             
         BE    BLDTX                                                            
         L     R4,AKEYT                                                         
         USING KEYCD,R4                                                         
         LR    R6,R4               R6=FIRST ENTRY                               
         SR    R5,R5                                                            
         OC    NXTSORT,NXTSORT     NEXT SORT RECORD                             
         BZ    BLDT5               FIRST TIME                                   
*                                                                               
BLDT3    LA    R5,1                SET NOT FIRST INDICATOR                      
         MVC   KEYCD(KEYCLNQ),NXTSORT                                           
         AH    R0,=H'1'                                                         
         CH    R0,=Y(MXKEY)                                                     
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R4,KEYCLNQ(R4)                                                   
*                                                                               
BLDT5    GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)         TEST EOF                                     
         BZ    BLDTX                                                            
         MVC   NXTSORT,0(RF)                                                    
         LTR   R5,R5               TEST FIRST TIME                              
         BZ    BLDT3               IF IT IS, GET NEXT                           
*                                                                               
BLDT7    LA    R1,L'ACCKEY         SET LENGTH FOR COMPARE                       
         BCTR  R1,0                                                             
         CLI   KEYREC-KEYCD(R6),C' '    TEST TRANSACTION RECORD                 
         BNH   *+6                                                              
         BCTR  R1,0                DON'T COMPARE ON SUBREF                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEYREC-KEYCD(0,R6),NXTSORT+(KEYREC-KEYCD)                        
         BE    BLDT3                                                            
*                                                                               
BLDTX    ST    R0,NTAB             SAVE NUMBER OF ITEMS                         
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         MVI   NXTSORT,EOT                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
NEWKY    DC    XL42'00'                                                         
OLDKY    DC    XL42'00'                                                         
ORGKY    DC    XL42'00'                                                         
*                                                                               
NTAB     DC    F'0'                                                             
AKEYT    DC    A(KEYT)                                                          
NXTSORT  DC    XL(KEYCLNQ)'00'     NEXT SORT RECORD                             
*                                                                               
*              SORT BY TYPE/NEW/OLD                                             
SORTCRD1 DC    CL80'SORT FIELDS=(1,1,A,44,42,A,2,42,A),FORMAT=BI,WORK=1X        
                '                                                               
RECCRD1  DC    CL80'RECORD TYPE=F,LENGTH=(130,,,,) '                            
*                                                                               
*              SORT BY OLD/TYPE/NEW                                             
SORTCRD2 DC    CL80'SORT FIELDS=(2,42,A,1,1,A,44,42,A),FORMAT=BI,WORK=1X        
                '                                                               
RECCRD2  DC    CL80'RECORD TYPE=F,LENGTH=(130,,,,) '                            
*                                                                               
*              SORT BY RECORD KEY                                               
SORTCRD3 DC    CL80'SORT FIELDS=(86,42,A),FORMAT=BI,WORK=1 '                    
RECCRD3  DC    CL80'RECORD TYPE=F,LENGTH=(130,,,,) '                            
         SPACE 2                                                                
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCB'S                                                               *         
***********************************************************************         
                                                                                
* INPUT KEY FILE                                                                
TKEY     DCB   DDNAME=TKEY,DSORG=PS,MACRF=(PM,GM),EODAD=SINFX          *        
               RECFM=FB,LRECL=130,BLKSIZE=32500                                 
                                                                                
* TEMP WORK FILE FOR KEYS                                                       
WKEY     DCB   DDNAME=WKEY,DSORG=PS,MACRF=(PM,GM),EODAD=SOUTX,         *        
               RECFM=FB,LRECL=130,BLKSIZE=23400                                 
                                                                                
*                                                                               
* INPUT ACCOUNT FILE (TRANSACTIONS & SPECIAL)                                   
TTRN     DCB   DDNAME=TTRN,DSORG=PS,MACRF=(GM),EODAD=UBTRX,            *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
                                                                                
* OUTPUT ACCOUNT FILE                                                           
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
*                                                                               
* KEY TABLE                                                                     
MXKEY    EQU   300                                                              
KEYT     DS    (MXKEY)CL(KEYCLNQ)                                               
          EJECT                                                                 
ACNVD    DSECT                                                                  
       ++INCLUDE ACNVWORK                                                       
          EJECT                                                                 
       ++INCLUDE ACNVDSECT                                                      
          EJECT                                                                 
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACNV02    08/16/00'                                      
         END                                                                    
