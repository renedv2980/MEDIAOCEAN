*          DATA SET ACREP8802  AT LEVEL 058 AS OF 06/03/15                      
*PHASE AC8802A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE CHOPPER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'CLIENT/EXPENSE ANALYSIS'                                        
AC8802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC88**,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC88D,RC                                                         
         SPACE 1                                                                
         LA    R9,2048(RB)         R9 IS SECOND BASE REGISTER                   
         LA    R9,2048(R9)                                                      
         USING AC8802+4096,R9                                                   
         ST    R5,RELO                                                          
         SPACE 1                                                                
         L     R8,=A(TABHEADS)                                                  
         A     R8,RELO                                                          
         USING TABHEADS,R8                                                      
         SPACE 3                                                                
*        REQFRST                                                                
         SPACE 2                                                                
CEA      CLI   MODE,REQFRST                                                     
         BNE   CEB                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDACC,C'Y'        RESET FOR READS                              
         MVI   FCRDHIST,C'Y'                                                    
         SPACE 1                                                                
         BAS   RE,SETSORT                                                       
         SPACE 1                                                                
         L     R1,=A(TABET)                                                     
         A     R1,RELO                                                          
         ST    R1,ATABET                                                        
         L     R1,=A(TABLVA)                                                    
         A     R1,RELO                                                          
         ST    R1,ATABLVA                                                       
         L     R1,=A(TABLVB)                                                    
         A     R1,RELO                                                          
         ST    R1,ATABLVB                                                       
         L     R1,=A(TABLVC)                                                    
         A     R1,RELO                                                          
         ST    R1,ATABLVC                                                       
         L     R1,=A(TABLVD)                                                    
         A     R1,RELO                                                          
         ST    R1,ATABLVD                                                       
         SPACE 2                                                                
         LA    R1,TABLVAH                                                       
         ST    R1,TABETH                                                        
         LA    R1,TABLVBH                                                       
         ST    R1,TABLVAH                                                       
         LA    R1,TABLVCH                                                       
         ST    R1,TABLVBH                                                       
         LA    R1,TABLVDH                                                       
         ST    R1,TABLVCH                                                       
         SPACE 1                                                                
         LA    R0,TABMAX                                                        
         STH   R0,TABLINES     MAX NO IN TABLE                                  
         L     R4,ATABET                                                        
         USING LINED,R4                                                         
         MVC   ETCODE(L'ETCODE+L'ACCODE),=20X'FF'                               
         MVC   ARCID,SPACES                                                     
         BAS   RE,BUCKZAP                                                       
         MVI   ACTIVE,C'N'                                                      
         SPACE 1                                                                
         LH    R6,TABLINES                                                      
         BCTR  R6,0                                                             
         MVC   L'MYLINE(L'MYLINE,R4),0(R4)                                      
         LA    R4,L'MYLINE(R4)                                                  
         BCT   R6,*-10                                                          
         SPACE 1                                                                
         L     R2,ATABET                                                        
         L     R0,ATABLVA                                                       
         BAS   R6,MOVELONG                                                      
         SPACE 1                                                                
         L     R2,ATABLVA                                                       
         L     R0,ATABLVB                                                       
         BAS   R6,MOVELONG                                                      
         SPACE 1                                                                
         L     R2,ATABLVB                                                       
         L     R0,ATABLVC                                                       
         BAS   R6,MOVELONG                                                      
         SPACE 1                                                                
         L     R2,ATABLVC                                                       
         L     R0,ATABLVD                                                       
         BAS   R6,MOVELONG                                                      
         B     CEA05                                                            
         SPACE 1                                                                
MOVELONG LA    R1,L'MYLINE                                                      
         MH    R1,TABLINES                                                      
         LR    R3,R1                                                            
         MVCL  R0,R2                                                            
         BR    R6                                                               
         SPACE 2                                                                
         SPACE 1                                                                
CEA05    LA    R4,TOTLINE                                                       
         BAS   RE,BUCKZAP                                                       
         MVC   ETCODE(56),SPACES                                                
         MVC   ARCID(13),=C'REQUEST TOTAL'                                      
         SPACE 1                                                                
         LA    R0,OTHETLIN                                                      
         L     R2,ATABET                                                        
         LA    R1,L'MYLINE                                                      
         MH    R1,=H'5'            5 EXTRA LINES TO CLEAR                       
         LR    R3,R1                                                            
         MVCL  R0,R2                                                            
         SPACE 1                                                                
         MVC   OTHETLIN+20(6),=C'OTHERS'                                        
         SPACE 1                                                                
         XC    PDATES(56),PDATES                                                
         MVC   PLIN(70),SPACES                                                  
         MVC   PLIN+70(70),SPACES                                               
         SPACE 1                                                                
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   PSTRT,WORK+6                                                     
         MVC   PERIOD,SPACES                                                    
         MVI   DMCB+4,6                                                         
         BASR  RE,RF                                                            
         MVC   PERIOD(6),WORK+6                                                 
         SPACE 1                                                                
         MVC   WORK(4),QEND                                                     
         MVI   DMCB+4,1                                                         
         BASR  RE,RF                                                            
         MVC   PEND,WORK+6                                                      
         MVI   DMCB+4,6                                                         
         BASR  RE,RF                                                            
         CLC   PSTRT,PEND                                                       
         BE    *+16                                                             
         MVC   PERIOD+10(6),WORK+6                                              
         MVC   PERIOD+7(2),=C'TO'                                               
*&&UK                                                                           
         MVI   PERIOD+3,C' '                                                    
         MVI   PERIOD+13,C' '                                                   
*&&                                                                             
         SPACE 1                                                                
         SR    R2,R2               GET NUMBER OF MONTHS                         
         SR    R3,R3                                                            
         IC    R2,PSTRT+1                                                       
         CH    R2,=H'10'                                                        
         BL    *+8                                                              
         SH    R2,=H'6'                                                         
         IC    R3,PEND+1                                                        
         CH    R3,=H'10'                                                        
         BL    *+8                                                              
         SH    R3,=H'6'                                                         
         SR    R3,R2                                                            
         A     R3,=F'1'                                                         
         CLC   PSTRT(1),PEND                                                    
         BE    *+8                                                              
         AH    R3,=H'12'                                                        
         SPACE 1                                                                
         STC   R3,ELCODE           LOOK FOR MINI-ELEMENT                        
         LA    R6,MONTB                                                         
CEA10    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                REQUEST OPTION NOT IN TABLE                  
         CLI   2(R6),C'Z'          IF Z IGNORE OPTION                           
         BE    *+14                                                             
         CLC   QOPT3,2(R6)                                                      
         BNE   CEA10                                                            
         SPACE 1                                                                
         MVC   WORK(1),PSTRT       YEAR                                         
         MVI   WORK+1,X'0F'        PACKED                                       
         MVC   WORK+2(1),PSTRT+1   MONTH                                        
         MVI   WORK+3,X'0F'        PACKED                                       
         SPACE 1                                                                
         IC    R3,1(R6)            LENGTH                                       
         SH    R3,=H'3'            NUMBER OF BUCKETS                            
         CH    R3,=H'7'                                                         
         BL    *+6                                                              
         DC    H'0'                TABLE ERROR- MORE THAN SIX BUCKETS           
         SPACE 1                                                                
         ST    R3,CNT                                                           
         LA    R5,PDATES                                                        
         MVC   0(2,R5),PSTRT                                                    
         CH    R3,=H'1'            ONE ITEM                                     
         BE    CEA14                                                            
         SPACE 1                                                                
CEA12    IC    R2,3(R6)            NUMBER OF MONTHS IN THIS BUCKET              
         BCT   R2,*+8                                                           
         B     *+12                                                             
         BAS   RE,ADDMON           ADD ONE MONTH                                
         B     *-12                                                             
         MVC   2(1,R5),WORK        YEAR FOR END                                 
         MVC   3(1,R5),WORK+2      MONTH FOR END                                
         SPACE 1                                                                
         BAS   RE,ADDMON           NEW START MONTH                              
         LA    R5,4(R5)            PDATES                                       
         LA    R6,1(R6)            NEXT NUMBER OF MONTHS                        
         MVC   0(1,R5),WORK        START YEAR                                   
         MVC   1(1,R5),WORK+2      START MONTH                                  
         BCT   R3,CEA12            NUMBER OF BUCKETS                            
         L     R3,CNT              UP CNT  FOR TOTALS                           
         LA    R3,1(R3)                                                         
         ST    R3,CNT                                                           
         SPACE 1                                                                
CEA14    MVC   0(2,R5),PSTRT       FOR TOTALS                                   
         MVC   2(2,R5),PEND                                                     
         SPACE 1                                                                
CEA16    LA    R4,PLIN             HEADLINE                                     
         LA    R3,PDATES                                                        
         BAS   RE,HEADMON                                                       
         CLI   QOPT2,C'Y'          INCLUDE LAST YEAR                            
         BNE   CEXIT                                                            
         SPACE 1                                                                
         LA    R4,PDATES           SET UP LAST YEARS                            
         LA    R3,PDATES+28                                                     
         L     R5,CNT                                                           
         XR    R6,R6                                                            
         SPACE 1                                                                
CEA18    MVC   0(4,R3),0(R4)                                                    
         IC    R6,0(R3)                                                         
         BCTR  R6,0                                                             
         TM    0(R3),X'0F'         1980                                         
         BM    *+8                                                              
         SH    R6,=H'6'                                                         
         STC   R6,0(R3)                                                         
         IC    R6,2(R3)                                                         
         BCTR  R6,0                                                             
         TM    2(R3),X'0F'                                                      
         BM    *+8                                                              
         SH    R6,=H'6'                                                         
         STC   R6,2(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,CEA18                                                         
         SPACE 1                                                                
         LA    R4,PLIN+70                                                       
         LA    R3,PDATES+28                                                     
         BAS   RE,HEADMON                                                       
         B     CEXIT                                                            
         SPACE 2                                                                
ADDMON   CP    WORK+2(2),=P'120'   DECEMBER                                     
         BNE   *+16                                                             
         AP    WORK(2),=P'10'      ADD 1 TO YEAR                                
         ZAP   WORK+2(2),=P'0'                                                  
         AP    WORK+2(2),=P'10'                                                 
         BR    RE                                                               
         EJECT                                                                  
*              TABLE TO BUILD BUCKET DATES                                      
*              BYTE 1              NUMBER OF MONTHS REQUESTED                   
*              BYTE 2              LENGTH OF ELEMENT                            
*              BYTE 3              OPTION FOR QOPT3                             
*              BYTE 4-N            NUMBER OF MONTHS IN EACH BUCKET              
*                                                                               
MONTB    DC    X'FF02'             SO I CAN START WITH NEXTEL                   
A1       DC    X'04',AL1(A2-A1),C'A',4X'01'                                     
A2       DC    X'05',AL1(A3-A2),C'A',5X'01'                                     
A3       DC    X'06',AL1(A4-A3),C'A',6X'01'                                     
A4       EQU   *                                                                
M1       DC    X'01',AL1(M2-M1),C'Z',X'01'                                      
M2       DC    X'02',AL1(M3-M2),C'Z',2X'01'                                     
M3       DC    X'03',AL1(M4-M3),C'Z',3X'01'                                     
M4       DC    X'04',AL1(M5-M4),C'Z',X'03',X'01'                                
M5       DC    X'05',AL1(M6-M5),C'Z',X'03',2X'01'                               
M6       DC    X'06',AL1(M7-M6),C'Z',X'03',3X'01'                               
M7       DC    X'07',AL1(M8-M7),C'Z',2X'03',X'01'                               
M8       DC    X'08',AL1(M9-M8),C'Z',2X'03',2X'01'                              
M9       DC    X'09',AL1(MA-M9),C'Z',2X'03',3X'01'                              
MA       DC    X'0A',AL1(MB-MA),C'Z',3X'03',X'01'                               
MB       DC    X'0B',AL1(MC-MB),C'Z',3X'03',2X'01'                              
MC       DC    X'0C',AL1(MD-MC),C'Z',3X'03',3X'01'                              
MD       DC    X'0000'             END OF TABLE                                 
         EJECT                                                                  
*              BUILD HEADLINES  FOR MONTHS FROM PDATES                          
         SPACE 2                                                                
HEADMON  NTR1                                                                   
         L     R5,CNT                                                           
         SPACE 1                                                                
HEDM1    MVC   WORK(2),0(R3)                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+3)                                  
         MVC   4(3,R4),WORK+3      MMM                                          
         MVC   7(2,R4),WORK+7      YY                                           
         CLC   0(2,R3),2(R3)       SAME END                                     
         BE    HEDLOP                                                           
         SPACE 1                                                                
         MVC   WORK(2),2(R3)       SET UP FOR SPAN MMM-MMM/YY                   
         MVC   0(3,R4),WORK+3                                                   
         MVI   3(R4),C'-'                                                       
         GOTO1 (RF)                                                             
         MVC   4(3,R4),WORK+3                                                   
         MVC   7(2,R4),WORK+7                                                   
         SPACE 1                                                                
HEDLOP   LA    R4,10(R4)                                                        
         LA    R3,4(R3)                                                         
         BCT   R5,HEDM1                                                         
         SPACE 1                                                                
         CLI   CNT+3,1                                                          
         BE    EXIT                                                             
         S     R4,=F'10'                                                        
         MVC   0(10,R4),=C'    TOTAL '                                          
         B     EXIT                                                             
         EJECT                                                                  
*        LEDGFRST                                                               
         SPACE 2                                                                
CEB      CLI   MODE,LEDGFRST                                                    
         BNE   CEC                                                              
         SPACE 1                                                                
         MVC   SAVET,=20X'FF'                                                   
         L     R4,=A(WRKTAB)                                                    
         L     R6,ADLEDGER                                                      
         LA    R5,L'WRKTAB                                                      
         MH    R5,TABLINES                                                      
         SR    R0,R0                                                            
         IC    R1,=X'40'                                                        
         SLL   R1,24                                                            
         MVCL  R4,R0               BLANKS OUT WRKTAB                            
         L     R4,=A(WRKTAB)           RESTORE R4                               
         SPACE 1                                                                
CEB10    MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL            GETEL USES R6                                
         BE    CEB100                                                           
         MVI   FCRDACC,C'N'        NO WORKCODES YOUR DONE                       
         MVI   FCRDHIST,C'N'                                                    
         MVC   P+1(33),=CL33'** ERROR ** NO WORKCODES FOUND   '                 
         GOTO1 ACREPORT                                                         
         B     CEXIT                                                            
         SPACE 1                                                                
CEB100   LH    R2,TABLINES                                                      
CEB101   MVC   0(17,R4),2(R6)      FILLS IN WRKTAB                              
         LA    R4,17(R4)                                                        
         BAS   RE,NEXTEL                                                        
         BNE   CEB20                                                            
         BCT   R2,CEB101                                                        
         DC    H'0'                TOO MANY WORK CODES                          
         SPACE 1                                                                
CEB20    DS    0H                  FIX TABLE HEADERS                            
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         LA    R5,TABLVAH                                                       
         LA    R3,ACHRLEVA                                                      
         LA    R2,4                FOUR LEVELS                                  
         SR    R6,R6                                                            
         SR    R1,R1                                                            
         SPACE 1                                                                
CEB30    LTR   R1,R1                                                            
         BNZ   CEB32                                                            
         IC    R6,0(R3)                                                         
         STC   R6,9(R5)            LENGTH OF THIS LEVELS KEY                    
         MVC   10(15,R5),1(R3)     DESCRIPTION OF THIS LEVEL                    
         MVI   8(R5),0                                                          
         CH    R6,=H'12'                                                        
         BNE   CEB34                                                            
         LA    R1,1                                                             
         MVI   8(R5),1                                                          
         B     CEB34                                                            
CEB32    MVI   8(R5),X'FF'                                                      
CEB34    LA    R3,16(R3)                                                        
         L     R5,0(R5)                                                         
         BCT   R2,CEB30                                                         
         SPACE 1                                                                
CEB50    MVI   TABETH+8,0                                                       
         CLI   QOPT1,C' '                                                       
         BE    CEB60                                                            
         CLI   QOPT1,C'4'                                                       
         BNE   CEB501                                                           
         MVI   TABLVDH+8,1                                                      
         B     CEB60                                                            
CEB501   CLI   QOPT1,C'3'                                                       
         BNE   CEB502                                                           
         MVI   TABLVCH+8,1                                                      
         B     CEB60                                                            
CEB502   CLI   QOPT1,C'2'                                                       
         BNE   CEB503                                                           
         MVI   TABLVBH+8,1                                                      
         B     CEB60                                                            
CEB503   CLI   QOPT1,C'1'                                                       
         BNE   CEB504                                                           
         MVI   TABLVAH+8,1                                                      
         B     CEB60                                                            
CEB504   MVI   TABETH+8,1          DEFAULT TO SUMMARY ONLY                      
         MVI   QOPT1,C'S'                                                       
         SPACE 1                                                                
CEB60    LA    R5,TABETH                                                        
         LA    R2,5                                                             
         SR    R6,R6                                                            
         SPACE 1                                                                
CEB601   LTR   R6,R6                                                            
         BNZ   CEB602                                                           
         CLI   8(R5),1                                                          
         BNE   CEB603                                                           
         LA    R6,1                                                             
         B     CEB603                                                           
CEB602   MVI   8(R5),X'FF'                                                      
CEB603   L     R5,0(R5)                                                         
         BCT   R2,CEB601                                                        
         B     CEXIT                                                            
         EJECT                                                                  
*        PROCLEVA                                                               
         SPACE 2                                                                
CEC      CLI   MODE,PROCLEVA                                                    
         BNE   CED                                                              
         SPACE 1                                                                
         MVI   SAVESW,C'N'                                                      
         CLI   TABLVAH+8,X'FF'                                                  
         BE    CEXIT               NOT INTERESTED                               
         LA    R1,TABLVAH                                                       
         ST    R1,ATAB                                                          
         MVI   LEVLET,C'A'                                                      
         BAS   RE,LEVUP            RELEASE RECORDS TO SORTER,UPDATE             
*                                  TABLES                                       
         B     CEXIT                                                            
         SPACE 2                                                                
*        PROCLEVB                                                               
         SPACE 2                                                                
CED      CLI   MODE,PROCLEVB                                                    
         BNE   CEE                                                              
         SPACE 1                                                                
         CLI   TABLVBH+8,X'FF'                                                  
         BE    CEXIT                                                            
         LA    R1,TABLVBH                                                       
         ST    R1,ATAB                                                          
         MVI   LEVLET,C'B'                                                      
         BAS   RE,LEVUP                                                         
         B     CEXIT                                                            
         SPACE 2                                                                
*        PROCLEVC                                                               
         SPACE 2                                                                
CEE      CLI   MODE,PROCLEVC                                                    
         BNE   CEF                                                              
         SPACE 1                                                                
         CLI   TABLVCH+8,X'FF'                                                  
         BE    CEXIT                                                            
         LA    R1,TABLVCH                                                       
         ST    R1,ATAB                                                          
         MVI   LEVLET,C'C'                                                      
         BAS   RE,LEVUP                                                         
         B     CEXIT                                                            
         SPACE 2                                                                
*        PROCLEVD                                                               
         SPACE 2                                                                
CEF      CLI   MODE,PROCLEVD                                                    
         BNE   CEG                                                              
         SPACE 1                                                                
         CLI   TABLVDH+8,X'FF'                                                  
         BE    CEXIT                                                            
         LA    R1,TABLVDH                                                       
         ST    R1,ATAB                                                          
         MVI   LEVLET,C'D'                                                      
         BAS   RE,LEVUP                                                         
         B     CEXIT                                                            
         SPACE 2                                                                
*        SBACFRST                                                               
         SPACE 2                                                                
CEG      CLI   MODE,SBACFRST                                                    
         BNE   CEH                                                              
         MVI   WANT,C'N'                                                        
         L     R4,ADSUBAC          LOOK FOR * TE -                              
         USING TRSUBHD,R4                                                       
         SR    R3,R3                                                            
         CLI   TRSBACNT,C'*'                                                    
         BNE   CEXIT                                                            
         SPACE 1                                                                
         LA    R4,TRSBACNT+1                                                    
         LA    R5,8                IS THIS ENOUGH                               
CEG10    CLI   0(R4),C'-'                                                       
         BE    CEG20                                                            
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,CEG10                                                         
         B     CEXIT                                                            
         SPACE 1                                                                
CEG20    MVI   WANT,C'Y'                                                        
         L     R4,ADSUBAC                                                       
         MVC   DUB,=20X'FF'                                                     
         CLI   PROGPROF+2,0        LENGTH OF T/E CODE                           
         BE    *+8                                                              
         IC    R3,PROGPROF+2                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),TRSBACNT+1                                                
         CLC   SAVET,DUB                                                        
         BE    CEXIT                                                            
         MVC   SAVET,DUB           NEW E TYPE                                   
         SPACE 1                                                                
         L     R5,=A(WRKTAB)           FIND WORKCODE                            
         LH    R3,TABLINES                                                      
         SPACE 1                                                                
CEG30    LA    RE,SAVET                                                         
         CLI   PROGPROF+1,C'Y'     USE FIRST POSITION FOR NAME                  
         BE    CEG300                                                           
         LA    RE,SAVET+1          FIND WORK CODE                               
         ZIC   RF,PROGPROF         PROFILE(AS AC86) TO MOVE                     
         LTR   RF,RF               T/E CODE AROUND                              
         BZ    CEG300                                                           
         LA    RE,SAVET-1(RF)                                                   
CEG300   CLC   0(2,R5),0(RE)                                                    
         BE    CEG40               FOUND THE WORKCODE                           
         LA    R5,L'WRKTAB(R5)                                                  
         BCT   R3,CEG30                                                         
         SPACE 1                                                                
CEG301   LA    R5,OTHETLIN         HAVENT FOUND IT                              
         ST    R5,TABETH+4                                                      
         LA    R5,OTHLINA                                                       
         ST    R5,TABLVAH+4                                                     
         LA    R5,OTHLINB                                                       
         ST    R5,TABLVBH+4                                                     
         LA    R5,OTHLINC                                                       
         ST    R5,TABLVCH+4                                                     
         LA    R5,OTHLIND                                                       
         ST    R5,TABLVDH+4                                                     
         B     CEG50                                                            
         SPACE 1                                                                
CEG40    LA    R1,TABETH           R5,POINTS TO LINE IN WRKTAB                  
         LA    R2,5                                                             
         MVI   LEVLET,C'E'         E FOR EXPENSE TYPE                           
         USING LINED,R4                                                         
CEG400   CLI   8(R1),X'FF'                                                      
         BE    CEG50               FINISHED                                     
         L     R4,28(R1)           PT R4 AT TABLE                               
         LH    R6,TABLINES                                                      
CEG401   CLC   ETCODE,SAVET                                                     
         BE    CEG403                                                           
         CLI   ETCODE,X'FF'                                                     
         BE    CEG402                                                           
         LA    R4,L'MYLINE(R4)                                                  
         BCT   R6,CEG401                                                        
         SPACE 1                                                                
         B     CEG301              TROUBLE,PRETEND WE HAVEN'T FOUND IT          
         SPACE 1                                                                
CEG402   MVC   ETCODE,SAVET                                                     
         CLI   LEVLET,C'E'                                                      
         BNE   CEG403                                                           
         MVC   ARCID(15),2(R5)     MOVE IN NAME OF WORKCODE FRM WRKTAB          
         MVI   LEVLET,C'A'         A IS FOR APPLE                               
CEG403   ST    R4,4(R1)            STORE ADDRESS OF LINE IN TABLE HEAD          
         L     R1,0(R1)            FIND ADDRESS OF NEXT TABLE                   
         BCT   R2,CEG400                                                        
CEG50    B     CEXIT                                                            
         EJECT                                                                  
*        PROCHIST                                                               
         SPACE 2                                                                
CEH      CLI   MODE,PROCHIST                                                    
         BNE   CEI                                                              
         CLI   WANT,C'Y'                                                        
         BNE   CEXIT                                                            
         SPACE 1                                                                
         L     R4,ADTRANS                                                       
         CLI   0(R4),X'45'                                                      
         BNE   CEXIT                                                            
         USING TRHISTD,R4                                                       
         SPACE 1                                                                
         ZAP   AMT,TRHSCR                                                       
         SP    AMT,TRHSDR                                                       
         SPACE 1                                                                
         AP    AMT,=P'50'          ROUNDING                                     
         CP    AMT,=P'0'                                                        
         BH    *+10                                                             
         SP    AMT,=P'100'                                                      
         DP    AMT,=P'100'                                                      
         SPACE 1                                                                
         L     R5,CNT              NO OF MONTHS (YOU'D FORGOTTEN HUH!)          
         LA    R3,PDATES                                                        
         LA    R6,BUCKETS-LINED                                                 
         SPACE 1                                                                
CEH10    CLC   TRHSYEAR(2),0(R3)                                                
         BL    CEH20                                                            
         CLC   TRHSYEAR(2),2(R3)                                                
         BH    CEH20                                                            
         BAS   RE,ADDIN                                                         
         SPACE 1                                                                
CEH20    CLI   QOPT2,C'Y'                                                       
         BNE   CEH40                                                            
         LA    R6,42(R6)           LAST YEARS BUCKETS                           
         CLC   TRHSYEAR(2),28(R3)                                               
         BL    CEH30                                                            
         CLC   TRHSYEAR(2),30(R3)                                               
         BH    CEH30                                                            
         BAS   RE,ADDIN                                                         
CEH30    SH    R6,=H'42'                                                        
         SPACE 1                                                                
CEH40    LA    R3,4(R3)                                                         
         LA    R6,L'BUCKETS(R6)                                                 
         BCT   R5,CEH10                                                         
         B     CEXIT                                                            
         EJECT                                                                  
*        REQLAST                                                                
         SPACE 2                                                                
CEI      CLI   MODE,REQLAST                                                     
         BNE   CEXIT                                                            
         SPACE 1                                                                
         MVI   SAVESW,C'Y'                                                      
         MVI   LEVLET,C'E'                                                      
         LA    R1,TABETH                                                        
         ST    R1,ATAB                                                          
         BAS   RE,LEVUP            PUT OUT E T. RECORDS TO SORTER               
         SPACE 1                                                                
CEI10    MVI   LEVLET,X'C0'        PUT OUT LAST DETAIL RECORDS                  
         LA    R2,4                                                             
         SR    R3,R3                                                            
CEI12    L     R1,0(R1)                                                         
         CLI   8(R1),X'FF'                                                      
         BE    CEI20                                                            
         IC    R3,LEVLET                                                        
         LA    R3,1(R3)                                                         
         STC   R3,LEVLET                                                        
         CLI   8(R1),1                                                          
         BNE   *+16                                                             
         MVC   LOWLEN,9(R1)                                                     
         MVC   LOWLET,LEVLET                                                    
         ST    R1,ATAB                                                          
         BAS   RE,LEVUP                                                         
         BCT   R2,CEI12                                                         
         SPACE 1                                                                
         USING LINED,R4                                                         
CEI20    MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'S'                                                       
         BE    CEI22                                                            
         MVC   SAVET,=20X'FF'                                                   
         MVI   RCSUBPRG,0                                                       
         B     *+8                                                              
CEI22    MVI   RCSUBPRG,1                                                       
         SPACE 1                                                                
CEI24    CLI   QOPT1,C'F'          F FOR FINISHED                               
         BE    CEXIT                                                            
         BAS   RE,GETSORT                                                       
         L     R4,DMCB+4           ADDRESS OF RECORD FROM SORTER                
         LTR   R4,R4                                                            
         BNZ   CEI30                                                            
         SPACE 1                                                                
         CLI   QOPT1,C'S'                                                       
         BNE   CEI26                                                            
         GOTO1 ACREPORT                                                         
         LA    R4,TOTLINE                                                       
         MVI   QOPT1,C'F'          F FOR FINISHED                               
         B     CEI300                                                           
         SPACE 1                                                                
CEI26    MVI   QOPT1,C'S'                                                       
         BAS   RE,SETSORT          PREPARE TO RESORT E.TYPE RECORDS             
         MVI   SAVESW,C'Y'                                                      
         MVI   LEVLET,C'E'                                                      
         LA    R1,TABETH                                                        
         ST    R1,ATAB                                                          
         BAS   RE,LEVUP            E.TYPE RECORDS NOW BACK IN SORTER            
         B     CEI20               DO SUMMARY PAGE                              
         SPACE 1                                                                
CEI30    CLI   QOPT1,C'S'                                                       
         BNE   CEI40               DETAIL PRINTING                              
         CLI   FORCEHED,C'Y'                                                    
         BE    CEI31                                                            
         SPACE 1                                                                
CEI300   ZIC   R1,LINE                                                          
         LA    R1,3(R1)                                                         
         ZIC   R2,MAXLINES                                                      
         CR    R1,R2                                                            
         BNH   CEI32                                                            
         MVI   FORCEHED,C'Y'                                                    
CEI31    MVC   HEAD8+1(12),=C'EXPENSE TYPE'                                     
         MVC   HEAD8+40(70),PLIN                                                
         MVC   HEAD9+1(12),=21C'-'                                              
         MVC   HEAD9+40(70),PLIN+70                                             
         MVC   HEAD3+94(16),PERIOD                                              
CEI32    MVC   PSECOND+1(L'ETCODE),ETCODE                                       
         SPACE 1                                                                
         MVC   PSECOND+L'ETCODE+2(15),ARCID                                     
         SPACE 1                                                                
         GOTO1 =V(SQUASHER),DMCB,PSECOND+1,(C' ',40),RR=RB                      
         SPACE 1                                                                
         LA    R1,PSECOND                                                       
         B     EDITOR                                                           
         SPACE 2                                                                
CEI40    CLC   ETCODE,SAVET                                                     
         BE    CEI42                                                            
         MVC   SAVET,ETCODE        NEW E TYPE                                   
         ZAP   LOWCNT,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     CEI44                                                            
CEI42    CLI   FORCEHED,C'Y'                                                    
         BE    CEI44                                                            
         ZIC   R1,LINE                                                          
         LA    R1,3(R1)                                                         
         ZIC   R2,MAXLINES                                                      
         CR    R1,R2                                                            
         BNH   CEI60                                                            
         MVI   FORCEHED,C'Y'                                                    
CEI44    MVC   HEAD8+1(21),=C'ACCOUNT CODE AND NAME'                            
         MVC   HEAD9+1(21),=21C'-'                                              
         MVC   HEAD8+40(70),PLIN                                                
         MVC   HEAD9+40(70),PLIN+70                                             
         MVC   HEAD3+94(16),PERIOD                                              
         SPACE 1                                                                
         CLI   SAVET,X'40'                                                      
         BNE   CEI48                                                            
CEI46    MVC   HEAD5+14(6),=C'OTHERS'                                           
         B     CEI60                                                            
CEI48    MVC   DUB,SAVET                                                        
         LA    R1,DUB                                                           
         LA    R2,8                                                             
CEI49    CLI   0(R1),X'40'                                                      
         BNE   *+8                                                              
         MVI   0(R1),X'FF'                                                      
         LA    R1,1(R1)                                                         
         BCT   R2,CEI49                                                         
         SPACE 1                                                                
         L     R1,ATABET                                                        
         LH    R2,TABLINES                                                      
         SPACE 1                                                                
CEI50    CLC   DUB,0(R1)                                                        
         BE    CEI52                                                            
         LA    R1,L'MYLINE(R1)                                                  
         BCT   R2,CEI50                                                         
         B     CEI46               HAVENT FOUND IT,IMPOSSIBLE.                  
         SPACE 1                                                                
CEI52    MVC   HEAD5+14(L'SAVET),SAVET                                          
         LR    R5,R4                                                            
         LR    R4,R1               SO WE CAN USE LINED                          
         MVC   HEAD5+15+L'SAVET(15),ARCID                                       
         GOTO1 =V(SQUASHER),DMCB,HEAD5+14,(C' ',30),RR=RB                       
         SPACE 1                                                                
         LR    R4,R5               RESTORE R4                                   
CEI60    CLI   ACTIVE,C'E'         ACTIVE SET BY LEVUP                          
         BNE   CEI62                                                            
         GOTO1 ACREPORT            E TYPE RECORD                                
         MVC   PSECOND+1(L'SAVET),SAVET                                         
         MVC   PSECOND+L'SAVET+2(15),ARCID                                      
         MVC   PSECOND+40(5),=C'TOTAL'                                          
         GOTO1 =V(SQUASHER),DMCB,PSECOND+1,(C' ',46),RR=RB                      
         LA    R1,PSECOND                                                       
         B     EDITOR                                                           
         SPACE 1                                                                
CEI62    CLC   ACTIVE,LOWLET                                                    
         BNE   *+14                                                             
         AP    LOWCNT,=P'1'                                                     
         B     CEI68                                                            
         SPACE 1                                                                
         CP    LOWCNT,=P'1'                                                     
         BH    *+14                                                             
         ZAP   LOWCNT,=P'0'                                                     
         B     CEI24              DONT PRINT TRIVIAL SUMMARIES                  
         ZAP   LOWCNT,=P'0'                                                     
         SPACE 1                                                                
         LA    R1,ACCODE+11           NOT A LOW LEVEL RECORD                    
         LA    R2,11               UNDERLINE THE KEY WITH *S                    
CEI64    CLI   0(R1),X'40'                                                      
         BNE   CEI66                                                            
         BCTR  R1,0                                                             
         BCT   R2,CEI64                                                         
CEI66    EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+1(0),=12C'*'                                             
         SPACE 1                                                                
CEI68    MVC   P+1(L'ACCODE),ACCODE                                             
         SPACE 1                                                                
         GOTO1 =V(CHOPPER),DMCB,(36,ARCID),(24,WORK),2,RR=RB                    
         SPACE 1                                                                
         MVC   P+14(24),WORK                                                    
         MVC   PSECOND+14(24),WORK+24                                           
         LA    R1,P                                                             
         B     EDITOR                                                           
         EJECT                                                                  
EDITOR   MVI   BYTE,0                                                           
         LA    R5,BUCKETS                                                       
ED10     LA    R6,40(R1)                                                        
         L     R2,CNT                                                           
         SPACE 1                                                                
ED15     CP    0(L'BUCKETS,R5),=P'0'                                            
         BE    ED20                                                             
         EDIT  (P6,0(R5)),(10,0(R6)),COMMAS=YES,MINUS=YES                       
         SPACE 1                                                                
ED20     LA    R6,10(R6)                                                        
         LA    R5,L'BUCKETS(R5)                                                 
         BCT   R2,ED15                                                          
         CLI   BYTE,1                                                           
         BE    ED30                                                             
         CLI   QOPT2,C'Y'                                                       
         BNE   ED30                                                             
         MVI   BYTE,1                                                           
         LA    R5,BUCKETS+42       LAST YEARS BUCKETS                           
         LA    R1,L'P(R1)                                                       
         B     ED10                                                             
ED30     MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         B     CEI24                                                            
CEXIT    XMOD1 1                                                                
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
LEVUP    NTR1                                                                   
         MVI   HALF,0                                                           
         L     R4,ATAB                                                          
         ZIC   R6,9(R4)                                                         
         BCTR  R6,0                LENGTH OF ACCOUNT KEY THIS LEVEL             
         L     R4,28(R4)           PT R4 AT TABLE                               
         USING LINED,R4                                                         
         L     R5,ADACCNAM                                                      
         USING ACNAMED,R5                                                       
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         LH    R3,TABLINES                                                      
         SPACE 1                                                                
LEV10    CLI   ACTIVE,C'N'                                                      
         BE    LEV20                                                            
         MVC   ACTIVE,LEVLET                                                    
         BAS   RE,PUTSORT                                                       
         CLI   SAVESW,C'Y'                                                      
         BE    LEV20                                                            
         BAS   RE,BUCKZAP                                                       
         MVI   ACTIVE,C'N'                                                      
LEV20    CLI   SAVESW,C'Y'                                                      
         BE    LEV22                                                            
         MVC   ARCID,SPACES                                                     
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ARCID(0),ACNMNAME                                                
         L     R1,ADACC                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   ACCODE(0),3(R1)                                                  
LEV22    LA    R4,L'MYLINE(R4)                                                  
         BCT   R3,LEV10                                                         
         SPACE 2                                                                
LEV30    CLI   HALF,1                                                           
         BE    EXIT                                                             
         CLI   LEVLET,C'E'                                                      
         BNE   LEV32                                                            
         LA    R4,OTHETLIN                                                      
         B     LEV40                                                            
LEV32    CLI   LEVLET,C'A'                                                      
         BNE   LEV34                                                            
         LA    R4,OTHLINA                                                       
         B     LEV40                                                            
LEV34    CLI   LEVLET,C'B'                                                      
         BNE   LEV36                                                            
         LA    R4,OTHLINB                                                       
         B     LEV40                                                            
LEV36    CLI   LEVLET,C'C'                                                      
         BNE   LEV38                                                            
         LA    R4,OTHLINC                                                       
         B     LEV40                                                            
LEV38    LA    R4,OTHLIND                                                       
LEV40    LA    R3,1                                                             
         MVI   HALF,1                                                           
         B     LEV10                                                            
         DROP  R5                                                               
         EJECT                                                                  
ADDIN    NTR1                      R6 CONTAINS THE DISPLACEMENT OF THE          
*                                  BUCKET,AMT THE AMMOUNT TO BE ADDED           
         LA    R5,TOTLINE                                                       
         LA    R5,0(R5,R6)                                                      
         AP    0(6,R5),AMT(6)                                                   
         SPACE 1                                                                
         LA    R1,TABETH                                                        
         LA    R2,5                                                             
         USING LINED,R4                                                         
ADD10    CLI   8(R1),X'FF'                                                      
         BE    EXIT                                                             
         L     R4,4(R1)                                                         
         MVI   ACTIVE,C'Y'                                                      
         LA    R4,0(R6,R4)                                                      
         AP    0(6,R4),AMT(6)                                                   
         L     R1,0(R1)                                                         
         BCT   R2,ADD10                                                         
         B     EXIT                                                             
         SPACE 4                                                                
BUCKZAP  NTR1                                                                   
         LA    R1,BUCKETS          CLEARS THE BUCKETS OF A RECORD               
         LA    R2,14                                                            
BUCK10   ZAP   0(6,R1),=P'0'                                                    
         LA    R1,L'BUCKETS(R1)                                                 
         BCT   R2,BUCK10                                                        
         B     EXIT                                                             
         SPACE 2                                                                
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD,0,RR=RB                        
         B     EXIT                                                             
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4),RR=RB                               
         B     EXIT                                                             
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',0,RR=RB                                  
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         LA    R2,20               REMOVE X'FF'S FROM KEY                       
         SPACE 1                                                                
GET10    CLI   0(R1),X'FF'                                                      
         BNE   *+8                                                              
         MVI   0(R1),X'40'                                                      
         LA    R1,1(R1)                                                         
         BCT   R2,GET10                                                         
         SPACE 1                                                                
         B     EXIT                                                             
         SPACE 1                                                                
EXIT     XIT1                                                                   
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(01,20,A),FORMAT=BI,WORK=1'                     
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=141'                                   
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
TABHEADS DS    0D                                                               
         SPACE 1                                                                
TABETH   DS    A                   ADDRESS OF NEXT TABLE DOWN                   
         DS    A                   ADDRESS OF LINE TO BE UPDATED                
         DS    CL1                 X'FF' FOR LAST TABLE NEEDED                  
         DC    X'00'                                                            
         DC    CL15'EXPENSE TYPE'                                               
ATABET   DS    A                                                                
         SPACE 1                                                                
TABLVAH  DS    A                                                                
         DS    A                                                                
         DS    CL1                                                              
         DS    CL1                 LENGTH 0-12 OF AC KEY                        
         DS    CL15                                                             
ATABLVA  DS    A                                                                
         SPACE 1                                                                
TABLVBH  DS    A                                                                
         DS    A                                                                
         DS    CL1                                                              
         DS    CL1                                                              
         DS    CL15                                                             
ATABLVB  DS    A                                                                
         SPACE 1                                                                
TABLVCH  DS    A                                                                
         DS    A                                                                
         DS    CL1                                                              
         DS    CL1                                                              
         DS    CL15                                                             
ATABLVC  DS    A                                                                
         SPACE 1                                                                
TABLVDH  DC    F'0'                                                             
         DS    A                                                                
         DC    X'FF'               NO MORE TABLES                               
         DS    CL1                                                              
         DS    CL15                                                             
ATABLVD  DS    A                                                                
         SPACE 4                                                                
TABMAX   EQU   300                                                              
         DS    0D                                                               
WRKTAB   DS    (TABMAX)CL17                                                     
         DS    0D                                                               
TABET    DS    (TABMAX)CL141                                                    
         DS    0D                                                               
TABLVA   DS    (TABMAX)CL141                                                    
         DS    0D                                                               
TABLVB   DS    (TABMAX)CL141                                                    
         DS    0D                                                               
TABLVC   DS    (TABMAX)CL141                                                    
         DS    0D                                                               
TABLVD   DS    (TABMAX)CL141                                                    
         EJECT                                                                  
LINED    DSECT                                                                  
MYLINE   DS    0CL141                                                           
ETCODE   DS    CL8                 EXPENSE A.C.KEY                              
ACCODE   DS    CL12                ACCOUNT KEY                                  
ARCID    DS    CL36                NAME OF RECORD/ACCOUNT                       
BUCKETS  DS    14PL6               MONTHLY TOTALS                               
ACTIVE   DS    CL1                 Y FOR YES,N FOR NO                           
         SPACE 2                                                                
AC88D    DSECT                                                                  
         SPACE 1                                                                
RELO     DS    F                                                                
CNT      DS    F                                                                
ATAB     DS    A                                                                
PDATES   DS    14CL4                                                            
PLIN     DS    14CL10                                                           
PERIOD   DS    CL16                                                             
SAVET    DS    CL8                                                              
PSTRT    DS    CL2                                                              
PEND     DS    CL2                                                              
AMT      DS    PL8                                                              
ELCODE   DS    CL1                                                              
SAVESW   DS    CL1                                                              
WANT     DS    CL1                                                              
LEVLET   DS    CL1                                                              
LOWLEN   DS    CL1                                                              
LOWLET   DS    CL1                                                              
LOWCNT   DS    PL4                                                              
TOTLINE  DS    CL141                                                            
OTHETLIN DS    CL141                                                            
OTHLINA  DS    CL141                                                            
OTHLINB  DS    CL141                                                            
OTHLINC  DS    CL141                                                            
OTHLIND  DS    CL141                                                            
TABLINES DS    H                                                                
         SPACE 4                                                                
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACREP8802 06/03/15'                                      
         END                                                                    
