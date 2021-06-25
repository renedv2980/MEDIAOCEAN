*          DATA SET T20E01     AT LEVEL 021 AS OF 05/01/02                      
*PHASE T20E01C,+0,NOAUTO                                                        
*INCLUDE ALPHALST                                                               
         TITLE 'DEMO DESCRIPTION AND EFFECTIVE DATE MODULE'                     
T20E01   CSECT                                                                  
         NMOD1 0,T20E01,RR=R8                                                   
LINLEN   EQU   88                                                               
DTABLEN  EQU   13                                                               
         L     RC,0(R1)            WORK AREA                                    
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         ST    R8,RELO                                                          
         B     ST                                                               
RELO     DC    A(0)                                                             
ST       CLC   SCRNPAGE,PREVPAG    SAME PAGE AS PREVIOUS                        
         BNE   HAVPAG               NO - CONTINUE                               
         SR    R7,R7                YES - BUMP PAGE BY ONE                      
         IC    R7,SCRNPAGE                                                      
         LA    R7,1(R7)                                                         
         STC   R7,SCRNPAGE                                                      
         STC   R7,PREVPAG                                                       
HAVPAG   L     R4,ADEMTAB                                                       
         LA    R2,DMCHDCHH                                                      
         MVC   FLDDATA(27),=C'DEMO CODES AND DESCRIPTIONS'                      
         FOUT  (R2)                                                             
         LA    R2,58(R2)           NEXT LINE                                    
         LA    R9,8(R2)                                                         
         MVC   0(23,R9),=C'DEM   DEMO  START   END'                             
         MVC   25(23,R9),0(R9)                                                  
         MVC   50(23,R9),0(R9)                                                  
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R9,8(R2)                                                         
         MVC   0(23,R9),=C'NO.   NAME   BOOK  BOOK'                             
         MVC   25(23,R9),0(R9)                                                  
         MVC   50(23,R9),0(R9)                                                  
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R9,8(R2)                                                         
         MVC   0(23,R9),=C'--- ------- ----- -----'                             
         MVC   25(23,R9),0(R9)                                                  
         MVC   50(23,R9),0(R9)                                                  
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    RE,WORKEND          CLEAR WORK AREA FOR UP TO 100                
         LA    RF,2300              DEMO CODES AND NAMES                        
         XCEF                                                                   
         LA    RF,WORKEND          SET LINE START                               
         L     RE,ADEMTAB          SET DEMO TABLE START                         
         LA    R6,44               SET NO. OF DEMOS ON A PAGE                   
         SR    R7,R7                                                            
         EJECT                                                                  
*        BUILD A LIST OF DEMOS IN FORMAT WHICH CAN BE HANDLED BY                
*          ALPHALST                                                             
*                                                                               
BLDDEM   CLI   0(RE),X'FF'         END OF TABLE BUILD                           
         BE    ENDBLD               YES - FIND PAGE                             
         TM    1(RE),4             NETWORK ONLY                                 
         BO    NXTDEM1                                                          
         IC    R7,CSOURCE           NO - CONTINUE BUILD                         
         EX    R7,*+12             VALID FOR THIS SOURCE                        
         BZ    NXTDEM1              NO - GET NEXT ENTRY                         
         B     *+8                                                              
         TM    1(RE),0  * EXECUTED * CHECK RATING SOURCE                        
         NI    1(RE),X'7F'                                                      
         IC    R7,0(RE)                                                         
         CVD   R7,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,RF),DUB+6(2)    MOVE DEMO NUMBER                             
         MVC   4(7,RF),6(RE)       MOVE DEMO NAME                               
         MVC   HALF,2(RE)          GET PROPER START-END DATES                   
         CLI   CSOURCE,2                                                        
         BNE   *+10                                                             
         MVC   HALF,4(RE)                                                       
         CLI   HALF,0                                                           
         BE    CHKEND              NO SPECIFIC START BOOK -CHECK END            
         BAS   R9,CVTBOOK          CONVERT BOOK TO ALPHA                        
         MVC   12(5,RF),WORK                                                    
CHKEND   CLI   HALF+1,X'FF'        NO SPECIFIC END BOOK - NEXT DEMO             
         BE    NXTDEM                                                           
         MVC   HALF(1),HALF+1                                                   
         BAS   R9,CVTBOOK                                                       
         MVC   18(5,RF),WORK                                                    
NXTDEM   LA    RE,DTABLEN(RE)      BUMP TABLES TO NEXT ENTRY                    
         LA    RF,23(RF)                                                        
         BCT   R6,BLDDEM                                                        
         CLI   0(RE),X'FF'         END OF DEMO TABLE                            
         BE    ENDBLD               YES - FIND PAGE                             
         MVC   0(23,RF),=C'LIST CONT. ON NEXT PAGE'                             
         LA    R6,44                                                            
         LA    RF,23(RF)                                                        
         B     BLDDEM                                                           
NXTDEM1  LA    RE,DTABLEN(RE)      GET NEXT DEMO ENTRY                          
         B     BLDDEM                                                           
         EJECT                                                                  
*                                                                               
*        CONVERT BOOK FROM BINARY YM TO ALPHA MMMYY                             
*                                                                               
CVTBOOK  IC    R7,HALF                                                          
         SRL   R7,4                                                             
         AH    R7,=H'70'                                                        
         CVD   R7,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB+6(2)                                               
         NI    HALF,X'0F'                                                       
         IC    R7,HALF                                                          
         MH    R7,=H'3'                                                         
         LA    R8,MONTAB-3(R7)                                                  
         MVC   WORK(3),0(R8)                                                    
         BR    R9                                                               
MONTAB   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         EJECT                                                                  
*                                                                               
*        GET START OF REQUESTED PAGE                                            
*                                                                               
ENDBLD   SR    R7,R7                                                            
         LA    RE,WORKEND                                                       
         IC    R7,SCRNPAGE                                                      
         BCTR  R7,R0                                                            
         LTR   R7,R7                                                            
         BZ    PAGE1                                                            
         MH    R7,=H'45'           BUMP BY NUMBER ON PAGE                       
         MH    R7,=H'23'           BUMP BY FIELD LENGTH                         
         AR    RE,R7                                                            
         CLI   0(RE),0             ANY DATA FOR THIS PAGE                       
         BNE   *+14                 YES - PROCESS                               
         LA    RE,WORKEND           NO - RESET TO PAGE 1                        
         MVC   SCRNPAGE(2),=X'0101'                                             
PAGE1    LR    R7,RE                                                            
         MVC   HALF,=H'01'                                                      
         MVC   HALF2,=H'16'                                                     
         XC    IOAREA(20),IOAREA                                                
NEXTLIN  XC    DATALIN,DATALIN                                                  
         GOTO1 =V(ALPHALST),DMCB,(X'03',(R7)),(X'17',HALF),            X        
               (X'02',HALF2),IOAREA,DATALIN,RR=RELO                             
         LA    R9,8(R2)                                                         
         MVC   0(L'DMCDTC0,R9),DATALIN                                          
         LA    R1,DMCB                                                          
         USING ALPARA,R1                                                        
         OC    ALOUT,ALOUT                                                      
         BZ    END                                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         CLI   ALNOLFT,0                                                        
         BNE   NEXTLIN                                                          
         DROP  R1                                                               
END      LA    R2,DEMPAGH          SEND PAGE NUMBER                             
         SR    R7,R7                                                            
         IC    R7,SCRNPAGE                                                      
         CVD   R7,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FLDDATA(2),DUB+6(2)                                              
         FOUT  (R2)                SEND PAGE NUMBER                             
         OI    6(R2),OI1C          INSERT CURSOR                                
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
ALPARA   DSECT                                                                  
ALCOL    DS    CL1                 ALPHALST PARAMETER DSECT                     
ALTABS   DS    CL3                                                              
ALILEN   DS    CL1                                                              
ALLCNT   DS    CL3                                                              
ALOSPC   DS    CL1                                                              
ALMAX    DS    CL3                                                              
ALWORK   DS    CL4                                                              
ALNOLFT  DS    CL1                                                              
ALOUT    DS    CL3                                                              
       ++INCLUDE GENOLD                                                         
       ++INCLUDE T20EWORK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021T20E01    05/01/02'                                      
         END                                                                    
