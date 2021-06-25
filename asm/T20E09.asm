*          DATA SET T20E09     AT LEVEL 025 AS OF 05/27/83                      
*PHASE T20E09C,+0,NOAUTO                                                        
         TITLE 'EXPLODE INPUT DAY-TIME INTO QUARTER HOURS'                      
T20E09   CSECT                                                                  
         NMOD1 0,T20E09                                                         
         L     RC,0(R1)            A(WORK AREA)                                 
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         MVI   BYTE,1                                                           
         CLI   SCRNPAGE,1          FIRST TIME                                   
         BNH   *+10                 YES - USE NEW DAY-TIME LIST                 
         MVC   DYTMLST,SAVDYTM      NO - USE OLD DAY-TIME LIST                  
         MVC   SAVDYTM,DYTMLST     SAVE DAY-TIME LIST                           
         XC    DYTMLST,DYTMLST     CLEAR DAY-TIME LIST                          
         LA    R5,DYTMLST          OUTPUT DAY AND TIME                          
         LA    R6,SAVDYTM          INPUT DAY AND TIME                           
         MVC   DUB(6),0(R6)        CURRENT DAY AND TIME                         
         LA    R7,128              CURRENT DAY (SET TO X'80')                   
         LA    R8,16               LINE COUNTER                                 
EXPLODE  SRL   R7,1                GET NEXT DAY                                 
         MVI   BYTE2,0                                                          
         LTR   R7,R7               ALL DAYS PROCESSED                           
         BZ    NXTDT                YES - NEXT DAY AND TIME                     
         EX    R7,DATST            DAY REQUIRED                                 
         BO    EXPLODE1             YES - EXPLODE TIME                          
         B     EXPLODE              NO - CHECK NEXT DAY                         
DATST    TM    0(R6),0    *EXECUTED* CHECK IF DAY REQUESTED                     
EXPLODE1 OC    3(2,R6),3(R6)       ANY END TIME                                 
         BZ    CHKBRK               NO - CHECK FOR BREAK                        
         MVC   1(2,R5),DUB+1       SET DAY,TIME,ADJ MO.                         
         MVC   3(2,R5),DUB+1                                                    
         MVC   5(1,R5),DUB+5                                                    
         STC   R7,0(R5)                                                         
         TM    0(R5),B'01111100'   MONDAY THRU FRIDAY                           
         BZ    NXTDOT                                                           
         CLC   1(2,R5),=H'600'                                                  
         BL    *+18                                                             
         CLC   1(2,R5),=H'1700'    BEFORE 5PM                                   
         BNL   *+8                 NO - USE DAY                                 
         MVI   0(R5),B'01111100'    YES - USE M-F AVERAGE                       
NXTDOT   MVC   DUB2(2),DUB+1                                                    
         LH    RF,DUB2             ADD 15 MINUTES                               
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         CH    RE,=H'44'                                                        
         BNH   *+12                                                             
         LA    RE,55                                                            
         B     *+8                                                              
         LA    RE,15                                                            
         AH    RE,DUB2                                                          
         CH    RE,=H'2400'                                                      
         BNH   *+8                                                              
         SH    RE,=H'2400'                                                      
         STH   RE,DUB2                                                          
         MVC   DUB+1(2),DUB2                                                    
         CLC   DUB+3(2),=H'600'                                                 
         BNL   *+14                                                             
         CLC   DUB+1(2),=H'600'                                                 
         BNL   NXTTIM                                                           
         CLC   DUB+3(2),DUB+1      END TIME LT START TIME                       
         BH    NXTTIM               NO - CONTINUE                               
NXTDAY   MVC   DUB(6),0(R6)         YES - NEXT DAY                              
         LA    R5,6(R5)            NEXT OUTPUT SLOT                             
         MVI   BYTE2,1                                                          
         BCT   R8,EXPLODE                                                       
         B     CHKPAG                                                           
*                                                                               
NXTTIM   LA    R5,6(R5)            NEXT QUARTER HOUR                            
         BCT   R8,EXPLODE1                                                      
         B     CHKPAG                                                           
         EJECT                                                                  
*        START TIME ONLY - CHECK FOR 2 MINUTE LEEWAY                            
*                                                                               
CHKBRK   MVC   1(2,R5),DUB+1       SET DAY,TIME,ADJ MO.                         
         MVC   3(2,R5),DUB+1                                                    
         MVC   5(1,R5),DUB+5                                                    
         STC   R7,0(R5)                                                         
         TM    0(R5),B'01111100'                                                
         BZ    CHKBRK1                                                          
         CLC   1(2,R5),=H'600'                                                  
         BL    *+18                                                             
         CLC   1(2,R5),=H'1700'                                                 
         BNL   *+8                                                              
         MVI   0(R5),B'01111100'                                                
CHKBRK1  LA    R5,6(R5)                                                         
         BCT   R8,*+8                                                           
         B     CHKPAG              END OF PAGE                                  
         MVC   DUB2(2),1(R6)                                                    
         LH    RF,DUB2                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         SRDA  RE,32                                                            
         D     RE,=F'15'                                                        
         CH    RE,=H'3'                                                         
         BL    SUB1                                                             
         CH    RE,=H'12'                                                        
         BH    ADD1                                                             
         B     EXPLODE             EXCESS LEEWAY - NEXT DAY                     
*                                                                               
ADD1     LH    RF,DUB2             ADD 15 MINUTES                               
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         CH    RE,=H'44'                                                        
         BNH   *+12                                                             
         LA    RE,55                                                            
         B     *+8                                                              
         LA    RE,15                                                            
         AH    RE,DUB2                                                          
         CH    RE,=H'2400'                                                      
         BNH   *+8                                                              
         SH    RE,=H'2400'                                                      
         STH   RE,DUB2                                                          
         B     SCNDQH                                                           
*                                                                               
SUB1     LH    RF,DUB2             SUBTRACT 15 MINUTES                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         CH    RE,=H'15'                                                        
         BL    *+12                                                             
         LA    RE,15                                                            
         B     *+8                                                              
         LA    RE,55                                                            
         LH    RF,DUB2                                                          
         CH    RF,=H'3'                                                         
         BH    *+8                                                              
         LA    RF,2400(RF)                                                      
         SR    RF,RE                                                            
         STH   RF,DUB2                                                          
*                                                                               
*        DO SECOND QTR HOUR FOR BREAKS                                          
*                                                                               
SCNDQH   MVC   1(2,R5),DUB2                                                     
         MVC   3(2,R5),DUB2                                                     
         STC   R7,0(R5)                                                         
         TM    0(R5),B'01111100'                                                
         BZ    SEQDT                                                            
         CLC   1(2,R5),=H'600'                                                  
         BL    *+18                                                             
         CLC   1(2,R5),=H'1700'                                                 
         BNL   *+8                                                              
         MVI   0(R5),B'01111100'                                                
SEQDT    SH    R5,=H'6'                                                         
         CLC   1(2,R5),7(R5)       SORT OUTPUT FOR BREAKS                       
         BL    SEQDT2                                                           
         MVC   DUB2(6),6(R5)                                                    
         MVC   6(6,R5),0(R5)                                                    
         MVC   0(6,R5),DUB2                                                     
SEQDT2   LA    R5,6(R5)                                                         
         B     NXTDAY                                                           
*                                                                               
*        OUTPUT REQUESTED DAY AND TIME - GET NEXT DAY AND TIME                  
*                                                                               
NXTDT    LA    R7,128              RESET DAY COUNTER                            
         CLI   SAVDYTM+6,0                                                      
         BNE   *+14                                                             
         MVC   0(6,R5),0(R6)                                                    
         LA    R5,6(R5)                                                         
         LA    R6,6(R6)                                                         
         MVC   DUB(6),0(R6)                                                     
         CLI   0(R6),0                                                          
         BE    *+8                                                              
         BCT   R8,EXPLODE                                                       
         LA    R2,DEMPAGH                                                       
         MVC   FLDDATA(2),=C'01'                                                
         MVI   PREVPAG,0                                                        
         FOUT  (R2)                                                             
         B     EXXMOD                                                           
*        CHECK TO SEE IF CORRECT PAGE HAS BEEN DONE                             
*                                                                               
CHKPAG   SR    RE,RE               BUMP TO NEXT PAGE                            
         IC    RE,SCRNPAGE                                                      
         MVC   WORK(8),DUB                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         ST    R2,DMWORK                                                        
         LA    R2,DEMPAGH                                                       
         UNPK  FLDDATA(2),DUB+6(2)                                              
         FOUT  (R2)                                                             
         L     R2,DMWORK                                                        
         MVC   DUB,WORK                                                         
         CLI   SCRNPAGE,1                                                       
         BNH   EXXMOD                                                           
         CLC   SCRNPAGE,BYTE       CORRECT PAGE                                 
         BE    EXXMOD               YES - EXIT                                  
         XC    DYTMLST,DYTMLST      NO - GET NEXT PAGE                          
         LA    R5,DYTMLST                                                       
         LA    R8,16                                                            
         SR    RE,RE                                                            
         IC    RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLI   BYTE2,1             NEXT DAY                                     
         BE    EXPLODE              YES                                         
         B     EXPLODE1                                                         
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
         LTORG                                                                  
       ++INCLUDE GENOLD                                                         
       ++INCLUDE T20EWORK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025T20E09    05/27/83'                                      
         END                                                                    
