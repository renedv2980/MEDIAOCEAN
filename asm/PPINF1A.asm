*          DATA SET PPINF1A    AT LEVEL 022 AS OF 05/01/02                      
*PHASE T41A1AA,+0,NOAUTO       NOTE: "A" PHASE                                  
*                                                                               
* KWAN 05/99    CORRECT FILTER ERROR DISPLAY                                    
*                                                                               
         TITLE 'T41A1A   PRINTPAK  INFO   DIVISIONS'                            
T41A1A   CSECT                                                                  
         NMOD1 0,T41A1A                                                         
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
*                                                                               
*                                                                               
*                                                                               
         CLI   SINIFLTH+5,0        FILTER IS NOT NEEDED                         
         BE    *+16                                                             
         LA    R2,SINIFLTH                                                      
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
         LA    R2,SINHDRH          BUILD HEADLINES                              
         MVC   FLDDATA+1(18),=C'DIVISION CODE/NAME'                             
         MVC   FLDDATA+27(18),FLDDATA+1                                         
         MVC   FLDDATA+53(18),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(18),DASH                                               
         MVC   FLDDATA+27(18),DASH                                              
         MVC   FLDDATA+53(18),DASH                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*                                                                               
         LA    RE,REC2             CLEAR SAVE AREA                              
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    R6,REC2                                                          
         LA    R7,42                                                            
         LA    R5,KEY              BUILD FIRST KEY                              
         USING DIVHDRD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         OC    PREVKEY,PREVKEY     FIRST TIME                                   
         BZ    *+10                                                             
         MVC   KEY,PREVKEY          NO - RESTORE PREV KEY                       
         XC    PREVKEY,PREVKEY                                                  
RHI      BAS   RE,HIGH                                                          
         B     HAVREC                                                           
*                                                                               
RSEQ     BAS   RE,SEQ                                                           
HAVREC   LA    R5,KEY                                                           
         CLC   KEY(7),KEYSAVE                                                   
         BNE   REND                                                             
         BAS   RE,GETREC                                                        
         L     R5,AREC             BUILD TABLE FOR ALPHA LIST                   
         MVC   0(3,R6),PDIVKDIV                                                 
         MVC   3(20,R6),PDIVNAME                                                
         LA    R6,23(R6)                                                        
         BCT   R7,RSEQ                                                          
         MVC   PREVKEY,KEY         SAVE KEY FOR NEXT READ                       
*                                                                               
         MVI   PDIVKDIV+3,X'FF'                                                 
*                                                                               
* END OF PRODUCT HEADERS SO FORMAT SCREEN                                       
REND     CLI   REC2,0              ANY DATA                                     
         BNE   FORMAT                                                           
         B     FRMTEND                                                          
*                                                                               
FORMAT   XC    DMWORK(20),DMWORK                                                
         LA    R6,REC2                                                          
         LA    R7,0                                                             
FORMAT1  CLI   0(R6),0             COUNT NUMBER OF ENTRIES                      
         BE    FORMAT2                                                          
         LA    R6,23(R6)                                                        
         LA    R7,1(R7)                                                         
         B     FORMAT1                                                          
*                                                                               
FORMAT2  GOTO1 FRMTALPH,DMCB,(23,REC2),(R7),14,(3,DMWORK)                       
FORMAT3  LA    R6,DMWORK                                                        
         LA    RF,FLDDATA+1                                                     
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         MVC   0(3,RF),0(R7)       MOVE DATA TO SCREEN LINE                     
         MVI   3(RF),C'/'                                                       
         MVC   4(20,RF),3(R7)                                                   
         SR    RE,RE               DECREMENT COUNT                              
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,23(R5)                                                        
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT COLUMN                                  
         LA    RF,26(RF)                                                        
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3                                                          
*                                                                               
FRMTEND  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT  OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
DASH     DC    40C'-'                                                           
LINLEN   EQU   88                                                               
NOFNDERR EQU   53                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENEROL                                                      
*                                                                               
DIVHDRD  DSECT                                                                  
       ++INCLUDE PDIVREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSINFOWRK                                                     
