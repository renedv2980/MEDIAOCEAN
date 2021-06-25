*          DATA SET SPGOL12    AT LEVEL 012 AS OF 12/18/17                      
*PHASE T20212B,+0                                                               
         TITLE 'T20212 - SPOTPAK GOALS - LOCKIN RECALL'                         
T20212   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20212                                                         
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
* NO LOCKIN RECALL UNDER PIGGYBACKS *                                           
         LA    R2,GOLPRH                                                        
         MVI   ERRCD,INVERR                                                     
         CLI   SVPRD2,0                                                         
         BNE   GLERR                                                            
*                                                                               
         XC    BDATA,BDATA                                                      
         LA    R2,GOLACT1H                                                      
*                                                                               
         CLI   8(R2),C'R'                                                       
         BE    RCLEDT                                                           
         MVI   ERRCD,INVERR                                                     
         B     GLERR                                                            
*                                                                               
RCLEDT   MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLMKT1H                                                      
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLDPT1H                                                      
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         GOTO1 USER2               'EDTDPTLN'                                   
*                                                                               
         MVI   ERRCD,MSSNGERR                                                   
         CLI   BDPT,0                                                           
         BE    GLERR                                                            
         CLI   BSLN,0                                                           
         BE    GLERR                                                            
*                                                                               
         LA    R2,GOLPER1H                                                      
         GOTO1 USER3               'EDTPER'                                     
*                                                                               
         OC    GOLMKT1,SPACES                                                   
         FOUT  GOLMKT0H,GOLMKT1,4                                               
*                                                                               
         OC    GOLDPT1,SPACES                                                   
         FOUT  GOLDPT0H,GOLDPT1,4                                               
*                                                                               
         OC    GOLPER1,SPACES                                                   
         FOUT  GOLPER0H,GOLPER1,13                                              
*                                                                               
         EJECT                                                                  
         XC    GOLDOL1,GOLDOL1                                                  
         FOUT  GOLDOL1H                                                         
*                                                                               
         XC    GOLPTS1,GOLPTS1                                                  
         FOUT  GOLPTS1H                                                         
*                                                                               
* CLEAR LINES 2-14                                                              
*                                                                               
         LA    R4,GOLPER1H                                                      
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
*                                                                               
RCLEDT8  IC    R5,0(R4)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,RCLEDTOC                                                      
         BZ    RCLEDT10                                                         
         EX    R5,RCLEDTXC                                                      
         FOUT  (R4)                                                             
*                                                                               
RCLEDT10 LA    R4,9(R5,R4)                                                      
         CLI   0(R4),0                                                          
         BNE   RCLEDT8                                                          
*                                                                               
         LA    R2,GOLMKT1H                                                      
         TM    4(R2),X'20'                                                      
         BO    RCLEDT12                                                         
         FOUT  GOLNAM1H,SVMKTNAM,15                                             
*                                                                               
         OI    4(R2),X'20'                                                      
         B     RCLEDT12                                                         
*                                                                               
RCLEDTOC OC    8(0,R4),8(R4)                                                    
RCLEDTXC XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
RCLEDT12 DS    0H                                                               
         MVI   FRSTSW,0                                                         
         LA    R2,GOLACT1H                                                      
         SPACE 1                                                                
*************************************************************                   
* BUILD GOAL KEY *                                                              
* NOTE THAT LOCKIN DATA NEVER RESIDES IN PIGGYBACK RECORDS, *                   
* ONLY IN SOLO RECORDS FOR EACH BRAND                       *                   
*************************************************************                   
         SPACE 1                                                                
         XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SVAGYMD                                                   
         MVC   GKEYCLT,SVCLT                                                    
         MVC   GKEYPRD,SVPRD                                                    
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,SVEST                                                    
         MVC   GKEYDPT,BDPT                                                     
         MVC   GKEYSLN,BSLN                                                     
         MVC   GKEYSEC,BTLN        WILL ALWAYS = BSLN                           
*                                                                               
         MVC   KEY,GKEY                                                         
*                                                                               
         GOTO1 READ                                                             
*                                                                               
         LA    RE,GOALREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         LA    R8,BWEEKS           POINT TO WEEK LIST                           
         EJECT                                                                  
RLK      LA    R6,GDELEM                                                        
         XC    KEYSAVE,KEYSAVE                                                  
RLK2     SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXXMOD                                                           
         CLI   0(R6),X'30'                                                      
         BE    RLK4                                                             
         CLI   0(R6),X'31'                                                      
         BE    RLK4                                                             
         B     RLK2                                                             
RLK4     OC    0(2,R8),0(R8)       TEST END OF WEEK LIST                        
         BZ    RLK2                                                             
         CLC   2(2,R6),0(R8)       THIS ELEM TO WEEK LIST                       
         BL    RLK2                                                             
         BE    RLK6                                                             
         LA    R8,2(R8)                                                         
         B     RLK4                                                             
*                                                                               
RLK6     BAS   RE,FMT                                                           
*                                                                               
RLKX     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   RLK2                                                             
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      NTR1                                                                   
         SR    R0,R0                                                            
         ST    R2,BLNADDR          SAVE LINE START ADDRESS                      
*                                                                               
         MVC   8(3,R2),=C'*M '     INDICATE ELEMENT TYPE                        
         CLI   0(R6),X'30'         TEST MONTHLY                                 
         BE    *+8                                                              
         MVI   9(R2),C'W'                                                       
*                                                                               
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         OI    4(R2),X'20'         MARKET                                       
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               MARKET NAME (SKIP)                           
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         OI    4(R2),X'20'         DPT/LEN                                      
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         L     R0,8(R6)            DOLLARS                                      
         MH    R0,=H'10'           WEEKLY X 10                                  
         CLI   0(R6),X'30'                                                      
         BNE   *+8                                                              
         MH    R0,=H'10'           MONTHLY X 100                                
         EDIT  (R0),(11,WORK2),2,ALIGN=LEFT,ZERO=NOBLANK                        
         MVC   8(11,R2),WORK2      MOVE DATA AND BLANKS                         
*                                                                               
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   WORK2,SPACES                                                     
         LH    R0,6(R6)            GET SPOTS                                    
         LA    R4,WORK2                                                         
         EDIT  (R0),(4,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
*                                                                               
         AR    R4,R0                                                            
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         L     R0,12(R6)           POINTS                                       
         SLL   R0,8                                                             
         SRL   R0,8                                                             
         CLI   SVNEWDEM,C'Y'                                                    
         BNE   *+8                                                              
         ICM   R0,7,16(R6)                                                      
*                                                                               
         CLI   0(R6),X'31'                                                      
         BNE   FMT20                                                            
* NEED A DECIMAL PLACE FOR WEEKLY                                               
         EDIT  (R0),(6,(R4)),1,ALIGN=LEFT,ZERO=NOBLANK                          
         B     FMT22                                                            
         EJECT                                                                  
*                                                                               
FMT20    EDIT  (R0),(6,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
*                                                                               
FMT22    MVC   8(11,R2),WORK2                                                   
         OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,WORK2)                                 
*                                                                               
         MVC   13(3,R2),=C'-1W'                                                 
         MVC   8(5,R2),WORK2                                                    
         OC    8(13,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
GLERR    GOTO1 ERROR                                                            
         PRINT OFF                                                              
       ++INCLUDE SPGOLWRK                                                       
 END                                                                            
