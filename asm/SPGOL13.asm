*          DATA SET SPGOL13    AT LEVEL 011 AS OF 12/18/17                      
*PHASE T20213B,+0,NOAUTO                                                        
         TITLE 'T20213 - SPOTPAK GOALS - LOCKIN ADD/DELETE'                     
T20213   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20213                                                         
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
         EJECT                                                                  
         LA    R2,GOLPRH                                                        
         MVI   ERRCD,INVERR                                                     
         CLI   SVPRD2,0            NO PIGGYBACKS ALLOWED                        
         BNE   GLERR                                                            
*                                                                               
         XC    BDATA,BDATA                                                      
         LA    R2,GOLACT1H                                                      
         XC    GKEY,GKEY                                                        
*                                                                               
LK2      ST    R2,BLNADDR                                                       
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         MVC   BACT,9(R2)          SAVE M OR W                                  
         CLC   =C'AW',8(R2)                                                     
         BE    LKMKT                                                            
         CLC   =C'AM',8(R2)                                                     
         BE    LKMKT                                                            
         CLI   8(R2),C'*'                                                       
         BE    *+12                                                             
         MVI   ERRCD,INVERR                                                     
         B     GLERR                                                            
* MAKE SURE NO CHANGED FIELDS ON * LINE                                         
         MVI   ERRCD,CHDTAERR                                                   
         LR    R4,R2                                                            
         SR    R5,R5                                                            
         LA    R0,7                                                             
         B     LK4B                                                             
*                                                                               
LK4A     TM    1(R4),X'20'         TEST PROTECTED                               
         BO    LK4B                                                             
         TM    4(R4),X'20'         TEST VALIDATED                               
         BZ    GLERR                                                            
*                                                                               
LK4B     IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         BCT   R0,LK4A                                                          
*                                                                               
         B     LKMKT                                                            
         EJECT                                                                  
LKMKT    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   LKMKT2                                                           
         MVI   ERRCD,MSSNGERR                                                   
         OC    BMKT,BMKT           DID WE HAVE A MARKET ABOVE                   
         BZ    GLERR                                                            
* CLEAR MKTNAME DATA (IF ANY)                                                   
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R4,0(R5,R2)                                                      
         OC    8(16,R4),8(R4)                                                   
         BZ    LKMKTX                                                           
         XC    8(16,R4),8(R4)                                                   
         FOUT  (R4)                                                             
         B     LKMKTX                                                           
*                                                                               
LKMKT2   GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    LKMKTX                                                           
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R4,R2                                                            
         FOUT  (R4),SVMKTNAM,15                                                 
*                                                                               
LKMKTX   OI    4(R2),X'20'                                                      
         EJECT                                                                  
LKDPT    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         IC    R0,0(R2)            SKIP MKT NAME                                
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   LKDPT2                                                           
         CLI   BDPT,0                                                           
         BNZ   LKDPTX                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
LKDPT2   GOTO1 USER2               'EDTDPTLN'                                   
*                                                                               
         MVI   ERRCD,SLNERR                                                     
         CLI   BSLN,0                                                           
         BE    GLERR                                                            
*                                                                               
LKDPTX   OI    4(R2),X'20'                                                      
         EJECT                                                                  
LKDOLS   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         MVI   ERRCD,MSSNGERR                                                   
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LTR   R5,R5                                                            
         BZ    GLERR                                                            
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R5)                                     
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
*                                                                               
         L     R0,4(R1)                                                         
         SRDA  R0,32                                                            
         LA    RE,10               WEEKLY IS IN DIMES                           
         CLI   BACT,C'M'           TEST MONTHLY                                 
         BNE   *+8                                                              
         LA    RE,100              MONTHLY IS IN DOLLARS                        
         DR    R0,RE                                                            
         ST    R1,BDOLS                                                         
*                                                                               
LKDOLX   OI    4(R2),X'20'                                                      
         EJECT                                                                  
* EDIT SPOTS/POINTS                                                             
* FIELD SEPARATOR IS C'/'                                                       
*                                                                               
LKSPTS   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R4,8(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         STM   R4,R5,WORK                                                       
LKSPT2A  CLI   0(R4),C'/'                                                       
         BE    LKSPT2B                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,LKSPT2A                                                       
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
LKSPT2B  L     R1,WORK             DATA START                                   
         LR    R0,R1                                                            
         SR    R0,R4               START-END                                    
         LPR   R0,R0               GIVES LEN                                    
         BAS   RE,TSTNUM           MAKE SURE NUMERIC                            
*                                                                               
         L     R1,WORK             DATA START                                   
         L     RE,WORK+4           ORIGINAL LEN                                 
         SR    RE,R5               GIVES DATA LEN                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1) ** EXECUTED **                                       
         CVB   R0,DUB                                                           
         STH   R0,HALF2            SAVE SPOTS                                   
         EJECT                                                                  
* EDIT POINTS                                                                   
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         MVI   ERRCD,MSSNGERR                                                   
         LTR   R5,R5                                                            
         BZ    GLERR                                                            
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
*                                                                               
         L     R0,4(R1)                                                         
         SRDA  R0,32                                                            
         LA    RE,10               WEEKLY POINTS ARE IN TENTHS                  
         CLI   BACT,C'M'           TEST MONTHLY                                 
         BNE   *+8                                                              
         LA    RE,100              MONTHLY ARE IN DOLLARS                       
         DR    R0,RE                                                            
         ST    R1,BPTS                                                          
*                                                                               
*                                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
LKPER    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         MVI   ERRCD,INVERR                                                     
*                                                                               
         GOTO1 USER3               'EDTPER'                                     
*                                                                               
         MVI   ERRCD,INVERR                                                     
         OC    BWEEKS+2(2),BWEEKS+2  TEST ONLY 1 WEEK                           
         BNZ   GLERR                 MORE IS ERROR                              
*                                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
         CLI   BACT,C'*'                                                        
         BE    LKNEXT                                                           
*                                                                               
* BUILD RECORD KEY                                                              
*                                                                               
LKKEY    LA    RE,GOALREC                                                       
         ST    RE,AREC                                                          
*                                                                               
         XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SVAGYMD                                                   
         MVC   GKEYCLT,SVCLT                                                    
         MVC   GKEYPRD,SVPRD                                                    
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,SVEST                                                    
         MVC   GKEYDPT,BDPT                                                     
         MVC   GKEYSLN,BSLN                                                     
         MVC   GKEYSEC,BSLN                                                     
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
         MVC   KEY(13),GKEY                                                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LKADD2                                                           
*                                                                               
LKADD1   GOTO1 GETREC                                                           
*                                                                               
         B     LKADD4                                                           
         EJECT                                                                  
* BUILD NEW RECORD                                                              
*                                                                               
LKADD2   LA    R0,14                                                            
         LA    R1,GOALREC                                                       
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVC   GKEY(13),KEYSAVE                                                 
         MVC   GLENGTH,=H'100'                                                  
         MVC   GAGYALPH,AGYALPHA                                                
         MVC   GDELEM(2),=X'204C'                                               
         MVC   GBUYNAME(12),=C'T202 LOCK-IN'                                    
         SR    R0,R0                                                            
         IC    R0,SVADVAGY                                                      
         SRL   R0,4                                                             
         STC   R0,GADVAGY                                                       
*                                                                               
LKADD4   XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'3010'                                                 
         CLI   BACT,C'W'           TEST WEEKLY                                  
         BNE   *+8                                                              
         MVI   ELEM,X'31'                                                       
         MVC   ELEM+2(2),BWEEKS                                                 
         MVC   ELEM+4(1),SVPRD                                                  
         MVC   ELEM+5(1),BSLN                                                   
         MVC   ELEM+6(2),HALF2     SPOTS                                        
         MVC   ELEM+8(4),BDOLS                                                  
         MVC   ELEM+12(4),BPTS                                                  
         MVC   ELEM+12(1),SVDEMOS                                               
         CLI   SVNEWDEM,C'Y'                                                    
         BNE   LKADD6                                                           
         MVI   ELEM+1,19                                                        
         MVC   ELEM+12(3),SVDEMOS                                               
         MVC   ELEM+15(4),BPTS                                                  
*                                                                               
LKADD6   LA    R6,GDELEM                                                        
         MVC   ELCODE,ELEM                                                      
*                                                                               
LKADD8   BAS   RE,NEXTEL                                                        
         BNE   LKADD10                                                          
         CLC   2(2,R6),BWEEKS                                                   
         BL    LKADD8                                                           
         BH    LKADD10                                                          
* DELETE EXISTING ELEMENT                                                       
         GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
         EJECT                                                                  
* ADD NEW ELEM                                                                  
LKADD10  GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
*                                                                               
         EJECT                                                                  
         TM    GCNTRLS,X'80'       TEST REC IS DELETED                          
         BZ    LKADD30             NO                                           
*                                                                               
         NI    GCNTRLS,X'7F'       RESET DELETE                                 
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
* UNSET DIRECTORY CONTROL BIT TOO                                               
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
*                                                                               
         GOTO1 DIR                                                              
*                                                                               
         CLI   SVADVAGY,0          TEST ADVTSR CLIENT                           
         BZ    LKNEXT              NO                                           
         SR    R0,R0                                                            
         IC    R0,KEY+1                                                         
         SRL   R0,4                                                             
         STC   R0,KEY+11                                                        
         NI    KEY+1,X'0F'                                                      
         OC    KEY+1(1),SVADVAGY                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
*                                                                               
         GOTO1 DIR                                                              
*                                                                               
         B     LKNEXT                                                           
         EJECT                                                                  
LKADD30  CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BE    LKADD40                                                          
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         B     LKNEXT                                                           
*                                                                               
LKADD40  GOTO1 PUTREC                                                           
*                                                                               
         B     LKNEXT                                                           
         EJECT                                                                  
LKNEXT   L     R4,BLNADDR                                                       
         MVI   8(R4),C'*'          INDICATE ACTION COMPLETED                    
         FOUT  (R4)                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
* CHECK FOR EMPTY LINE                                                          
         LA    RE,7                                                             
         LR    R4,R2                                                            
*                                                                               
LKN2     TM    1(R4),X'20'         TEST PROTECTED                               
         BO    LKN4                                                             
         CLI   5(R4),0                                                          
         BNE   LK2                                                              
*                                                                               
LKN4     IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   RE,LKN2                                                          
*                                                                               
* CLEAR PROTECTED FIELDS ON REMAINING LINES                                     
*                                                                               
LKX2     TM    1(R2),X'20'                                                      
         BZ    LKX4                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2) ** EXECUTED **                                     
         BZ    LKX4                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2) ** EXECUTED **                                     
         FOUT  (R2)                                                             
*                                                                               
LKX4     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   LKX2                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
TSTNUM   MVI   ERRCD,INVERR                                                     
TSTNUM2  CLI   0(R1),C'0'                                                       
         BL    GLERR                                                            
         CLI   0(R1),C'9'                                                       
         BH    GLERR                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,TSTNUM2                                                       
         BR    RE                                                               
*                                                                               
GLERR    GOTO1 ERROR                                                            
*                                                                               
EXXMOD   XMOD1 1                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGOLWRK                                                       
 END                                                                            
