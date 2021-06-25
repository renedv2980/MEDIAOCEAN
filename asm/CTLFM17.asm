*          DATA SET CTLFM17    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA0217A                                                                  
         TITLE 'CTLFM17 - CONTROL FILE MAINT - MARKET AUTH RECORDS'             
CTLFM17  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFMN**                                                       
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTKREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              VALIDATE KEY FIELDS & READ RECORD                                
*                                                                               
KEYVAL   XC    CTKKEY,CTKKEY                                                    
         MVI   CTKKTYP,C'K'                                                     
*                                  GET AGENCY ID FROM UTL                       
         L     RE,AUTL                                                          
         USING UTLD,RE                                                          
         MVC   CTKKUSER,TAGY                                                    
         DROP  RE                                                               
*                                  VALIDATE MARKET NUMBER                       
         LA    R1,MKTMNUMH                                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         TM    FLDH+4,X'08'        TEST NUMERIC                                 
         BZ    EFNN                                                             
         OC    FLDH(4),FLDH                                                     
         BZ    EIIF                                                             
         OC    FLDH(2),FLDH                                                     
         BNZ   EIIF                                                             
         MVC   CTKKMKT,FLDH+2                                                   
*                                                                               
         MVC   KEY,CTKKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   KEYV2               SET ACTN=DISPLAY                             
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
KEYV2    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET READ-FOR-UPDATE INDIC.                   
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
         TM    DMCB+8,X'10'        TEST RECORD N/F                              
         BZ    KEYV4                                                            
         CLI   ACTN,ADD            ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV4    CLI   ACTN,ADD                                                         
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        TEST RECORD DELETED                          
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        ONLY VALID FOR DISPLAY                       
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY MARKET AUTH RECORD                                       
*                                                                               
DISPREC  TWAXC MKTASRTH                                                         
         LA    R5,CTKDATA                                                       
*                                                                               
DIS2     CLI   0(R5),0                                                          
         BE    DIS8                                                             
         CLI   0(R5),X'92'                                                      
         BE    DIS6                                                             
*                                                                               
DIS4     ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DIS2                                                             
*                                  DISPLAY MARKET AUTH ELEMENT                  
         USING CTAUTHD,R5                                                       
DIS6     GOTO1 VDATCON,DMCB,(3,CTAUTAST),(9,MKTASRT)                            
         GOTO1 (RF),(R1),(3,CTAUTAND),(9,MKTAEND)                               
         GOTO1 (RF),(R1),(3,CTAUTNST),(9,MKTNSRT)                               
         GOTO1 (RF),(R1),(3,CTAUTNND),(9,MKTNEND)                               
         B     DIS4                                                             
*                                  SET NEXT ACTION & EXIT                       
DIS8     TM    CTKSTAT,X'80'                                                    
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,MKTASRTH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA                                                
         B     EXIT                                                             
         EJECT                                                                  
*              CHANGE/ADD MARKET AUTH RECORD                                    
*                                                                               
DATAVAL  MVI   TEMP,0                                                           
         GOTO1 ABLDREC             BUILD VIRGIN RECORD                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         XC    TEMP,TEMP                                                        
         LA    R5,TEMP                                                          
         USING CTAUTHD,R5          BUILD MARKET AUTH ELEMENT                    
         MVC   CTAUTEL(2),=X'9210'                                              
         LA    R1,MKTASRTH                                                      
         BAS   RE,VALDATE                                                       
         BNE   EXIT                                                             
         MVC   CTAUTAST,DUB                                                     
         LA    R1,MKTAENDH                                                      
         BAS   RE,VALDATE                                                       
         BNE   EXIT                                                             
         MVC   CTAUTAND,DUB                                                     
         CLC   CTAUTAND,CTAUTAST                                                
         BNH   EIIF                                                             
         LA    R1,MKTNSRTH                                                      
         BAS   RE,VALDATE                                                       
         BNE   EXIT                                                             
         MVC   CTAUTNST,DUB                                                     
         LA    R1,MKTNENDH                                                      
         BAS   RE,VALDATE                                                       
         BNE   EXIT                                                             
         MVC   CTAUTNND,DUB                                                     
         CLC   CTAUTNND,CTAUTNST                                                
         BNH   EIIF                                                             
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         LA    R1,MKTMNUMH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         MVC   KEY,KEYSAVE         RESTORE KEY/A(RECORD)                        
         ST    R4,AREC                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              VALIDATE A DATE FIELD FOR MMM/YY                                 
*                                                                               
VALDATE  NTR1                                                                   
         GOTO1 AFVAL                                                            
         BZ    VALDATX                                                          
         MVI   FERN,29                                                          
         GOTO1 VDATVAL,DMCB,(2,FLD),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    VALDATX                                                          
         MVI   FERN,X'FF'                                                       
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,DUB)                                    
VALDATX  CLI   FERN,X'FF'                                                       
         XIT1                                                                   
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFME8D                                                                      
       ++INCLUDE CTLFME8D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTLFM17   05/01/02'                                      
         END                                                                    
