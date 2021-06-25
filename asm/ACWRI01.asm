*          DATA SET ACWRI01    AT LEVEL 007 AS OF 12/08/03                      
*PHASE T61401A                                                                  
         TITLE 'T61401 - ACCPAK WRITER APPLICATION'                             
T61401   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61401                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,WRIOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         GOTO1 VALOPTS                                                          
         SPACE 1                                                                
         CLI   DOWNOPT,0           IF WE ARE DOWNLOADING                        
         BE    VREC2                                                            
         CLI   CONOUT,C' '            AND OUTPUT TYPE NOT REQUESTED             
         BH    VREC2                                                            
         MVC   CONOUT(8),=CL8'DOWN'   DEFAULT TO OUTPUT OF 'DOWN'               
         OI    CONOUTH+6,X'80'                                                  
         MVI   CONOUTH+4,4                                                      
         MVC   TWAOUT,CONOUT                                                    
         SPACE 1                                                                
VREC2    GOTO1 INITDRON            INITIALIZE DRONE                             
         LA    R2,WRILEDGH         LEDGER                                       
         GOTO1 VALLEDG                                                          
         LA    R2,WRIACCH          OPTIONAL ACCOUNT                             
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALACC                                                           
         OC    QACCOUNT,QACCOUNT   ACCOUNT ENTERED?                             
         BNZ   *+8                 YES                                          
         MVI   REQSML,C'L'         NO, INDICATE LONG JOB                        
         MVI   OPTION,0                                                         
         LA    R2,WRIFILTH         OPTIONAL FILTERS                             
         GOTO1 VALFILT                                                          
         LA    R2,WRIPERH          PERIOD EXPRESSIONS                           
         GOTO1 VALPERD                                                          
         LA    R2,WRIHEADH         HEADERS                                      
         MVI   MAX,5                                                            
         GOTO1 VALHEAD                                                          
         LA    R2,WRIMIDH          MID LINE                                     
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
         LA    R2,WRIROWSH         ROWS                                         
         MVI   MAX,6                                                            
         GOTO1 VALROWS                                                          
         LA    R2,WRICOLSH         COLUMNS                                      
         MVI   MAX,16                                                           
         GOTO1 VALCOLS                                                          
         LA    R2,WRITITH          USER TITLES                                  
         GOTO1 VALTITS                                                          
         GOTO1 WRAPDRON                                                         
         SPACE 1                                                                
         OI    WRICHATH+6,X'80'    SHOW WHAT IS USED                            
         MVC   WRICHAT,=CL16'USING BUCKETS'                                     
         CLI   FRCETRNS,C'Y'                                                    
         BNE   *+10                                                             
         MVC   WRICHAT,=CL16'USING DETAILS'                                     
         B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,HOOK             APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
         SPACE 1                                                                
         MVC   ACCOMFAC,ACOMFACS   SET UP FOR ACCIO                             
         MVC   ACIOAPRT,TWAVPRNT                                                
         LA    R1,IOHOOK                                                        
         ST    R1,ACIOHOOK                                                      
         MVC   ACIOACCS,TWAACCS    PASS THROUGH LIMIT ACCESS                    
         MVC   ACIOAUTH,TWAAUTH                 AND AUTHORIZATION               
         LA    R2,LEDGLIST         PROCESS UP TO 3 LEDGERS                      
         LA    R0,3                                                             
         SPACE 1                                                                
PREP2    MVC   QUNIT(2),0(R2)                                                   
         GOTO1 ACCIO,DMCB,ACCIOD                                                
         LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         BE    *+8                                                              
         BCT   R0,PREP2                                                         
         SPACE 1                                                                
         MVI   GLMODE,GLOUTPUT     THEN PRINT THE REPORTS                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   ACMODE,PROCTRNS     IF THERE IS SOMETHING INTERESTING            
         BE    IOHOOK2                                                          
         CLI   ACMODE,PROCHIST                                                  
         BE    IOHOOK2                                                          
         CLI   ACMODE,PROCBUDG                                                  
         BE    IOHOOK2                                                          
         CLI   ACMODE,PROCACC                                                   
         BE    IOHOOK2                                                          
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK2  MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         SPACE 1                                                                
HOOK     NTR1                                                                   
         CLI   GLHOOK,GLHEAD                                                    
         BNE   XIT                                                              
         GOTO1 GENHEAD                                                          
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE ACWRIWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*CTGENFILE                                                                      
*DRGLOBAL                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*ACWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE ACWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACWRIF1D                                                       
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACWRI01   12/08/03'                                      
         END                                                                    
