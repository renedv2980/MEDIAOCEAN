*          DATA SET GEKWX05    AT LEVEL 002 AS OF 05/24/96                      
*PHASE TF2005A,+0                                                               
         TITLE '$KWX MK3 - MESSAGE DISPLAY, CHANGE && DELETE ACTIONS'           
         PRINT NOGEN                                                            
KWX05    CSECT                                                                  
         NMOD1 0,**KWX05*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX05+4096,R9       R9 = 2ND BASE                                
         USING TWAD,RA             RA = TWA                                     
         USING GWS,RC              RC = GWS                                     
*                                                                               
T001     ST    RB,ABASE2                                                        
         ST    R9,A2NDBAS2                                                      
         ST    RD,AREGSAV2                                                      
         B     T010                                                             
         EJECT                                                                  
*              FURTHER PARAMETER CHECKS ON PRE-PROCESSED PARAMETERS             
*                                                                               
T010     TM    MSGSTAT,ENDED       CANT CHANGE OR DELETE IF ENDED               
         BZ    T015                                                             
         CLI   ACTION,DIM                                                       
         BE    T015                                                             
         MVI   FERN,BKENDED                                                     
         B     ERROR                                                            
*                                                                               
T015     CLI   FXREF,0             DELETE REQUIRES REF=                         
         BNE   T020                                                             
         CLI   ACTION,DEM                                                       
         BNE   T018                                                             
T016     MVI   FNDX,2                                                           
         MVI   FERN,MISSING                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T018     CLI   ACTION,CHM          CHANGE REQUIRES REF= IF NOTHING ON           
         BNE   T025                DISPLAY                                      
         OC    DISPLOM,DISPLOM                                                  
         BNZ   T025                                                             
         B     T016                                                             
*                                                                               
T020     CLC   REFLO,MSGRECHI      REFS MUST START IN BOOK RANGE                
         BNH   T025                                                             
         MVI   FERN,RECNFND                                                     
         MVC   FNDX,FXREF                                                       
         B     ERROR                                                            
*                                                                               
T025     CLI   FXNEXT,0            NEXT INVALID IF LAST ON DISPLAY              
         BE    T030                                                             
         CLC   DISPHIM,MSGRECHI                                                 
         BL    T030                                                             
         MVI   FERN,RECNFND                                                     
         MVC   FNDX,FXNEXT                                                      
         B     ERROR                                                            
*                                                                               
T030     CLI   FXLAST,0            LAST INVALID IF NONE IN BOOK                 
         BE    T035                                                             
         OC    MSGRECHI,MSGRECHI                                                
         BNZ   T035                                                             
         MVI   FERN,RECNFND                                                     
         MVC   FNDX,FXLAST                                                      
         B     ERROR                                                            
*                                                                               
T035     CLI   ACTION,DIM          DISPLAY CANT HAVE MORE THAN ONE OF           
         BE    *+12                REF=, NEXT AND LAST                          
         CLI   ACTION,CHM                                                       
         BNE   T050                                                             
         SR    R0,R0                                                            
         ZIC   R1,FXREF                                                         
         AR    R0,R1                                                            
         IC    R1,FXNEXT                                                        
         AR    R0,R1                                                            
         IC    R1,FXLAST                                                        
         AR    R0,R1                                                            
         BNZ   T038                                                             
         OC    MSGRECHI,MSGRECHI   NO PARMS - IS BOOK EMPTY                     
         BNZ   T038                                                             
         MVI   FERN,RECNFND                                                     
         B     ERROR                                                            
T038     CH    R0,=H'2'            FLD NOS MORE THAN 2                          
         BNH   T050                                                             
         MVI   FERN,INCPARMS                                                    
         MVI   SYNTAX,C'Y'                                                      
         MVC   FNDX,FXREF                                                       
         CLC   FNDX,FXNEXT                                                      
         BH    *+10                                                             
         MVC   FNDX,FXNEXT                                                      
         CLC   FNDX,FXLAST                                                      
         BH    ERROR                                                            
         MVC   FNDX,FXLAST                                                      
         B     ERROR                                                            
         EJECT                                                                  
*              SET VARIABLES FROM PARAMETERS                                    
*                                                                               
T050     CLI   FXREF,0             IF NO REF                                    
         BNE   T055                                                             
         CLI   ACTION,DIM          DISPLAY DEFAULTS TO 1-END                    
         BNE   T052                                                             
         MVI   REFLO+1,1                                                        
         MVC   REFHI,MSGRECHI                                                   
         B     T055                                                             
T052     MVC   REFLO(4),DISPLOM    CHANGE DEFAULTS TO CHUNKS ON DISPLAY         
         B     T060                                                             
*                                                                               
T055     CLI   FXNEXT,0            NEXT MEANS ONES AFTER THIS DISPLAY           
         BE    T060                                                             
         LH    R1,DISPHIM                                                       
         LA    R1,1(R1)                                                         
         STH   R1,REFLO                                                         
         MVC   REFHI,MSGRECHI                                                   
         B     T065                                                             
*                                                                               
T060     CLI   FXLAST,0            LAST MEANS LAST IN BOOK                      
         BE    T065                                                             
         MVC   REFLO,MSGRECHI                                                   
         MVC   REFHI,REFLO                                                      
*                                                                               
T065     LH    R1,REFHI            SET NUM TO THE RANGE                         
         LA    R1,1(R1)                                                         
         SH    R1,REFLO                                                         
         STH   R1,NUM                                                           
         B     T070                                                             
         EJECT                                                                  
*              DISPLAY REQUIRED CHUNKS UNLESS ALREADY ON DISPLAY                
*                                                                               
T070     CLI   ACTION,DIM          ARE REQUIRED CHUNKS ALREADY DISPLAYD         
         BE    T080                                                             
*                                                                               
T075     CLC   REFLO,DISPLOM       FOR CHANGE/DELETE THEY NEED ONLY BE          
         BL    T080                INCLUDED IN DISPLAY                          
         CLC   REFHI,DISPHIM                                                    
         BNH   T100                                                             
*                                                                               
T080     XC    PARAS(8),PARAS      DISPLAY THEM                                 
         MVC   PARAS+2(2),REFLO                                                 
         MVC   PARAS+6(2),NUM                                                   
         GOTO1 ADISSCRN,PARAS                                                   
*                                                                               
T085     MVC   PARAS(4),DISPLOM    SET UP HEADER MESSAGE                        
         MVC   PARAS+10(2),MSGRECHI                                             
         GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     RF,4(R1)                                                         
         MVC   0(11,RF),=C'DISPLAYED -'                                         
         LA    RF,12(RF)                                                        
         CLI   ACTION,DIM                                                       
         BNE   T090                                                             
         CLC   DISPHIM,MSGRECHI                                                 
         BNE   *+14                                                             
T088     MVC   0(17,RF),=C'ENTER NEXT ACTION'                                   
         B     OKXIT                                                            
         MVC   0(18,RF),=C'HIT ENTER FOR NEXT'                                  
         LA    R2,KWXTABH                                                       
         ST    R2,ACURSOR                                                       
         OI    KWXACTH+6,X'80'                                                  
         CLI   FXNEXT,0                                                         
         BNE   OKXIT                                                            
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(12),=C'DISPLAY,NEXT'                                      
         B     OKXIT                                                            
*                                                                               
T090     CLI   ACTION,CHM          CHANGE                                       
         BNE   T095                                                             
         LA    R2,KWXDATAH                                                      
         ST    R2,ACURSOR                                                       
         MVC   0(17,RF),=C'NOW ENTER CHANGES'                                   
         XC    KWXACT,KWXACT                                                    
         MVC   KWXACT(6),=C'CHANGE'                                             
         OI    KWXACTH+6,X'80'                                                  
         B     MOREXIT                                                          
*                                                                               
T095     CLC   REFLO(4),DISPLOM    DELETE                                       
         BE    T097                CANT DELETE IF NOT ON DISPLAY                
         MVC   0(26,RF),=C'PLEASE AMEND DELETE ACTION'                          
         B     MOREXIT                                                          
T097     LA    R2,KWXTABH                                                       
         ST    R2,ACURSOR                                                       
         OI    KWXACTH+6,X'80'                                                  
         MVC   0(19,RF),=C'HIT ENTER TO DELETE'                                 
         B     MOREXIT                                                          
         EJECT                                                                  
*              NOW HANDLE CHANGES                                               
*                                                                               
T100     CLI   ACTION,CHM          SET PARAMS FOR VALIDATE/UPDATE LOOP          
         BNE   T150                                                             
         LH    R3,DISPLOM                                                       
         LA    R4,1                                                             
         LH    R5,REFHI                                                         
         LH    R0,REFLO            1ST UPDATE CHUNK NUM                         
         GOTO1 ,PARAS,(SAVMODE,0),ADDBUFF,IO,KWXDATAH,(R0)                      
         MVI   BYTE,0              CHANGE MKER                                  
*                                                                               
T110     STH   R3,2(R1)            LOOP TO READ/VALIDATE/WRITE A CHUNK          
         GOTO1 AGETCHNK                                                         
         BZ    ERROR                                                            
         GOTO1 AVALMESS                                                         
         LA    R0,ADDBUFF          RESET BUMPED OUT PNTER                       
         ST    R0,4(R1)                                                         
         BZ    ERROR                                                            
         BM    T115                NO CHANGE                                    
         GOTO1 APUTCHNK                                                         
         BZ    ERROR                                                            
         MVI   BYTE,1                                                           
T115     BXLE  R3,R4,T110          BUMP CHUNK NUMBER                            
*                                                                               
T118     TM    TRMTYPE,T3270       REDISPLAY IF NOT 3270                        
         BO    T120                                                             
         XC    PARAS(2),PARAS                                                   
         MVC   PARAS+2(2),DISPLOM                                               
         LH    R1,DISPHIM                                                       
         LA    R1,1(R1)                                                         
         SH    R1,DISPLOM                                                       
         ST    R1,PARAS+4                                                       
         GOTO1 ADISSCRN,PARAS                                                   
*                                                                               
T120     CLI   BYTE,0              ANYTHING CHANGED                             
         BNE   T125                                                             
         MVI   FERN,NOCHANGE                                                    
         B     ERROR                                                            
*                                                                               
T125     MVC   PARAS(4),REFLO      COMPLETION MESSAGE                           
         MVC   PARAS+10(2),MSGRECHI                                             
         GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     RF,PARAS+4                                                       
         MVC   0(27,RF),=C'CHANGED - ENTER NEXT ACTION'                         
         B     OKXIT                                                            
         EJECT                                                                  
*              HANDLE DELETIONS                                                 
*                                                                               
T150     MVC   PARAS+10(2),MSGRECHI SAVE FOR EDITREF CALL                       
         XC    PARAS(8),PARAS                                                   
         MVI   PARAS,MESSAGE                                                    
         MVC   PARAS+2(2),REFLO                                                 
         MVC   PARAS+6(2),REFHI                                                 
         GOTO1 ADELCHNK,PARAS      DELETE                                       
         BZ    ERROR                                                            
*                                                                               
T155     CLC   REFLO(4),DISPLOM    REDISPLAY REF NUM RANGE IF NOT ALL           
         BE    T160                DELETED                                      
         XC    PARAS(2),PARAS                                                   
         MVC   PARAS+2(2),DISPLOM                                               
         LH    R1,DISPHIM                                                       
         LA    R1,1(R1)                                                         
         SH    R1,DISPLOM                                                       
         ST    R1,PARAS+4                                                       
         GOTO1 ADISSCRN,PARAS                                                   
*                                                                               
T160     MVC   PARAS(4),REFLO      COMPLETION MESSAGE                           
         GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     RF,PARAS+4                                                       
         MVC   0(27,RF),=C'DELETED - ENTER NEXT ACTION'                         
*                                                                               
T165     DS    0H                  AMEND HEADER REC                             
         GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         BZ    ERROR                                                            
         LA    R4,IO                                                            
         USING HDRD,R4                                                          
         MVC   HDMSGHI,MSGRECHI                                                 
         CLI   SAVMODE,MESSAGE                                                  
         BNE   *+10                                                             
         MVC   HDFRECMH,FRECMHI                                                 
         GOTO1 APUTCHNK,(R1),,IO                                                
         BZ    ERROR                                                            
         B     OKXIT                                                            
         EJECT                                                                  
*              EXITS BACK TO ROOT                                               
*                                                                               
ERROR    SR    R0,R0               CC = EQU FOR ERROR                           
         B     EXIT                                                             
*                                                                               
MOREXIT  LNR   RB,RB               CC = NEG FOR MORE INPUT                      
*                                                                               
OKXIT    LTR   RB,RB               CC = POS OK COMPLETED                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* NESTED INCLUDES                                                               
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GEKWX05   05/24/96'                                      
         END                                                                    
