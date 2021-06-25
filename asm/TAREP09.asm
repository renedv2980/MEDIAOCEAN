*          DATA SET TAREP09    AT LEVEL 038 AS OF 11/30/12                      
*PHASE T70309A,*                                                                
         TITLE 'T70309 - BALANCE CREATE - VALIDATE SCREEN'                      
T70309   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70309                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING T703FFD,RA           SCREEN                                      
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TBALD,R7            BALANCE REC INTERFACE DSECT                  
*                                                                               
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BE    VREC                                                             
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              VALIDATE RECORD                                                  
*                                                                               
VREC     DS    0H                                                               
         XC    OPTS,OPTS           CLEAR OPTIONS BYTE                           
         XC    FILTCURR,FILTCURR         & FILTERS                              
         XC    FILTEMP,FILTEMP                                                  
*                                                                               
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         LA    R2,SBCPERH          RUN FOR WHAT PERIOD                          
         GOTO1 ANY                                                              
         MVC   THEPDTE,TGTODAY1    SET START DATE = TODAY                       
         MVC   PENDDTE,THEPDTE     AND END DATE = START DATE                    
*                                                                               
         TM    OPTS,OFORCE         IF FORCING CHECK DATE THEN SKIP              
         BO    VREC50                                                           
         CLC   =C'NEXTBDAY ',WORK  IF THIS IS A TURNAROUND THEN SKIP            
         BE    VREC50                                                           
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     SYSTEM PERIOD VALIDATION ROUTINE             
*                                                                               
         MVC   THEPDTE,PVALPSTA    SAVE START DATE                              
         MVC   PENDDTE,PVALPEND    AND END DATE                                 
*                                                                               
         LA    R2,FHEAD            SET R2 FOR PDVAL                             
         MVI   FHEAD+5,8                                                        
         MVI   FHEAD,16                                                         
         MVC   NPERIOD(8),=C'NEXTBDAY '  SET TO GET NEXT BUSINESS DAY           
         MVC   TGTODAY1,PVALPSTA         BASED ON START DATE OF PERIOD          
         MVC   TGTODAY2,PVALCSTA                                                
*                                                                               
VREC50   LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO PDVAL TO GET NEXTBDAY OR               
*                                  FORCED CHECK DATE RESOLVED                   
*                                                                               
         MVC   PCHKDTE,PVALPSTA    SET CHECK DATE                               
*                                                                               
         LA    R2,SBCCURRH         FILTER BY CURRENCY                           
         CLI   5(R2),0                                                          
         BE    VREC60                                                           
         CLI   8(R2),C'U'          DEFAULT TO US DOLLARS                        
         BNE   VREC55                                                           
         MVC   SBCCURR(6),=C'U.S.  '                                            
         MVI   FILTCURR,C'U'                                                    
         B     VREC60                                                           
*                                                                               
VREC55   CLI   8(R2),C'C'          CANADIAN                                     
         BNE   VREC58                                                           
         MVC   SBCCURR(6),=C'CANADA'                                            
         MVI   FILTCURR,C'C'                                                    
         B     VREC60                                                           
*                                                                               
VREC58   CLI   8(R2),C'E'          EUROS                                        
         BNE   INVERR                                                           
         MVC   SBCCURR(6),=C'EUROS '                                            
         MVI   FILTCURR,C'E'                                                    
*                                                                               
VREC60   LA    R2,SBCEMPH          FILTER BY EMPLOYER                           
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SBCEMPNH                        
         MVC   FILTEMP,SBCEMP                                                   
         OC    FILTEMP,SPACES                                                   
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS FIELD                                
*                                                                               
VALOPT   NTR1                                                                   
         LA    R2,SBCOPTH          OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
*                                                                               
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VOPT40   CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VOPT50                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    OPTS,OTRACE         SET TRACE ON                                 
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
*                                                                               
VOPT50   CLC   =C'FORCECDTE',SCDATA1   FORCE CHECK DATE                         
         BNE   VOPT55                                                           
         OI    OPTS,OFORCE         SET FORCE ON                                 
*                                                                               
VOPT55   LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VOPT40           AND CONTINUE                                 
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
         USING SCAND,R3                                                         
ADDISP   NTR1                                                                   
         ZIC   RF,SCLEN1           L'LHS                                        
         ZIC   RE,SCLEN2           + L'RHS                                      
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AH    RE,=H'1'            + '=' SIGN IF THERE IS A RIGHT HALF          
         LA    RF,1(RF,RE)         + DELIMITER                                  
         AH    RF,HALF             + L'SO FAR                                   
         STH   RF,HALF             = CURRENT DISPLACEMENT INTO FIELD            
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERXIT                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERXIT                                                            
*                                                                               
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     ERXIT                                                            
*                                                                               
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     ERXIT                                                            
*                                                                               
ERXIT    DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TABALD                                                         
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF9D                                                       
         EJECT                                                                  
*DDPERVAL                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDMASTD                                                                        
*ACGENBOTH                                                                      
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038TAREP09   11/30/12'                                      
         END                                                                    
