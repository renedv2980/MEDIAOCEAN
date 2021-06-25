*          DATA SET ACLFM34    AT LEVEL 037 AS OF 05/01/02                      
*PHASE T60334A,+0                                                               
         TITLE 'GENEREAL LEDGER OFFICE RULES'                                   
T60334   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**LFM34*,RR=R6,CLEAR=YES                               
         LR    R8,RC                                                            
         USING LWSD,R8                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R6,PRELO                                                         
         MVI   ERROR,X'FF'               INITIALIZE:                            
         MVI   NUMPRS,0                  NUMBER OF OFFICE PARIS                 
         MVI   ELMLEN,3                  ELEMENT LENGTH                         
*                                                                               
*-----------------------------------------------------------------*             
*              CHECK CONTROL MODES                                              
*-----------------------------------------------------------------*             
MODE1    CLI   MODE,BUILDKEY                                                    
         BNE   MODE2                                                            
         BAS   RE,BLDK100                                                       
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,DSPLYREC                                                    
         BNE   MODE3                                                            
         BAS   RE,DSPL100                                                       
         B     XIT                                                              
*                                                                               
MODE3    BAS   RE,BLDR100                                                       
         B     XIT                                                              
*                                                                               
*-----------------------------------------------------------------*             
*              BUILD RATE KEY (MODE=BUILDKEY)                                   
*-----------------------------------------------------------------*             
BLDK100  NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      COMPANY CODE                                 
         TM    OFCLEDGH+4,X'20'                                                 
         BO    *+12                                                             
         MVI   ANYKEY,C'Y'                                                      
         OI    OFCLEDGH+4,X'20'                                                 
         CLI   OFCLEDGH+5,0                                                     
         BE    *+14                                                             
         MVI   KEY+1,C'S'          UNIT S                                       
         MVC   KEY+2(1),OFCLEDG    LEDGER CODE                                  
*                                                                               
         CLI   MYANYKEY,C'Y'                                                    
         BE    XIT                                                              
         MVI   MYANYKEY,C'Y'                                                    
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              DISPLAY RATE RECORD (MODE=DSPLYREC)                              
*-------------------------------------------------------------------*           
DSPL100  NTR1                                                                   
         USING GLRELD,R3                                                        
         LA    R3,IO                    ADDRESS OF RECORD                       
         AH    R3,DATADISP              POINT PAST KEY TO ELEMENTS              
DSPL110  CLI   0(R3),X'00'              CHECK FOR ANY EXISTING GLRULES          
         BE    DSPL118                  ELEMENT                                 
         CLI   0(R3),GLRELQ                                                     
         BE    DSPL119                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSPL110                                                          
*                                                                               
DSPL118  CLI   LOGACT,C'N'              DOES ACTION AGREE WITH ELEMENT          
         BNE   NEWERR                   NON-EXISTENCE                           
         B     DSPL900                                                          
DSPL119  CLI   LOGACT,C'N'              DOES ACTION AGREE WITH ELEMENT          
         BE    AMDERR                   EXISTENCE                               
*                                                                               
         USING MYSCRND,R2                                                       
DSPL120  DS    0H                                                               
         TWAXC OFCSUB1H                 CLEAR SCREEN                            
         ZIC   R5,GLRPRS                MUMBER OF OFFICE PAIRS                  
         LA    R4,GLROFFP               START ADDRESS OF OFFICE PAIRS           
         LA    R2,OFCSUB1H              SCREEN DISPLAY POSITION                 
DSPL200  MVC   MYSBOFF,0(R4)            SUB OFFICE TO SCREEN                    
         OI    MYSBOFFH+6,X'80'         TRANSMIT                                
         MVC   MYGLOFF,L'GLRSUBO(R4)    G/L OFFICE TO SCREEN                    
         OI    MYGLOFFH+6,X'80'         TRANSMIT                                
         LA    R4,L'GLROFFP(R4)         BUMP TO NEXT PAIR                       
         LA    R2,MYSCRENT(R2)          BUMP TO NEXT DISPLAY AREA               
         BCT   R5,DSPL200               LOOP THROUGH PARIS                      
*                                                                               
DSPL900  DS    0H                                                               
         LA    R2,LOGACTH                                                       
         OI    LOGACTH+6,X'40'          FOR INQUIRY POSITION CURSOR             
         CLI   LOGACT,C'I'              TO RECORD FIELD                         
         BE    XIT                                                              
         CLI   LOGACT,C'E'                                                      
         BE    XIT                                                              
         LA    R2,OFCSUB1H              FOR AMEND OR NEW POSITION TO            
         OI    OFCSUB1H+6,X'40'         FIRST INPUT FIELD                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        BUILD NEW REC, AMEND EXISTING REC (MODE=BUILDREC)                      
*-------------------------------------------------------------------*           
BLDR100  NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         USING GLRELD,R3                                                        
         LA    R3,ELEMENT                                                       
         MVI   GLREL,GLRELQ                                                     
*                                                                               
         USING MYSCRND,R4                                                       
         LA    R4,OFCSUB1H              PROCESS ALL SCREEN ENTRIES              
         LA    R6,SCRENTR               MAX NUM OF ENTRIES ON SCREEN            
BLDR320  CLI   MYSBOFFH+5,0             IF BOTH OFFICES ARE BLANK               
         BNE   BLDR400                  SKIP PROCESSING AND BUMP TO             
         CLI   MYGLOFFH+5,0             NEXT OFFICE PAIR                        
         BE    BLDR680                                                          
*                                                                               
BLDR400  LR    R2,R4                    FOR ERROR MSG USE                       
         CLI   MYSBOFFH+5,0             ANYTHING INPUT INTO SUB OFFICE          
         BE    MISSERR                                                          
         TM    COMPSTA4,CPYSOFF2        CHECK INDICATOR                         
         BO    BLDR420                                                          
         CLI   MYSBOFFH+5,1             ANYTHING INPUT INTO SUB OFFICE          
         BE    BLDR450                                                          
         B     OFCERR                                                           
BLDR420  CLI   MYSBOFFH+5,2             ANYTHING INPUT INTO SUB OFFICE          
         BNE   TWOCERR                                                          
*                                                                               
BLDR450  CLI   MYSBOFF,C' '                                                     
         BE    INVERR                                                           
         LA    R2,MYSCOFF(R4)           FOR ERROR MSG USE                       
         CLI   MYGLOFFH+5,0             ANYTHING INPUT INTO G/L OFFICE          
         BE    MISSERR                                                          
         TM    COMPSTA4,CPYSOFF2        CHECK INDICATOR                         
         BO    BLDR470                                                          
         CLI   MYGLOFFH+5,1             ANYTHING INPUT INTO G/L OFFICE          
         BE    BLDR500                                                          
         B     OFCERR                                                           
BLDR470  CLI   MYGLOFFH+5,2             ANYTHING INPUT INTO G/L OFFICE          
         BNE   TWOCERR                                                          
*                                                                               
BLDR500  CLI   MYGLOFF,C' '                                                     
         BE    INVERR                                                           
         BAS   RE,ADDTOTAB              ADD ELEMENT TO TABLE                    
         CLI   ERROR,X'FE'              WAS AN ERROR ENCOUNTERED?               
         BE    XIT                      IF YES EXIT WITH ERROR MESSAGE          
BLDR680  LA    R4,MYSCRENT(R4)          BUMP FIELD ON SCREEN                    
         BCT   R6,BLDR320               PROCESS NEXT SCREEN ENTRY               
*                                                                               
         LA    R2,IO2                   RECORD MUST BE IN IO2 FOR               
         LA    R3,IOLENQ                UPDATING                                
         LA    R4,IO                    ACTUALLY REMOVAL AND READDING           
         SR    R5,R5                                                            
         ICM   R5,3,IO+42                                                       
         MVCL  R2,R4                                                            
         GOTO1 REMANEL,DMCB,(X'E6',0)   REMOVE ALL E6 ELEMENTS                  
*                                                                               
         LA    R3,ELEMENT                                                       
         LA    R5,GLROFFP               BEGINNING OF OFFICE PARIS               
         ZIC   R7,NUMPRS                                                        
         CH    R7,=H'0'                 NO SORT UNLESS MORE THAN                
         BE    BLDRXIT                  ONE ENTRY                               
         CH    R7,=H'1'                 NO SORT UNLESS MORE THAN                
         BNH   BLDR700                  ONE ENTRY                               
         GOTO1 CALLOV,DMCB,0,X'D900A12'    GET ADDR OF XSORT                    
         L     RF,DMCB                  ADDR OF XSORT                           
         LA    R6,L'GLROFFP                                                     
         GOTO1 (RF),DMCB,(0,(R5)),(R7),(R6),(R6),0                              
*                                                                               
BLDR700  DS    0H                                                               
         LA    R3,ELEMENT                                                       
         MVC   GLRLN,ELMLEN                                                     
         MVC   GLRPRS,NUMPRS                                                    
         GOTO1 ADDANEL                  ADD ELEMENT TO REC                      
         OI    LOGRECH+6,X'40'          POSITION CURSOR                         
*                                                                               
BLDRXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ADD NEW ELEMENT TO TABLE                                               
*-------------------------------------------------------------------*           
ADDTOTAB NTR1                                                                   
         OC    MYSBOFF,SPACES                                                   
         LA    R5,GLROFFP               START OF OFFICE PAIRS                   
         LA    R6,SCRENTR               MAX POSSIBLE SCREEN ENTRIES             
ADDT100  OC    0(L'GLROFFP,R5),0(R5)    NO MORE OFFICE PAIRS                    
         BZ    ADDT200                                                          
         CLC   0(L'GLRSUBO,R5),MYSBOFF  IS THIS DUPLICATE ENTRY                 
         BE    DUPERR                   SET ERROR MESSAGE                       
         LA    R5,L'GLROFFP(R5)         ELSE BUMP TABLE AND CHECK               
         BCT   R6,ADDT100               NEXT ENTRY                              
*                                                                               
ADDT200  MVC   0(L'GLRSUBO,R5),MYSBOFF  OFFICES TO ELEMENT                      
         MVC   L'GLRSUBO(L'GLRGNLO,R5),MYGLOFF                                  
         OC    0(L'GLRSUBO+L'GLRGNLO,R5),SPACES                                 
         ZIC   R6,NUMPRS                TOTAL NUMBER OF OFFICE PAIRS            
         LA    R6,1(R6)                                                         
         STC   R6,NUMPRS                                                        
         ZIC   R6,ELMLEN                TOTAL ELEMENT LENGTH                    
         LA    R6,4(R6)                                                         
         STC   R6,ELMLEN                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ERROR MESSAGES                                                         
*-------------------------------------------------------------------*           
TWOCERR  MVC   LOGHEAD(39),=C'ONLY TWO CHARACTER OFFICE CODE ACCEPTED'          
         MVI   ERROR,X'FE'                                                      
         OI    6(R2),X'40'              POSITION CURSOR                         
         B     XIT                                                              
*                                                                               
OFCERR   MVC   LOGHEAD(39),=C'ONLY ONE CHARACTER OFFICE CODE ACCEPTED'          
         MVI   ERROR,X'FE'                                                      
         OI    6(R2),X'40'              POSITION CURSOR                         
         B     XIT                                                              
*                                                                               
DUPERR   MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(27),=C'DUPLICATE SUBSIDIARY OFFICE'                      
         LR    R2,R4                                                            
         OI    6(R2),X'40'              POSITION CURSOR                         
         B     XIT                                                              
*                                                                               
MISSERR  MVI   ERROR,1                  MISSING INPUT FIELD IF ONLY ONE         
         OI    6(R2),X'40'              POSITION CURSOR                         
         B     XIT                                                              
*                                                                               
INVERR   MVI   ERROR,2                  INVALID INPUT FIELD                     
         OI    6(R2),X'40'              POSITION CURSOR                         
         B     XIT                                                              
*                                                                               
NEWERR   MVI   ERROR,RECNOTON                                                   
         OI    LOGACTH+6,X'40'                                                  
         B     XIT                                                              
*                                                                               
AMDERR   MVI   ERROR,RECONFLE                                                   
         OI    LOGACTH+6,X'40'                                                  
         B     XIT                                                              
*                                                                               
*-------------------------------------------------------------------*           
*        EXIT ROUTINE                                                           
*-------------------------------------------------------------------*           
XIT      DS    0H                                                               
         OI    LOGSERVH+1,X'01'                                                 
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LITERALS                                                         
*-------------------------------------------------------------------*           
LWSD     DSECT                                                                  
SCRENTR  EQU   51                  MAX NUMBER OF ENTRIES ON SCREEN              
PRELO    DS    F                   RELOCATION FACTOR                            
NUMPRS   DS    XL1                 NUMBER OF OFFICE PAIRS                       
ELMLEN   DS    XL1                 ELEMENT LENGTH                               
*                                                                               
TEMPOFFA DS    0CL4                OFFICE CODE WORK AREA                        
TEMPSB   DS    CL2                                                              
TEMPGL   DS    CL2                                                              
LWSX     EQU   *                                                                
         EJECT                                                                  
*                                                                               
*        DSECT TO COVER (SCREEN) OFFICE-OFFICE PAIR                             
MYSCRND  DSECT                                                                  
MYSBOFFH DS    CL(L'OFCSUB1H)        OFFICE HEADER                              
MYSBOFF  DS    CL(L'OFCSUB1)         OFFICE SUB                                 
MYSCOFF  EQU   *-MYSBOFFH            LENGTH OF ONE OFFICE ENTRY                 
MYGLOFFH DS    CL(L'OFCGNL1H)        OFFICE HEADER                              
MYGLOFF  DS    CL(L'OFCGNL1)         OFFICE G/L                                 
MYSCRENT EQU   *-MYSBOFFH            LENGTH OF ONE OFFICE PAIR                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMCBD                                                       
MYANYKEY DS    CL1                                                              
*                                                                               
*        ACLFMWORK                                                              
*        ACGENFILE                                                              
*        ACLFMEQU                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACLFM34   05/01/02'                                      
         END                                                                    
