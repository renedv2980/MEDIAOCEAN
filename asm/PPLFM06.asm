*          DATA SET PPLFM06    AT LEVEL 073 AS OF 05/01/02                      
*PHASE T40406A,+0                                                               
         TITLE 'PPLFM06 - SHARE RECORDS'                                        
T40406   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40406                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T404FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING PSHRRECD,R8                                                      
         USING PSHREL05,R3                                                      
*                                                                               
         ZAP   PCTOTS,=P'0'        SET TOTS TO PACKED ZERO                      
         SPACE                                                                  
         CLI   SVFMTSW,0           IS IT A FORMAT                               
         BNE   EDT10               NO.                                          
         SPACE                                                                  
* THIS SECTION DISPLAYS THE RECORD *                                            
         SPACE                                                                  
FMT00    XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
FMT05    LA    R3,REC+33          R3 POINTS TO 01 ELEMENT                       
         MVI   ELCODE,5                                                         
         BAS   RE,NEXTEL          R3 POINTS TO 05 ELEMENT                       
         BE    *+8                                                              
         B     EXIT                                                             
* CLEARS THE SCREEN                                                             
         XC    SHRTOT,SHRTOT       CLEAR PCT TOT                                
         LA    R2,SHRF1H                                                        
FMT10    CLI   1(R2),X'20'         IS IT A PROTECTED FIELD                      
         BE    FMT20                                                            
         ZIC   RF,0(R2)            RF HAS LENGTH OF FIELD                       
         SH    RF,=H'9'            SUBTRACT HEADER LENGTH +1                    
         EX    RF,CLROC            IS FIELD EMPTY                               
         EX    RF,CLRCLC            "   "     "                                 
         BE    FMT20               YES.                                         
         EX    RF,CLRXC            NO. XC FIELD.                                
         FOUT  (R2)                                                             
FMT20    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    FMT10                                                            
         B     FMT25                                                            
*                                                                               
CLROC    OC    8(0,R2),SPACES                                                   
CLRCLC   CLC   8(0,R2),SPACES                                                   
CLRXC    XC    8(0,R2),8(R2)                                                    
         SPACE                                                                  
* GET THE 05 ELEMENTS                                                           
FMT25    DS    0H                                                               
         MVI   COLSW,1                                                          
         LA    R2,SHRF1H                                                        
         B     FMT40                                                            
FMT30    BAS   RE,NEXTEL                                                        
         BNE   FMT50                    NO MORE 05 ELEMENTS                     
FMT40    DS    0H                                                               
         MVC   8(25,R2),PSHRACCT                                                
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(7,R2),EDPTRN                                                   
         ED    8(7,R2),PSHRPCT                                                  
         FOUT  (R2)                                                             
         AP    PCTOTS,PSHRPCT       ADD TO PCT TOTS                             
         SPACE                                                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               ACT-COL2                                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               PCT-COL2                                     
FMT45    ZIC   R0,0(R2)                                                         
         AR    R2,R0               ACT-COL1                                     
         SPACE                                                                  
         CLI   0(R2),9                                                          
         BH    FMT30                                                            
         SPACE                                                                  
         CLI   COLSW,2                                                          
         BE    FMT50                                                            
         MVI   COLSW,2                                                          
         LA    R2,SHRF1H                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               PCT-COL1                                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               ACT-C0L2                                     
         B     FMT30                                                            
         SPACE                                                                  
FMT50    DS    0H                  PERCENT TOTAL                                
         MVC   SHRTOT,TEDPTRN                                                   
         ED    SHRTOT,PCTOTS                                                    
         LA    R2,SHRTOTH                                                       
         FOUT  (R2)                                                             
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
EDPTRN   DS    0H                                                               
         DC    X'402020214B2020'                                                
TEDPTRN  DC    X'402020202020202020214B2020'                                    
         EJECT                                                                  
* THIS SECTION ADDS A RECORD OR CHANGES THE 05 ELEMENTS *                       
         SPACE                                                                  
         DS    0H                                                               
EDT10    LA    R3,REC+33           R3 POINTS TO 01 ELEMENT                      
         CLI   SVACT,C'A'          IS IT AN ADD                                 
         BE    EDT20                                                            
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R2,SHRF1            IS IT A DELETE                               
         CLC   0(6,R2),=C'DELETE'                                               
         BE    DELRTN                                                           
         SPACE                                                                  
* DELETE THE 05 ELEMENTS                                                        
         MVI   ELCODE,5                                                         
         BAS   RE,NEXTEL                                                        
         BNE   EDT20A              NO 05 ELEMENTS                               
EDT15    GOTO1 VRECUP,DMCB,(0,REC),0(R3),0                                      
         CLI   0(R3),5             IS IT AN 05 ELEMENT                          
         BE    EDT15               YES. DELETE IT.                              
         B     EDT20A              NO MORE 05 ELEMENTS.                         
         SPACE                                                                  
* BUILD THE NEW KEY                                                             
EDT20    LA    RE,REC                                                           
         LA    RF,L'REC                                                         
         XCEF                                                                   
         MVC   REC(13),SVKEY                                                    
         MVI   REC+26,38           LENGTH OF SHRREC WITHOUT 05 ELEM             
*        MVC   SHRAGYA,AGYALPHA  *********  WHY NOT IN PRINT   ***              
         SPACE                                                                  
* BUILD THE 01 ELEMENT                                                          
         MVC   0(2,R3),=X'0105'                                                 
EDT20A   GOTO1 VDATCON,DMCB,(5,0),(3,PSHRACDT)       NEW ACT DATE               
         SPACE                                                                  
* BUILD THE 05 ELEMENT                                                          
         LA    R2,SHRF1H                                                        
EDT30    LA    R3,REC+33           R3 POINTS TO 05 ELEM                         
EDT30A   CLI   5(R2),0             TEST IF ACCT FIELD IS EMPTY                  
         BE    EDT40                                                            
         MVC   ELEM(2),=X'051E'                                                 
         MVC   ELEM+2(L'PSHRACCT),8(R2)                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             IS PERCENT FIELD EMPTY                       
         BNE   EDT30B                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     ERRORTN                                                          
EDT30B   ZIC   R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R0)                                     
         CLI   0(R1),X'FF'                                                      
         BE    PCTIERR                                                          
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         AP    PCTOTS,DUB               ADD TO PCT TOTS                         
         CP    PCTOTS,=P'10000'                                                 
         BNH   EDT30C                                                           
         SPACE                                                                  
         MVC   SHRTOT,TEDPTRN                                                   
         ED    SHRTOT,PCTOTS                                                    
         FOUT  SHRTOTH                                                          
         MVI   ERRCD,238                                                        
         B     ERRORTN                                                          
         SPACE                                                                  
EDT30C   MVC   ELEM+27(L'SHRPCT),DUB+5                                          
         SPACE                                                                  
EDT30D   BAS   RE,NEXTEL           ROUTINE TO ADD ELEMENTS IN                   
         BNE   EDT30E                ALPHA-NUMERIC ORDER                        
         CLC   ELEM(27),0(R3)                                                   
         BH    EDT30D                                                           
         BE    DUPERRTN                                                         
EDT30E   GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R3)                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   WRITE10             END OF SCREEN                                
         B     EDT30                                                            
*                                                                               
EDT40    DS    0H                  ACCT FIELD IS BLANK, SO                      
         ZIC   R0,0(R2)            SKIP PCT FIELD AND GET                       
         AR    R2,R0               NEXT ACCT FIELD.                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   WRITE10             END OF SCREEN                                
         B     EDT30A                                                           
         EJECT                                                                  
* WRITE THE RECORD *                                                            
         SPACE                                                                  
WRITE10  CLI   SVACT,C'A'                                                       
         BE    WRITE20                                                          
         GOTO1 PUTREC                                                           
         BAS   RE,ANYCHK                                                        
         B     WRITE30                                                          
WRITE20  GOTO1 ADDREC                                                           
         MVC   SVKEY+14(4),KEY+14                                               
         BAS   RE,ANYCHK                                                        
         SPACE                                                                  
WRITE30  DS    0H                                                               
         MVC   SHRTOT,TEDPTRN                                                   
         ED    SHRTOT,PCTOTS                                                    
         LA    R2,SHRTOTH                                                       
         FOUT  (R2)                                                             
         CP    PCTOTS,=P'10000'                                                 
         BE    EXIT                                                             
         MVI   ERRCD,238                                                        
         B     ERRORTN                                                          
         SPACE                                                                  
*                                                                               
NEXTEL   CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLC   ELCODE,0(R3)                                                     
         BNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT EQUAL                       
*                                                                               
DELRTN   MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),SVKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVI   KEY+13,X'80'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
         MVI   REC+15,X'80'                                                     
         GOTO1 PUTREC                                                           
         XC    LFMMSG,LFMMSG                                                    
         FOUT  LFMMSGH,=C'RECORD DELETED',14                                    
         MVI   ERRAREA,1                                                        
         B     EXIT                                                             
         SPACE                                                                  
         SPACE                                                                  
*                                                                               
ANYCHK   CLI   REC+14,29           WAS SCREEN LEFT BLANK ON CHA/ADD             
         BHR   RE                                                               
         LA    R2,SHRF1H           YES. ERROR.                                  
         MVI   ERRCD,MSSNGERR                                                   
         B     ERRORTN                                                          
*                                                                               
DUPERRTN DS    0H                                                               
         S     R2,=F'33'           SUBTRACT L'ACCT FIELD                        
         MVI   ERRCD,DUPENTRY      TO SET CURSOR TO ACCT FIELD                  
         B     ERRORTN                                                          
*                                                                               
PCTIERR  DS    0H                  NO PCT INPUT                                 
         MVI   ERRCD,NOTNUM                                                     
         B     ERRORTN                                                          
*                                                                               
ERRORTN  GOTO1 ERROR                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
PCTOTS   DS    PL6                 PERCENT TOTALS                               
COLSW    DS    CL1                 COLUMN                                       
         SPACE                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PLFMWRK                                                        
         EJECT                                                                  
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF6D                                                       
         EJECT                                                                  
       ++INCLUDE PPGENSHR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073PPLFM06   05/01/02'                                      
         END                                                                    
