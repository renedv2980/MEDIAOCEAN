*          DATA SET ACLFM13S   AT LEVEL 017 AS OF 05/01/02                      
*PHASE T60313A,+0                                                               
         TITLE 'MODULE TO HANDLE RETAIL SCHEME'                                 
*                                                                               
*                                                                               
T60313   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM13*                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         SPACE 1                                                                
         MVI   THISACT,0                                                        
         CLI   LOGACT,C'N'                                                      
         BNE   *+8                                                              
         OI    THISACT,NEW                                                      
         CLI   LOGACT,C'A'                                                      
         BNE   *+8                                                              
         OI    THISACT,CHA                                                      
         CLI   LOGACT,C'I'                                                      
         BNE   *+8                                                              
         OI    THISACT,DIS                                                      
         CLI   LOGACT,C'E'                                                      
         BNE   *+8                                                              
         OI    THISACT,DIS                                                      
         SPACE 1                                                                
         CLC   THISACT,LASTACT                                                  
         BE    RTLINT                                                           
         CLI   LASTACT,0                                                        
         BE    RTLINT              FIRST TIME                                   
         CLI   THISACT,NEW                                                      
         BNE   RTLINT                                                           
         MVI   ANYKEY,C'Y'         FIRST TIME FOR NEW                           
         XC    LASTKEY,LASTKEY                                                  
RTLINT   MVC   LASTACT,THISACT                                                  
         EJECT                                                                  
*              BUILD KEY                                                        
BSNY     EQU   X'6E'                                                            
         SPACE 2                                                                
RTL001   LA    R3,LOGENDH                                                       
         LA    R8,LOGSTRCH                                                      
         USING LIND,R8                                                          
RTL00    CR    R8,R3                                                            
         BE    RTL00B                                                           
         TM    LINSTH+4,X'20'                                                   
         BO    RTL00A                                                           
         MVC   LINNM,SPACES        CLEAR ALL PROTECTED NAME FILEDS              
RTL00A   OI    LINNMH+6,X'80'                                                   
         LA    R8,LINLEN(R8)                                                    
         B     RTL00                                                            
         SPACE 1                                                                
RTL00B   CLI   MODE,BUILDKEY                                                    
         BNE   RTL20                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'3'                                                       
         LA    R2,LOGADVTH                                                      
         GOTO1 ANY                                                              
         CLI   5(R2),1                                                          
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         MVC   KEY+2(1),LOGADVT                                                 
         TM    4(R2),X'20'                                                      
         BO    RTL04                                                            
         SPACE 2                                                                
*              READ ADVERTISER LEDGER RECORD                                    
         GOTO1 READ                                                             
         LA    R3,IO                                                            
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL                                                         
         BNE   DIE                                                              
         USING ACHEIRD,R3                                                       
RTL01    MVC   SAVELVA,ACHRLEVA                                                 
         MVC   SAVELVB,ACHRLEVB                                                 
         MVC   SAVELVC,ACHRLEVC                                                 
         MVC   SAVELVD,ACHRLEVD                                                 
         MVC   LOWLEV,SAVELVB      DISP TO LOWEST LEVEL                         
         CLI   SAVELVC,0                                                        
         BNE   *+10                                                             
         MVC   LOWLEV,SAVELVA      FOR TWO LEVEL STRUCTURE                      
         FOUT  LOGADNMH,SPACES,36                                               
         NI    LOGSCHMH+4,X'DF'                                                 
         FOUT  LOGSCNMH,SPACES,15                                               
         NI    LOGMKTCH+4,X'DF'                                                 
         FOUT  LOGMKTNH,SPACES,36                                               
*                                                                               
*              GET ADVERTISER                                                   
         SPACE 2                                                                
         LA    R2,LOGADVTH                                                      
         SR    R4,R4                                                            
         IC    R4,LOWLEV                                                        
         EX    R4,MVSTAR                                                        
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    LOGADVTH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
RTL04    LA    R2,LOGSCHMH              SCHEME                                  
         GOTO1 ANY                                                              
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVI   KEY+2,C'3'                                                       
         MVC   KEY+3(1),LOGADVT                                                 
         MVC   KEY+4(2),LOGSCHM                                                 
         TM    4(R2),X'20'                                                      
         BO    RTL08                                                            
*                                                                               
         SPACE 2                                                                
         LA    R2,LOGSCHMH                                                      
         FOUT  LOGSCNMH,SPACES,15                                               
         NI    LOGMKTCH+4,X'DF'                                                 
         FOUT  LOGMKTNH,SPACES,36                                               
         GOTO1 READ                                                             
         LA    R3,IO                                                            
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DIE                                                              
         USING ACANALD,R3                                                       
         MVC   LOGSCNM(15),ACANDESC                                             
         OI    LOGSCHMH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
*              MARKET                                                           
         SPACE 2                                                                
RTL08    LA    R2,LOGMKTCH                                                      
         GOTO1 ANY                                                              
         CLI   5(R2),1                                                          
         BH    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         GOTO1 MOVE                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'3'                                                       
         MVC   KEY+2(1),LOGADVT                                                 
         MVC   KEY+3(15),WORK                                                   
         TM    4(R2),X'20'                                                      
         BO    RTL10                                                            
*                                                                               
         FOUT  LOGMKTNH,SPACES,36                                               
         LA    R2,LOGMKTCH                                                      
         GOTO1 READ                REGION MARKET RECORD                         
         GOTO1 NAMOUT                                                           
         OI    LOGMKTCH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
RTL10    CLI   ANYKEY,C'Y'                                                      
         BNE   XIT                                                              
         XC    LASTKEY,LASTKEY     IF KEY CHANGE START AGAIN                    
         NI    LOGSTRCH+4,X'DF'                                                 
         MVI   LOGSTRCH+5,0                                                     
         B     XIT                                                              
         EJECT                                                                  
RTL20    LA    R2,LOGSTRCH                                                      
         CLI   5(R2),0                                                          
         BE    RTL30               IF NO INPUT START AT NEXT                    
         USING LIND,R8                                                          
         LR    R8,R2                                                            
         LA    R3,LOGENDH          TAB FIELD                                    
RTL22    CR    R8,R3                                                            
         BNL   RTL40               NO CHANGES SO BUILDREC                       
         CLI   LINSTH+5,0                                                       
         BE    *+12                                                             
         TM    LINSTH+4,X'20'                                                   
         BNO   RTL24             CHANGED STORE SO VALIDATE AND DISPLAY          
         CLI   THISACT,DIS                                                      
         BNE   *+12                                                             
         TM    LINUNTH+4,X'20'                                                  
         BNO   RTL24               CHANGED UNITS ON DISPLAY                     
         LA    R8,LINLEN(R8)                                                    
         B     RTL22                                                            
         SPACE 1                                                                
RTL24    MVI   ANYKEY,C'Y'                                                      
         SR    R0,R0                                                            
RTL25    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      SET-UP FIRST READ                            
         MVI   KEY+1,C'3'                                                       
         MVC   KEY+2(1),LOGADVT                                                 
         ZIC   R3,LOGMKTCH+5                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),LOGMKTC    MARKET                                       
         ZIC   R1,LOWLEV                                                        
         LA    R1,KEY+3(R1)                                                     
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),8(R2)       STORE CODE TO KEY                            
         GOTO1 READ                                                             
         BAS   RE,DSP              DISPLAY NAME AND UNITS                       
         OI    4(R2),X'20'                                                      
         AH    R0,=H'1'                                                         
         LA    R2,LINLEN(R2)                                                    
         LA    R3,LOGENDH          TAB FILED                                    
         CR    R2,R3                                                            
         BNL   DSPEND              END OF SCREEN                                
         CLI   5(R2),0                                                          
         BNE   RTL25               NEXT STORE                                   
         BAS   RE,RTLCLRLN                                                      
         B     DSPEND                                                           
         EJECT                                                                  
*              DISPLAY STORE RECORDS                                            
RTL30    LA    R2,LOGSTRCH                                                      
         SR    R0,R0                                                            
         BAS   RE,RTLCLRLN         CLEAR SCREEN                                 
         CLI   LASTKEY,X'FF'                                                    
         BNE   *+10                                                             
         XC    LASTKEY,LASTKEY     IF END START AGAIN                           
         MVC   KEY,LASTKEY                                                      
         CLI   KEY,0                                                            
         BNE   RTL38               NOT FIRST START AT LAST                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      SET-UP FIRST READ                            
         MVI   KEY+1,C'3'                                                       
         MVC   KEY+2(1),LOGADVT                                                 
         ZIC   R3,LOGMKTCH+5                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),LOGMKTC                                                 
         ZIC   R1,LOWLEV                                                        
         LA    R4,KEY                                                           
         LA    R1,3(R1,R4)                                                      
         ZIC   R3,0(R1)                                                         
         AH    R3,=H'1'                                                         
         STC   R3,0(R1)                                                         
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         SPACE 1                                                                
RTL34    MVI   LASTKEY,X'FF'                                                    
         ZIC   R5,LOWLEV                                                        
         AH    R5,=H'2'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEYSAVE(0),KEY                                                   
         BNE   DSPEND              NOT THIS CLUSTER                             
**T                                                                             
         LA    R3,IO                                                            
         MVI   ELCODE,GDAELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   RTL34D                                                           
         USING GDAELD,R3                                                        
RTL34A   LA    R5,LINSTR-LIND(R2)                                               
         CLI   GDATYPE,GDATRTLS                                                 
         BE    RTL34B                                                           
         LA    R5,LINEND-LIND(R2)                                               
         CLI   GDATYPE,GDATRTLE                                                 
         BNE   RTL34C                                                           
RTL34B   GOTO1 DATCON,DMCB,(1,GDADATE),(6,(R5))                                 
                                                                                
         LA    R5,LINDASH-LIND(R2)                                              
         MVI   0(R5),C'-'                                                       
         LA    R5,LINSTR-LIND(R2)                                               
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         MVI   5(R5),C'*'                                                       
         LA    R5,LINEND-LIND(R2)                                               
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         MVI   0(R5),C'*'                                                       
                                                                                
         LA    R4,LINDASHH-LIND(R2)                                             
         OI    6(R4),X'80'         TRANSMIT FIELD                               
         LA    R4,LINSTRH-LIND(R2)                                              
         OI    6(R4),X'80'         TRANSMIT FIELD                               
         LA    R4,LINENDH-LIND(R2)                                              
         OI    6(R4),X'80'         TRANSMIT FIELD                               
RTL34C   BAS   RE,NEXTEL                                                        
         BE    RTL34A                                                           
RTL34D   DS    0H                                                               
**T                                                                             
         CLI   LOGACT,C'N'         IF NEW DON'T FILTER ON SCHEME                
         BE    RTL36               NO SCHEME CODE                               
         LA    R3,IO                                                            
         MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
         SPACE 1                                                                
RTL35    BNE   RTL38                                                            
         USING ACDISTD,R3                                                       
         CLC   ACDICODE,LOGSCHM    FIND MATCHING SCHEME                         
         BE    RTL36                                                            
         BAS   RE,NEXTEL                                                        
         B     RTL35                                                            
         SPACE 1                                                                
RTL36    BAS   RE,DSP                                                           
         LA    R3,IO                                                            
         ZIC   R5,LOWLEV                                                        
         LA    R5,3(R5,R3)                                                      
         MVC   8(L'LINSTC,R2),0(R5)                                             
         OI    6(R2),X'80'                                                      
         LA    R2,LINLEN(R2)                                                    
         AH    R0,=H'1'                                                         
         CH    R0,=H'10'                                                        
         BE    DSPEND                                                           
         SPACE 1                                                                
RTL38    LA    R1,KEY+14           READ HIGH TO NEXT ACCOUNT                    
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         ZIC   R3,0(R1)                                                         
         AH    R3,=H'1'                                                         
         STC   R3,0(R1)                                                         
         GOTO1 HIGH                                                             
         B     RTL34                                                            
         SPACE 1                                                                
DSPEND   MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(30),=CL30'NO RECORDS TO DISPLAY'                         
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGADVTH                                                      
         CH    R0,=H'0'                                                         
         BE    XIT                                                              
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(30),=CL30'RECORDS DISPLAYED'                             
         CLI   LOGACT,C'I'                                                      
         BE    XIT                                                              
         CLI   LOGACT,C'E'                                                      
         BE    XIT                                                              
         MVC   LOGHEAD(35),=CL35'RECORDS DISPLAYED ENTER CHANGES'               
         LA    R2,LOGSTRCH                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY NAME AND UNITS                                           
DSP      NTR1                                                                   
         LR    R8,R2                                                            
         USING LIND,R8                                                          
         GOTO1 NAMOUT                                                           
         OI    LINNMH+6,X'80'                                                   
         LA    R3,IO                                                            
         MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
DSP4     BNE   DSP12                                                            
         USING ACDISTD,R3                                                       
         CLC   ACDICODE,LOGSCHM                                                 
         BE    DSP8                                                             
         BAS   RE,NEXTEL                                                        
         B     DSP4                                                             
         SPACE 1                                                                
DSP8     EDIT  ACDIVAL,(12,LINUNT),2,ALIGN=LEFT                                 
         OI    LINUNTH+4,X'20'                                                  
         OI    LINUNTH+6,X'80'                                                  
DSP12    MVC   LASTKEY,IO                                                       
         B     RTXIT                                                            
         EJECT                                                                  
*              BUILDREC                                                         
         SPACE 2                                                                
RTL40    ZAP   NEWAMT,=P'0'                                                     
         LA    R2,LOGSTRCH                                                      
RTL51    LA    R3,LOGENDH          TAB FIELD                                    
         CR    R2,R3               FINISHED STORES                              
         BE    RTL60               UPDATE MARKET RECORD                         
         USING LIND,R8                                                          
         LR    R8,R2                                                            
         CLI   5(R2),0                                                          
         BE    RTL55                                                            
         SPACE 2                                                                
         TM    LINUNTH+4,X'20'                                                  
         BO    RTL55               NOT CHANGED                                  
         SR    R4,R4                                                            
         IC    R4,LOWLEV                                                        
         LA    R5,KEY+3(R4)                                                     
         LA    R6,KEY+32                                                        
         SR    R6,R5                                                            
         EX    R6,MVSPA                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,STRCD            MOVE IN STORE CODE                           
*                                                                               
         BAS   R9,RDIO2                                                         
         LA    R2,LINUNTH          GET TO UNITS FIELD                           
         ZAP   DUB,=P'0'                                                        
         CLI   5(R2),0                                                          
         BE    RTL52                                                            
         GOTO1 VALICASH            GET UNITS IN DUB                             
RTL52    LA    R3,IO2                                                           
         MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
         BNE   RTL56               NO 62 ELEMENT                                
         SPACE 2                                                                
         USING ACDISTD,R3                                                       
         CLC   ACDICODE,LOGSCHM                                                 
         BE    RTL53                                                            
         BAS   RE,NEXTEL                                                        
         BE    *-14                                                             
         B     RTL56               NO MATCHING 62                               
*                                                                               
RTL53    CP    ACDIVAL,DUB     SAME VALUE                                       
         BE    RTL55      DO NOT WRITE                                          
         SP    NEWAMT,ACDIVAL      REDUCE OLD AMOUNT                            
         CP    DUB,=P'0'           INPUT VALUE ZERO                             
         BE    DELEL               DELETE ELEMENT                               
         AP    NEWAMT,DUB          ADD NEW AMOUNT                               
         ZAP   ACDIVAL,DUB                                                      
         SPACE 2                                                                
RTL54    GOTO1 PUTREC              WRITE UPDATED RECORD                         
RTL55    LA    R8,LINLEN(R8)                                                    
         LR    R2,R8                                                            
         B     RTL51               NEXT INPUT FIELD                             
*                                                                               
RTL56    CP    DUB,=P'0'           NO ELEMENT FOR ZERO                          
         BE    RTL55                                                            
         AP    NEWAMT,DUB                                                       
         LA    R3,ELEMENT                                                       
         MVI   ACDIEL,X'62'                                                     
         MVI   ACDILEN,X'0A'                                                    
         MVC   ACDICODE,LOGSCHM                                                 
         ZAP   ACDIVAL,DUB                                                      
         GOTO1 ADDANEL                                                          
         B     RTL54                                                            
*                                                                               
DELEL    MVI   ACDIEL,X'FF'                                                     
         GOTO1 REMANEL,DMCB,0                                                   
         B     RTL54                                                            
         EJECT                                                                  
*                                                                               
*              UPDATE MARKET, REGION, ADVERTISER TOTALS                         
RTL60    SR    R4,R4                                                            
         CLI   SAVELVC,0                                                        
         BE    RTL67A              ONLY TWO LEVELS                              
         IC    R4,SAVELVB                                                       
         LA    R5,KEY+3(R4)                                                     
         SR    R6,R6                                                            
         IC    R6,SAVELVC                                                       
         SR    R6,R4                                                            
         BCTR  R6,0                                                             
         EX    R6,MVSPA                                                         
         SPACE 1                                                                
RTL61    BAS   R9,RDIO2                                                         
         LA    R3,IO2                                                           
         MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
         BNE   RTL68                                                            
         SPACE 2                                                                
         USING ACDISTD,R3                                                       
         CLC   ACDICODE,LOGSCHM                                                 
         BE    RTL66                                                            
         BAS   RE,NEXTEL                                                        
         BE    *-14                                                             
         B     RTL68                                                            
*                                                                               
RTL66    AP    ACDIVAL,NEWAMT      ADD NEW AMOUNT                               
         CP    ACDIVAL,=P'0'                                                    
         BNE   RTL67                                                            
         MVI   ACDIEL,X'FF'        IF TOTAL NOW ZERO DELETE ELEMENT             
         GOTO1 REMANEL,DMCB,0                                                   
RTL67    GOTO1 PUTREC                                                           
RTL67A   CLI   COMPANY,BSNY                                                     
         BE    RTL70                                                            
         ZIC   R4,LOWLEV                                                        
         EX    R4,CLCSTAR                                                       
         BE    RTL70                                                            
         IC    R4,SAVELVA                                                       
         LA    R5,KEY+3(R4)                                                     
         XR    R6,R6                                                            
         IC    R6,SAVELVB                                                       
         SR    R6,R4                                                            
         BNP   RTL69               LEV A = LEV B                                
         BCTR  R6,0                                                             
         EX    R6,CLCSPA                                                        
         BE    RTL69                                                            
         EX    R6,MVSPA                                                         
         B     RTL61                                                            
RTL69    IC    R4,LOWLEV                                                        
         EX    R4,MVSTAR                                                        
         B     RTL61                                                            
*                                                                               
RTL68    LA    R3,ELEMENT                                                       
         MVI   ACDIEL,X'62'                                                     
         MVI   ACDILEN,X'0A'                                                    
         MVC   ACDICODE,LOGSCHM                                                 
         ZAP   ACDIVAL,NEWAMT                                                   
         CP    ACDIVAL,=P'0'                                                    
         BE    RTL67A              DON'T ADD A ZERO ELEMENT                     
         GOTO1 ADDANEL                                                          
         B     RTL67                                                            
*                                                                               
RTL70    LA    R2,LOGSTRCH                                                      
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(30),=CL30'RECORDS AMENDED'                               
         B     XIT                                                              
         SPACE 1                                                                
CLCSTAR  CLC   KEY+3(0),=12C'*'                                                 
CLCSPA   CLC   0(0,R5),SPACES                                                   
MVSTAR   MVC   KEY+3(0),=12C'*'                                                 
MVSPA    MVC   0(0,R5),SPACES                                                   
STRCD    MVC   0(0,R5),8(R2)                                                    
         EJECT                                                                  
         SPACE 1                                                                
*              CLEAR STORE NAME                                                 
*              TURN ON VALIDITY BIT FOR REMAINDER OF SCREENS                    
RTLCLRLN NTR1                                                                   
         LA    R3,LOGENDH          TAB FIELD                                    
         LR    R8,R2                                                            
         USING LIND,R8                                                          
RTEXT    CR    R8,R3                                                            
         BE    RTXIT                                                            
         MVC   LINSTC,SPACES                                                    
         OI    LINSTH+4,X'20'         STORE                                     
         OI    LINSTH+6,X'80'                                                   
         MVC   LINNM,SPACES        STORE NAME                                   
         OI    LINNMH+6,X'80'                                                   
         MVC   LINUNT,SPACES                                                    
         OI    LINUNTH+4,X'20'        UNITS                                     
         OI    LINUNTH+6,X'80'                                                  
         MVC   LINSTR,SPACES                                                    
         OI    LINSTRH+4,X'20'        UNITS                                     
         OI    LINSTRH+6,X'80'                                                  
         MVC   LINDASH,SPACES                                                   
         OI    LINDASHH+4,X'20'        UNITS                                    
         OI    LINDASHH+6,X'80'                                                 
         MVC   LINEND,SPACES                                                    
         OI    LINENDH+4,X'20'        UNITS                                     
         OI    LINENDH+6,X'80'                                                  
         LA    R8,LINLEN(R8)                                                    
         B     RTEXT                                                            
RTXIT    XIT1                                                                   
*                                                                               
*              READ AND MOVE IO1 TO IO2 FOR UPDATE                              
         SPACE 1                                                                
RDIO2    MVI   UPDATE,C'Y'         READ FOR UPDATE                              
         GOTO1 READ                                                             
         MVC   DMWORK2,DMWORK                                                   
         LA    RF,IO2                                                           
         LA    R1,IOLENQ                                                        
         LA    RE,IO                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         BR    R9                                                               
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
DIE      DC    F'0'                                                             
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
PATCH    DC    10F'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMECD                                                       
ELCODE   DS    CL1                                                              
SAVELVA  DS    CL1                                                              
SAVELVB  DS    CL1                                                              
SAVELVC  DS    CL1                                                              
SAVELVD  DS    CL1                                                              
LOWLEV   DS    CL1                                                              
NEWAMT   DS    PL6                                                              
LASTKEY  DS    CL32                                                             
LASTACT  DS    XL1                                                              
THISACT  DS    XL1                                                              
NEW      EQU   X'80'                                                            
CHA      EQU   X'40'                                                            
DIS      EQU   X'20'                                                            
         EJECT                                                                  
*        ACLFMWORK                                                              
*              DSECT FOR A LINE                                                 
LIND     DSECT                                                                  
LINSTH   DS    CL8                 STORE                                        
LINSTC   DS    CL7                                                              
LINNMH   DS    CL8                 NAME                                         
LINNM    DS    CL36                                                             
LINUNTH  DS    CL8                 UNITS                                        
LINUNT   DS    CL12                                                             
LINSTRH  DS    CL8                 EFFECTIVE START                              
LINSTR   DS    CL6                                                              
LINDASHH DS    CL8                 SEPERATOR                                    
LINDASH  DS    CL1                                                              
LINENDH  DS    CL8                 EFFECTIVE END                                
LINEND   DS    CL6                                                              
LINLEN   EQU   *-LIND                                                           
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
         SPACE 2                                                                
*        ACGENBOTH                                                              
*        ACLFMEQU                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACLFM13S  05/01/02'                                      
         END                                                                    
