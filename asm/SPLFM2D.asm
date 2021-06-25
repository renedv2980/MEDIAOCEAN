*          DATA SET SPLFM2D    AT LEVEL 047 AS OF 05/01/02                      
*PHASE T2192DA                                                                  
         TITLE 'SPLFM2D - NETWORK DEFINITION RECORD'                            
T2192D   CSECT                                                                  
         NMOD1 0,T2192D                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING NDEFRECD,R8                                                      
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         CLI   SVACT,C'A'                                                       
         BE    FMT2                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              REREAD REC                                   
         B     FMT4                                                             
*                                                                               
FMT2     OC    SVKEY+8(2),SVKEY+8    SEE IF ADDING CLT NWK                      
         BZ    EXXMOD                NO                                         
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY        COPY BASE NETWORK                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FMT2A              YES BASE NETWORK RECORD EXISTS                
         LA    R2,LFMKEYH                                                       
         MVI   ERRCD,NOFNDERR                                                   
         B     LFMERR                                                           
*                                                                               
FMT2A    OC    SVKEY+10(1),SVKEY+10  SEE IF ADDING EST NWK                      
         BZ    FMT3               NO - GO AHEAD                                 
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY      COPY CLT NETWORK                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FMT2B              YES CLT NETWORK RECORD EXISTS                 
         MVI   ERRAREA,X'FF'      ERROR - MUST HAVE CLIENT NETWORK              
         XC    LFMMSG,LFMMSG      INORDER TO ADD ESTIMATE NETWORK               
         MVC   LFMMSG(37),=CL37'** CLIENT NETWORK RECORD NOT FOUND **'          
         FOUT  LFMMSGH                                                          
         LA    R2,LFMKEYH                                                       
         OI    6(R2),X'40'        PT CURSOR TO THIS FIELD                       
         XIT1  REGS=(R2)                                                        
*                                                                               
FMT2B    XC    KEY,KEY            RESTORE READ-SEQ BEFORE                       
         MVC   KEY(8),SVKEY       CHECKING EXISTENCE OF CLT NWK REC             
         GOTO1 HIGH                                                             
*                                                                               
FMT3     DS    0H                                                               
         GOTO1 GETREC                                                           
         MVC   REC(13),SVKEY                                                    
         MVC   KEY(13),SVKEY                                                    
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         MVI   SVFMTSW,1           ALTER FORMAT TO EDIT                         
*                                  TO GET 'REC ADDED' MESSAGE                   
*                                                                               
FMT4     DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         FOUT  NWKSTATH,SPACES,4                                                
         FOUT  NWKREGH,SPACES,4                                                 
         FOUT  NWKQHOH,SPACES,4                                                 
         FOUT  NWKSHRH,SPACES,7                                                 
         FOUT  NWKADDAH,SPACES,4                                                
         XC    LFMKEXP,LFMKEXP                                                  
         BAS   RE,PUTSTATS         GO DISPLAY STATIONS                          
         B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              REREAD REC                                   
         B     EDT0D                                                            
*                                                                               
EDT0     OC    SVKEY+8(2),SVKEY+8    SEE IF ADDING A CLT EXECPTION              
         BZ    EDT0D               NO                                           
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY        COPY BASE NETWORK                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EDT0B                                                            
         LA    R2,LFMKEYH                                                       
         MVI   ERRCD,NOFNDERR                                                   
         B     LFMERR                                                           
*                                                                               
EDT0B    DS    0H                                                               
         GOTO1 GETREC                                                           
         MVC   REC(13),SVKEY                                                    
         MVC   KEY(13),SVKEY                                                    
         GOTO1 ADDREC                                                           
         MVC   KEY,SVKEY                                                        
         B     FMT                 GO REFORMAT                                  
EDT0D    LA    R2,NWKSTATH                                                      
         GOTO1 ANY                                                              
         MVC   KEY+2(4),8(R2)                                                   
         CLC   8(4,R2),=C'ZZZZ'       BYPASS READ                               
         BE    EDT1                                                             
         MVC   KEY(17),ZEROS       ATTEMPT TO READ STATION                      
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'T'          MUST BE TV                                   
         MVC   KEY+2(4),NWKSTAT                                                 
         OC    KEY+2(4),SPACES                                                  
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),AGYALPHA                                                
         LA    R8,REC2                                                          
         ST    R8,AREC             READ STATION INTO REC2                       
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 STA                                                              
* NEED TO MAKE SURE STATION IS NOT A NETWORK                                    
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY        MOVE OD11/AGY                                
         MVC   KEY+4(4),NWKSTAT                                                 
         OC    KEY+4(4),SPACES                                                  
         MVI   ERRCD,STNETERR                                                   
         CLC   KEY+4(4),SVKEY+4    COMPARE TO NETWORK IN PROC                   
         BE    LFMERR                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    LFMERR                                                           
*                                                                               
         LA    R8,REC              RESET R8 TO NDEFREC                          
         ST    R8,AREC                                                          
EDT1     LA    R9,ELEM                                                          
         USING NDEFEL01,R9                                                      
         XC    ELEM(20),ELEM                                                    
         MVC   ELEM(2),=X'0110'                                                 
         MVC   NDEFSTA,NWKSTAT                                                  
         OC    NDEFSTA,SPACES                                                   
         LA    R2,NWKREGH                                                       
         CLI   5(R2),0                                                          
         BE    EDT4                                                             
         MVC   NDEFRGN,8(R2)                                                    
*                                                                               
EDT4     LA    R2,NWKQHOH                                                       
         CLI   5(R2),0             NOT INPUT                                    
         BE    EDT6                                                             
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R5)                                         
         MVI   ERRCD,INVERR                                                     
         CLI   DMCB,0                                                           
         BE    EDT4A                                                            
         B     LFMERR                                                           
*                                                                               
EDT4A    L     R5,DMCB+4                                                        
         C     R5,=F'21000'          3.5 HOURS MAX                              
         BH    LFMERR                                                           
         C     R5,=F'-21000'                                                    
         BL    LFMERR                                                           
         CVD   R5,DUB                                                           
         DP    DUB,=P'1500'                                                     
         CP    DUB+5(3),=P'0'      MUST BE 15 MIN MULTIPLES                     
         BNE   LFMERR                                                           
         M     R4,=F'1'                                                         
         D     R4,=F'100'                                                       
         STH   R5,NDEFOSET                                                      
*                                                                               
EDT6     LA    R2,NWKSHRH         COST SHARE  3 DECIMALS                        
         GOTO1 ANY                 REQUIRED                                     
         CLC   8(3,R2),=C'DEL'       TO DELETE STATION                          
         BE    DELELEM                                                          
         CLC   8(2,R2),=C'NB '        NOT BOUGHT                                
         BNE   EDT6C                                                            
         MVC   NDEFPCT,=F'-1'                                                   
         B     EDT7                                                             
*                                                                               
EDT6C    DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(3,NWKSHR),(R5)                                    
         MVI   ERRCD,INVERR                                                     
         CLI   DMCB,0                                                           
         BE    *+8                                                              
         B     LFMERR                                                           
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'0'            CAN'T BE NEGATIVE                            
         BL    LFMERR                                                           
         C     R5,=F'100000'       CAN'T EXCEED 100 PERCENT                     
         BH    LFMERR                                                           
         MVC   NDEFPCT,DMCB+4                                                   
*                                                                               
EDT7     LA    R2,NWKADDAH                                                      
         XC    ADDASTA,ADDASTA                                                  
         CLI   5(R2),0             NO INPUT                                     
         BE    EDT8                                                             
         CLC   8(4,R2),=C'LAST'    ADD AS LAST ELEM                             
         BNE   EDT7B                                                            
         MVI   ADDASTA,X'FF'                                                    
         B     EDT8                                                             
*                                                                               
EDT7B    CLC   8(4,R2),=C'FRST'      ADD AS FIRST ELEM                          
         BNE   EDT7C                                                            
         MVI   ADDASTA,X'01'                                                    
         B     EDT8                                                             
*                                                                               
EDT7C    MVC   ADDASTA,8(R2)                                                    
         OC    ADDASTA,SPACES                                                   
         B     EDT8                                                             
*                                                                               
EDT8     DS    0H                  USED TO CHK IF STATION WAS IN                
*                                  ANOTHER NETWORK                              
*                                  NOW ADD ELEM TO RECORD                       
*                                  OR REPLACE ON MATCHING STATIONS              
EDT22    LA    R2,REC+24                                                        
         MVI   ELCODE,X'01'                                                     
         CLI   0(R2),X'01'                                                      
         BE    EDT24                                                            
         CLI   0(R2),0             IF REC HAD NO ELEMS                          
         BNE   EDT23                                                            
         MVC   NDEFLEN,=H'24'      MUST SET LEN FOR RECUP                       
         B     ADDEL                                                            
*                                                                               
EDT23    BAS   RE,NEXTEL                                                        
         BNE   EDT30                                                            
EDT24    CLC   2(4,R2),NDEFSTA                                                  
         BNE   EDT23                                                            
*                                  STATION FOUND - CHECK FOR ADD AFTER          
         OC    ADDASTA,ADDASTA                                                  
         BNZ   EDT26               POSTION CHANGE                               
         MVC   0(ELLEN,R2),ELEM    ON MATCH I CAN SWITCH ELEMS                  
         B     WRITE                                                            
*                                                                               
EDT26    DS    0H                                                               
         GOTO1 VRECUP,DMCB,(0,REC),(R2)                                         
*                                  NEED TO DELETE OLD ELEM                      
         B     EDT30               THEN ADD NEW ONE                             
*                                                                               
EDT30    DS    0H                                                               
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'01'                                                     
         CLI   ADDASTA,X'01'       SEE IF ADDING AS FIRST ELEM                  
         BNE   EDT32               NO                                           
         B     ADDEL               YES - R2 IS SET                              
*                                                                               
EDT32    CLI   ADDASTA,X'FF'       SEE IF ADDING AS LAST ELEM                   
         BNE   EDT34               NO                                           
         MVI   ELCODE,X'01'                                                     
EDT33    BAS   RE,NEXTEL           GETS ME TO END OF RECORD                     
         BNE   ADDEL                                                            
         B     EDT33                                                            
*                                                                               
EDT34    CLI   0(R2),X'01'                                                      
         BE    EDT36                                                            
EDT35    BAS   RE,NEXTEL                                                        
         BNE   ADDERR                                                           
EDT36    CLC   2(4,R2),ADDASTA                                                  
         BNE   EDT35                                                            
         ZIC   R0,1(R2)            NEED TO GET PAST ADD AFTER ELEM              
         AR    R2,R0                                                            
         B     ADDEL               FOUND ADD AFTER STATION                      
*                                  GO ADD ELEM                                  
*                                                                               
ADDERR   LA    R2,NWKADDAH                                                      
         MVI   ERRCD,NOFNDERR         STATION NOT IN NETWORK                    
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
*                                                                               
ADDEL    GOTO1 VRECUP,DMCB,(0,REC),ELEM,(R2)                                    
         B     WRITE                                                            
*                                                                               
*                                                                               
DELELEM  CLI   SVACT,C'A'                                                       
         BNE   DEL2                                                             
         MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
DEL2     LA    R2,REC+24                                                        
         MVI   ELCODE,X'01'                                                     
         CLI   0(R2),X'01'                                                      
         BE    DEL4                                                             
         CLI   0(R2),0                                                          
         BE    DERROR                                                           
*                                                                               
DEL3     BAS   RE,NEXTEL                                                        
         BNE   DERROR                                                           
DEL4     CLC   2(4,R2),NDEFSTA                                                  
         BNE   DEL3                                                             
         GOTO1 VRECUP,DMCB,(0,REC),(R2)                                         
         B     WRITE                                                            
*                                                                               
DERROR   LA    R2,NWKSTATH         SET CURSOR TO STATION                        
         MVI   ERRCD,NOFNDERR                                                   
         B     LFMERR                                                           
*                                                                               
WRITE    DS    0H                                                               
         OC    SVKEY+8(2),SVKEY+8  SEE IF CLT NETWORK                           
         BNZ   WRITE5              YES - BYPASS 02 ELEM ADD                     
         BAS   RE,ADD02                                                         
*                                                                               
WRITE5   MVC   KEY,SVKEY                                                        
         MVC   REC(13),SVKEY                                                    
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    ADDNDEF                                                          
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC              REREAD REC                                   
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
         B     REQREC         GO GENERATE TURNAROUND                            
*                                                                               
ADDNDEF  GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     REQREC         GO GENERATE TURNAROUND                            
*                                                                               
         EJECT                                                                  
REQREC   DS    0H                                                               
         XC    REC(110),REC                                                     
         MVI   REC+10,44                                                        
         MVI   REC+14,106                                                       
         MVI   REC+26,X'40'                                                     
         MVC   REC+27(79),REC+26                                                
         MVC   REC+26(2),=C'44'                                                 
         MVC   REC+28(2),AGYALPHA                                               
         MVC   REC+30(1),SVEBCMED                                               
         MVC   REC+31(3),=C'ALL'                                                
         OC    SVCLT,SVCLT                                                      
         BZ    *+10                                                             
         MVC   REC+31(3),SVEBCCLT                                               
         MVC   REC+40(4),SVKEY+4           NETWORK IN MKT                       
         CLI   SVKEY+10,0                SEE IF ESTIMATE EXCEPTION              
         BE    REQREC5                                                          
         EDIT  (B1,SVKEY+10),(3,REC+49),0,FILL=0                                
*                                                                               
REQREC5  MVI   REC+87,C'N'                                                      
         MVC   REC+94(7),=C'CONTROL'                                            
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                      
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     FMT4                                                             
*              GO REDISPLAY RECORD                                              
         DROP  R9                                                               
*                                                                               
         EJECT                                                                  
PUTSTATS DS    0H                                                               
         NTR1                                                                   
         LA    R5,NWKSTA1H                                                      
         USING DISFLDD,R5                                                       
         LA    R6,STANUM           SET R6 FOR BCT                               
         XC    TOTAL,TOTAL                                                      
         LA    R2,REC+24                                                        
         USING NDEFEL01,R2                                                      
         MVI   ELCODE,X'01'                                                     
         CLI   0(R2),X'01'                                                      
         BE    PUTS4                                                            
         CLI   0(R2),0             NO ELEMS                                     
         BE    PUTS5               GO CLEAR REST OF SCREEN                      
*                                                                               
PUTS2    BAS   RE,NEXTEL                                                        
         BNE   PUTS5                                                            
PUTS4    MVC   DISSTA,NDEFSTA                                                   
         MVC   DISREG,NDEFRGN                                                   
         EDIT  NDEFOSET,(4,DISOSET),0,FLOAT=-,ZERO=BLANK                        
         CLC   NDEFPCT,=F'-1'         NOT BOUGHT                                
         BNE   PUTS4B                                                           
         MVC   DISSHR(6),=CL6'NB'                                               
         B     PUTS4C                                                           
*                                                                               
PUTS4B   DS    0H                                                               
         EDIT  NDEFPCT,(6,DISSHR),3                                             
PUTS4C   FOUT  (R5)                                                             
         CLC   NDEFPCT,=F'-1'             NOT BOUGHT                            
         BE    PUTS4D                                                           
         L     R0,TOTAL                                                         
         MVC   FULL,NDEFPCT                                                     
         A     R0,FULL                                                          
         ST    R0,TOTAL                                                         
PUTS4D   LA    R5,FLDNUM(R5)       BUMP TO NEXT FLD                             
         BCT   R6,PUTS2                                                         
*                                                                               
*                                  TOO MANY STATIONS TO DISPLAY                 
*                                  BUT STILL NEED TO TOTAL SHARES               
PUTS4E   BAS   RE,NEXTEL                                                        
         BNE   PUTS10                                                           
         CLC   NDEFPCT,=F'-1'      BYPASS NOT BOUGHT STATIONS                   
         BE    PUTS4E                                                           
         L     R0,TOTAL                                                         
         MVC   FULL,NDEFPCT                                                     
         A     R0,FULL                                                          
         ST    R0,TOTAL                                                         
         B     PUTS4E                                                           
*                                                                               
PUTS5    DS    0H                                                               
         LTR   R6,R6                                                            
         BZ    PUTS10                                                           
PUTS8    OC    8(FLDLEN,R5),8(R5)                                               
         BZ    PUTS9                                                            
         XC    8(FLDLEN,R5),8(R5)                                               
         FOUT  (R5)                                                             
PUTS9    LA    R5,FLDNUM(R5)                                                    
         BCT   R6,PUTS8                                                         
*                                                                               
PUTS10   MVC   LFMKEXP(6),=C'TOTAL='                                            
         EDIT  TOTAL,(7,WORK2),3                                                
         MVC   LFMKEXP+6(7),WORK2                                               
         FOUT  LFMKEXPH                                                         
*                                                                               
PUTSX    XIT1                                                                   
         EJECT                                                                  
ADD02    NTR1                                                                   
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'02'            SEE IF 02 ELEM EXISTS                    
         BAS   RE,NEXTEL1                                                       
         BE    ADD02X                                                           
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    ADD02A                                                           
*                                  IF NOT AN ADD 02 ELEM MUST EXIST             
         DC    H'0'                NO 02 ELEM FOUND                             
*                                                                               
ADD02A   DS    0H                                                               
         LA    R1,STAWRK                                                        
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'V'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         LA    RF,27               OLD MAX NETWORKS                             
         CLI   STAPVRSN,C'N'                                                    
         BNE   *+8                                                              
         LA    RF,63               NEW MAX NETWORKS                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    RE,5                LOWEST NTWK NUMBER                           
         LA    R1,ELEM                                                          
ADD02A1  STC   RE,0(R1)                                                         
         LA    R1,1(R1)            NEXT SLOT IN TABLE                           
         LA    RE,1(RE)            NEXT NUMBER                                  
         BCT   RF,ADD02A1                                                       
         MVI   0(R1),X'FF'         SET E-O-T FLAG                               
*                                                                               
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY                                                     
         GOTO1 HIGH                                                             
         B     ADD02C                                                           
ADD02B   GOTO1 SEQ                                                              
ADD02C   CLC   KEY(4),KEYSAVE      CHK AGENCY                                   
         BNE   ADD02H                                                           
         CLC   KEY(8),SVKEY        BYPASS THIS NETWORK                          
         BE    ADD02B                                                           
         OC    KEY+8(2),KEY+8      SEE IF CLT NETWORK                           
         BNZ   ADD02B              YES - BYPASS                                 
         GOTO1 GETREC                                                           
         LA    R2,REC2+24                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL1                                                       
         BE    *+6                                                              
         DC    H'0'                UNASSIGNED NETWORK                           
         LA    R5,ELEM                                                          
ADD02E   CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID CODE                                 
         CLC   0(1,R5),2(R2)       MATCH CODES                                  
         BE    ADD02F                                                           
         LA    R5,1(R5)                                                         
         B     ADD02E                                                           
*                                                                               
ADD02F   MVI   0(R5),0             ZERO ENTRY IN TABLE                          
         B     ADD02B              GO DO NEXT NETWORK                           
*                                                                               
ADD02H   LA    R5,ELEM                                                          
*                                                                               
ADD02I   CLI   0(R5),X'FF'                                                      
         BE    NOMORE                                                           
         CLI   0(R5),0                                                          
         BNE   ADD02J              FIND LOWEST AVAILABLE CODE                   
         LA    R5,1(R5)                                                         
         B     ADD02I                                                           
*                                                                               
ADD02J   DS    0H                                                               
         ZIC   R0,0(R5)            SAVE NUMBER                                  
         XC    ELEM(10),ELEM                                                    
         MVC   ELEM(2),=X'0203'                                                 
         STC   R0,ELEM+2           SET NUMBER IN 02 ELEM                        
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL1                                                       
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T FIND 02 ELEM                       
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,(R2)                                    
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
ADD02X   XIT1                                                                   
*                                                                               
NOMORE  FOUT  LFMMSGH,=C'** MAX NETWORKS EXCEEDED - CONTACT HDS **',41          
         L     RD,BASERD                                                        
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
NEXTEL1  CLI   0(R2),0             END OF REC                                   
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         EJECT                                                                  
LFMERR   GOTO1 ERROR                                                            
*                                                                               
STANUM   EQU   48                  NUMBER OF STATIONS I CAN DISPLAY             
FLDLEN   EQU   23                  LENGTH OF DISPLAY FLD                        
FLDNUM   EQU   31                  23 + 8 HEADER                                
ELLEN    EQU   16                  ELEMENT LENGTH                               
*                                                                               
TOTAL    DS    F                   FOR SHARE TOTAL                              
ADDASTA  DS    CL4                                                              
ZEROS    DC    20C'0'                                                           
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMED                                                                        
       ++INCLUDE SPLFMEDD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENNDEF                                                      
*                                                                               
         EJECT                                                                  
DISFLDD  DSECT                                                                  
DISHEAD  DS    CL8                                                              
DISFLD   DS    0CL23                                                            
DISSTA   DS    CL4                                                              
         DS    CL1                                                              
DISREG   DS    CL4                                                              
         DS    CL2                                                              
DISOSET  DS    CL4                                                              
         DS    CL2                                                              
DISSHR   DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPLFM2D   05/01/02'                                      
         END                                                                    
