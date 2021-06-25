*          DATA SET SRVIO00    AT LEVEL 002 AS OF 11/14/14                      
*PHASE T15500A                                                                  
         TITLE '$VIOLATE - DISPLAY TERMINAL VIOLATION DATA'                     
VIOLATE  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**$VIO**                                                   
         USING WORKD,RC                                                         
         MVC   SRPARS,0(R1)        MOVE CALLING PARAMS                          
         L     R3,SRPAR6           R3=A(TWA)                                    
         USING SRVIOFFD,R3                                                      
         LA    R8,IOAREA                                                        
         USING SRCONFFD,R8                                                      
         L     RA,SRPAR1           RA=A(SYSFAC)                                 
         USING SYSFACD,RA                                                       
         L     RE,VSSB                                                          
         MVC   TSRLEN,SSBTWAL-SSBD(RE)                                          
         MVC   FACNAM,SSBSYSN4-SSBD(RE)                                         
         MVC   DAYNO,SSBDAYNO-SSBD(RE)                                          
         MVC   DATEB,SSBDATEB-SSBD(RE)                                          
         LLC   R0,SSBSECV-SSBD(RE)                                              
         STH   R0,VIOCNT           SET NUMBER OF VIOLATES TODAY                 
         XC    INTERM,INTERM       SET DEFAULT START TERMINAL                   
         ZAP   INTIME,=P'0'        SET DEFAULT START TIME                       
         MVC   INDAYNO,DAYNO       SET DEFAULT DAY NUMBER                       
         MVI   MODE,0              SET DEFAULT MODE                             
         EJECT                                                                  
***********************************************************************         
* SR FIELD CAN BE =VIOLATE,X WHERE X CAN BE A=ALL,T=TODAY,OR U=UTL    *         
* P1 CAN BE SET TO ALL,TOD,OR DAY VALUE FOR ADRFILE SCAN              *         
* P1 CAN BE SET TO DAY,HH:MM TO SPECIFY A DAY AND START TIME FOR SCAN *         
* IF NOT A DAY VALUE P1 CAN BE A LUID VALUE FOR UTL SCAN              *         
***********************************************************************         
VIO1     LA    R2,SRVP1H           TEST VALUE ENTERED IN P1                     
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   VIO2                                                             
         CLC   SRVID+8(2),=C',A'   TEST =VIOLATE,A FOR ADRFILE ALL              
         BNE   *+12                                                             
         MVI   INDAYNO,0           SET ALL DAYS                                 
         B     VIO30                                                            
         CLC   SRVID+8(2),=C',T'   TEST =VIOLATE,T FOR ADRFILE TODAY            
         BNE   *+14                                                             
         MVC   INDAYNO,DAYNO       SET TODAY                                    
         B     VIO30                                                            
         CLC   SRVID+8(2),=C',U'   TEST =VIOLATE,U FOR UTL MODE                 
         BE    VIO10                                                            
         B     VIO10               DEFAULT IS UTL MODE                          
*                                                                               
VIO2     CLC   FHDA(3),=C'ALL'     SHOW ALL VIOLATES FROM ADRFILE               
         BNE   *+12                                                             
         MVI   INDAYNO,0           SET ALL DAYS                                 
         B     VIO4                                                             
         CLC   FHDA(3),=C'TOD'     SHOW TODAYS VIOLATES FROM ADRFILE            
         BNE   *+14                                                             
         MVC   INDAYNO,DAYNO       SET TODAY                                    
         B     VIO4                                                             
         CLC   FHDA(3),=C'UTL'     SHOW VIOLATIONS FROM UTL                     
         BE    VIO10                                                            
*                                                                               
         LA    RE,DAYTAB           SEARCH DAY OF WEEK TABLE                     
VIO3     CLI   0(RE),X'FF'                                                      
         BE    VIO9                                                             
         CLC   FHDA(3),1(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'DAYTAB(RE)                                                  
         B     VIO3                                                             
         MVC   INDAYNO,0(RE)       SAVE INPUT DAY NUMBER                        
*                                                                               
VIO4     CLI   FHIL,3              TEST IF IT LOOKS LIKE DAY,HH:MM              
         BE    VIO30                                                            
         CLI   FHDA+3,C','                                                      
         BNE   VIO30                                                            
         CLI   FHIL,9                                                           
         BNE   ERROR2                                                           
         LA    RE,FHDA+4           RE=A(TIME)                                   
         CLI   2(RE),C':'                                                       
         BE    VIO5                                                             
         CLI   2(RE),C'.'                                                       
         BNE   ERROR2                                                           
*                                                                               
VIO5     CLC   0(2,RE),=C'00'      VALIDATE TIME HH:MM OR HH.MM                 
         BL    ERROR2                                                           
         CLC   0(2,RE),=C'23'                                                   
         BH    ERROR2                                                           
         CLC   3(2,RE),=C'00'                                                   
         BL    ERROR2                                                           
         CLC   3(2,RE),=C'59'                                                   
         BH    ERROR2                                                           
         MVC   WORK+0(2),0(RE)     WORK=C'HHMM00'                               
         MVC   WORK+2(2),3(RE)                                                  
         MVC   WORK+4(2),=C'00'                                                 
         PACK  INTIME,WORK(6)                                                   
         B     VIO30                                                            
*                                                                               
VIO9     L     RF,SRPAR4           VALIDATE TERMINAL NUMBER OR LUID             
         L     RF,CTERMVAL-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2)                                                   
         ICM   R5,15,4(R1)         R5=A(FIRST UTL ENTRY)                        
         BZ    ERROR1                                                           
         ST    R5,INTERM                                                        
         EJECT                                                                  
***********************************************************************         
* SCAN UTL FOR ENTRIES THAT ARE FLAGGED AS HAVING A VIOLATION         *         
***********************************************************************         
VIO10    MVI   MODE,0              SET UTL DISPLAY MODE                         
         SAM31                                                                  
         L     R5,VUTL                                                          
         USING UTLD,R5                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         AHI   R5,6                R5=A(FIRST UTL ENTRY)                        
         OC    INTERM,INTERM                                                    
         BZ    VIO11                                                            
         L     R5,INTERM                                                        
*                                                                               
VIO11    LA    R2,SRVLN1H          R2=A(FIRST DISPLAY LINE)                     
         USING VIOLINED,R2                                                      
*                                                                               
VIO12    MVI   VIONUM,0            SET VIOLATION NUMBER AND STATUS              
         MVI   VIOSTA,1                                                         
         CLI   TSSVNUM,0           TEST SUSPECT SECURITY VIOLATION NUM          
         BE    VIO20                                                            
         MVC   VIONUM,TSSVNUM                                                   
         MVI   VIOSTA,0                                                         
         TM    VIONUM,X'80'        TEST IF CURRENTLY ACTIVE                     
         BZ    *+8                                                              
         MVI   VIOSTA,1                                                         
         NI    VIONUM,255-X'F0'                                                 
*                                                                               
VIO14    MVC   VIODAY(2),=C'..'    TERMINAL DATE                                
         CLC   TDATEB,DATEB                                                     
         BNE   VIO15                                                            
         LLC   RE,DAYNO            SET TODAY                                    
         SLL   RE,2                                                             
         LA    RE,DAYTAB(RE)                                                    
         MVC   VIODAY(2),1(RE)                                                  
*                                                                               
VIO15    LH    R0,TNUM             TERMINAL NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  VIOTERM,DUB                                                      
*                                                                               
         MVC   VIOLUID,TSYM        TERMINAL LUID                                
*                                                                               
         LLC   RE,VIONUM           VIOLATION REASON                             
         MHI   RE,L'VIOTXT                                                      
         LA    RE,VIOTXT(RE)                                                    
         MVC   VIOWHY,0(RE)                                                     
*                                                                               
         LH    R0,TNUM             READ TWA1 FOR CONNECT DATA                   
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TSRLEN                                                
         XC    SRCONFFD(255),SRCONFFD                                           
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(1,(R0)),SRCONFFD,TO24=Y            
         SAM31                                                                  
*                                                                               
         LA    R4,WORK             CHECK IF VALID CONNECT SCREEN                
         MVC   WORK,SPACES                                                      
         CLI   CNTSREQH,L'CNTSREQ+8                                             
         BNE   VIO18                                                            
         CLI   CNTIDH,L'CNTID+8                                                 
         BNE   VIO18                                                            
         CLI   CNTSYSH,L'CNTSYS+8                                               
         BNE   VIO18                                                            
         CLI   CNTPGMH,L'CNTPGM+8                                               
         BNE   VIO18                                                            
*                                                                               
         LA    R1,CNTID            USER ID                                      
         LA    RF,L'CNTID                                                       
         BRAS  RE,SETDATA                                                       
*                                                                               
         LA    R1,CNTSYS           SYSTEM                                       
         LA    RF,L'CNTSYS                                                      
         BRAS  RE,SETDATA                                                       
*                                                                               
         LA    R1,CNTPGM           PROGRAM                                      
         LA    RF,L'CNTPGM                                                      
         BRAS  RE,SETDATA                                                       
*                                                                               
VIO16    MVC   PWD,CNTPWD          PASSWORD                                     
         LA    R1,PWD                                                           
         LA    RF,L'PWD                                                         
         MVI   BYTE,0                                                           
         CLC   SRVP2(8),=C'WHATISIT'                                            
         BNE   *+8                                                              
         OI    BYTE,X'01'                                                       
VIO16A   CLI   0(R1),C' '          MASK OUT PASSWORD WITH STARS                 
         BNH   VIO16B                                                           
         OI    BYTE,X'80'          SET PASSWORD PRESENT                         
         TM    BYTE,X'01'                                                       
         BO    *+8                                                              
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         BCT   RF,VIO16A                                                        
VIO16B   TM    BYTE,X'80'          TEST PASSWORD PRESENT                        
         BZ    VIO17                                                            
         LA    R1,PWD                                                           
         LA    RF,L'PWD                                                         
         BRAS  RE,SETDATA                                                       
*                                                                               
VIO17    AHI   R4,-1               REMOVE TRAILING DELIMITER                    
         MVI   0(R4),C' '                                                       
         B     VIO19                                                            
*                                                                               
VIO18    MVC   WORK(20),=C'Unknown connect data'                                
         CLI   VIONUM,TSSVRGTM     SET BY SRPWD00 ON PASSWORD FAILURE           
         BNE   *+10                                                             
         MVC   WORK(37),=C'Failed to input password on reconnect'               
*                                                                               
VIO19    MVC   VIODATA,WORK                                                     
         OI    VIOHDR+6,FHOITR     TRANSMIT LINE                                
         CLI   VIOSTA,1                                                         
         BNE   *+8                                                              
         OI    VIOHDR+6,FHOIHI     SET HIGH INTENSITY IF ACTIVE                 
*                                                                               
         LA    R2,VIOLINEL(R2)     BUMP TO NEXT LINE                            
         CLI   VIOHDR,VIOLINEL                                                  
         BL    OKEXIT              EXIT IF END OF TWA                           
*                                                                               
VIO20    BXLE  R5,R6,VIO12                                                      
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* READ ADRFILE LOOKING FOT $CT VIOLATION ENTRIES                      *         
***********************************************************************         
VIO30    MVI   MODE,1              SET ADRFILE DISPLAY MODE                     
         OC    VIOCNT,VIOCNT                                                    
         BNZ   *+14                                                             
         CLC   INDAYNO,DAYNO       EXIT IF NO VIOLATES FOR TODAY                
         BE    OKEXIT                                                           
         SAM24                                                                  
         MVC   SRVHL1+3(5),=C'HH.MM' CHANGE HEADLINE TO SHOW TIME               
         L     RE,SRPAR3                                                        
         OI    TSTATU-UTLD(RE),TSTATMIO SET CAN EXCEED MAXIOS                   
*                                                                               
VIO31    LA    R2,SRVLN1H          R2=A(FIRST DISPLAY LINE)                     
         USING VIOLINED,R2                                                      
         L     RF,VADRBUFF         COPY ADRFILE BUFFER HDR TO IOAREA            
         AHI   RF,-18                                                           
         LA    RE,IOAREA                                                        
         AHI   RE,-18                                                           
         MVC   0(18,RE),0(RF)                                                   
*                                                                               
VIO32    MVC   DSKADR,=X'00000000' SET TO READ SEQ FIRST ADRFILE BLOCK          
         LA    RE,IOAREA                                                        
         ST    RE,IOANEXT          SET A(FIRST REC) IN IOAREA HEADER            
         AH    RE,IOALENB                                                       
         ST    RE,IOALAST          SET A(END OF IOAREA)                         
         MVC   0(8,RE),=C'*TRKSTR*'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ATRKB            SET A(FULL TRACK BUFFER)                     
         A     RE,=AL4(TRKLEN)                                                  
         MVC   0(8,RE),=C'*TRKEND*'                                             
         L     R0,ATRKB                                                         
         L     R1,=AL4(TRKLEN)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR FULL TRACK BUFFER                      
         XC    ADRCNT,ADRCNT                                                    
         MVI   EOFFLAG,0                                                        
         LA    R0,TRKOPT           SET FULL TRACK READ OPTION                   
         GOTO1 VDATAMGR,DMCB,((R0),DMRSEQ),ADRFILE,DSKADR,IOAREA,ATRKB          
         TM    8(R1),X'80'                                                      
         BO    VIO34               END OF FILE                                  
         CLI   8(R1),0                                                          
         BE    VIO36                                                            
         B     OKEXIT                                                           
*                                                                               
VIO33    LA    R0,TRKOPT           SET FULL TRACK READ OPTION                   
         GOTO1 VDATAMGR,DMCB,((R0),DMRSEQ),ADRFILE,DSKADR,IOAREA,ATRKB          
         TM    8(R1),X'80'                                                      
         BO    VIO34               END OF FILE                                  
         CLI   8(R1),0                                                          
         BE    VIO36                                                            
         B     OKEXIT                                                           
*                                                                               
VIO34    MVI   EOFFLAG,1           SET EOF AND COPY ADRREC TO IOAREA            
         L     R0,VADRBUFF                                                      
         LH    R1,IOALENB                                                       
         LA    RE,IOAREA                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         LA    RE,IOAREA                                                        
         ST    RE,IOANEXT          SET A(FIRST ENTRY)                           
         L     R1,VADRBUFF                                                      
         AHI   R1,-12                                                           
         L     R1,0(R1)            R1=A(NEXT ENTRY IN ADRBUFF)                  
         S     R1,VADRBUFF                                                      
         AR    RE,R1                                                            
         C     RE,IOANEXT                                                       
         BL    OKEXIT                                                           
         ST    RE,IOALAST          SET A(LAST ENTRY)                            
*                                                                               
VIO36    L     R7,IOANEXT          R7=A(ADRFILE RECORD)                         
         USING LOGRECD,R7                                                       
*                                                                               
VIO37    CLC   LOGID(3),=C'$CT'    TEST VIOLATION LOG RECORD                    
         BNE   VIO38                                                            
         CLI   LOGDAYNO,1          TEST IF DAY IS DEFINED IN RECORD             
         BL    VIO37B                                                           
         CLI   LOGDAYNO,7                                                       
         BH    VIO37B                                                           
         CLI   INDAYNO,0           TEST IF DAY INPUT IS ZERO                    
         BE    VIO40                                                            
         CLC   LOGDAYNO,INDAYNO    TEST IF DAY MATCHES                          
         BNE   VIO38                                                            
VIO37A   CP    LOGTIME,INTIME      TEST TIME WITH INPUT TIME                    
         BNL   VIO40                                                            
         B     VIO38                                                            
VIO37B   CLI   INDAYNO,0           SHOW NO DAY ENTRIES IF ALL DAYS              
         BE    VIO40                                                            
*                                                                               
VIO38    AH    R7,IOALENR          BUMP TO NEXT RECORD IN BLOCK                 
         C     R7,IOALAST                                                       
         BL    VIO37                                                            
         CLI   EOFFLAG,0           EXIT IF EOF FLAG SET                         
         BE    VIO33                                                            
         B     OKEXIT                                                           
*                                                                               
VIO40    LH    R0,ADRCNT           BUMP VIOLATION LOG RECORD COUNT              
         AHI   R0,1                                                             
         STH   R0,ADRCNT                                                        
         MVC   VIONUM,LOGID+3      SET VIOLATION NUMBER AND STATUS              
         NI    VIONUM,X'0F'                                                     
         MVI   VIOSTA,0                                                         
         LA    R4,WORK             R4=A(AREA TO BUILD CONNECT DATA)             
         MVC   WORK,SPACES                                                      
*                                                                               
         LLC   RE,LOGDAYNO         EVENT DAY                                    
         SLL   RE,2                                                             
         LA    RE,DAYTAB(RE)                                                    
         MVC   VIODAY(2),1(RE)                                                  
         MVC   LASTDAY,1(RE)                                                    
*                                                                               
         OI    LOGTIME+3,X'0F'     EVENT TIME P'0HHMMSSF'                       
         UNPK  DUB,LOGTIME                                                      
         MVC   VIOTERM+0(2),DUB+2  HH                                           
         MVI   VIOTERM+2,C':'                                                   
         MVC   VIOTERM+3(2),DUB+4  MM                                           
         MVC   LASTTIME,VIOTERM                                                 
*                                                                               
         MVC   VIOLUID,LOGLUID     TERMINAL LUID                                
*                                                                               
         LLC   RE,VIONUM           VIOLATION REASON                             
         MHI   RE,L'VIOTXT                                                      
         LA    RE,VIOTXT(RE)                                                    
         MVC   VIOWHY,0(RE)                                                     
*                                                                               
         LA    R1,LOGTEXT+00       USER ID                                      
         LHI   RF,16                                                            
         BRAS  RE,SETDATA                                                       
*                                                                               
         LA    R1,LOGTEXT+16       SYSTEM                                       
         LHI   RF,16                                                            
         BRAS  RE,SETDATA                                                       
*                                                                               
         LA    R1,LOGTEXT+32       PROGRAM                                      
         LA    RF,16                                                            
         BRAS  RE,SETDATA                                                       
*                                                                               
VIO41    MVC   PWD,LOGPWD          PASSWORD                                     
         LA    R1,PWD                                                           
         LA    RF,L'PWD                                                         
         MVI   BYTE,0                                                           
         CLC   SRVP2(8),=C'WHATISIT'                                            
         BNE   *+8                                                              
         OI    BYTE,X'01'                                                       
VIO41A   CLI   0(R1),C' '          MASK OUT PASSWORD WITH STARS                 
         BNH   VIO41B                                                           
         OI    BYTE,X'80'          SET PASSWORD PRESENT                         
         TM    BYTE,X'01'                                                       
         BO    *+8                                                              
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         BCT   RF,VIO41A                                                        
VIO41B   TM    BYTE,X'80'          TEST PASSWORD PRESENT                        
         BZ    VIO42                                                            
         LA    R1,PWD                                                           
         LA    RF,L'PWD                                                         
         BRAS  RE,SETDATA                                                       
*                                                                               
VIO42    BCTR  R4,0                REMOVE TRAILING DELIMITER                    
         MVI   0(R4),C' '                                                       
*                                                                               
VIO43    MVC   VIODATA,WORK        USERID/SYSTEM/PROGRAM/PASSWORD               
         OI    VIOHDR+6,FHOITR     TRANSMIT LINE                                
         CLI   VIOSTA,1                                                         
         BNE   *+8                                                              
         OI    VIOHDR+6,FHOIHI     SET HIGH INTENSITY IF ACTIVE                 
*                                                                               
         LA    R2,VIOLINEL(R2)     BUMP TO NEXT LINE                            
         CLI   VIOHDR,VIOLINEL                                                  
         BL    VIO44               TEST IF END OF SCREEN                        
         CLC   ADRCNT,VIOCNT                                                    
         BNE   VIO38                                                            
         CLC   INDAYNO,DAYNO                                                    
         BNE   VIO38                                                            
         CLC   SRVP3(3),=C'ALL'    TEST TO READ ALL OF ADRFILE                  
         BE    VIO38                                                            
         B     OKEXIT              EXIT IF LAST VIOLATION TODAY                 
*                                                                               
VIO44    LA    R2,SRVP1H           END OF SCREEN                                
         XC    SRVP1,SRVP1                                                      
         MVC   SRVP1(3),LASTDAY    SET DAY,TIME OF LAST ENTRY IN SRVP1          
         MVI   SRVP1+3,C','                                                     
         MVC   SRVP1+4(5),LASTTIME                                              
         MVI   SRVP1H+5,9                                                       
         OI    SRVP1H+6,FHOITR                                                  
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO MOVE CONNECT SCREEN DATA TO DISPLAY LINE              *         
***********************************************************************         
SETDATA  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)                                                    
         BNZ   *+10                                                             
         MVC   0(8,R4),=C'NO INPUT'                                             
*                                                                               
         LA    R4,0(R4,RF)                                                      
SETD02   CLI   0(R4),C' '                                                       
         BH    SETD04                                                           
         BCTR  R4,0                                                             
         BCT   RF,SETD02                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         MVI   0(R4),C'?'                                                       
*                                                                               
SETD04   MVI   1(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXIT AND OK EXIT                                              *         
***********************************************************************         
         USING FHD,R2                                                           
ERROR1   MVC   SRVMSG(L'ERR1),ERR1                                              
         B     ERRORX                                                           
ERROR2   MVC   SRVMSG(L'ERR2),ERR2                                              
*                                                                               
ERRORX   SAM24                                                                  
         NI    SRVIDH+6,255-FHOICU UNSET CURSOR                                 
         OI    FHOI,FHOICU         INSERT CURSOR AT ERROR                       
         B     EXIT                                                             
*                                                                               
OKEXIT   SAM24                                                                  
         XC    SRVMSG,SRVMSG       MOVE IN FACPAK SYSTEM NAME                   
         LA    RE,SRVMSG                                                        
         MVI   0(RE),C'('                                                       
         MVC   1(4,RE),FACNAM                                                   
         AHI   RE,4                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C')'                                                       
         AHI   RE,3                                                             
         XR    R0,R0                                                            
         ICM   R0,3,VIOCNT                                                      
         BNZ   *+14                                                             
         MVC   0(22,RE),=C'No Security Violations'                              
         B     EXIT                                                             
         MVC   0(23,RE),=C'xxx Security Violations'                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,RE),DUB+5(3)                                                 
         CHI   R0,1                                                             
         BNE   *+8                                                              
         MVI   22(RE),C' '                                                      
*                                                                               
         LA    R2,SRVP1H           TEST PARAMETER ENTERED                       
         CLI   FHIL,0                                                           
         BE    EXIT                                                             
         NI    SRVIDH+6,255-FHOICU UNSET CURSOR                                 
         LA    R2,SRVP2H                                                        
         OI    FHOI,FHOICU         INSERT CURSOR AT NEXT FIELD                  
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* EQUATES, LITERALS AND CONSTANTS                                     *         
***********************************************************************         
*&&UK                                                                           
TRKOPT   EQU   X'00'               NORMAL READ                                  
TRKLEN   EQU   18432               AREA FOR READING TWA1                        
*&&                                                                             
*&&US                                                                           
TRKOPT   EQU   X'10'               FULL TRACK READ                              
TRKLEN   EQU   57334               AREA FOR FULL TRACK                          
*&&                                                                             
         LTORG                                                                  
*                                                                               
DAYTAB   DS    0CL4                                                             
         DC    X'00',C'ALL'                                                     
         DC    X'01',C'MON'                                                     
         DC    X'02',C'TUE'                                                     
         DC    X'03',C'WED'                                                     
         DC    X'04',C'THU'                                                     
         DC    X'05',C'FRI'                                                     
         DC    X'06',C'SAT'                                                     
         DC    X'07',C'SUN'                                                     
         DC    X'FF',C'...'                                                     
*                                                                               
VIOTXT   DS    0CL3                                                             
         DC    C'Vio'              0=VIOLATE BIT IS ON                          
         DC    C'C=6'              1=CONNECT NON PID 6 ATTEMPTS                 
         DC    C'Pid'              2=CONNECT PID LOCKED                         
         DC    C'>50'              3=CONNECT MORE THAN 50 ATTEMPTS              
         DC    C'R=3'              4=RECONNECT PASSWORD 3 ATTEMPTS              
         DC    C'Pix'              5=CONNECT PID NOT LOCKED                     
*                                                                               
ERR1     DC    CL60'ED/0002 Invalid terminal number or LUID'                    
ERR2     DC    CL60'ED/0002 Format is DAY or DAY,HH:MM'                         
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDIR   DC    CL8'DMRDIR  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
ADRFILE  DC    CL8'ADRFILE '                                                    
SPACES   DC    256C' '                                                          
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WORKD    DSECT                                                                  
SRPARS   DS    0XL24                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
*                                                                               
DMCB     DS    XL24                                                             
DUB      DS    D                                                                
DSKADR   DS    F                                                                
ATRKB    DS    A                                                                
INTERM   DS    A                                                                
INTIME   DS    PL4                                                              
TSRLEN   DS    H                                                                
VIOCNT   DS    H                                                                
ADRCNT   DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
VIONUM   DS    X                                                                
VIOSTA   DS    X                                                                
EOFFLAG  DS    X                                                                
MODE     DS    X                                                                
DAYNO    DS    X                                                                
DATEB    DS    XL3                                                              
INDAYNO  DS    X                                                                
         DS    XL3                                                              
FACNAM   DS    CL4                                                              
PWD      DS    CL10                                                             
LASTDAY  DS    CL3                                                              
LASTTIME DS    CL5                                                              
WORK     DS    CL256                                                            
*                                                                               
         DS    0D                                                               
IOALAST  DS    A                   A(END OF BUFFER)                             
         DS    XL2                 N/D                                          
IOALENB  DS    H               -18 BUFFER LENGTH                                
IOANUMR  DS    H               -16 NUM OF RECORDS IN BUFFER                     
IOALENR  DS    H               -14 RECORD LENGTH                                
IOANEXT  DS    A               -12 A(NEXT RECORD IN BUFFER)                     
IOAREAH  DS    CL8             -08 LABEL                                        
IOAREA   DS    6400C               IOAREA 80 RECS X 80 BYTES                    
         DS    0D                                                               
TRKBUFFH DS    CL8                                                              
TRKBUFF  DS    (TRKLEN)X           TRACK BUFFER OR TWA1 READ AREA               
TRKBUFFT DS    CL8                                                              
WORKL    EQU   *-WORKD                                                          
                                                                                
***********************************************************************         
* DSECT FOR VIOLATE LOG RECORD                                        *         
***********************************************************************         
LOGRECD  DSECT                     LOG RECORD FOR SECURITY VIOLATIONS           
LOGREC   DS    0CL80               CURRENT ADRFILE RECORD SIZE                  
LOGID    DS    CL4                 $CT. IDENTIFIES A SRCON LOG REC              
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGTEXT  DS    CL48                FREE FORM TEXT STARTS HERE                   
LOGTEXT1 DS    0CL15                                                            
LOGPWD   DS    CL10                INPUT PASSWORD                               
         DS    CL4                                                              
LOGDAYNO DS    XL1                 DAY NUMBER FROM SSB MON=1,SUN=7              
LOGSYSIX DS    XL1                 FACPAK AOR/TOR ID                            
                                                                                
***********************************************************************         
* DSECT FOR DISPLAY LINE                                              *         
***********************************************************************         
VIOLINED DSECT                                                                  
VIOHDR   DS    XL8                                                              
VIODAY   DS    CL2                                                              
         DS    CL1                                                              
VIOTERM  DS    CL5                 TERMINAL NUMBER OR TIME HH.MM                
         DS    CL1                                                              
VIOLUID  DS    CL8                 TERMINAL LUID                                
         DS    CL1                                                              
VIOWHY   DS    CL3                 REASON                                       
         DS    CL1                                                              
VIODATA  DS    CL56                USER-ID/SYSTEM/PROGRAM/PASSWORD              
VIOLINEL EQU   *-VIOLINED                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR SCREEN                                                    *         
***********************************************************************         
SRVIOFFD DSECT                                                                  
         DS    XL64                                                             
* SRVIOFFD                                                                      
       ++INCLUDE SRVIOFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
SRCONFFD DSECT                                                                  
         DS    CL64                                                             
* SRCONFFD                                                                      
       ++INCLUDE SRCONFFD                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
                                                                                
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
                                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRVIO00   11/14/14'                                      
         END                                                                    
