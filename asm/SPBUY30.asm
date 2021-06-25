*          DATA SET SPBUY30    AT LEVEL 036 AS OF 11/19/19                      
*PHASE T21130C                                                                  
         TITLE 'T21130 - SPOTPAK BUY - MGA DISPLAY'                             
T21130   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21130,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21130+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         L     R7,ADBLOCK                                                       
         USING MGABLKD,R7                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO MVI   BYTE,MGAQBLD        ALWAYS REBUILD TABLE!                        
         BAS   RE,CALLBMGA         BUILD MG TABLE                               
*                                                                               
         BAS   RE,CKPFKEY          CHECK IF A PF KEY WAS HIT                    
*                                                                               
         CLI   SVMGINIT,C'A'       DID WE ALREADY INITIALIZE TSAR               
         BE    B20                                                              
*                                                                               
B10      MVI   SVMGINIT,C'A'                                                    
         XC    SVLSTREC,SVLSTREC                                                
*                                                                               
         LA    R2,MGAINP1H         POINT TO INPUT FIELD                         
         CLI   8(R2),C'*'          IF IT STARTS WITH A *                        
         BNE   B20                 IT IS INVALID NOW                            
         NI    4(R2),X'DF'         FORCE VALIDATION                             
         MVI   5(R2),0             SET INPUT LEN = 0                            
         XC    MGAINP1,MGAINP1     AND CLEAR THE STUPID FIELD                   
*                                                                               
B20      BAS   RE,DISPLAY          DISPLAY TABLE                                
*                                                                               
BX       B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHECK IF A PF KEY WAS ENTERED                                          
*                                                                               
CKPFKEY  NTR1                                                                   
         CLI   PFKEY,0                                                          
         BE    CPX                                                              
         CLI   PFKEY,1             TOP                                          
         BNE   CP10                                                             
         XC    SVLSTREC,SVLSTREC   SET AS IF FIRST TIME IN                      
         B     CPX                                                              
*                                                                               
CP10     CLI   PFKEY,2             PREV                                         
         BNE   CP20                                                             
         BAS   RE,POPTAB                                                        
         MVC   SVLSTREC,LSTTAB                                                  
         B     CPX                                                              
*                                                                               
CP20     CLI   PFKEY,3             NEXT                                         
         BE    CPX                                                              
*                                                                               
CPX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        DISPLAY THE TABLE BUILT BY TSAR                                        
*                                                                               
DISPLAY  NTR1                                                                   
         BAS   RE,CLRTWA                                                        
         MVC   MGATOTL(L'MGATOTL),SPACES  CLEAR TOTAL LINE                      
         OI    MGATOTLH+6,X'80'                                                 
         MVC   MGAGTOT(L'MGAGTOT),SPACES                                        
         OI    MGAGTOTH+6,X'80'                                                 
*                                                                               
         LA    R2,MGAINP1H         CHECK START AT FIELD                         
         TM    4(R2),X'20'                                                      
         BO    D20                                                              
         CLC   8(3,R2),=C'*MG'     STILL MGA                                    
         BE    D20                                                              
         XC    SVLSTREC,SVLSTREC                                                
         XC    LSTTAB(LSTTABLN),LSTTAB                                          
         CLI   5(R2),0                                                          
         BE    D20                                                              
         CLC   8(3,R2),=C'MGA'     STILL MGA                                    
         BNE   DXX                                                              
*                                                                               
         XC    SVSTCODE,SVSTCODE                                                
         CLI   5(R2),3                                                          
         BE    D20                                                              
         CLI   5(R2),5                                                          
         BL    ERRINV                                                           
         CLI   12(R2),C'A'         SKIP /                                       
         BL    ERRINV                                                           
*                                                                               
D10      ZIC   R1,5(R2)                                                         
         AHI   R1,-5                                                            
         CHI   R1,1                ONLY MOVE IN CODE                            
         BNH   *+8                                                              
         LA    R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVSTCODE(0),12(R2)                                               
         OC    SVSTCODE,SPACES                                                  
*                                                                               
D20      OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,MGALN1H                                                       
         BAS   RE,GETGTOT          GET GRAND TOTAL                              
         XC    LASTCODE,LASTCODE                                                
*                                                                               
         USING MGENTRYD,R6                                                      
         LA    R6,ENTRY                                                         
         XC    0(MGERECL,R6),0(R6)                                              
         MVC   MGECODE,SVSTCODE    SET START AT CODE                            
         MVI   BYTE,MGAQRDH        READ HIGH                                    
*                                                                               
         OC    SVLSTREC,SVLSTREC   IS THIS FIRST TIME IN                        
         BZ    *+8                                                              
         MVI   BYTE,MGAQGET        NO GET LAST RECORD READ                      
*                                                                               
         BAS   RE,CALLBMGA                                                      
         CLI   ENTRY,X'FE'         TEST TOTAL REC (EOF)                         
         BNL   D32                                                              
*                                                                               
         MVC   SVLSTREC,MGATSNUM   SAVE LAST REC READ                           
*                                                                               
         CLI   MGAERR,MGAQEOF      END OF FILE                                  
         BE    DX                                                               
         CLI   POPSW,C'Y'          DID WE JUST POP                              
         BE    *+8                 YES - DON'T PUSH IT BACK                     
         BAS   RE,PUSHTAB                                                       
         MVI   POPSW,C'N'                                                       
         BAS   RE,GETTOT           GET THE TOTAL REC FOR THIS CODE              
         B     D40                                                              
*                                                                               
D30      MVI   BYTE,MGAQNXT         READ NEXT                                   
         BAS   RE,CALLBMGA                                                      
*                                                                               
         CLI   ENTRY,X'FE' TEST TOTAL REC (EOF)                                 
         BL    D34                                                              
*                                                                               
D32      XC    SVLSTREC,SVLSTREC                                                
         MVC   SVSTCODE,=C'AA'     SET LOW CODE                                 
         B     DX                                                               
*                                                                               
D34      MVC   SVLSTREC,MGATSNUM   SAVE LAST REC READ                           
*                                                                               
         CLI   MGAERR,MGAQEOF      END OF FILE                                  
         BE    DX                                                               
*                                                                               
         USING MGENTRYD,R6                                                      
D40      LA    R6,ENTRY                                                         
         CLC   LASTCODE,MGECODE                                                 
         BE    D50                                                              
         CLC   =X'FEFE',MGECODE    GRAND TOTAL                                  
         BE    DX                                                               
         OC    LASTCODE,LASTCODE   IGNORE FIRST TIME                            
         BNZ   DXNXT                                                            
*                                                                               
D50      MVC   LASTCODE,MGECODE                                                 
         CLI   MGETYPE,X'FE'       TOTAL                                        
         BE    D30                                                              
         USING LINED,R2                                                         
         BAS   RE,BLDLINE                                                       
         OI    6(R2),X'80'         TRANSMIT LINE                                
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
*                                                                               
         LA    R5,MGALSTLH                                                      
         CR    R2,R5               HAVE WE REACHED THE END                      
         BNH   D30                                                              
*                                                                               
DXNXT    MVC   BUYMSG(18),=C'HIT ENTER FOR NEXT'                                
         B     DXX                                                              
*                                                                               
DX       MVC   BUYMSG(19),=C'HIT ENTER FOR FIRST'                               
         XC    SVLSTREC,SVLSTREC                                                
*                                                                               
DXX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CLEAR THE SCREEN                                                       
*                                                                               
CLRTWA   NTR1                                                                   
         LA    R2,MGALN1H                                                       
         LA    R4,MXLINES                                                       
*                                                                               
CT10     XC    8(L'MGALN1,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            BUMP PAST LINE                               
         AR    R2,R1                                                            
         BCT   R4,CT10                                                          
*                                                                               
CTX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET RECORD NUMBER IN LSTTAB                                            
*                                                                               
PUSHTAB  NTR1                                                                   
         MVC   TLSTTAB+2(LSTTABLN-2),LSTTAB                                     
         MVC   TLSTTAB(2),MGATSNUM                                              
         MVC   LSTTAB(LSTTABLN),TLSTTAB                                         
         B     XIT                                                              
*                                                                               
*        POP RECORD NUMBER IN LSTTAB                                            
*                                                                               
POPTAB   NTR1                                                                   
         MVI   POPSW,C'Y'                                                       
         XC    TLSTTAB(LSTTABLN),TLSTTAB                                        
         MVC   TLSTTAB(LSTTABLN-2),LSTTAB+2                                     
         MVC   LSTTAB(LSTTABLN),TLSTTAB                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        BUILD A LINE TO DISPLAY                                                
*                                                                               
         USING LINED,R2                                                         
         USING MGENTRYD,R6                                                      
BLDLINE  NTR1                                                                   
         MVC   8(L'MGALN1,R2),SPACES                                            
         MVC   LCODE,MGECODE                                                    
*                                                                               
         CLC   =C'NX',LCODE        NX = NC HAVE BEEN SKIPPED - TOO MANY         
         BE    *+14                                                             
         CLC   =C'*X',LCODE        NX = NC HAVE BEEN SKIPPED - TOO MANY         
         BNE   BL05                                                             
         MVC   LTYPE(L'NCMESS),NCMESS                                           
         B     BL52                                                             
NCMESS   DC    C'*** OVER 200 NC SPOTS FOUND - TOO MANY TO DISPLAY'             
*                                                                               
BL05     CLI   MGETYPE,0                                                        
         BNE   BL10                                                             
         MVI   LTYPE,C'-'                                                       
         CLI   MGESTA,X'E8'                                                     
         BNL   *+10                                                             
         MVC   LTYPE(3),=C'MS-'                                                 
         MVC   LMSRTG(4),MGERTG      RATING                                     
         B     BL20                                                             
*                                                                               
BL10     MVI   LTYPE,C'+'                                                       
         CLI   MGESTA,X'E8'                                                     
         BNL   *+10                                                             
         MVC   LTYPE(3),=C'MG+'                                                 
         MVC   LMGRTG(4),MGERTG      RATING                                     
*                                                                               
BL20     EDIT  MGELINE,(3,LLINE),FILL=0                                         
         CLC   MGEDATE,=X'FFFF'                                                 
         BNE   BL25                                                             
         MVC   LDATE,=C'*MISSED*'                                               
         B     BL52                                                             
*                                                                               
BL25     GOTO1 VDATCON,DMCB,(2,MGEDATE),(4,LDATE)                               
*                                                                               
BL27     CLI   MGESPNUM,1                                                       
         BE    BL30                                                             
         MVI   LDATE+5,C'-'                                                     
         EDIT  MGESPNUM,(2,LDATE+6),FILL=0                                      
*                                                                               
BL30     EDIT  MGESLN,(3,LSLN)                                                  
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),MGETIME,LTIME                                          
*                                                                               
         ICM   R0,15,MGECOST                                                    
         LA    R5,LMSCOST                                                       
         CLI   MGETYPE,0                                                        
         BE    *+8                                                              
         LA    R5,LMGCOST                                                       
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(7,0(R5)),FLOAT=$,ZERO=NOBLANK                              
*                                                                               
BL50     MVC   LPGMNM,MGEPGMNM       PROGRAM                                    
         CLI   SVPOLPRD,0          TEST BRAND POL                               
         BNE   BL52                                                             
         MVC   FULL(1),MGEPRD1                                                  
         BAS   RE,GETPRD                                                        
         MVC   LPRD(3),FULL+1                                                   
         B     BL52             ***** PRD2 DOES NOT FIT ON SCREEN!!!!           
         CLI   MGEPRD2,0                                                        
         BE    BL52                                                             
         MVC   FULL(1),MGEPRD2                                                  
         BAS   RE,GETPRD                                                        
         MVI   LPRD+3,C'-'                                                      
         MVC   LPRD+4(3),FULL+1                                                 
*                                                                               
BL52     OC    MGESTA,MGESTA                                                    
         BZ    BLX                                                              
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MGESTA                                                 
*                                                                               
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    BLX                                                              
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVC   LSTA,WORK+20        SHOW NETWORK ONLY!                           
*                                                                               
BLX      B     XIT                                                              
         SPACE 2                                                                
GETPRD   MVC   FULL+1(3),=C'???'                                                
         L     R1,ASVCLIST                                                      
*                                                                               
GETPRD2  CLI   0(R1),C'A'                                                       
         BLR   RE                                                               
         CLC   FULL(1),3(R1)                                                    
         BE    GETPRD4                                                          
         LA    R1,4(R1)                                                         
         B     GETPRD2                                                          
GETPRD4  MVC   FULL+1(3),0(R1)     MOVE PRODUCT CODE                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        GET TOTAL                                                              
*                                                                               
GETTOT   NTR1                                                                   
         MVC   SVTSNUM,SVLSTREC    SAVE LAST REC NUMBER READ                    
         USING MGENTRYD,R6                                                      
         LA    R6,ENTRY                                                         
         MVC   THISCODE,MGECODE                                                 
         XC    0(MGERECL,R6),0(R6)                                              
         MVC   MGECODE,THISCODE                                                 
         MVC   MGESTA,=3X'FF'                                                   
         MVI   MGETYPE,X'FE'       SET TO READ TOTAL RECORD                     
         MVI   BYTE,MGAQRDH        READ HIGH                                    
         BAS   RE,CALLBMGA                                                      
         CLI   MGAERR,MGAQEOF      END OF FILE                                  
         BE    GTX                                                              
         CLC   MGECODE,THISCODE                                                 
         BNE   GTX                                                              
         CLI   MGETYPE,X'FE'                                                    
         BNE   GTX                                                              
         MVI   BYTE2,0                                                          
         BAS   RE,TOTAL                                                         
*                                                                               
GTX      MVC   SVLSTREC,SVTSNUM                                                 
         MVI   BYTE,MGAQGET        GET LAST RECORD READ                         
         OC    SVLSTREC,SVLSTREC   IS THIS FIRST TIME IN                        
         BNZ   *+8                                                              
         MVI   BYTE,MGAQRDH        READ HIGH                                    
         BAS   RE,CALLBMGA                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        GET GRAND TOTAL                                                        
*                                                                               
GETGTOT  NTR1                                                                   
         USING MGENTRYD,R6                                                      
         MVC   SVTSNUM,SVLSTREC    SAVE LAST REC NUMBER READ                    
         LA    R6,ENTRY                                                         
         XC    0(MGERECL,R6),0(R6)                                              
         MVC   MGECODE,=X'FEFE'    SET TO READ GRAND TOTAL ENTRY                
         MVI   BYTE,MGAQRDH        READ HIGH                                    
         BAS   RE,CALLBMGA                                                      
         CLI   MGAERR,MGAQEOF      END OF FILE                                  
         BE    GX                                                               
         CLC   MGECODE,=X'FEFE'                                                 
         BNE   GX                                                               
         MVI   BYTE2,1                                                          
         BAS   RE,TOTAL                                                         
*                                                                               
GX       XC    ENTRY,ENTRY                                                      
         MVI   BYTE,MGAQRDH        READ HIGH                                    
         BAS   RE,CALLBMGA                                                      
         MVC   SVLSTREC,SVTSNUM                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT OUT TOTAL                                                        
*                                                                               
TOTAL    NTR1                                                                   
         USING TLINED,R2                                                        
         LA    R2,MGATOTLH         TOTAL LINE                                   
         CLI   BYTE2,0                                                          
         BE    *+8                                                              
         LA    R2,MGAGTOTH         GRAND TOTAL LINE                             
*                                                                               
         LA    R4,LTMISS           MISSED TOTAL                                 
         L     R0,MGETMISS                                                      
         BAS   RE,PRTTOT                                                        
*                                                                               
         LA    R4,LTMSRTG          MISSED RATING TOTAL                          
         L     R0,MGEMSRTG                                                      
         BAS   RE,PRTRTG                                                        
*                                                                               
         LA    R4,LTMG             MAKEGOOD TOTAL                               
         L     R0,MGETMG                                                        
         BAS   RE,PRTTOT                                                        
*                                                                               
         LA    R4,LTMGRTG          MAKEGOOD RATING TOTAL                        
         L     R0,MGEMGRTG                                                      
         BAS   RE,PRTRTG                                                        
         OI    6(R2),X'80'         TRANSMIT LINE                                
         B     XIT                                                              
*                                                                               
*        EDIT OUT TO TOTAL LINE                                                 
*                                                                               
PRTTOT   NTR1                                                                   
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(7,0(R4)),FLOAT=$                                           
         B     XIT                                                              
*                                                                               
*        EDIT OUT RATINGS TO TOTAL LINE                                         
*                                                                               
PRTRTG   NTR1                                                                   
         LA    RE,SVBRDEMS                                                      
         OC    0(3,RE),0(RE)                                                    
         BNZ   *+8                                                              
         LA    RE,SVDEMOS                                                       
*                                                                               
         LAY   RF,SV00APRF+6       TEST 2 DEC IMPS ACTIVE                       
         CLI   0(RF),C'Y'                                                       
         JNE   PRTRTG03            NO                                           
         CLI   1(RE),C'I'          YES, IS CATEGORY AN IMP?                     
         JE    PRTRTG06                 YES                                     
*                                                                               
PRTRTG03 CLI   SV00PROF+9,C'Y'     TEST 2 DEC RTGS ACTIVE                       
         JNE   PRTRTG10            NO                                           
*                                                                               
         CLI   1(RE),C'R'                                                       
         BE    *+12                                                             
         CLI   1(RE),C'E'                                                       
         BNE   PRTRTG10                                                         
*                                                                               
*  ROUND TO 1 DECIMAL VALUE                                                     
PRTRTG06 LHI   R1,2                                                             
         MR    R0,R0               X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
*                                                                               
PRTRTG10 EDIT  (R0),(5,0(R4)),1                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CALL BLDMGA                                                            
*        R7 - MGABLOCK                                                          
*                                                                               
CALLBMGA NTR1                                                                   
         CLI   BYTE,0              IF BUILD TABLE                               
         BNE   *+8                                                              
         MVI   0(R7),0             CLEAR MGTSINIT                               
         XC    1(MGALNQ-1,R7),1(R7)                                             
         MVC   MGAACOM,VCOMFACS    SET A(COMFAS)                                
         MVC   MGGETBUY,VGETBUY                                                 
         MVC   MGATSAR,VTSAR       A(TSAR)                                      
         MVC   MGAIO,AREC4                                                      
         MVC   MGAACT,BYTE                                                      
         MVC   MGAENTRY,ENTRY                                                   
         MVC   MGATSNUM,SVLSTREC                                                
         LA    R1,SVDEMOS          A(SVDEMOS)                                   
         ST    R1,MGADEM                                                        
         LA    R1,SVBRDEMS         A(SVBRDEM)                                   
         ST    R1,MGABRDEM                                                      
         MVC   MGAAGMD,SVAGYMD     SET AGY/MED                                  
         MVC   MGACLT,SVCLT            CLIENT                                   
         MVC   MGAPRD,SVPRD            PRODUCT                                  
         CLC   =C'POL',QPRD                                                     
         BE    *+10                                                             
         MVC   MGAPRD,SVPOLPRD         PRODUCT                                  
         MVC   MGASTA,SVMKT            MKT/STA                                  
         MVC   MGAEST,SVEST            ESTIMATE                                 
         MVC   MG1OR2,SV1OR2                                                    
                                                                                
* ALWAYS REBUILD BUFFER!                                                        
                                                                                
         OI    MGAOPT2,MGAOPT2_NODSK   SET DO NOT WRITE TO DISK                 
         MVI   MGATSRPGS,40        REQUEST 40 PAGES (18432 BYTES/PAGE)          
         OI    MGATSRPGS,X'80'     SET FLAG FOR BOTH TSAR BUFFERS               
*                                                                               
         TM    SVOPT1,SVOPT1_NONOCHG  TEST SUPPRESS NOCHG SPOTS                 
         BZ    *+8                                                              
         MVI   MGAOPT,MGONONC                                                   
*                                                                               
         CLI   SV00PROF+9,C'Y'     TEST 2 DECIMAL RATINGS                       
         JNE   *+8                                                              
         OI    MGAOPT2,MGAOPT2_2DEC                                             
*                                                                               
         LAY   RF,SV00APRF+6       TEST 2 DECIMAL IMPS                          
         CLI   0(RF),C'Y'                                                       
         JNE   *+8                                                              
         OI    MGAOPT2,MGAOPT2_2DECIMP                                          
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AC2'  BLDMGA                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),MGABLKD                                                     
         MVC   ENTRY,MGAENTRY                                                   
         MVC   BYTE,MGAERR                                                      
         CLI   MGAERR,0                                                         
         BE    YES                                                              
         B     NO                  ***** - NO RECORDS FOUND                     
         EJECT                                                                  
*                                                                               
*        CALL TSAR                                                              
*                                                                               
CALLTSAR NTR1                                                                   
         LA    R4,TSARBLK                                                       
         USING TSARD,R4                                                         
         MVC   TSACOM,VCOMFACS     A(COMFACS)                                   
         MVI   TSPAGL,1            USE TEMPSTR PAGE 1                           
* FOLLOWING MUST AGREE WITH SPBUY13/SPBLDMGA                                    
         MVI   TSPAGN,4            USE 4 PAGES                                  
         MVI   TSKEYL,MGEKEYL      KEY LENGTH                                   
         LA    R1,MGERECL          RECORD LENGTH                                
         STH   R1,TSRECL                                                        
         OI    TSINDS,TSIXTTWA     14K RECORDS                                  
         MVC   TSACTN,BYTE                                                      
         LA    R1,ENTRY            A(RECORD)                                    
         ST    R1,TSAREC                                                        
         GOTO1 VTSAR,TSARBLK                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
ERRINV   MVI   ERRCD,INVERR                                                     
         B     BUYERR                                                           
*                                                                               
BUYERR   MVI   BYTE,TSASAV         SAVE TSAR                                    
         BAS   RE,CALLTSAR                                                      
         GOTO1 ERROR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MXLINES  EQU   12                  LINES PER SCREEN                             
         EJECT                                                                  
LINED    DSECT                                                                  
         DS    CL8                 FAKE HEADER                                  
         DS    CL1                                                              
LCODE    DS    CL2                                                              
         DS    CL1                                                              
LTYPE    DS    CL1     + OR -                                                   
LSTA     DS    CL3     OTHER STA                                                
         DS    CL1                                                              
LLINE    DS    CL3                                                              
         DS    CL1                                                              
LDATE    DS    CL8                                                              
         DS    CL1                                                              
LSLN     DS    CL3                                                              
         DS    CL1                                                              
LTIME    DS    CL11                                                             
         DS    CL1                                                              
LMSCOST  DS    CL7                                                              
         DS    CL1                                                              
LMSRTG   DS    CL5                                                              
LMGCOST  DS    CL7                                                              
         DS    CL1                                                              
LMGRTG   DS    CL5                                                              
         DS    CL1                                                              
LPGMNM   DS    CL13                                                             
         ORG   LPGMNM+8                                                         
LSTAR    DS    CL2                                                              
LPRD     DS    CL3                                                              
         EJECT                                                                  
TLINED   DSECT                                                                  
         DS    CL8                 FAKE HEADER                                  
LTMISS   DS    CL7                                                              
         DS    CL1                                                              
LTMSRTG  DS    CL5                                                              
         DS    CL1                                                              
LTMG     DS    CL7                                                              
         DS    CL1                                                              
LTMGRTG  DS    CL5                                                              
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE SPMGADN                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
         PRINT ON                                                               
SPBUYWKD DSECT                                                                  
         ORG   MSTRBUY                                                          
         DS    0D                                                               
SVTSNUM  DS    H                                                                
POPSW    DS    CL1                                                              
LASTCODE DS    CL2                                                              
THISCODE DS    CL2                                                              
TSARBLK  DS    CL48                                                             
TLSTTAB  DS    8XL2                TABLE OF LAST 8 PAGES' 1ST REC NUM           
ENTRY    DS    CL(L'MGAENTRY)                                                   
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SPBUY30   11/19/19'                                      
         END                                                                    
