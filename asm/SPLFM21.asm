*          DATA SET SPLFM21    AT LEVEL 027 AS OF 05/01/02                      
*PHASE T21921A                                                                  
         TITLE 'SPLFM21 - STATION MASTER T21921'                                
T21921   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21921,RR=R8                                                   
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING STARECD,R8                                                       
*                                                                               
         MVI   NETPAK,C'N'                                                      
         L     RF,VCOMFACS                   MUST CHK IF NETPAK                 
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3                                                        
         BNE   *+12                                                             
         MVI   NETPAK,C'Y'                                                      
         B     MAIN10                                                           
         DROP  R1,RF                                                            
*                                                                               
         LA    R1,WILIST                                                        
         LA    R0,(WILISTX-WILIST)/L'WILIST                                     
         B     MAIN00            <=============  NOP  ==============>           
         CLC   0(2,R1),AGYALPHA                                                 
         BE    MAIN10                                                           
         LA    R1,L'WILIST(R1)                                                  
         BCT   R0,*-14                                                          
*                                                                               
MAIN00   XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(37),=C'** ERROR ** USE SPECIAL FILE MAINT **'             
         B     EDT06ERR                                                         
*                                                                               
WILIST   DS    0CL2                                                             
         DC    C'WGWIWJWRWT'                                                    
WILISTX  EQU   *                                                                
*                                                                               
MAIN10   CLI   SVFMTSW,0                     TEST FORMAT OR EDIT                
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
MYEXIT   XIT1 REGS=(R2)                                                         
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
         FOUT  STAMKTH,SMKT,4                                                   
         LA    R2,STAMKNMH                                                      
         BAS   R9,FMTMKT                                                        
         FOUT  STAPAYRH,SREP,3                                                  
         LA    R3,SREP                                                          
         LA    R2,STAPRNMH                                                      
         BAS   R9,FMTREP                                                        
         FOUT  STATSHRH,SREP+3,3                                                
         LA    R3,SREP+3                                                        
         LA    R2,STATSNMH                                                      
         BAS   R9,FMTREP                                                        
         FOUT  STATRARH,SREP+6,3                                                
         LA    R3,SREP+6                                                        
         LA    R2,STATANMH                                                      
         BAS   R9,FMTREP                                                        
         FOUT  STACHANH,SCHNL,4                                                 
*                                                                               
         XC    STABKTY,STABKTY     CLEAR BOOK FIELD 1ST                         
         OC    SBKTYPE,SBKTYPE                                                  
         BZ    FMT01A                                                           
         MVC   STABKTY,SBKTYPE                                                  
FMT01A   FOUT  STABKTYH                                                         
         CLI   SVAPROF+7,C'C'      SEE IF CANADIAN NETWORK                      
         BNE   FMT2                                                             
         FOUT  STANETAH,SCANNTWK,4                                              
         B     FMT2X                                                            
*                                                                               
FMT2     DS    0H                                                               
         FOUT  STANETAH,SNETWRK,3                                               
FMT2X    MVC   STATYPE(1),STYPE                                                 
         CLI   NETPAK,C'Y'                                                      
         BNE   FMT3                                                             
         MVC   STATYPE+1(1),SUBMEDIA                                            
         MVC   STATYPE+2(1),SPTYPE                                              
FMT3     FOUT  STATYPEH                                                         
         FOUT  STATWIXH,STWIX,20                                                
         FOUT  STAFAXH,SFAX,12                                                  
         MVC   STATBNK,SPACES                                                   
         OC    STIMEBK,STIMEBK                                                  
         BZ    FMT4                                                             
         MVC   DUB(4),STIMEBK                                                   
         L     R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  STATBNK(9),DUB                                                   
FMT4     FOUT  STATBNKH                                                         
         EDIT  (B2,SNEWTAX),(6,STATAX),3,ZERO=BLANK,ALIGN=LEFT                  
         FOUT  STATAXH                                                          
         FOUT  STASIZEH,SSIZE,1                                                 
*                                                                               
         FOUT  STANTISH,SNTISTA,4                                               
*                                                                               
         FOUT  STAGSTH,SGSTCODE,1   GOODS & SERVICE TAX CODE                    
         BAS   RE,DISPPST           PROVINCE SERVICE TAX CODE                   
*                                                                               
         CLI   SVAPROF+7,C'C'      THE REST ARE FOR CANADA ONLY                 
         BNE   FMTX                                                             
         FOUT  STACOUNH,SCOUNTRY,1                                              
         CLI   STACOUN,C'C'                                                     
         BNE   *+10                                                             
         MVC   STACOUN+1(2),=C'AN'                                              
         CLI   STACOUN,C'U'        SET TO USA/VSA                               
         BL    *+10                                                             
         MVC   STACOUN+1(2),=C'SA'                                              
         XC    STACTX,STACTX                                                    
         OC    SCANTAX,SCANTAX                                                  
         BZ    FMT6                                                             
         EDIT  SCANTAX,(5,STACTX),2                                             
*                                                                               
FMT6     FOUT  STACTXH                                                          
         XC    STAFEE,STAFEE                                                    
         OC    SSVCFEE,SSVCFEE                                                  
         BZ    FMTX                                                             
         EDIT  SSVCFEE,(5,STAFEE),2                                             
         FOUT  STAFEEH                                                          
*                                                                               
*                                                                               
FMTX     B     EXXMOD                                                           
         EJECT                                                                  
* ********************************************************************          
* EDT                                                                           
* ********************************************************************          
EDT      DS    0H                                                               
         CLI   SVAPROF+7,C'C'      SEE IF CANADIAN                              
         BNE   USELFM              NO - FORGET IT !                             
         LA    R1,STAWRK                                                        
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'V'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPVRSN,C'N'                                                    
         BNE   USELFM                                                           
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(37),=C'** ERROR ** USE SPECIAL FILE MAINT **'             
         B     EDT06ERR                                                         
*                                                                               
USELFM   XC    SAVSMKT,SAVSMKT                                                  
         XC    STAMKNM,STAMKNM        CLEAR NAMES                               
         XC    STAPRNM,STAPRNM                                                  
         XC    STATSNM,STATSNM                                                  
         XC    STATANM,STATANM                                                  
         CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
         MVC   SAVSMKT,SMKT          SAVE OLD MKT                               
EDT0     LA    R2,STAMKTH               MARKET CODE                             
         GOTO1 ANY                                                              
         GOTO1 PACK                                                             
         OI    DUB+7,X'0F'                                                      
         UNPK  SMKT,DUB                                                         
*--IN NETPAK GOALS THE DEFAULT MARKET NUMBERS ARE 7777, AND 777                 
*--BECAUSE OF THIS, THESE MARKET NUMBERS CANNOT BE ASSIGNED TO                  
*--REGULAR STATIONS.                                                            
         CLI   NETPAK,C'Y'                                                      
         BNE   EDT01                                                            
         CLC   SMKT,=CL4'7777'                                                  
         BE    *+14                                                             
         CLC   SMKT,=CL4'0777'                                                  
         BNE   EDT01                                                            
         MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
EDT01    BAS   R9,EDTMKT                                                        
         CLI   SVACT,C'A'         ACTION IS ADD?                                
         BNE   EDT02                                                            
         CLI   SVEBCMED,C'N'      AND NETWORK - CHECK DUPLICATE MKT             
         BNE   EDT1                                                             
         BAS   R9,DUPMKT                                                        
         B     EDT1                                                             
*                                                                               
EDT02    CLI   SVEBCMED,C'N'      CHG ACT & NET-NO CHG OF MKT ALLOWED           
         BE    EDT06                                                            
         CLI   T219FFD+1,C'*'     CHG ACT & SPOT - CHG ALLOWED ONLY             
         BE    EDT1               FOR DDS TERMINALS                             
         OC    SVEBCCLT,SVEBCCLT  OR FOR CLT-SPECIFIC RECORD                    
         BNZ   EDT1               CHANGE IS ALLOWED                             
EDT06    CLC   SAVSMKT,SMKT       OTHERWISE -MARKETS MUST MATCH                 
         BE    EDT1                                                             
         MVI   ERRCD,NOCHGERR                                                   
         CLI   SVEBCMED,C'N'      DIFFERENT MSGS FOR NET AND SPOT               
         BE    LFMERR                                                           
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(58),=C'** ERROR ** MAY NOT CHANGE THIS FIELD (CALX        
               L DDS - MKT FIX)'                                                
EDT06ERR MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     MYEXIT                                                           
*                                                                               
EDT1     MVI   ERRCD,INVERR                                                     
         LA    R2,STAPAYRH              PAYING REP                              
         LA    R3,STAPRNMH                                                      
         BAS   R9,EDTREP                                                        
         MVC   SREP(3),DUB                                                      
EDT2     LA    R2,STATSHRH              TIME SHEET REP                          
         LA    R3,STATSNMH                                                      
         BAS   R9,EDTREP                                                        
         MVC   SREP+3(3),DUB                                                    
EDT3     LA    R2,STATRARH              TRAFFIC REP                             
         LA    R3,STATANMH                                                      
         BAS   R9,EDTREP                                                        
         MVC   SREP+6(3),DUB                                                    
EDT4     LA    R2,STACHANH              CHANNEL                                 
         XC    SCHNL,SCHNL                                                      
         CLI   5(R2),0                                                          
         BE    EDT5                                                             
         CLI   SVEBCMED,C'T'                                                    
         BNE   EDT4C                                                            
         CLI   5(R2),2                                                          
         BNE   LFMERR              2 CHARACTERS FOR TV                          
         MVC   SCHNL(2),STACHAN                                                 
         B     EDT5                                                             
*                                                                               
EDT4C    CLI   5(R2),4          4 CHARACTERS FOR RADIO                          
         BNE   LFMERR                                                           
         MVC   SCHNL(4),STACHAN                                                 
*                                                                               
EDT5     LA    R2,STANETAH         NETWORK AFF                                  
         XC    SNETWRK,SNETWRK                                                  
         XC    SCANNTWK,SCANNTWK                                                
         CLI   5(R2),0                                                          
         BE    EDT6                                                             
         TM    4(R2),X'04'         TEST ALPHA                                   
         BZ    LFMERR                                                           
         CLI   SVAPROF+7,C'C'      SEE IF CANADIAN NETWORK                      
         BNE   EDT5C                                                            
         MVC   SCANNTWK,8(R2)                                                   
         OC    SCANNTWK,SPACES                                                  
         B     EDT6                                                             
*                                                                               
EDT5C    DS    0H                                                               
         CLI   5(R2),3                                                          
         BH    LFMERR                                                           
         MVC   SNETWRK,8(R2)                                                    
         OC    SNETWRK,SPACES                                                   
*                                                                               
EDT6     MVI   ERRCD,INVERR                                                     
         LA    R2,STATYPEH         STATION TYPE                                 
         MVI   STYPE,C' '                                                       
         MVI   SUBMEDIA,C' '                                                    
         CLI   NETPAK,C'Y'                                                      
         BE    EDT6B                                                            
         CLI   5(R2),0                                                          
         BE    EDT7                                                             
         B     EDT6D                                                            
*                                                                               
EDT6B    CLI   5(R2),0                                                          
         BE    LFMERR                                                           
         LA    R5,MTYPETAB                                                      
EDT6C    CLC   8(1,R2),0(R5)                                                    
         BE    EDT6D                                                            
         CLI   0(R5),X'FF'       END OF TABLE                                   
         BE    LFMERR                                                           
         LA    R5,1(R5)                                                         
         B     EDT6C                                                            
*                                                                               
EDT6D    MVC   STYPE,8(R2)                                                      
*                                                                               
EDT6E    CLI   5(R2),2                                                          
         BL    EDT6G                                                            
         CLI   NETPAK,C'Y'                                                      
         BNE   LFMERR                                                           
         CLI   9(R2),C'N'                                                       
         BE    EDT6F                                                            
         CLI   9(R2),C'R'                                                       
         BE    EDT6F                                                            
         CLI   9(R2),X'40'                                                      
         BNH   EDT6G                                                            
         B     LFMERR                                                           
*                                                                               
EDT6F    MVC   SUBMEDIA,9(R2)                                                   
*                                                                               
EDT6G    CLI   SPTYPE,X'40'        FIRST TIME                                   
         BNH   *+26                YES BYPASS DDS CHECK                         
         MVI   ERRCD,NOCHGERR                                                   
         CLC   SPTYPE,10(R2)        ANY CHANGE TO SPTYPE                        
         BE    EDT7                                                             
         CLI   SVAOFFC,C'*'        DDS TERMINAL                                 
         BNE   LFMERR              NO INPUT NOT ALLOWED                         
*                                                                               
         MVC   SPTYPE,STYPE        MOVE DEFAULT                                 
         MVI   ERRCD,INVERR        RESET ERROR MESSAGE                          
*                                                                               
         CLI   5(R2),3                                                          
         BL    EDT7                                                             
         LA    R5,PTYPETAB                                                      
EDT6H    CLC   10(1,R2),0(R5)                                                   
         BE    EDT6I                                                            
         CLI   0(R5),X'FF'       END OF TABLE                                   
         BE    LFMERR                                                           
         LA    R5,1(R5)                                                         
         B     EDT6H                                                            
*                                                                               
EDT6I    MVC   SPTYPE,10(R2)                                                    
         B     EDT7                                                             
*                                  MEDIA TYPE TABLE FOR NETPAK                  
MTYPETAB DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'D'                RADIO                                        
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FFFF'                                                          
*                                  POSTING TYPE TABLE FOR NETPAK                
PTYPETAB DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FFFF'                                                          
*                                                                               
*                                                                               
EDT7     LA    R2,STATWIXH         TWIX NUMBER                                  
         XC    STWIX,STWIX                                                      
         CLI   5(R2),0                                                          
         BE    EDT8                                                             
         MVC   STWIX,8(R2)                                                      
*                                                                               
EDT8     LA    R2,STAFAXH          FAX NUMBER                                   
         XC    SFAX,SFAX                                                        
         CLI   5(R2),0                                                          
         BE    EDT9                                                             
         MVC   SFAX,8(R2)                                                       
*                                                                               
EDT9     LA    R2,STATBNKH         TIME BANK                                    
         XC    STIMEBK,STIMEBK                                                  
         CLI   5(R2),0                                                          
         BE    EDT10                                                            
         CLI   5(R2),9                                                          
         BH    LFMERR                                                           
         TM    4(R2),X'08'                                                      
         BZ    LFMERR                                                           
         GOTO1 PACK                                                             
         ST    R0,FULL                                                          
         MVC   STIMEBK,FULL                                                     
EDT10    LA    R2,STATAXH                                                       
         XC    SNEWTAX,SNEWTAX     NEW TAX RATE                                 
         CLI   5(R2),0                                                          
         BE    EDT12                                                            
         ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(3,8(R2)),(R5)                                     
         CLI   DMCB,X'FF'          INVALID                                      
         BE    LFMERR                                                           
         L     R5,DMCB+4                                                        
         C     R5,=F'32767'                                                     
         BH    LFMERR                                                           
         LTR   R5,R5                                                            
         BM    LFMERR                                                           
         STCM  R5,3,SNEWTAX                                                     
EDT12    LA    R2,STASIZEH                                                      
         MVI   SSIZE,0                                                          
         CLI   5(R2),0                                                          
         BE    EDT14                                                            
         CLI   8(R2),C'A'                                                       
         BL    LFMERR                                                           
         CLI   8(R2),C'Z'                                                       
         BH    LFMERR                                                           
         MVC   SSIZE,8(R2)                                                      
*                                                                               
EDT14    LA    R2,STACOUNH         COUNTRY                                      
         MVI   SCOUNTRY,0                                                       
         CLI   SVAPROF+7,C'C'      ONLY FOR CANADA                              
         BNE   EDT16                                                            
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    EDT16                                                            
         CLI   5(R2),3                                                          
         BH    LFMERR                                                           
         BCTR  RE,0                                                             
         LA    R1,EDTCOUN                                                       
EDT15    CLI   0(R1),0                                                          
         BE    LFMERR                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     EDT15                                                            
         MVC   SCOUNTRY,8(R2)                                                   
         B     EDT16                                                            
EDTCOUN  DC    CL3'CAN'            CAN STA/CAN $                                
         DC    CL3'USA'            USA STA/USA $                                
         DC    CL3'VSA'            USA STA/CAN $                                
         DC    X'00'                                                            
*                                                                               
EDT16    LA    R2,STACTXH          CANADIAN C-58 TAX                            
         XC    SCANTAX,SCANTAX                                                  
         CLI   SVAPROF+7,C'C'      ONLY FOR CANADA                              
         BNE   EDT18                                                            
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)                                                       
         BZ    EDT18                                                            
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R5)                                     
         CLI   0(R1),0                                                          
         BNE   LFMERR                                                           
         L     R5,4(R1)                                                         
         C     R5,=F'32767'                                                     
         BH    LFMERR                                                           
         LTR   R5,R5                                                            
         BM    LFMERR                                                           
         STCM  R5,3,SCANTAX                                                     
*                                                                               
EDT18    LA    R2,STAFEEH          CANADIAN C-58 MEDIA SERVICE FEE              
         XC    SSVCFEE,SSVCFEE                                                  
         CLI   SVAPROF+7,C'C'      ONLY FOR CANADA                              
         BNE   EDT20                                                            
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)                                                       
         BZ    EDT20                                                            
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R5)                                     
         CLI   0(R1),0                                                          
         BNE   LFMERR                                                           
         L     R5,4(R1)                                                         
         C     R5,=F'32767'                                                     
         BH    LFMERR                                                           
         LTR   R5,R5                                                            
         BM    LFMERR                                                           
         STCM  R5,3,SSVCFEE                                                     
*                                                                               
EDT20    MVI   ERRCD,INVERR                                                     
         LA    R2,STANTISH         NTI STATION                                  
         MVC   DUB,=4X'40'                                                      
         CLI   5(R2),0                                                          
         BE    EDT20D                                                           
         CLI   NETPAK,C'Y'                                                      
         BNE   EDT20D                                                           
         MVC   DUB(4),8(R2)                                                     
         OC    DUB(4),=4X'40'                                                   
EDT20D   MVI   NTICHG,0                                                         
         CLC   SNTISTA,DUB                                                      
         BE    EDT30                                                            
         MVC   SNTISTA,DUB                                                      
         MVI   NTICHG,X'FF'                                                     
*                                                                               
EDT30    LA    R2,STAGSTH          GOODS & SERVICE TAX CODE                     
         MVI   SGSTCODE,0                                                       
         CLI   5(R2),0                                                          
         BE    EDT39                                                            
         LA    R5,GSTTAB                                                        
EDT35    CLC   0(1,R5),STAGST     VALID GST CODE?                               
         BE    EDT38                                                            
         CLI   0(R5),X'FF'        END OF TABLE?                                 
         BE    LFMERR             INVALID GST CODE                              
         LA    R5,1(R5)           NEXT ENTRY IN GST TABLE                       
         B     EDT35                                                            
*                                                                               
EDT38    MVC   SGSTCODE,STAGST    MOVE IN CORRECT GST CODE                      
         B     EDT39                                                            
*                                                                               
GSTTAB   DC    C'S'               STANDARD                                      
         DC    C'U'                                                             
         DC    C'X'                                                             
         DC    C'Z'               ZERO                                          
         DC    X'FFFF'            END OF TABLE                                  
*                                                                               
EDT39    CLI   NETPAK,C'Y'         IF NETWORK                                   
         BE    EDT40               SKIP                                         
         XC    SBKTYPE,SBKTYPE                                                  
         LA    R2,STABKTYH         BOOKTPE                                      
         CLI   STABKTYH+5,0        ANY INPUT?                                   
         BE    EDT40                                                            
         MVI   ERRCD,INVERR                                                     
         OC    SVEBCCLT,SVEBCCLT   ONLY VALID IF CLIENT PRESENT                 
         BZ    LFMERR              INVALID INPUT                                
         MVC   SBKTYPE,STABKTY                                                  
         B     EDT40                                                            
*                                                                               
         EJECT                                                                  
EDT40    LA    R2,STAPSTH          PROVINCE SERVICE TAX CODE                    
         XC    SPST,SPST                                                        
         CLI   5(R2),0                                                          
         BE    EDTX                                                             
         BAS   RE,VALPST                                                        
*                                                                               
EDTX     ST    R8,AREC             RESET AREC TO STATION MASTER                 
         MVC   KEY,SVKEY                                                        
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   WRITE                                                            
         MVC   REC(17),SVKEY                                                    
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
*            KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            'N'-TYPE RECORDS IS TO BE 144.                                     
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         GOTO1 CNADDSTA                                                         
         GOTO1 =A(CHKNTI),DMCB,(RC),RR=RELO                                     
*                                                                               
***********************************************************************         
*ATTENTION: THIS CODE CREATES THE TYPE 'N' RECORDS.  THESE RECORDS              
*           ARE PSEUDO-PASSIVE POINTERS TO 'S'-RECORDS IN THAT THEY             
*           CONTAIN THE MARKET/STATION INFO MSPACKED W/.IN KEY.                 
*                                                                               
         XCEF  REC2,1000           USE REC2 TO BUILD RECORD.                    
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
*                                                                               
         USING SFXKEYD,R7          TYPE-'N' RECORD FORMAT.                      
         MVI   SFXKTYPE,C'N'                                                    
         MVC   SFXKAGCY,STAKAGY    MOVE AGENCY INTO NEW RECORD.                 
         MVC   SFXKMED,STAKMED      "   MEDIA   "    "    "   .                 
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,SFXKMKCL                              
*                                  MSPACKED MARKET/STATION.                     
         MVC   SFXKCLT,STAKCLT     MOVE CLIENT INTO NEW RECORD.                 
         MVC   SFXRCLEN,=H'20'     L(N-RECORD)=20.                              
         DROP  R7                                                               
*                                                                               
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         GOTO1 CNADDSTA                                                         
         GOTO1 =A(CHKNTI),DMCB,(RC),RR=RELO                                     
*                                                                               
         ST    R8,AREC             RESTORE AREC=A(REC).                         
***********************************************************************         
*                                                                               
         B     REQREC                                                           
*                                                                               
WRITE    LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA               REREAD REC BEFORE WRITE                      
         TM    DMCB+8,X'50'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
*            KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            'N'-TYPE RECORDS IS TO BE 144.                                     
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         ST    R8,AREC                                                          
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 STA                                                              
         GOTO1 CNCHASTA                                                         
         GOTO1 =A(CHKNTI),DMCB,(RC),RR=RELO                                     
         B     REQREC                                                           
         EJECT                                                                  
REQREC   XC    REC(110),REC             GENERATE REQUEST RECORD                 
         MVI   REC+10,45                                                        
         MVI   REC+14,106                                                       
         MVI   REC+26,X'40'                                                     
         MVC   REC+27(79),REC+26                                                
         MVC   REC+26(2),=C'45'                                                 
         MVC   REC+28(2),AGYALPHA                                               
         MVC   REC+30(1),SVEBCMED                                               
         MVC   REC+31(3),=C'ALL'                                                
         CLC   SVKEY+9(3),=C'000'                                               
         BE    *+10                                                             
         MVC   REC+31(3),SVKEY+9                                                
         MVC   REC+36(1),SVKEY                                                  
         MVC   REC+44(5),SVKEY+2                                                
         MVC   REC+94(7),=C'CONTROL'                                            
         MVC   REC+93(1),SVACT                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                      
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
*                                                                               
VALPST   NTR1                                                                   
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,STAPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 VPSTVAL,DMCB,(R4)                                                
         MVI   ERRCD,INVERR                                                     
         CLI   PSTERR,0                                                         
         BNE   LFMERR                                                           
         MVC   SPST,PSTOUT         OUTPUT                                       
         BAS   RE,DISPPST           PROVINCE SERVICE TAX CODE                   
*                                                                               
VPX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
DISPPST  NTR1                                                                   
         MVC   STAPST,SPACES       OUTPUT                                       
         OI    STAPSTH+6,X'80'                                                  
         OC    SPST,SPST           IS THERE ANYTHING TO DISPLAY                 
         BZ    DPX                                                              
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,SPST                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 VPSTVAL,DMCB,(R4)                                                
         MVC   STAPST,PSTOUT       OUTPUT                                       
*                                                                               
DPX      B     XIT                                                              
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
EDTMKT   MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(4),SMKT                                                    
         MVC   KEY+6(2),AGYALPHA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA                                                            
         MVI   ERRCD,NOFNDERR                                                   
         TM    DMCB+8,X'10'          TEST ON FILE                               
         BNZ   LFMERR                                                           
         USING MKTRECD,R7                                                       
         FOUT  STAMKNMH,MKTNAME,24                                              
         ST    R8,AREC         RESET AREC                                       
         BR    R9                                                               
         DROP  R7                                                               
         EJECT                                                                  
*             ROUTINE TO FORMAT MARKET                                          
*                                                                               
FMTMKT   MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(4),SMKT                                                    
         MVC   KEY+6(2),AGYALPHA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA                                                            
         TM    DMCB+8,X'10'                                                     
         BNZ   FMTM1       RECORD NOT FOUND                                     
         USING MKTRECD,R7                                                       
         FOUT  (R2),MKTNAME,24                                                  
         B     FMTMX                                                            
*                                                                               
FMTM1    FOUT  (R2),=C'**MKT NOT ON FILE**',19                                  
FMTMX    ST    R8,AREC        RESET AREC TO R8                                  
         BR    R9                                                               
         DROP  R7                                                               
         EJECT                                                                  
*             ROUTINE TO READ GENERAL STATION MASTER RECORD                     
*                                                                               
RDGENSTA MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'KEY-1),KEY                                               
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA                                                            
         MVI   ERRCD,NOFNDERR                                                   
         TM    DMCB+8,X'10'                                                     
         BNZ   LFMERR            RECORD NOT FOUND                               
         MVC   SAVSMKT,18(R7)    SAVE GENERAL STATION MARKET                    
         ST    R8,AREC           RESET AREC TO R8                               
         BR    R9                                                               
         EJECT                                                                  
*                                  FOR NETWORK                                  
DUPMKT   MVI   KEY,0               ONLY ONE STATION IN ONE MARKET               
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         LA    R7,REC2                                                          
         GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=C'STATION',KEY,(R7),0                
DUP5     CLC   SVEBCMED,1(R7)                                                   
         BNE   DUP10                                                            
         CLC   AGYALPHA,7(R7)                                                   
         BE    DUP7                                                             
DUPSEQ   GOTO1 VDATAMGR,DMCB,=CL8'DMRSEQ',=C'STATION',KEY,(R7),0                
         B     DUP5                                                             
DUP7     CLC   SMKT,18(R7)         CHK MARKET NUMBER                            
         BNE   DUPSEQ                                                           
         MVI   ERRCD,245           DUPLICATE CODE ERROR                         
         B     LFMERR                                                           
*                                                                               
DUP10    BR    R9                                                               
         EJECT                                                                  
*             ROUTINE TO FORMAT REPS                                            
*                                                                               
FMTREP   CLC   0(3,R3),=C'000'                                                  
         BNE   FMTR0                                                            
         FOUT  (R2),SPACES,22                                                   
         B     FMTRX                                                            
*                                                                               
FMTR0    MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(3),0(R3)           R3 POINTS TO REP CODE                   
         MVC   KEY+5(2),AGYALPHA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA                                                            
         TM    DMCB+8,X'10'       TEST FOUND                                    
         BNZ   FMTR1                                                            
         USING REPRECD,R7                                                       
         FOUT  (R2),RNAME,22                                                    
         B     FMTRX                                                            
*                                                                               
FMTR1    FOUT  (R2),=C'**REP NOT ON FILE**',19                                  
FMTRX    ST    R8,AREC        RESET AREC                                        
         BR    R9                                                               
*                                                                               
         DROP  R7                                                               
*                                                                               
EDTREP   GOTO1 ANY                                                              
         CLI   5(R2),3                  MUST INPUT 3 CHARS                      
         BNE   LFMERR                                                           
***********           NOTE    REP NOW ALPHA-NUMERIC                             
         MVC   DUB(3),8(R2)                                                     
         CLC   DUB(3),=C'000'                                                   
         BE    EDTRX                                                            
*                                       READ REP INTO REC2                      
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'KEY-1),KEY                                               
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(3),DUB                                                     
         MVC   KEY+5(2),AGYALPHA                                                
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA                                                            
         MVI   ERRCD,NOFNDERR                                                   
         TM    DMCB+8,X'10'                                                     
         BNZ   LFMERR                                                           
         USING REPRECD,R7                                                       
         FOUT  (R3),RNAME,22                                                    
EDTRX    ST    R8,AREC        RESET AREC TO MASTER REC                          
         BR    R9                       RETURN                                  
         DROP  R7                                                               
*                                                                               
XIT      XIT1                                                                   
RELO     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CHKNTI   NMOD1 0,*CHKNTI*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   SVEBCMED,C'N'       NETWORK                                      
         BNE   CHKNTIEX                                                         
         CLI   NTICHG,X'FF'                                                     
         BNE   CHKNTIEX                                                         
         MVI   NTICHG,0                                                         
         LA    R5,REC2                                                          
         ST    R5,AREC                                                          
         USING SLSRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D75'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(5),SVKEY+2    STATION CALL LETTERS                         
         MVC   KEY+8(2),SVKEY+9    CLIENT CODE                                  
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CHKNTI1                                                          
         MVI   NTICHG,X'FF'                                                     
         GOTO1 GETREC                                                           
*                                                                               
CHKNTI1  XC    REC2(50),REC2                                                    
         MVC   SLSKEY(13),KEYSAVE                                               
         MVC   SLSLEN,=XL2'001F'                                                
         MVC   SLSEL01(2),=XL2'0107'                                            
*                                                                               
         XC    KEY,KEY             SET UP KEY                                   
         MVC   KEY(13),SVKEY                                                    
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         MVC   COMMAND,=CL8'DMRDHI'                                             
         GOTO1 STA                 READ STATION REC                             
*                                                                               
         MVC   SLSNTI(4),SNTISTA                                                
         MVC   SLSNTI+4(1),STYPE                                                
         SPACE 1                                                                
CHKNTI8  CLI   SLSNTI,X'40'                                                     
         BH    CHKNTI20                                                         
         CLI   NTICHG,X'FF'                                                     
         BNE   CHKNTIEX                                                         
         MVC   KEY(13),SLSKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
         MVI   KEY+13,X'80'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
         LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         MVI   SLSCNTL,X'C0'                                                    
         GOTO1 PUTREC                                                           
         B     CHKNTIEX                                                         
*                                                                               
CHKNTI20 CLI   NTICHG,X'FF'                                                     
         BE    CHKNTI25                                                         
         LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(13),SLSKEY                                                   
         GOTO1 ADDREC                                                           
         B     CHKNTIEX                                                         
*                                                                               
CHKNTI25 MVC   KEY(13),SLSKEY                                                   
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         TM    KEY+13,X'80'                                                     
         BZ    CHKNTI27                                                         
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
         NI    SLSCNTL,X'7F'                                                    
*                                                                               
CHKNTI27 LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         GOTO1 PUTREC                                                           
*                                                                               
CHKNTIEX XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
GENOLD   DSECT                                                                  
         ORG   REC2+1000                                                        
SAVSMKT  DS    CL4                 SAVED OLD MKT ON CHGES                       
NETPAK   DS    CL1                                                              
NTICHG   DS    CL1                 SWITCH FOR CHANGE IN NTI STATION             
PSTOUT   DS    CL64                64 BYTE OUT AREA FOR PST                     
         EJECT                                                                  
T219FFD  DSECT                                                                  
         ORG   LFMTABH                                                          
*SPLFME1                                                                        
       ++INCLUDE SPLFME1D                                                       
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMSTA         DSECT FOR TYPE-'N' RECORDS.                  
         EJECT                                                                  
       ++INCLUDE SPGENSLST                                                      
*                                                                               
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPSTAPACKD                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPLFM21   05/01/02'                                      
         END                                                                    
