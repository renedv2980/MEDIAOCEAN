*          DATA SET ACCLB51    AT LEVEL 080 AS OF 08/16/00                      
*PHASE T62151A                                                                  
CLB51    TITLE '- PC COMMUNICATIONS GENERAL CALLS'                              
CLB51    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB51**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         ICM   RF,15,ORCVHDRF                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
*                                                                               
RCVSNDRQ DS    0H                  ** RECEIVE SEND REQUEST **                   
         XC    REQLST,REQLST       CLEAR REQUEST LIST                           
         B     EXITY                                                            
*                                                                               
RCVSNDEL DS    0H                  ** RECEIVE SEND ELEMENT REQUEST **           
         L     R3,AMHEL            ADD RECEIVED CODE TO REQUEST LIST            
         USING MHELD,R3                                                         
         LA    RE,REQLST                                                        
         OC    0(L'MHCODE,RE),0(RE)                                             
         BZ    *+12                                                             
         LA    RE,L'MHCODE(RE)                                                  
         B     *-14                                                             
         MVC   0(L'MHCODE,RE),MHCODE                                            
         DROP  R3                                                               
         B     EXITY                                                            
*                                                                               
RCVACV   DS    0H                  ** RECEIVE ACCOUNT VALIDATION **             
         MVI   ACCODEN,0                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* MAP FIELD DATA RECEIVE                                              *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
*                                                                               
RCVAC    DS    0H                  ** RECEIVE ACCOUNT CODE **                   
         XR    RF,RF                                                            
         IC    RF,ACCODEN                                                       
         LA    RE,1(RF)                                                         
         STC   RE,ACCODEN                                                       
         MH    RF,=Y(L'ACTKULA)                                                 
         A     RF,AIO4                                                          
         MVC   0(L'ACTKULA,RF),DATA                                             
         B     EXITY                                                            
*                                                                               
RCVJOB   DS    0H                  ** RECEIVE JOB CODE **                       
         XC    CSBILCUR,CSBILCUR   MAKE SETUP PICKUP DEFAULT                    
         XC    CSCURBIL,CSCURBIL                                                
         GOTO1 ASETUP,BOPARM,DATA,0,0                                           
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* SEND REQUESTED DETAILS                                              *         
***********************************************************************         
         SPACE 1                                                                
SNDREQ   DS    0H                                                               
         LA    R2,REQLST           ALL MAP CODES RECEIVED ARE SENT              
SNDREQ02 OC    0(L'MHCODE,R2),0(R2)                                             
         BZ    SNDREQ10                                                         
         GOTO1 ASETMHEL,BOPARM,(R2)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AMHEL                                                         
         USING MHELD,R3                                                         
         LH    RF,MHSND                                                         
         LA    RF,CLB51(RF)                                                     
         BASR  RE,RF                                                            
*                                                                               
SNDREQ08 LA    R2,L'MHCODE(R2)                                                  
         B     SNDREQ02                                                         
*                                                                               
SNDREQ10 DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND ACCOUNT DETAILS                                                *         
***********************************************************************         
         SPACE 1                                                                
SNDACV   DS    0H                                                               
         XR    R0,R0                                                            
         ICM   R0,1,ACCODEN                                                     
         BZ    EXITY                                                            
         LA    R3,1                R3 = INDEX VALUE                             
         GOTO1 ASNDHDR,BOPARM,MH#ACV                                            
         L     R4,AIO4             R4 = A(ACCOUNT CODES)                        
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
SNDACV02 MVC   ACTKEY,BCSPACES     READ/VALIDATE ACCOUNT                        
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,0(R4)                                                    
         GOTO1 AGETACT,0                                                        
         BE    SNDACV04                                                         
         CLI   ACCODEN,1           TEST ONLY 1 ACCOUNT SENT                     
         BE    *+8                                                              
         STC   R3,FVINDX           SET INDEX                                    
         B     EXITN                                                            
SNDACV04 GOTO1 ASNDDATA,BOPARM,20,(L'ACNAME,ACNAME)                             
         LA    R4,L'ACTKULA(R4)                                                 
         LA    R3,1(R3)                                                         
         BCT   R0,SNDACV02                                                      
         POP   USING                                                            
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND JOB DETAILS                                                    *         
***********************************************************************         
         SPACE 1                                                                
SNDJOB   DS    0H                                                               
         PUSH  USING                                                            
         L     R5,AGOPBLK                                                       
         USING GOBLOCK,R5                                                       
         L     R4,GOABEXT                                                       
         USING GOBBLOCK,R4                                                      
*                                                                               
         GOTO1 ASNDHDR,BOPARM,MH#JOPT                                           
         GOTO1 ASNDDATA,(R1),1,BCJOBCOD                                         
         GOTO1 (RF),(R1),2,BCJOBNAM                                             
         CLI   GOPCBFRM,0                                                       
         BNE   *+8                                                              
         MVI   GOPCBFRM,1                                                       
         GOTO1 (RF),(R1),3,GOPCBFRM                                             
         GOTO1 (RF),(R1),4,BCCMPPRF+(PPRRECVU-PPRELD)                           
         GOTO1 (RF),(R1),5,GOSRGAC+L'ACTKCPY                                    
         GOTO1 (RF),(R1),6,GODSCAC+L'ACTKCPY                                    
         GOTO1 (RF),(R1),7,GOSRGPCT                                             
         GOTO1 (RF),(R1),8,GODSCPCT                                             
         GOTO1 (RF),(R1),9,GOIDUE                                               
         GOTO1 (RF),(R1),10,CSBILCUR                                            
         GOTO1 (RF),(R1),11,CSCURBIL+(CURTDECP-CURTABD)                         
         GOTO1 (RF),(R1),12,CSEXCRAT                                            
*                                                                               
         ZAP   BOPL81,BCPZERO      SEND NET/COMM PENDING                        
         ZAP   BOPL82,BCPZERO                                                   
         L     R3,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         XR    RF,RF                                                            
         USING SCIELD,R3                                                        
SJOB02   CLI   SCIEL,0                                                          
         BE    SJOB04                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCBAP                                                 
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R3,RF,SJOB02                                                     
         ZAP   BOPL81,SCIAMNT                                                   
         ZAP   BOPL82,SCIADMN                                                   
SJOB04   GOTO1 ASNDDATA,BOPARM,21,BOPL81                                        
         GOTO1 (RF),(R1),22,BOPL82                                              
         GOTO1 (RF),(R1),23,GOAGYCOM                                            
         DROP  R3                                                               
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEND USER CONNECTED DATA                                 *         
***********************************************************************         
         SPACE 1                                                                
USER     NTR1  ,                                                                
         GOTO1 ASNDHDR,BOPARM,01                                                
         GOTO1 ASNDDATA,(R1),1,CSBPID                                           
*                                                                               
         LA    R3,CSCURCPY         SEND AGENCY CURRENCY DATA                    
         USING CURTABD,R3                                                       
         GOTO1 (RF),(R1),02,CURTCUR        CURRENCY CODE                        
         GOTO1 (RF),(R1),12,CURTDECP       NUMBER OF DEC. PLACES                
         CLI   CUCTRY,CTRYGER                                                   
         BNE   USER02                                                           
         GOTO1 (RF),(R1),13,CURTCUR        CURRENCY PREFIX                      
         GOTO1 (RF),(R1),14,2              TRAILING BLANKS AFTER PREFIX         
         B     USER04                                                           
USER02   GOTO1 (RF),(R1),13,CURTSYMB       CURRENCY PREFIX                      
         DROP  R3                                                               
*                                                                               
USER04   DS    0H                                                               
         CLC   BCCPYSEC,C' '                                                    
         BNH   USER06                                                           
         GOTO1 (RF),(R1),3,BCCPYSEC                                             
*                                                                               
USER06   DS    0H                                                               
         MVI   USINDS,0                                                         
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BZ    *+8                                                              
         OI    USINDS,USIFCB                                                    
         CLI   CUCTRY,CTRYUSA      NO VAT IN USA                                
         BE    *+8                                                              
         OI    USINDS,USIVAT                                                    
         TM    CUSTAT,CUSDDS       CONNECTED TO DDS USER                        
         BZ    *+8                                                              
         OI    USINDS,USIDDS                                                    
         CLI   P#MANBIL,C'Y'       DISALLOW MANUAL BILL NUMBERS                 
         BNE   *+8                                                              
         OI    USINDS,USIDMBN                                                   
         GOTO1 (RF),(R1),4,USINDS                                               
         GOTO1 (RF),(R1),5,BCCLILEN                                             
         GOTO1 (RF),(R1),6,BCPROLEN                                             
         GOTO1 (RF),(R1),7,BCJOBLEN                                             
         ICM   RE,1,P#BACKDT                                                    
         BNZ   *+8                                                              
         LA    RE,60                                                            
         STC   RE,BOBYTE1                                                       
         GOTO1 (RF),(R1),8,BOBYTE1                                              
         GOTO1 (RF),(R1),9,BCTODAYP   IN CASE THE Y2K SYSTEM                    
         ICM   RE,1,P#RETDAY                                                    
         BNZ   *+8                                                              
         LA    RE,7                                                             
         STC   RE,BOBYTE1                                                       
         GOTO1 (RF),(R1),10,BOBYTE1                                             
         MVC   BOBYTE1,CUCTRY                                                   
         CLI   BOBYTE1,0                                                        
         BNE   *+8                                                              
*&&UK*&& MVI   BOBYTE1,CTRYGBR                                                  
*&&US*&& MVI   BOBYTE1,CTRYUSA                                                  
         GOTO1 (RF),(R1),11,BOBYTE1                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD WORKCODE LIST FOR INTIALISATION SEND               *         
***********************************************************************         
                                                                                
WORKCODE NTR1  ,                                                                
                                                                                
         USING WCORECD,IOKEY                                                    
         XC    WCOKEY,WCOKEY       READ WORKCODE RECORD                         
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         GOTO1 AIO,IO1+IOACCDIR+IOHI                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
WC10     CLC   WCOKEY(WCOKWRK-WCOKEY),IOKEYSAV                                  
         BNE   EXIT                                                             
         GOTO1 ASNDHDR,BOPARM,02                                                
                                                                                
         GOTO1 ASNDDATA,BOPARM,1,WCOKWRK                                        
                                                                                
         GOTO1 AIO,IO1+IOACCMST+IOGET                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('WCOELQ',AIO1),0                    
         CLI   12(R1),0                                                         
         BNE   WC20                                                             
                                                                                
         L     R5,12(R1)                                                        
         USING WCOELD,R5                                                        
         GOTO1 ASNDDATA,BOPARM,2,WCODESC                                        
         DROP  R5                                                               
                                                                                
WC20     GOTO1 AIO,IO1+IOACCDIR+IOSEQ  GET NEXT RECORD                          
         BE    WC10                                                             
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE TO BUILD WORKGROUP LIST FOR INTIALISATION SEND              *         
***********************************************************************         
                                                                                
WORKGRP  NTR1  ,                                                                
                                                                                
         USING WGRRECD,IOKEY                                                    
         XC    WGRKEY,WGRKEY       READ WORK GROUP RECORD                       
         MVI   WGRKTYP,WGRKTYPQ                                                 
         MVI   WGRKSUB,WGRKSUBQ                                                 
         MVC   WGRKCPY,CUABIN                                                   
         MVC   WGRKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         GOTO1 AIO,IO1+IOACCDIR+IOHI                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
WG10     CLC   WGRKEY(WGRKCODE-WGRKEY),IOKEYSAV                                 
         BNE   EXIT                                                             
         GOTO1 ASNDHDR,BOPARM,03                                                
         GOTO1 ASNDDATA,BOPARM,1,WGRKCODE                                       
                                                                                
         GOTO1 AIO,IO1+IOACCMST+IOGET                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('NAMELQ',AIO1),0                    
         CLI   12(R1),0                                                         
         BNE   WG20                                                             
                                                                                
         L     R5,12(R1)                                                        
         USING NAMELD,R5                                                        
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         GOTO1 ASNDDATA,BOPARM,2,((RF),NAMEREC)                                 
         DROP  R5                                                               
                                                                                
WG20     GOTO1 AIO,IO1+IOACCDIR+IOSEQ  GET NEXT RECORD                          
         BE    WG10                                                             
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE TO BUILD WORKCODE TYPE LIST FOR INTIALISATION RECORD DLOAD  *         
* ??                                                                  *         
***********************************************************************         
                                                                                
WORKTYP  NTR1  ,                                                                
         GOTO1 ASNDHDR,BOPARM,04                                                
                                                                                
         LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
                                                                                
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         BNE   WT10                                                             
         GOTO1 ASNDDATA,BOPARM,1,(L'UC@INT,UC@INT)                              
         GOTO1 ASNDDATA,BOPARM,1,(L'UC@EXT,UC@EXT)                              
         B     EXIT                                                             
                                                                                
WT10     CLI   CUCTRY,CTRYGBR      TEST UK                                      
         BNE   EXIT                                                             
         GOTO1 ASNDDATA,BOPARM,1,(L'UC@TIME,UC@TIME)                            
         GOTO1 ASNDDATA,BOPARM,1,(L'UC@COST,UC@COST)                            
         B     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* ROUTINE TO BUILD TRANSACTION TYPE LIST FOR INTIALISATION REC DLOAD  *         
***********************************************************************         
                                                                                
TRNTYP   NTR1  ,                                                                
         GOTO1 ASNDHDR,BOPARM,05                                                
                                                                                
         LA    R5,TTTAB                                                         
         USING TTTABD,R5                                                        
TT10     CLI   TTNUM,EOT           TEST TYPE IS VALID                           
         BE    EXIT                                                             
         CLI   TTCTRY,CTRYALL                                                   
         BE    *+14                                                             
         CLC   TTCTRY,CUCTRY                                                    
         BNE   TT20                                                             
         TM    TTIND1,TTIDDS                                                    
         BZ    *+12                                                             
         CLI   CUSTAT,CUSDDS                                                    
         BNE   TT20                                                             
                                                                                
         GOTO1 ASNDDATA,BOPARM,1,TTNUM                                          
         MVC   BOWORK1,BCSPACES                                                 
         MVI   BOWORK1+00,AC#ESCL                                               
         MVC   BOWORK1+01(2),TTDICT                                             
         MVI   BOWORK1+03,15                                                    
         GOTO1 VDICTAT,BOPARM,C'SL  ',BOWORK1                                   
         GOTO1 ASNDDATA,BOPARM,2,(15,BOWORK1)                                   
                                                                                
TT20     LA    R5,TTTABLN(R5)      BUMP TO NEXT TYPE                            
         B     TT10                                                             
         DROP  R5                                                               
                                                                                
***********************************************************************         
* ROUTINE TO BUILD FORMAT LIST FOR INTIALISATION SEND                 *         
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1  ,                                                                
         PUSH  USING                                                            
         USING BFMRECD,IOKEY                                                    
         XC    BFMKEY,BFMKEY       READ FORMAT RECORD                           
         MVI   BFMKTYP,BFMKTYPQ                                                 
         MVC   BFMKCPY,CUABIN                                                   
         MVI   BFMKSUB,BFMKSUBQ                                                 
*                                                                               
FMT02    GOTO1 AIO,IOHI+IOACCDIR+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BFMKEY(BFMKFMT-BFMKEY),IOKEYSAV                                  
         BNE   EXIT                                                             
         GOTO1 ASNDHDR,BOPARM,6                                                 
*                                                                               
         GOTO1 ASNDDATA,BOPARM,1,BFMKFMT                                        
*                                                                               
         CLI   BFMKLANG,0          TEST LANGUAGE CODE                           
         BE    FMT06                                                            
         L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         CLI   BFMKLANG,LANGEUK                                                 
         BNE   FMT04                                                            
         MVI   BFMKLANG,LANGENG                                                 
*                                                                               
         USING LANGTABD,R2                                                      
FMT04    CLI   LANGTABD,FF                                                      
         BE    FMT06                                                            
         CLC   LANGCODE,BFMKLANG                                                
         BE    *+12                                                             
         LA    R2,LANGTABL(R2)                                                  
         B     FMT04                                                            
*                                                                               
         GOTO1 ASNDDATA,BOPARM,2,(L'LANGFULN,LANGFULN)                          
         DROP R2                                                                
*                                                                               
FMT06    GOTO1 AIO,IO1+IOACCMST+IOGET                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('NAMELQ',AIO1),0                    
         CLI   12(R1),0                                                         
         BNE   FMT08                                                            
*                                                                               
         L     R5,12(R1)                                                        
         USING NAMELD,R5                                                        
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q)                                                   
         GOTO1 ASNDDATA,BOPARM,3,((RF),NAMEREC)                                 
         DROP  R5                                                               
*                                                                               
FMT08    MVI   BFMKLVL,FF          SET FOR NEXT FORMAT HEADER RECORD            
         B     FMT02                                                            
         DC    H'0'                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR SECURITY FOR EACH ACTION                       *         
***********************************************************************         
                                                                                
ACTION   NTR1                                                                   
         GOTO1 ASNDHDR,BOPARM,07                                                
         LA    R5,ACTTAB                                                        
ACTION0  CLI   0(R5),X'00'                                                      
         BE    ACTIONX                                                          
         GOTO1 ASNDDATA,BOPARM,01,(R5)                                          
         MVI   BOBYTE1,C'Y'                                                     
         GOTO1 VSECRET,BOPARM,('SECPRACT',ASECBLK),('RECBIL',(R5))              
         BE    *+8                                                              
         MVI   BOBYTE1,C'N'                                                     
         GOTO1 ASNDDATA,BOPARM,02,BOBYTE1                                       
         LA    R5,1(R5)                                                         
         B     ACTION0                                                          
ACTIONX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RECEIVE JOB (FOR JOB/WC VALUES)                          *         
***********************************************************************         
         SPACE 1                                                                
RCVJWJOB DS    0H                                                               
         MVC   JWJOB,DATA                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO RECEIVE WORK CODE (FOR JOB/WC VALUES)                    *         
***********************************************************************         
         SPACE 1                                                                
RCVJWWC  DS    0H                                                               
         MVC   JWWC,DATA                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SEND W/C AND JOB VALUES                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SNDJW    DS    0H                                                               
         XC    CSBILCUR,CSBILCUR   MAKE SETUP PICKUP DEFAULT                    
         XC    CSCURBIL,CSCURBIL                                                
         GOTO1 ASETUP,BOPARM,JWJOB,0,0                                          
*                                                                               
         L     R5,AGOPBLK                                                       
         USING GOBLOCK,R5                                                       
         LA    R2,IOKEY                                                         
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,JWJOB                                                    
         MVC   TRNKWORK,JWWC                                                    
         GOTO1 AGETOPT,BOPARM,TRNKEY                                            
         GOTO1 ASNDHDR,BOPARM,MH#JWCOP                                          
         GOTO1 ASNDDATA,BOPARM,03,GOAGYCOM                                      
         DROP  R2                                                               
*                                                                               
         USING VTCD,JWVTCD                                                      
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUABIN                                                    
         TM    BCCPYST1,CPYSOROE   TEST ON OFFICES                              
         BNO   *+10                                                             
         MVC   VTCOFFC,CSOFFICE                                                 
         MVC   VTCTYPE,GOTAXCOD                                                 
         MVC   VTCCOMF,ACOM                                                     
         MVC   VTCINVD,BCTODAYP                                                 
         GOTO1 VVATICAN,VTCD                                                    
         GOTO1 ASNDDATA,BOPARM,04,VTCRATE                                       
         GOTO1 (RF),(R1),05,VTCTTYPE                                            
*                                                                               
         MVI   BOBYTE1,C'Y'        DEFAULT IS SURCHARGEABLE                     
*&&UK                                                                           
         LA    R2,IOKEY                                                         
         USING WCORECD,R2                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,JWWC                                                     
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   SJW06                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BNE   SJW06                                                            
         L     R2,AIO1                                                          
         LA    R3,WCORFST                                                       
         USING WCOELD,R3                                                        
         XR    RF,RF                                                            
SJW02    CLI   WCOEL,WCOELQ                                                     
         BE    SJW04                                                            
         CLI   WCOEL,0                                                          
         BE    SJW06                                                            
         IC    RF,WCOLN                                                         
         BXH   R3,RF,SJW02                                                      
SJW04    TM    WCOSTAT2,WCOSNSCH   TEST WC IS NOT SURCHARGEABLE                 
         BZ    SJW06                                                            
         MVI   BOBYTE1,C'N'                                                     
SJW06    GOTO1 ASNDDATA,BOPARM,06,BOBYTE1                                       
*&&                                                                             
*                                                                               
SNDJWX   B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ACTTAB   DC    AL1(ACTSET,ACTUPD,ACTALC,ACTWOF,ACTXFR,ACTEDT,ACTZOO)            
         DC    AL1(ACTAFRM,ACTSUM,ACTLST,ACTZWO,ACTFFRM,ACTINS,ACTLPAR)         
         DC    AL1(ACTLFT,ACTDRA,ACTFEE,ACTFOP,ACTPRB,ACTREV,ACTRVL)            
         DC    AL1(ACTMT1,ACTMT2,ACTAALC,ACTZWR,ACTAULC,ACTRCVR,ACTARV)         
         DC    AL1(ACTFLI,ACTCOL,ACTHLP,ACTJLI,ACTSNM,ACTSDF,ACTDWN)            
         DC    AL1(ACTEDTLV,ACTFT1)                                             
         DC    X'00'                                                            
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* MAP TABLE                                                          *          
**********************************************************************          
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
         SPACE 1                                                                
*                                  ** REQUEST KEY **                            
M#REQ    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#INI)         ELEMENT CODE                                 
         DC    AL2(M#REQX+1-M#REQ) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDRQ-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDREQ-CLB51)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
M#REQX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** USER DATA ELEMENT **                      
M#USER   DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(01)             ELEMENT CODE                                 
         DC    AL2(M#USERX+1-M#USER) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDEL-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(USER-CLB51)     SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR PERSON-ID                   
         DC    CL5'PID  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSBPID)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE COMPANY CURRENCY                
         DC    CL5'CPCUR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSCPYCUR)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE FOR SECONDARY CURRENCY          
         DC    CL5'SCCUR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCCPYSEC)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE FOR INDICATORS                  
         DC    CL5'INDS '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'USINDS)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE FOR CLIENT LENGTH               
         DC    CL5'CLILN'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCCLILEN)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE FOR PRODUCT LENGTH              
         DC    CL5'PROLN'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCPROLEN)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE FOR JOB LENGTH                  
         DC    CL5'JOBLN'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCJOBLEN)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE FOR JOB LENGTH                  
         DC    CL5'BCKDT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'P#BACKDT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE FOR JOB LENGTH                  
         DC    CL5'TODAY'          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCTODAYP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE FOR JOB LENGTH                  
         DC    CL5'RETDY'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'P#RETDAY)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE FOR JOB LENGTH                  
         DC    CL5'CTRY'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CUCTRY)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE                                 
         DC    CL5'CURDP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'CURTDECP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'CURPR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'CURTSYMB)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'CURTB'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#USERX  DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** WORK-CODE ELEMENT **                      
M#WC     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(02)             ELEMENT CODE                                 
         DC    AL2(M#WCX+1-M#WC)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDEL-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(WORKCODE-CLB51) SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR WORKCODE                    
         DC    CL5'WCODE'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WCOKWRK)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR NAME                        
         DC    CL5'WNAME'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WCODESC)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#WCX    DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** WORKGROUP ELEMENT **                      
M#WG     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(03)             ELEMENT CODE                                 
         DC    AL2(M#WGX+1-M#WG)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDEL-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(WORKGRP-CLB51)  SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR CODE                        
         DC    CL5'WGRP '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'WGRKCODE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR CODE                        
         DC    CL5'WGNAM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#WGX    DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** WORK TYPE ?? **                           
M#WT     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(04)             ELEMENT CODE                                 
         DC    AL2(M#WTX+1-M#WT)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDEL-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(WORKTYP-CLB51)  SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR CODE                        
         DC    CL5'WCTYP'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#WTX    DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** TRANSACTION TYPE **                       
M#TT     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(05)             ELEMENT CODE                                 
         DC    AL2(M#TTX+1-M#TT)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDEL-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(TRNTYP-CLB51)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'TRNTP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE                                    
         DC    AL1(L'TTNUM)        DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'TRNNM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#TTX    DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** FORMAT RECORD **                          
M#FMT    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(06)             ELEMENT CODE                                 
         DC    AL2(M#FMTX+1-M#FMT) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDEL-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(FORMAT-CLB51)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'FORMT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BFMKFMT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'FTLAN'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'FTNAM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#FMTX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** FORMAT RECORD **                          
M#ACT    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(07)             ELEMENT CODE                                 
         DC    AL2(M#ACTX+1-M#ACT) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVSNDEL-CLB51) FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(ACTION-CLB51)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'ACT  '          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'YESNO'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#ACTX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** ACCOUNT VALIDATION REQUEST KEY **         
M#ACV    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#ACV)         ELEMENT CODE                                 
         DC    AL2(M#ACVX+1-M#ACV) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(RCVACV-CLB51)   FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDACV-CLB51)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR WORKCODE                    
         DC    CL5'CODE'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVAC-CLB51)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE FOR NAME                        
         DC    CL5'NAME'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#ACVX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
*                                  ** GET JOB VALUES REQUEST KEY **             
M#JOPT   DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#JOPT)        ELEMENT CODE                                 
         DC    AL2(M#JOPTX+1-M#JOPT) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDJOB-CLB51)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR WORKCODE                    
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCJOBCOD)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB51)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR NAME                        
         DC    CL5'NAME'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCJOBNAM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'FRMAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSFORMAT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'RCVAC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PPRRECV-1)    DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'SRGAC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GOSRGAC-1)    DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'DSCAC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GODSCAC-1)    DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'SRGPC'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GOSRGPCT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'DSCPC'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GODSCPCT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'IDUE '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GOIDUE)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'CUR  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSBILCUR)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'CURDP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CURTDECP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE                                 
         DC    CL5'EXCHR'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSEXCRAT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(21)             MAPPING CODE                                 
         DC    CL5'NET  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BOPL81)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(22)             MAPPING CODE                                 
         DC    CL5'COMM '          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BOPL82)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(23)             MAPPING CODE                                 
         DC    CL5'CMRAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GOAGYCOM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#JOPTX  DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
M#JWCOP  DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#JWCOP)       ELEMENT CODE                                 
         DC    AL2(M#JWCOPX+1-M#JWCOP) DISP TO NEXT ELEMENT HEADER              
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDJW-CLB51)    SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR WORKCODE                    
         DC    CL5'JOBCD'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'BCJOBCOD)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJWJOB-CLB51) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR NAME                        
         DC    CL5'W/C '           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKWORK)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJWWC-CLB51)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'COMRT'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'GOAGYCOM)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'VATRT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'VTCRATE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'VATTP'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'VTCTTYPE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'SURCH'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#JWCOPX DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
         EJECT                                                                  
**********************************************************************          
* TRANSACTION TYPE TABLE                                             *          
**********************************************************************          
         SPACE 1                                                                
TTTABD   DSECT                                                                  
TTNUM    DS    XL1                 TRANSACTION TYPE NUMBER                      
TTIND1   DS    XL1                 INDICATORS - ONE                             
TTIDDS   EQU   X'80'               DDS ONLY BATCH TYPE                          
TTINOD   EQU   X'40'               DON'T DISPLAY AT ALL                         
TTCTRY   DS    XL1                 COUNTRY                                      
TTDICT   DS    AL2                 DICTIONARY REF#                              
TTTABLN  EQU    *-TTTABD                                                        
         SPACE 1                                                                
CLB51    CSECT                                                                  
TTTAB    DS    0X                  TYPE/INDICATOR/COUNTRY                       
*&&US                                                                           
         DC    AL1(1,0,CTRYALL),AL2(AC#TT001)                                   
         DC    AL1(3,0,CTRYALL),AL2(AC#TT003)                                   
         DC    AL1(5,0,CTRYALL),AL2(AC#TT005)                                   
         DC    AL1(6,0,CTRYALL),AL2(AC#TT006)                                   
         DC    AL1(7,0,CTRYALL),AL2(AC#TT007)                                   
         DC    AL1(8,0,CTRYALL),AL2(AC#TT008)                                   
         DC    AL1(14,0,CTRYALL),AL2(AC#TT014)                                  
         DC    AL1(15,0,CTRYALL),AL2(AC#TT015)                                  
         DC    AL1(19,TTINOD+TTIDDS,CTRYALL),AL2(AC#TT019)                      
         DC    AL1(20,0,CTRYALL),AL2(AC#TT020)                                  
         DC    AL1(21,0,CTRYALL),AL2(AC#TT021)                                  
         DC    AL1(22,0,CTRYALL),AL2(AC#TT022)                                  
         DC    AL1(26,0,CTRYALL),AL2(AC#TT026)                                  
         DC    AL1(27,0,CTRYALL),AL2(AC#TT027)                                  
         DC    AL1(30,0,CTRYALL),AL2(AC#TT030)                                  
         DC    AL1(33,0,CTRYALL),AL2(AC#TT033)                                  
         DC    AL1(34,0,CTRYALL),AL2(AC#TT034)                                  
         DC    AL1(36,0,CTRYALL),AL2(AC#TT036)                                  
         DC    AL1(37,0,CTRYALL),AL2(AC#TT037)                                  
         DC    AL1(41,0,CTRYALL),AL2(AC#TT041)                                  
         DC    AL1(45,0,CTRYALL),AL2(AC#TT045)                                  
         DC    AL1(46,0,CTRYALL),AL2(AC#TT046)                                  
         DC    AL1(47,TTINOD,CTRYALL),AL2(AC#TT047)                             
         DC    AL1(48,0,CTRYALL),AL2(AC#TT048)                                  
         DC    AL1(49,0,CTRYALL),AL2(AC#TT049)                                  
         DC    AL1(51,0,CTRYALL),AL2(AC#TT051)                                  
         DC    AL1(53,0,CTRYALL),AL2(AC#TT053)                                  
         DC    AL1(54,0,CTRYALL),AL2(AC#TT054)                                  
         DC    AL1(55,0,CTRYALL),AL2(AC#TT055)                                  
         DC    AL1(56,0,CTRYALL),AL2(AC#TT056)                                  
         DC    AL1(57,0,CTRYALL),AL2(AC#TT057)                                  
         DC    AL1(58,0,CTRYALL),AL2(AC#TT058)                                  
         DC    AL1(61,0,CTRYUSA),AL2(AC#TT061)                                  
         DC    AL1(62,0,CTRYALL),AL2(AC#TT062)                                  
         DC    AL1(96,0,CTRYALL),AL2(AC#TT096)                                  
         DC    AL1(97,0,CTRYALL),AL2(AC#TT097)                                  
         DC    AL1(98,0,CTRYALL),AL2(AC#TT098)                                  
         DC    AL1(99,0,CTRYALL),AL2(AC#TT099)                                  
*&&                                                                             
*&&UK                                                                           
         DC    AL1(1,0,CTRYALL),AL2(AC#TT001)                                   
         DC    AL1(2,0,CTRYALL),AL2(AC#TT002)                                   
         DC    AL1(3,0,CTRYALL),AL2(AC#TT003)                                   
         DC    AL1(5,0,CTRYALL),AL2(AC#TT005)                                   
         DC    AL1(6,0,CTRYALL),AL2(AC#TT006)                                   
         DC    AL1(7,0,CTRYALL),AL2(AC#TT007)                                   
         DC    AL1(8,0,CTRYALL),AL2(AC#TT008)                                   
         DC    AL1(11,0,CTRYALL),AL2(AC#TT011)                                  
         DC    AL1(14,0,CTRYALL),AL2(AC#TT014)                                  
         DC    AL1(17,0,CTRYALL),AL2(AC#TT017)                                  
         DC    AL1(18,0,CTRYALL),AL2(AC#TT018)                                  
         DC    AL1(19,0,CTRYALL),AL2(AC#TT019)                                  
         DC    AL1(21,0,CTRYALL),AL2(AC#TT021)                                  
         DC    AL1(22,0,CTRYALL),AL2(AC#TT022)                                  
         DC    AL1(26,0,CTRYALL),AL2(AC#TT026)                                  
         DC    AL1(27,0,CTRYGER),AL2(AC#TT027)                                  
         DC    AL1(30,0,CTRYALL),AL2(AC#TT030)                                  
         DC    AL1(34,0,CTRYALL),AL2(AC#TT034)                                  
         DC    AL1(36,0,CTRYALL),AL2(AC#TT036)                                  
         DC    AL1(37,0,CTRYALL),AL2(AC#TT037)                                  
         DC    AL1(45,0,CTRYALL),AL2(AC#TT045)                                  
         DC    AL1(46,0,CTRYGER),AL2(AC#TT046)                                  
         DC    AL1(49,0,CTRYALL),AL2(AC#TT049)                                  
         DC    AL1(57,0,CTRYALL),AL2(AC#TT057)                                  
         DC    AL1(58,0,CTRYALL),AL2(AC#TT058)                                  
         DC    AL1(59,0,CTRYALL),AL2(AC#TT059)                                  
         DC    AL1(70,0,CTRYGBR),AL2(AC#TT070)                                  
         DC    AL1(70,0,CTRYGER),AL2(AC#TT070)                                  
         DC    AL1(70,0,CTRYHOL),AL2(AC#TT070)                                  
         DC    AL1(71,0,CTRYGBR),AL2(AC#TT071)                                  
         DC    AL1(71,0,CTRYGER),AL2(AC#TT071)                                  
         DC    AL1(71,0,CTRYHOL),AL2(AC#TT071)                                  
         DC    AL1(75,0,CTRYGBR),AL2(AC#TT075)                                  
         DC    AL1(75,0,CTRYHOL),AL2(AC#TT075)                                  
         DC    AL1(79,0,CTRYALL),AL2(AC#TT079)                                  
         DC    AL1(96,0,CTRYALL),AL2(AC#TT096)                                  
         DC    AL1(97,0,CTRYALL),AL2(AC#TT097)                                  
         DC    AL1(98,0,CTRYALL),AL2(AC#TT098)                                  
         DC    AL1(99,0,CTRYALL),AL2(AC#TT099)                                  
*&&                                                                             
TTTABX   DC    AL1(EOT)                                                         
         EJECT                                                                  
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* FASECRET                                                                      
       ++INCLUDE FASECRETD                                                      
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
USINDS   DS    XL1                 USER INDICATOR BYTE                          
USIFCB   EQU   X'80'               FOREIGN CURRENCY BILLING                     
USIVAT   EQU   X'40'               VAT REQUIRED                                 
USICOV   EQU   X'20'               COVER PAGE ALLOWED                           
USISUM   EQU   X'10'               SUMMARY PAGE ALLOWED                         
USIGRP   EQU   X'08'               GROUPING HEADERS/TOTALS ALLOWED              
USIDDS   EQU   X'04'               CONNECTED TO DDS                             
USIDMBN  EQU   X'02'               DISALLOW MANUAL BILL NUMBERS                 
*                                                                               
ACCODEN  DS    XL1                 ACCOUNT CODE COUNT                           
*                                                                               
JWJOB    DS    CL12                JOB (FOR JOB/WORKCODE VALUES)                
JWWC     DS    CL2                 WORK-CODE (FOR JOB/WORKCODE VLAUES)          
JWVTCD   DS    XL(VTCLNQ)          VATICAN BLOCK                                
*                                                                               
REQLST   DS    XL(50*L'MHCODE)                                                  
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080ACCLB51   08/16/00'                                      
         END                                                                    
