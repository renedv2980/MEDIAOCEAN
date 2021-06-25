*          DATA SET REREPJB02  AT LEVEL 019 AS OF 05/01/02                      
*          DATA SET REREPJB02  AT LEVEL 017 AS OF 04/28/86                      
*PHASE REJB02A                                                                  
         TITLE 'REJB02'                                                         
REJB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**JB02**,RR=RE                                                 
*                                                                               
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
**NOP    B     JB001                                                            
         B     MH000                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
MH000    XC    KEY,KEY                                                          
         MVC   KEY(4),=X'0C00D1C2'                                              
         GOTO1 HIGHDIR                                                          
         B     MH004                                                            
*                                                                               
MH002    GOTO1 SEQDIR                                                           
*                                                                               
MH004    CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         LA    R4,ADVT                                                          
*                                                                               
MH006    CLC   KEY+19(4),0(R4)     MATCH ADVERTISER                             
         BE    MH010                                                            
         LA    R4,8(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   MH006                                                            
         B     MH002                                                            
*                                                                               
MH010    GOTO1 HEXOUT,DMCB,KEY,P+10,27,=C'N'                                    
         MVC   P+70(27),KEY                                                     
         GOTO1 REPORT                                                           
         B     MH002                                                            
         EJECT                                                                  
JB001    OPEN  (IN,INPUT)                                                       
         LA    R2,INREC+4                                                       
         ICM   R5,15,=X'99999999'                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
JB010    GET   IN,INREC                                                         
         CLC   0(4,R2),=X'0C00D1C2'   JB CONTRACT                               
         BNE   JB010                                                            
         CLC   4(12,R2),=C'RWKODAFNYSMS'                                        
         BNE   JB010                                                            
*                                                                               
JB030    XC    KEY,KEY             BUILD 8C KEY                                 
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),=C'JB'                                                 
         ICM   RE,15,23(R2)        CONTRACT NUMBER 9'S COMPLEMENT               
         LR    RF,R5                                                            
         SR    RF,RE                                                            
         STCM  RF,15,KEY+23                                                     
         BAS   RE,HIGHDIR          READ 8C POINTER                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETCONT          GET THE RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         B     JB044                                                            
*                                                                               
         LA    R4,ADVT                                                          
*                                                                               
JB034    DS    0H                                                               
*        CLC   19(4,R2),0(R4)      MATCH ADVERTISER                             
*        BE    JB040                                                            
*        LA    R4,8(R4)                                                         
*        CLI   0(R4),X'FF'                                                      
*        BNE   JB034                                                            
*        CLC   19(4,R2),JBREC+19   TEST SUSAN CLOBBERED IT                      
         BE    JB010                                                            
         MVC   SAVEIT(4),JBREC+19                                               
         MVC   SAVEIT+4(4),19(R2)                                               
         LA    R4,SAVEIT                                                        
         B     JB042                                                            
SAVEIT   DS    CL8                                                              
*                                                                               
JB040    CLC   JBREC+19(4),4(R4)   TEST FOR ANY CHANGE NECESSARY                
         BE    JB010                                                            
JB042    MVC   P(4),0(R4)                                                       
         GOTO1 REPORT                                                           
JB044    MVC   P(6),=C'GETREC'                                                  
         GOTO1 HEXOUT,DMCB,JBREC,P+10,27,=C'N'                                  
         MVC   P+67(4),JBREC+19                                                 
         GOTO1 REPORT                                                           
******   MVC   JBREC+19(4),4(R4)   CHANGE THE ADVERTISER                        
******   BAS   RE,PUTCONT          PUT THE RECORD                               
         B     JB010               READ NEXT TAPE RECORD                        
*                                                                               
JB900    CLOSE (IN)                FINISHED                                     
         MVI   MODE,REQLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE                                                 
*                                                                               
         SPACE                                                                  
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
*                                                                               
SEQDIR   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
*                                                                               
GETCONT  LA    R6,GETREC                                                        
         XC    JBREC(250),JBREC                                                 
         MVC   JBREC+250(250),JBREC                                             
         MVC   JBREC+500(250),JBREC                                             
         MVC   JBREC+750(250),JBREC                                             
         B     LINKFILE                                                         
*                                                                               
PUTCONT  ST    RE,FULL                                                          
         MVC   P(6),=C'PUTREC'                                                  
         GOTO1 HEXOUT,DMCB,JBREC,P+10,27,=C'N'                                  
         MVC   P+67(4),JBREC+19                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         L     RE,FULL                                                          
         LA    R6,PUTREC                                                        
         CLI   QOPTION1,C'Y'                                                    
         BE    LINKFILE                                                         
         BR    RE                                                               
*                                                                               
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               JBREC,(0,DMWORK)                                                 
         B     DMCHECK                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
*                                                                               
         SPACE                                                                  
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
*                                                                               
         MVC   WORK(41),=C'*** DATA MANAGER ERROR IN INPUT PHASE ***'           
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'            BLOW UP                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO TRACE DATA MANAGER CALLS                                    
*                                                                               
         SPACE                                                                  
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
ADVT     DC    CL8'BMW X812'                                                    
         DC    CL8'CP  YCE '                                                    
         DC    CL8'FL  FRIT'                                                    
         DC    CL8'GULFGO  '                                                    
         DC    CL8'HW  QHBI'                                                    
         DC    CL8'KS  KELS'                                                    
         DC    CL8'PBELPCB '                                                    
         DC    CL8'PIP 279 '                                                    
         DC    CL8'PR  PRUF'                                                    
         DC    CL8'TWEN20  '                                                    
         DC    CL8'CON 1239'                                                    
         DC    CL8'ELT ELTO'                                                    
         DC    CL8'GREYGRH '                                                    
         DC    CL8'GWS L016'                                                    
         DC    CL8'KRAFKRT '                                                    
         DC    CL8'LBR LESS'                                                    
         DC    CL8'PHIL3122'                                                    
         DC    CL8'PM  PLY '                                                    
         DC    CL8'SER SELC'                                                    
         DC    CL8'MOBIMBOL'                                                    
         DC    CL8'MP  1042'                                                    
         DC    CL8'NM  XNJ '                                                    
         DC    CL8'PA  PN  '                                                    
         DC    CL8'PD  PEDA'                                                    
         DC    CL8'PHP PO  '                                                    
         DC    CL8'PS  PRSP'                                                    
         DC    CL8'PUB N079'                                                    
         DC    CL8'RCB RCBG'                                                    
         DC    CL8'RHC 3053'                                                    
         DC    CL8'RV  RVIC'                                                    
         DC    CL8'SEA X691'                                                    
         DC    CL8'SHC SHCL'                                                    
         DC    CL8'SMP STMP'                                                    
         DC    CL8'STANC001'                                                    
         DC    CL8'SUGASDF '                                                    
         DC    CL8'SW  SAW '                                                    
         DC    CL8'SWC SWT '                                                    
         DC    CL8'SYNTSTEX'                                                    
         DC    CL8'TEN TENL'                                                    
         DC    CL8'TF  TFS '                                                    
         DC    CL8'MONAMDCC'                                                    
         DC    CL8'NAL 2048'                                                    
         DC    CL8'OA  QOAR'                                                    
         DC    CL8'PACBPBPG'                                                    
         DC    CL8'PEP PPS '                                                    
         DC    CL8'PMS PNMS'                                                    
         DC    CL8'PSA X152'                                                    
         DC    CL8'PW  PWJ '                                                    
         DC    CL8'RF  XRJ '                                                    
         DC    CL8'RMC 231 '                                                    
         DC    CL8'SAG SAST'                                                    
         DC    CL8'SER SLR '                                                    
         DC    CL8'SHP F002'                                                    
         DC    CL8'MOTOMTC '                                                    
         DC    CL8'NCC NMT '                                                    
         DC    CL8'OPT OPL '                                                    
         DC    CL8'PACTPCT '                                                    
         DC    CL8'PHF PNHF'                                                    
         DC    CL8'PRSAPRAR'                                                    
         DC    CL8'PT  PDT '                                                    
         DC    CL8'RCARRCR '                                                    
         DC    CL8'RFC FUR '                                                    
         DC    CL8'RT  RTC '                                                    
         DC    CL8'SB  S003'                                                    
         DC    CL8'SERVSMC '                                                    
         DC    CL8'SL  X949'                                                    
         DC    CL8'USA AIR '                                                    
         DC    CL8'VOLVVI  '                                                    
         DC    CL8'WAREWG  '                                                    
         DC    CL8'WENDX941'                                                    
         DC    CL8'WSL X236'                                                    
         DC    CL8'AD  ADS '                                                    
         DC    CL8'ALPHX070'                                                    
         DC    CL8'AMPIAMMP'                                                    
         DC    CL8'APS APSI'                                                    
         DC    CL8'ASL ABS '                                                    
         DC    CL8'BASSBIC '                                                    
         DC    CL8'BR  BFUN'                                                    
         DC    CL8'CACPCCPR'                                                    
         DC    CL8'BUCABUSL'                                                    
         DC    CL8'CITIN094'                                                    
         DC    CL8'CONAX564'                                                    
         DC    CL8'CR  2095'                                                    
         DC    CL8'DARRDARB'                                                    
         DC    CL8'DP  DEPO'                                                    
         DC    CL8'FCM FCMG'                                                    
         DC    CL8'FLORFDOC'                                                    
         DC    CL8'FW  X494'                                                    
         DC    CL8'GH  X799'                                                    
         DC    CL8'HBO X994'                                                    
         DC    CL8'HOC HCC '                                                    
         DC    CL8'JEE JEEP'                                                    
         DC    CL8'KL  KLT '                                                    
         DC    CL8'LB  LBAR'                                                    
         DC    CL8'MAR 3143'                                                    
         DC    CL8'MAY MCTR'                                                    
         DC    CL8'MB  D012'                                                    
         DC    CL8'MC  MTRC'                                                    
         DC    CL8'MCD MD  '                                                    
         DC    CL8'MI  MINS'                                                    
         DC    CL8'MIACMCRT'                                                    
         DC    CL8'USARUS  '                                                    
         DC    CL8'WAR WRBK'                                                    
         DC    CL8'WC  WYAT'                                                    
         DC    CL8'WESTXWH '                                                    
         DC    CL8'AASEAAS '                                                    
         DC    CL8'ADA AMD '                                                    
         DC    CL8'AM  X049'                                                    
         DC    CL8'APC APG '                                                    
         DC    CL8'AR  ATR '                                                    
         DC    CL8'BA  BAT '                                                    
         DC    CL8'BD  BND '                                                    
         DC    CL8'BRANX746'                                                    
         DC    CL8'BV  BLV '                                                    
         DC    CL8'CASACASB'                                                    
         DC    CL8'CHC CLHO'                                                    
         DC    CL8'CL  CLC '                                                    
         DC    CL8'CONFCNCF'                                                    
         DC    CL8'CREMCRDM'                                                    
         DC    CL8'DE  DELX'                                                    
         DC    CL8'EDS EGDS'                                                    
         DC    CL8'FF  S006'                                                    
         DC    CL8'FORDFM  '                                                    
         DC    CL8'GA  X321'                                                    
         DC    CL8'GRAMGRM '                                                    
         DC    CL8'HC  HACL'                                                    
         DC    CL8'IMC IMCT'                                                    
         DC    CL8'KAISELEC'                                                    
         DC    CL8'KO  KODA'                                                    
         DC    CL8'LIBBLJ  '                                                    
         DC    CL8'MASSMDT '                                                    
         DC    CL8'USPSUP  '                                                    
         DC    CL8'WARDWRD '                                                    
         DC    CL8'WELLL047'                                                    
         DC    CL8'WT  WILD'                                                    
         DC    CL8'ACLIACL '                                                    
         DC    CL8'AH  QACM'                                                    
         DC    CL8'AMERAMPR'                                                    
         DC    CL8'APPLAPTC'                                                    
         DC    CL8'ASH ASHT'                                                    
         DC    CL8'BANKBCL '                                                    
         DC    CL8'BDS BRDS'                                                    
         DC    CL8'BURKBJ  '                                                    
         DC    CL8'CA  CNTA'                                                    
         DC    CL8'CD  CSD '                                                    
         DC    CL8'CI  CCI '                                                    
         DC    CL8'CLBK1025'                                                    
         DC    CL8'COTCC007'                                                    
         DC    CL8'CSW CSPW'                                                    
         DC    CL8'DISDDLIS'                                                    
         DC    CL8'EMP 1069'                                                    
         DC    CL8'FICIFICB'                                                    
         DC    CL8'FROSFRBR'                                                    
         DC    CL8'GAP GAPT'                                                    
         DC    CL8'HA  HAM '                                                    
         DC    CL8'HMC HT  '                                                    
         DC    CL8'ISR INSR'                                                    
         DC    CL8'KF  KFD '                                                    
         DC    CL8'LAT 2059'                                                    
         DC    CL8'MACOMANV'                                                    
         DC    CL8'MAXIHTW '                                                    
         SPACE 2                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         DC    CL8'**DCBS**'                                                    
IN       DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=JB900,          X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*JBREC**'                                                    
JBREC    DS    1000X               JB CONTRACT RECORD                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*INREC**'                                                    
INREC    DS    4004X               INPUT RECORD                                 
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019REREPJB02 05/01/02'                                      
         END                                                                    
