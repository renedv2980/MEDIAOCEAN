*          DATA SET SPREQ03    AT LEVEL 173 AS OF 04/28/17                      
*PHASE T20803A                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE SPFMTINO                                                               
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT CUSTENH-3341 10/28/16 ALLOW MKT 0000 D8 REQ FOR CANADA MEDIA N *         
*                            ALLOW MKT 0000 DZ REQ FOR CANADA ONLY FOR*         
*                            MEDIA N - OTHER MEDIA CODES DISALLOWED   *         
***********************************************************************         
*                                                                               
****************************************************************                
* GETBROAD /CLPACK REMOVED FROM INCLUDES  4/23/97                               
* ADDAD /GETDAY REMOVED FROM INCLUDES  6/19/97                                  
****************************************************************                
         TITLE 'SPREQ03 - REQUEST - VALIDATE DATA FIELDS - PART 1'              
T20803   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,T20803,RR=R9                                                 
         USING T20803,RB,RA,R8                                                  
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    RA,2048(RB)         NOTE RB,RA,R8 AS BASE REGISTERS              
         LA    RA,2048(RA)                                                      
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         EJECT                                                                  
         SR    RF,RF                                                            
         IC    RF,ROUTNUM                    RF=ROUTINE NUMBER                  
         SLL   RF,2                                                             
         LY    RF,ROUTADRT(RF)                                                  
*        L     RE,=A(ROUTADRT)                                                  
*        AR    RF,RE                                                            
         A     RF,RELO                       RF=A(VALIDATION ROUTINE)           
         BASR  RE,RF                                                            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
XXIT     XIT1                                                                   
         EJECT                                                                  
CLIVAL   NTR1                                CLIENT - FIND BITS                 
         LA    R7,SPTREC                                                        
         XC    0(100,R7),0(R7)                                                  
         GOTO1 AINITV                        02=ALL 04=XX                       
         CLI   FIND,0        CLT MISSING                                        
         BE    CLIVW                                                            
         CLC   RNUM,=C'SS'         FOR SS, NO OFFICE LIST (C'$')                
         BNE   CLT1E                                                            
         CLI   IFLD,C'$'           IS IT OFFICE LIST?                           
         BE    CLIVE                - YUP, GET OUT.  NOW.                       
*                                                                               
CLT1E    TM    FIND,X'02'         CLT=ALL                                       
         BZ    CLT2                                                             
         CLC   RNUM,=C'DZ'         FOR DZ ALLOW ALL, DON'T CHK LIM AC           
         BE    CLIVZ                                                            
         OC    6(2,R3),6(R3)       CLT=ALL CHK FOR LMT ACCESS                   
         BZ    CLIVZ               NO                                           
         CLI   6(R3),C'+'          MKT LMT ACC  ALL CLTS IS OK                  
         BE    CLIVZ                                                            
         B     CLIVF               LNT ACCESS ERROR                             
*                                                                               
CLT2     DS    0H                                                               
         CLC   =C'CGR=',IFLD        CLIENT GROUP                                
         BNE   CLTVGRP              NO-CHK GRP LIMIT ACCESS                     
         GOTO1 =A(CLTGRP),DMCB,RR=RELO                                          
         B     CLIVX                                                            
*                                                                               
************************************************                                
* CHK SPECIAL CLIENT GRP ACCESS LIMIT                                           
CLTVGRP  CLI   6(R3),C'*'                                                       
         BNE   CLT00                                                            
         CLI   7(R3),C'A'          CHECK IF ALPHA                               
         BL    CLT00                                                            
         CLI   7(R3),C'Z'                                                       
         BH    CLT00                                                            
         CLI   8(R3),C'0'          CHECK IF NUMERIC                             
         BL    CLT00                                                            
         CLI   8(R3),C'9'                                                       
         BH    CLT00                                                            
*        BAS   RE,VALGROUP            VALIDATE GROUP NUMBER                     
         GOTO1 =A(VALGROUP),DMCB,RR=RELO                                        
         BNE   CLIVF                  ACCESS ERROR                              
         B     CLT00                                                            
                                                                                
*                                                                               
CLT00    DS    0H                                                               
*                                                                               
CLT0     CLC   IFLD(4),=C'*ALL'    *ALL                                         
         BNE   CLT2A                                                            
         CLC   RNUM,=C'NV'       * TEST IF INVOICE REPORT                       
         BNE   CLT2AA                                                           
         MVC   RCLI,=C'ALL'                                                     
         MVI   ROAGY,C'Y'                                                       
         OI    FIND,X'02'                                                       
         B     CLIVX                                                            
CLT2AA   OC    6(2,R3),6(R3)       * FOR ALL OTHERS                             
         BNZ   CLIVE                                                            
         MVC   RCLI(2),=C'$*'                                                   
         OI    FIND,X'20'                                                       
         B     CLIVX                                                            
*                                                                               
CLT2A    DS    0H         CHECK IF USING 2CHAR OFFICE CODES                     
         XC    TEMP(OFCLENQ),TEMP  LENGTH OF OFFICED IS 48 BYTES                
         LA    RF,TEMP                                                          
         USING OFFICED,RF                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,6(R3)       ID AUTH VALUE                                
         MVC   OFCLMT,6(R3)                                                     
         MVC   OFCAGY,RAGY                                                      
         MVI   OFCOFC,C'?'                                                      
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         LA    RF,TEMP            RE-POINT RF AFTER GOTO1                       
         TM    OFCINDS,OFCINOLA   NO OACCES RECORDS FOUND ?                     
         BO    CLT2AC             DON'T BOTHER CONVERTING                       
         DROP  RF                                                               
*                         CONVERT 2CHAR OFFICE CODE TO TO 1HEX                  
         GOTO1 =A(CNVOFF),DMCB,RR=RELO                                          
         BNE   CLIVXX                                                           
*                                                                               
CLT2AC   CLI   IFLDH+5,5                                                        
         BNE   CLIVA                                                            
         CLC   IFLD(4),=C'ALL-'                                                 
         BNE   CLIVE                                                            
         MVC   ROAGY,IFLD+4                                                     
         OI    FIND,X'10'          X'10' = OLD OFFICE CODE INPUT                
         OC    6(2,R3),6(R3)                                                    
         BZ    CLIVO                                                            
         CLI   6(R3),C'+'          MKT LMT ACCESS                               
         BE    CLIVO                                                            
         CLI   6(R3),C'$'          OFFICE LIST LOCKOUT                          
         BE    CLIVO                                                            
         CLI   6(R3),C'*'          OFFICE LMT ACCESS                            
         BNE   CLIVF               CLT LMT ACC - ALL-N IS ERROR                 
         CLC   7(1,R3),ROAGY                                                    
         BE    CLIVO                                                            
         B     CLIVF               LNT ACCESS ERROR                             
*                                                                               
*                                                                               
CLIVA    DS    0H                                                               
         CLI   IFLDH+5,3                                                        
         BH    CLIVE                                                            
         CLI   IFLD,C'$'           ACCEPT $N                                    
         BE    *+12                                                             
         CLI   IFLD,C'*'                                                        
         BNE   CLI                                                              
         CLI   IFLDH+5,1                                                        
         BNH   CLIVE                                                            
         CLC   RNUM,=C'B1'         B1 DOES NOT ACCEPT OFFICE CODE               
         BE    CLIVE                                                            
         CLC   RNUM,=C'BU'         BU DOES NOT ACCEPT OFFICE CODE               
         BE    CLIVE                                                            
         OI    FIND,X'20'          NEW OFFICE CODE INPUT                        
         CLI   IFLDH+5,2                                                        
         BNE   CLIVA2                                                           
*                                  *N INPUT CHK FO LMT ACCESS                   
         OC    6(2,R3),6(R3)                                                    
         BZ    CLIVO                                                            
         CLI   IFLD,C'$'           ACCEPT $N                                    
         BE    CLIVOFF                                                          
         CLI   6(R3),C'+'          MKT LMT ACCESS                               
         BE    CLIVO                                                            
         CLI   6(R3),C'$'          OFFICE LIST LOCKOUT                          
         BE    CLIVO                                                            
         CLI   6(R3),C'*'          OFFICE LMT ACCESS                            
         BNE   CLIVF               ACCESS ERROR                                 
CLIVOFF  CLC   7(1,R3),IFLD+1                                                   
         BE    CLIVZ               OFF CODES MUST MATCH                         
         B     CLIVF               ACCESS ERROR                                 
*                                                                               
CLIVA2   DS    0H                  *NN OR *-N INPUT                             
         OC    6(2,R3),6(R3)                                                    
         BZ    CLIVO                                                            
         CLI   6(R3),C'+'          MKT LMT ACCESS                               
         BE    CLIVO                                                            
         CLI   6(R3),C'$'          OFFICE LIST                                  
         BE    CLIVO                                                            
         B     CLIVF               LNT ACCESS ERROR                             
*                                                                               
CLI      DS    0H                                                               
         GOTO1 CLPACK,DMCB,IFLD,KEY+2                                           
         CLI   DMCB,0                                                           
         BNE   CLIVE                                                            
         OC    KEY(2),KEY       IS AGY/MEDIA SET?                               
         BNZ   *+10                                                             
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   SAVEPD,KEY+4     SAVE PRODUCT PUT INTO KEY BY DEFAULT            
         XC    KEY+4(3),KEY+4                                                   
         GOTO1 ARSPT                         READ CLIENT HEADER                 
         MVC   KEY+4(3),SAVEPD      RESTORE PRODUCT                             
         CLC   FERN,=AL2(FE)                                                    
         BL    CLIVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(CLINOF)                   CLIENT NOT ON FILE           
         B     CLIVXX                                                           
*        CLC   RNUM,=C'X5'         P&G INFO TAPE                                
*        BE    PGCHK                                                            
*        CLC   RNUM,=C'XG'         P&G INTERFACE TAPE                           
*        BNE   CLIV                                                             
*PGCHK    CLC   IFLD(3),=C'PG '                                                 
*        BNE   CLIVE        ERROR CLI MUST BE PG FOR REQ XG                     
*        B     CLIV                                                             
*                                                                               
CLIV     DS    0H                                                               
         LA    R7,SPTREC                                                        
         USING CLTHDRD,R7                                                       
         MVC   NAME(20),CNAME                                                   
         MVC   CLIEXTRA,CEXTRA                                                  
         MVC   BAGYMD,CKEYAM       SAVE INFO FOR FUTURE VALIDATION              
         MVC   BCLT,CKEYCLT                                                     
         OI    FIND,X'04'                    CLIENT = XX                        
         MVC   CLIPROF,CPROF             SAVE CLIENT PROFILE                    
         MVC   CLIOFFC,COFFICE       SAVE OFFICE CODE                           
         TM    COPT2,COP2DIY         COP2DIY SET?                               
         BZ    *+8                                                              
         MVI   CLCOPT2,C'Y'          YES                                        
*                                                                               
         TM    COPT1,COP1GMI       IF GMI CLIENT                                
         BZ    *+8                                                              
         OI    FIND,X'80'          SET FIND=GMI FOR PRODVAL                     
*                                                                               
*        CLC   RAGY,=C'GZ'         SAVE BPCT VALUE FOR GM BILLING               
*        BE    *+14                                                             
*        CLC   RAGY,=C'*B'                                                      
*        BNE   CLIVODD                                                          
*        CLC   RAGY,=C'M2'         FOR GROUP M SPECIAL CASH/TRADE               
*        BNE   CLIVODD                                                          
         XC    DEMS,DEMS                                                        
         CLC   RNUM,=C'B1'                                                      
         BE    *+14                                                             
         CLC   RNUM,=C'D1'                                                      
         BNE   *+10                                                             
         MVC   DEMS(1),COPT4       SAVE TRADE FLAG                              
*                                                                               
         TM    DEMS,COP4MIDS       IF GRP M AND MIDAS                           
         BO    *+16                DON'T SPLIT INTO CASH/TRADE                  
         TM    DEMS,COP4TRD        IF GRP M AND TRADE FLAG PUT IN C             
         BZ    *+8                 TO INDICATE CASH REQUEST, TRADE              
         MVI   R2USER+20,C'C'      REQUEST WILL BE ADDED BY EOD                 
*                                                                               
*        CLC   RNUM,=C'K5'         FOR K5 DO NOT ALLOW OVERFLOW CLT             
*        BNE   *+12                                                             
*        TM    CINDS1,CIN1OVP      OVERFLOW PRODS ON THIS CLIENT ?              
*        BNZ   CLIVE                                                            
*                                                                               
CLIVODD  NI    ODDMNTS,X'FF'-ECTIME                                             
         NI    ODDMNTS,X'FF'-ECINT                                              
         NI    ODDMNTS,X'FF'-ECINTSP                                            
         TM    COPT3,COP3T+COP3TI                                               
         BZ    CLIVTIS                                                          
         TM    COPT3,COP3T         TIME                                         
         BZ    *+12                                                             
         OI    ODDMNTS,ECTIME      TIME                                         
         B     CLIV0                                                            
*                                                                               
         OI    ODDMNTS,ECINT       TIME + INT                                   
         B     CLIV0                                                            
*                                                                               
CLIVTIS  TM    COPT4,COP4TIS       TIME + INT +SPEC                             
         BZ    *+8                                                              
         OI    ODDMNTS,ECINTSP                                                  
*                                                                               
CLIV0    LA    R5,CLIST                                                         
         LA    R0,220                                                           
CLIV1    CLC   0(3,R5),=C'POL'                                                  
         BE    CLIV2                                                            
         LA    R5,4(R5)                                                         
         BCT   R0,CLIV1                                                         
         TM    ODDMNTS,NETSYS                                                   
         BNE   CLIVO                                                            
         LA    R5,CLIST2                     CLIST EXTENSION                    
         LA    R0,35                         35 MORE PRODUCTS                   
CLIV1B   CLC   0(3,R5),=C'POL'                                                  
         BE    CLIV2                                                            
         LA    R5,4(R5)                                                         
         BCT   R0,CLIV1B                                                        
         B     *+8                                                              
CLIV2    OI    FIND,X'08'                    CLI = XX(POL)                      
         B     CLIVO                                                            
CLIVE    MVC   FERN,=AL2(CLIINV)                   CLIENT INVALID               
         B     CLIVXX                                                           
*                                                                               
CLIVF    MVC   FERN,=AL2(ACCERR)           ACCESS ERROR                         
         B     CLIVXX                                                           
*                                                                               
CLIVO    OC    4(4,R3),4(R3)       CHECK LIMITED ACCESS+NEW SECURITY            
         BZ    CLIVZ                                                            
         TM    FIND,X'04'          IF SINGLE CLIENT, GOTO OFFICER               
         BO    CLIV100                                                          
         CLI   6(R3),C'+'          LMTED MKT ACCESS                             
         BE    CLIVZ                                                            
         CLI   6(R3),C'$'          TEST OFFICE LIST LOCKOUT                     
         BE    CLIV100                                                          
*                                                                               
         CLI   6(R3),C'*'          LIMIT BY OFFICE                              
         BNE   CLIVO1              NO                                           
**********************************************************                      
         CLI   7(R3),C'A'          IF SPECIAL GROUP ACCESS                      
         BL    CLIVOCHK                                                         
         CLI   7(R3),C'Z'                                                       
         BH    CLIVOCHK                                                         
         CLI   8(R3),C'0'                                                       
         BL    CLIVOCHK                                                         
         CLI   8(R3),C'9'                                                       
         BH    CLIVOCHK                                                         
         B     CLIVZ                OK                                          
**************************************************************                  
CLIVOCHK LA    R1,CACCESS           MATCH OFFICES                               
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    CLICHK02                                                         
         LA    R1,COFFICE                                                       
         LA    R0,1                                                             
*                                                                               
CLICHK02 CLC   7(1,R3),0(R1)        MATCH OFFICES                               
         BE    CLIVZ                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,CLICHK02                                                      
         B     CLIVF                                                            
*                                                                               
CLIVO1   DS    0H                                                               
         CLC   6(2,R3),KEY+2          CLIENT CODE                               
         BNE   CLIVF                                                            
CLIVZ    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
CLIVW    FOUT  (R6),NAME                                                        
CLIVX    MVC   CLISAVE,FIND                                                     
****************************************************************                
         CLC   =C'GZ',RAGY         FOR GM ONLY SPECIFIC CLT OR CGR              
*        BE    *+14                                                             
*        CLC   =C'*B',RAGY                                                      
         BNE   CLII2CK             FOR SPOT BILLING                             
         CLC   =C'B1',RNUM                                                      
         BE    *+14                                                             
         CLC   =C'D1',RNUM                                                      
         BNE   CLII2CK                                                          
         TM    FIND,X'04'          CLIENT ?                                     
         BO    *+12                                                             
         TM    FIND,X'40'          CGR ?                                        
         BZ    CLIVE                                                            
*                                                                               
CLII2CK  CLC   =C'I2',RNUM         FOR I2 FOR JW                                
         BNE   CLISKIP                                                          
         CLC   =C'JW',RAGY                                                      
         BE    FUDGECLI                                                         
         CLC   =C'H7',RAGY         AND  FOR H7                                  
         BE    FUDGECLI                                                         
         CLC   =C'FR',RAGY         FR SHOULD HAVE SAME AS H7                    
         BE    FUDGECLI                                                         
         CLC   =C'BC',RAGY         AND FOR MDSCP                                
         BE    FUDGECLI                                                         
CLISKIP  CLC   =C'A8',RNUM         AND DO SAME FOR A8                           
         BNE   CLISKP2                                                          
         CLC   =C'MIGLIACCIO',RNAME  ONLY FOR THIS USER NAME                    
         BNE   CLISKP2                                                          
FUDGECLI MVI   FIND,X'05'          ALLOW ALL(ETC) AND FUDGE IT HERE             
****************************************************************                
CLISKP2  DS    0H                                                               
         L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         B     CLIVXX                                                           
CLIVXX   B     XXIT                                                             
*                                                                               
SAVEPD   DS    CL3                                                              
*                                                                               
CLIV100  DS    0H               * TEST OFFICE LIST SECURITY *                   
         SPACE 1                                                                
         LA    R7,SPTREC                                                        
         USING CLTHDRD,R7                                                       
*        XC    DMCB(8),DMCB                                                     
*        MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
*        GOTO1 CALLOV,DMCB                                                      
*        CLI   4(R1),X'FF'                                                      
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         XC    TEMP(48),TEMP       LENGTH OF OFFICED IS 48 BYTES                
         LA    R5,TEMP                                                          
         USING OFFICED,R5                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,6(R3)       ID AUTH VALUE                                
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         CLI   COFFICE,0           ID ZERO, ASSUME *N REQ                       
         BNE   *+10                                                             
         MVC   OFCOFC,IFLD+1       AND MOVE IN REQ OFFICE                       
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),OFCCLT                             
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,CKEYAM                                                  
         MVC   OFCLMT(4),6(R3)                                                  
         MVC   OFCACCSC(3),CACCESS ACCESS LIST FROM CLIENT HEADER               
         MVI   OFCACCSM,X'FF'                                                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R5                                                               
*                                                                               
         GOTO1 OFFICER,DMCB,(C'N',TEMP),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   CLIVF                                                            
         B     CLIVZ                                                            
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
PROVAL   NTR1                                PRODUCT - FIND BITS                
***      GOTO1 AINITV                        02=ALL 04=XXX 08=POL 10=NO         
PROV     MVI   ROUTSUB,1                     20=XXX                             
         GOTO1 AINITV                                                           
**                                                                              
         CLI   FIND,1                                                           
         BNE   PROVO                         PRO = ALL OR MISSING               
                                                                                
         CLC   IFLD(3),=C'UNA'     ..IF UNA                                     
         BNE   PROVAA                                                           
         CLC   RNUM,=C'K2'         ..AND K2                                     
         BNE   PROVAA                                                           
         MVC   IFLD(3),=C'POL'     ..MAKE IT POL                                
         OI    FIND,X'04'          ..AND FUDGE FIND                             
PROVAA   CLC   IFLD(3),=C'POL'                                                  
         BNE   PROV1                                                            
         OI    FIND,X'08'                    PRO = POL                          
PROV1    OC    KEY+2(2),KEY+2                                                   
         BNZ   *+12                                                             
         MVI   FIND,X'21'        PRO=XXX OR POL AND NON SPEC CLI                
         B     PROVO                                                            
         CLC   IFLD(3),=C'NO '                                                  
         BNE   PROV2                                                            
         OI    FIND,X'10'                    PRO = NO                           
         MVC   KEY+4(3),=C'AAA'                                                 
         B     PROVO                                                            
PROV2    CLC   IFLD(3),=C'POL'                                                  
         BE    *+8                                                              
         OI    FIND,X'04'       PRO=XXX                                         
         MVC   KEY+4(3),IFLD                                                    
PROV3    GOTO1 ARSPT                         READ PRODUCT HEADER                
         CLC   FERN,=AL2(FE)                                                    
         BL    PROVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(PRONOF)                   PRO NOT ON FILE              
         B     PROVO                                                            
         TM    FIND,X'04'                                                       
         BZ    PROVO                                                            
         MVC   NAME(20),SPTREC+28                                               
*****************************************************************               
         LA    R1,SPTREC           ,AND BINARY PRD > X'3F'                      
         USING PRDHDR,R1                                                        
         CLC   =C'B1',RNUM         ,FOR SPOT BILLING                            
         BE    PROVTHTR                                                         
         CLC   =C'D1',RNUM                                                      
         BE    PROVTHTR                                                         
         CLC   =C'BU',RNUM         ,AND NET BILLING                             
         BE    PROVTHTR                                                         
         CLC   =C'DU',RNUM          SAVE THEATRICAL PROD FLAG                   
         BNE   *+10                                                             
PROVTHTR MVC   DEMS+1(1),POPT1                                                  
*                                                                               
         TM    CLISAVE,X'80'       ,IF GMI CLIENT                               
         BZ    PROV10                                                           
         CLC   =C'B1',RNUM         ,AND SPOT BILLING                            
         BE    *+14                                                             
         CLC   =C'D1',RNUM                                                      
         BNE   PROV10                                                           
         B     PROV08              ALWAYS CASH/TRA SEPARATE                     
**********8                                                                     
         CLC   =C'PTA',RCLI                                                     
         BE    PROVPTA                                                          
* CLIENT NOT PTA                                                                
         CLI   PCODE+1,X'3F'                                                    
         BNH   PROV10                                                           
         OC    PBILLCOM,PBILLCOM    ,MUST BE BILL FORMULA RECORD                
         BNZ   PROV08                                                           
         MVC   FERN,=AL2(504)                                                   
         B     PROVO                                                            
* CLIENT PTA                                                                    
PROVPTA  LA    R1,SPTREC           ,AND BINARY PRD > X'3F'                      
         USING PRDHDR,R1                                                        
         CLI   PCODE+1,X'40'       ,AND RL IS A TRADE PRODUCT                   
         BE    PROVPTA2                                                         
         CLI   PCODE+1,X'41'       AND FL IS TRADE                              
         BE    PROVPTA2                                                         
         CLI   PCODE+1,95          ,<= 95 IS NOT TRADE PRODUCT                  
         BNH   PROV10                                                           
*                                                                               
PROVPTA2 OC    PBILLCOM,PBILLCOM    ,MUST BE BILL FORMULA RECORD                
         BNZ   PROV08                                                           
         MVC   FERN,=AL2(504)                                                   
         B     PROVO                                                            
******************************                                                  
PROV08   MVI   RCARD2+31,C'C'       ,SET 'C' IN 2ND REQUEST CARD                
         B     PROV10                                                           
*                                                                               
PROV10   B     PROVO                                                            
**************************************************************                  
PROVE    MVC   FERN,=AL2(PROINV)                   INVALID PRODUCT              
PROVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
PROVX    MVC   PROSAVE,FIND                                                     
*                                                                               
         CLC   =C'DX',RNUM         DX AND CP ACCEPT 2ND PROD                    
         BE    PROVXD                                                           
         CLC   =C'CP',RNUM                                                      
         BE    PROVXD                                                           
         CLC   =C'RS',RNUM         AND RS                                       
         BE    PROVXD                                                           
         CLC   =C'RY',RNUM         AND RY                                       
         BE    PROVXD                                                           
         CLC   =C'K3',RNUM         AND K3                                       
         BNE   PROV00                                                           
*                                                                               
PROVXD   MVI   ROUTSUB,2           IS THERE A 2ND PROD ?                        
         GOTO1 AINITV                                                           
         MVC   FIND,PROSAVE        RESET FIND                                   
         CLI   IFLDH,1                                                          
         BL    PROV00              MISSING                                      
*                                                                               
         CLC   =C'RS',RNUM         ,,IF RS                                      
         BE    PROVXF                                                           
         CLC   =C'RY',RNUM         ,,OR RY                                      
         BNE   PROVXL                                                           
PROVXF   DS    0H                                                               
         CLC   =C'POL',0(R7)       ,,AND 1ST PROD = POL                         
         BE    PROVXH              ,,REJECT IT                                  
         CLC   =C'ALL',0(R7)       ,,OR  1ST PROD = ALL                         
         BE    PROVXH              ,,REJECT IT                                  
         CLC   =C'POL',IFLD        ,,OR  2ND PROD = POL                         
         BE    PROVXH              ,,REJECT IT                                  
         CLC   =C'ALL',IFLD        ,,OR  2ND PROD = ALL                         
         BNE   PROVXP                                                           
PROVXH   DS    0H                  ,,REJECT IT                                  
         MVC   FERN,=AL2(FMTNAV)                                                
         B     PROVXIT                                                          
PROVXL   DS    0H                                                               
         CLC   =C'K3',RNUM         ,,IF K3                                      
         BNE   PROVXP                                                           
         CLC   =C'ALL',IFLD        ,,AND 2ND PROD = ALL                         
         BE    PROVXT              ,,ACCEPT IT                                  
PROVXP   MVC   KEY+4(3),IFLD                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BH    *+14                                                             
         MVC   FERN,=AL2(PRONOF)                                                
         B     PROVXIT                                                          
PROVXT   DS    0H                                                               
         CLC   RNUM,=C'RS'                                                      
         BE    PROVXW                                                           
         CLC   RNUM,=C'RY'                                                      
         BE    PROVXW                                                           
         CLC   RNUM,=C'K3'                                                      
         BNE   PROVXZ                                                           
PROVXW   DS    0H                                                               
         MVC   RPRO1,IFLD          K3 & RY & RS PROD GOES HERE                  
         B     PROVXIT                                                          
PROVXZ   DS    0H                                                               
         MVC   RCARD2+9(3),IFLD    DX/CB SET PROD IN 2ND CARD                   
         B     PROVXIT                                                          
****                                                                            
PROV00   CLC   RNUM,=C'I5'         IF I5                                        
         BNE   CKSKIP                                                           
         CLC   RAGY,=C'CK'         FOR AGY CK                                   
         BNE   CKSKIP                                                           
         TM    FIND,X'04'          MUST BE SINGLE PRODUCT                       
         BO    CKSKIP                                                           
         OI    FIND,X'01'                                                       
         MVC   FERN,=AL2(PROINV)                                                
                                                                                
CKSKIP   EQU   *                                                                
                                                                                
         L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         B     PROVXIT                                                          
PROVXIT  B     XXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
PRMVAL   NTR1                                PRODUCT,MODE - FIND BITS           
         MVI   ROUTSUB,1                     02=ALL 04=XXX 08=POL               
         GOTO1 AINITV              10=XXX,ALL  20=XXX,YYY                       
         CLI   5(R4),0             40=POL,XXX  80=POL,XXX-YYY                   
         BE    PRMV2                         PROD,MODE NOT INPUT                
         CLC   =C'PGR=',IFLD         FOR I2                                     
         BE    PGRPVAL0             DO PROD GROUP VAL                           
         CLI   IFLDH,1                                                          
         BL    PRMVE                                                            
         BH    PRMV2                         PROD = ALL                         
         CLC   IFLD(3),=C'POL'                                                  
         BNE   PRMV1                                                            
*****************************************************                           
*        CLC   RNUM,=C'I2'         IF POL AND I2                                
*        BNE   PRMV0                                                            
*        CLI   CLIPROF+0,C'0'      CLIPROF+0 MUST = C'0'                        
*        BE    PRMV0                                                            
*        MVC   FERN,=AL2(1132)     MUST REQUEST SPECIFIC PRODUCT                
*        B     PRMVXX                                                           
PRMV0    EQU   *                                                                
***************************************************                             
         OI    FIND,X'08'                    PROD = POL                         
         B     *+8                                                              
PRMV1    OI    FIND,X'04'                    PROD = XXX                         
         OC    KEY+2(2),KEY+2                                                   
         BZ    PRMVE                         NON SPECIFIC CLIENT                
         CLI   IFLDH+5,3                                                        
         BH    PRMVE                                                            
         MVC   KEY+4(3),IFLD                                                    
         GOTO1 ARSPT                         READ PRODUCT HEADER                
         CLC   FERN,=AL2(FE)                                                    
         BL    PRMVO                         DISK ERROR                         
         BH    *+14                                                             
PRMV1A   MVC   FERN,=AL2(PRONOF)                   PROD NOT ON FILE             
         B     PRMVO                                                            
         TM    FIND,X'04'                                                       
         BZ    PRMV2                                                            
         MVC   NAME(20),SPTREC+28                                               
PRMV2    FOUT  (R6),NAME                                                        
         MVC   RPRO,IFLD                                                        
         CLI   FIND,0                                                           
         BE    PRMVX                                                            
         MVI   ROUTSUB,2                     EXTRACT 2ND FLD                    
         GOTO1 AINITV                                                           
PRMV2B   CLI   IFLDH,1                                                          
         BL    PRMV4                         2ND FLD = MISSING                  
         CLI   IFLDH+5,3                                                        
         BH    PRMVE                                                            
         CLC   IFLD(3),=C'POL'     2ND FLD=POL IS INVALID                       
         BE    PRMVE                                                            
         TM    FIND,X'0E'          1ST PRD MUST HAVE BEEN XXX OR POL            
         BZ    PRMVE               OR ALL                                       
         MVC   RPRO1,IFLD                                                       
         CLC   IFLD(3),=C'YES'     ALLOW NNN-YES FOR I2/N2/L2                   
         BNE   PRMV2BB                                                          
         CLC   RNUM,=C'I2'                                                      
         BE    PRMVX2                                                           
         CLC   RNUM,=C'L2'                                                      
         BE    PRMVX2                                                           
         CLC   RNUM,=C'N2'                                                      
         BE    PRMVX2                                                           
PRMV2BB  CLC   IFLD(2),=C'NO'     ALLOW NNN-NO FOR I2/N2/L2                     
         BNE   PRMV2CC                                                          
         CLC   RNUM,=C'I2'                                                      
         BE    PRMVX2                                                           
         CLC   RNUM,=C'N2'                                                      
         BE    PRMVX2                                                           
         CLC   RNUM,=C'L2'                                                      
         BE    PRMVX2                                                           
PRMV2CC  CLI   IFLDH,3                                                          
         BNE   PRMV2D                                                           
         TM    FIND,X'08'                                                       
*        BNZ   PRMVE               POL/ALL  INVALID                             
         BZ    PRMV2C              POL/ALL  INVALID                             
         CLC   RNUM,=C'I2'         ALLOW IT FOR I2/N2                           
         BE    PRMV2C                                                           
         CLC   RNUM,=C'L2'         ALLOW IT FOR L2                              
         BE    PRMV2C                                                           
         CLC   RNUM,=C'N2'                                                      
         BNE   PRMVE                                                            
PRMV2C   MVI   FIND,X'11'          X'10'= XXX,ALL                               
         B     PRMV4                                                            
*                                                                               
PRMV2D   DS    0H                                                               
         CLC   =C'I2',RNUM         FOR I2/N2                                    
         BE    *+14                                                             
         CLC   =C'N2',RNUM                                                      
         BNE   PRMV2DD                                                          
         TM    FIND,X'0E'          ACCEPT 1ST PRD POL/ALL/XXX                   
         BNZ   PRMV2F                                                           
*                                                                               
PRMV2DD  TM    FIND,X'0C'          1ST PRD MUST HAVE BEEN XXX OR POL            
         BZ    PRMVE                                                            
         CLC   IFLD(3),=C'REG'                                                  
         BNE   PRMV2F                                                           
         MVC   RPRO1,=C'   '                                                    
         TM    FIND,X'04'          XXX/REG                                      
         BNZ   PRMV4                                                            
         B     PRMVE               POL,REG IS INVALID                           
*                                                                               
PRMV2F   DS    0H                                                               
         CLC   KEY+4(3),IFLD                                                    
         BE    PRMVE                         ERROR RPRO=RPRO1                   
         MVC   KEY+4(3),IFLD                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PRMVO                                                            
         BE    PRMV1A                                                           
***                                                                             
         CLC   =C'I2',RNUM         FUDGE FOR I2/N2                              
         BE    *+14                                                             
         CLC   =C'N2',RNUM                                                      
         BNE   PRMV2G                                                           
         CLC   RPRO,=C'POL'         IF 1ST PROD = POL                           
         BE    PRMV2H               SET POL,NNN IS REQ CARD                     
         CLC   RPRO,=C'ALL'         IF 1ST PROD = ALL                           
         BE    PRMV4                                                            
***                                                                             
PRMV2G   TM    FIND,X'08'          IF POL                                       
         BZ    PRMV2H                                                           
         MVC   RPRO,RPRO1                                                       
         MVC   RPRO1,=C'   '                                                    
         MVI   FIND,X'41'          POL,XXX                                      
         B     PRMV4                                                            
*                                                                               
PRMV2H   DS    0H                                                               
         MVI   FIND,X'21'          X'20' = XXX,YYY                              
         CLC   RPRO,RPRO1                                                       
         BE    PRMVE                                                            
PRMV4    MVI   ROUTSUB,3                                                        
         GOTO1 AINITV                        EXTRACT 3RD FLD                    
         CLI   IFLDH,1                                                          
         BL    PRMVX                                                            
         BH    PRMVE               ALL IS INVALID                               
         CLC   IFLD(3),=C'POL'     2ND FLD=POL IS INVALID                       
         BE    PRMVE                                                            
         CLI   FIND,X'41'          PREVIOUS MUST HAVE BEEN POL,XXX              
         BNE   PRMVE                                                            
         CLC   KEY+4(3),IFLD                                                    
         BE    PRMVE                                                            
         MVC   KEY+4(3),IFLD                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PRMVO                                                            
         BE    PRMV1A              NOT FOUND                                    
         MVC   RPRO1,IFLD                                                       
         MVI   FIND,X'81'          POL,XXX-YYY                                  
         B     PRMVX                                                            
*                                                                               
PRMVE    MVC   FERN,=AL2(PRMNOF)                   PROD,MODE INVALID            
PRMVO    FOUT  (R6),NAME                                                        
PRMVX    MVC   PROSAVE,FIND                                                     
         TM    PROSAVE,X'F0'       SEE IF MORE THAN 1 PRD INPUT                 
         BZ    *+14                NO                                           
         MVI   PROSAVE,X'05'       YES - SET TO SPECIFIC PRD INPUT              
         MVC   KEY+4(3),RPRO       RESTORE KEY FOR EST READ                     
*                                                                               
         CLC   RNUM,=C'I2'         FOR I2/N2 SECOND PRD MUST GO                 
         BE    PRMVX1                                                           
         CLC   RNUM,=C'L2'         FOR L2 SECOND PRD MUST GO                    
         BE    PRMVX1                                                           
         CLC   RNUM,=C'N2'                                                      
         BNE   PRMVXX              IN CLOUMN 35                                 
PRMVX1   CLC   RPRO(3),=C'ALL'                                                  
         BNE   PRMVX2                                                           
         CLC   RPRO(3),RPRO1                                                    
         BNE   PRMVX2                                                           
         MVI   PROSAVE,0                                                        
PRMVX2   CLC   RPRO1,=C'   '                                                    
         BE    PRMVXX                                                           
         MVC   R3037+5(3),RPRO1                                                 
         MVC   RPRO1,=C'   '                                                    
*                                                                               
PRMVXX   DS    0H                                                               
         B     XXIT                                                             
         EJECT                                                                  
ESTVAL   NTR1                                                                   
         GOTO1 =A(ESTVL),DMCB,RR=RELO                                           
         B     XXIT                                                             
         EJECT                                                                  
MKTVAL   NTR1                                MARKET - FIND BITS                 
MKTV00   GOTO1 AINITV                        02=ALL 04=ALL-N 08=NNNN            
         CLI   FIND,3              MKT=ALL                                      
         BNE   MKTV                                                             
         CLI   6(R3),C'+'          CK FOR MKT LIMIT ACCESS                      
         BE    MKTACERR                                                         
         CLI   8(R3),C'+'          CK FOR MKT LIMIT ACCESS FOR WESTERN          
         BE    MKTACERR                                                         
         B     MKTVO                                                            
*                                                                               
MKTV     DS    0H                                                               
         CLI   FIND,1                        10=ALL-RN                          
         BE    *+26                                                             
         CLC   RNUM,=C'SC'          IF SC                                       
         BNE   MKTVO                                                            
         CLI   RMARK,C' '           AND MKT IS ALREADY THERE                    
         BH    MKTVX      DON'T OVERRIDE IT WITH BLANKS, JUST EXIT              
         B     MKTVO                MKT = MISSING OR ALL                        
*****************************************************************               
         CLC   RNUM,=C'Z5'          IF Z5                                       
         BNE   MKTV0                                                            
         CLI   RMARK,X'40'         MKT MUST BE BLANK AND NOT HAVE               
         BNH   MKTV0               BEEN FILLED IN BY STA VALIDATION             
         MVC   FERN,=H'800'        EITHER STA OR MKT                            
         B     MKTVO                                                            
********************************************************************            
MKTV0    CLI   IFLDH+5,5                                                        
         BL    MKTV4                                                            
         BH    MKTV1                                                            
         CLC   IFLD(4),=C'ALL-'              CHECK FOR ALL-N                    
         BNE   MKTVE                                                            
         CLI   IFLD+4,C'0'                                                      
         BL    MKTVE                                                            
         CLI   IFLD+4,C'9'                                                      
         BH    MKTVE                                                            
         CLI   6(R3),C'+'          CK FOR MKT LIMIT ACCESS                      
         BE    MKTACERR                                                         
         CLI   8(R3),C'+'          CK FOR MKT LIMIT ACCESS FOR WESTERN          
         BE    MKTACERR                                                         
         MVC   IFLD+3(1),IFLD+4              SET TO ALLN FORMAT                 
         OI    FIND,X'04'                    MKT = ALL-N                        
         B     MKTVO                                                            
MKTV1    CLC   IFLD(4),=C'ALL-'                                                 
         BNE   MKTVE                                                            
         CLI   6(R3),C'+'          CK FOR MKT LIMIT ACCESS                      
         BE    MKTACERR                                                         
         CLI   8(R3),C'+'          CK FOR MKT LIMIT ACCESS FOR WESTERN          
         BE    MKTACERR                                                         
         CLI   IFLDH+5,7                                                        
         BH    MKTVE                                                            
         BL    MKTV2                                                            
         CLC   IFLD+4(3),=C'REG'                                                
         BNE   MKTVE                                                            
         MVI   IFLD+3,C' '                                                      
         B     MKTV3                                                            
MKTV2    CLI   IFLD+4,C'R'                                                      
         BNE   MKTVE                                                            
         LA    R1,REGTAB                                                        
         LA    0,17                                                             
MKTV2A   CLC   IFLD+5(1),0(R1)                                                  
         BE    MKTV2B                                                           
         LA    R1,1(R1)                                                         
         BCT   0,MKTV2A                                                         
         B     MKTVE         ERROR                                              
*                                                                               
MKTV2B   MVC   IFLD+3(1),IFLD+5                                                 
         MVC   IFLD+3(1),IFLD+5                                                 
MKTV3    MVI   IFLD,C'G'                     SET SPECIAL MKT CODE               
         OI    FIND,X'10'                    MKT = ALL-RN OR ALL-REG            
         B     MKTVO                                                            
MKTV4    DS    0H                                                               
         CLC   RNUM,=C'D8'                   FOR D8 DISALLOW MKT 0000           
         BNE   MKTV4AA                                                          
         CLI   CANAGY,C'C'                   CANADIAN AGENCY?                   
         BNE   MKTV4A                        NO - MKT 0000 NOT ALLOWED          
         CLI   RMED,C'N'                     MEDIA N REQUEST?                   
         BNE   MKTV4A                        NO - MKT 0000 NOT ALLOWED          
         B     MKTV4A4                       CANADA/MED=N/MKT=0 ALLOWED         
*                                                                               
MKTV4AA  CLC   RNUM,=C'DZ'                   DZ REPORT?                         
         BNE   MKTV4AB                       NO                                 
         CLI   CANAGY,C'C'                   CANADIAN AGENCY?                   
         BNE   MKTV4A4                       NO - ANY MKT ALLOWED               
         CLI   RMED,C'N'                     MEDIA N REQUEST?                   
         BNE   MKTV4A                        NO - MKT 0000 NOT ALLOWED          
         CLC   =C'0000',IFLD                 MKT 0000?                          
         BNE   MKTVE                         NO - ONLY MKT 0000 ALLOWED         
         B     MKTV4A4                       CANADA/MED=N/MKT=0 ALLOWED         
*                                                                               
MKTV4AB  CLC   RNUM,=C'CM'                     AND FOR CM                       
         BE    MKTV4A                                                           
         CLC   RNUM,=C'SC'                     AND FOR SC                       
         BE    MKTV4A                                                           
         CLC   RNUM,=C'SX'                     AND FOR SX                       
         BNE   MKTV4A4                                                          
MKTV4A   DS    0H                                                               
         CLC   =C'0000',IFLD                                                    
         BE    MKTVE                                                            
*                                                                               
MKTV4A4  DS    0H                                                               
         LR    R4,R5                         RT JUSTIFY AT TEMP(4)              
         BCTR  R4,0                                                             
         LA    R7,4                                                             
         SR    R7,R5                                                            
         MVC   TEMP(4),=C'0000'                                                 
         LA    R7,TEMP(R7)                                                      
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),IFLD                                                     
         MVC   TEMP+4(4),=C'0000'            CHECK FOR NUMERIC                  
         MVZ   TEMP+4(4),TEMP                                                   
         CLC   TEMP+4(4),=C'0000'                                               
         BNE   MKTVE                                                            
         MVC   IFLD(4),TEMP                                                     
         MVI   KEYS,C'0'                                                        
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   KEYS,C'M'                                                        
         MVC   KEYS+1(1),REQMED                                                 
         MVC   KEYS+2(4),IFLD                                                   
         MVC   KEYS+6(2),AGY                                                    
         GOTO1 ARSTA                         READ MARKET RECORD                 
         CLC   FERN,=AL2(FE)                                                    
         BL    MKTVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(MKTNOF)                   MARKET NOT ON FILE           
         B     MKTVO                                                            
         LA    R7,SPTREC                                                        
         USING MKTRECD,R7                                                       
         CLI   6(R3),C'+'          CK FOR MKT LIMIT ACCESS                      
         BNE   MKTV4B                                                           
         MVC   HALF(1),7(R3)       SAVE MKT ACC                                 
         B     MKTV4C                                                           
MKTV4B   CLI   8(R3),C'+'          CK FOR MKT LIMIT ACCESS                      
         BNE   MKTV6                                                            
         MVC   HALF(1),9(R3)       SAVE MKT ACC                                 
                                                                                
MKTV4C   LA    R4,MKTLTACC                                                      
         LA    R5,3                                                             
*MKTV5    CLC   0(1,R4),7(R3)                                                   
MKTV5    CLC   0(1,R4),HALF                                                     
         BE    MKTV6                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,MKTV5                                                         
         B     MKTACERR                                                         
*                                                                               
MKTV6    MVC   NAME(24),MKTNAME                                                 
         OI    FIND,X'08'                    MKT = NNNN                         
         B     MKTVO                                                            
MKTVE    MVC   FERN,=AL2(MKTINV)                   INVALID MARKET               
         B     MKTVO                                                            
MKTACERR MVC   FERN,=AL2(ACCERR)                                                
MKTVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
MKTVX    MVC   MKTSAVE,FIND                                                     
         L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         B     MKTVXX                                                           
MKTVXX   B     XXIT                                                             
*                                                                               
*                                                                               
REGTAB   DC    C'123456789ABCDEFSD'                                             
         EJECT                                                                  
STATVAL  DS    0H                                                               
         CLC   RNUM,=C'Z5'                                                      
         BNE   *+8                                                              
         MVI   MKTSAVE,0                                                        
         MVC   STATWRK+99(1),1(R4)  SAVE REQ TBL FLD ENTRY(FROM 00)             
*                                      NEED IN MULTSTA REQS                     
*                                                                               
         JIF   RNUM,EQ,=C'44',AND,RO1,EQ,C'S',SHWVAL,JUMP=N                     
         NTR1                                STATION - FIND BITS                
         SPACE                                                                  
***                                                                             
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    STAVX               NO INPUT                                     
         LA    R2,IFLD                                                          
         LR    R1,R5               R5 = LEN INPUT                               
*                                                                               
         CLC   RNUM,=C'I2'         FOR I2 AND NEW BANDS                         
         BNE   STATVK5                                                          
         CLC   =C'ALL-DV',0(R2)    IS IT ALL-DV OR -SM                          
         BE    *+14                                                             
         CLC   =C'ALL-D ',0(R2)    OR    ALL-D  OR -S                           
         BNE   *+16                                                             
         CLI   RMED,C'T'                                                        
         BE    STATVI2                                                          
         B     STAVERR                                                          
*                                                                               
         CLC   =C'ALL-SM',0(R2)                                                 
         BE    STATVI2R                                                         
         CLC   =C'ALL-S ',0(R2)                                                 
         BE    STATVI2R                                                         
         CLC   =C'ALL-CM',0(R2)                                                 
         BE    STATVI2R                                                         
         CLC   =C'ALL-C ',0(R2)                                                 
         BNE   STATVK5                                                          
STATVI2R CLI   RMED,C'R'                                                        
         BNE   STAVERR                                                          
*                                                                               
STATVI2  MVC   RSTA(3),0(R2)       MOVE IN 'ALL'                                
         MVC   R2USER+7(1),4(R2)   MOVE IN NEW BAND                             
         OI    FIND,X'02'          TURN ON 'ALL' BITS                           
         B     STAVX                                                            
*                                                                               
STATVK5  CLC   RNUM,=C'K5'         FOR K5                                       
         BNE   COMLOOP                                                          
         CLC   =C'ALL,',0(R2)      IS IT ALL,N (N=SUBMEDIA)                     
         BNE   COMLOOP                                                          
         CHI   R5,5                LEN MUST = 5                                 
         BNE   COMLOOP                                                          
         CLI   4(R2),C'N'          SUBMEDIA = N,S,C                             
         BE    STATV0                                                           
         CLI   4(R2),C'S'                                                       
         BE    STATV0                                                           
         CLI   4(R2),C'C'                                                       
         BNE   STAVE                                                            
*                                                                               
STATV0   MVC   RSTA,0(R2)                                                       
         MVI   RSTA+3,C' '         RSTA=ALL N                                   
         OI    FIND,X'02'          TURN ON 'ALL' BITS                           
         B     STAVX                                                            
*                                                                               
COMLOOP  CLI   0(R2),C','          LOOK FOR COMMA DELIMITERS                    
         BE    STAV00                                                           
         LA    R2,1(R2)                                                         
         BCT   R1,COMLOOP                                                       
         B     STAVOK              DO REGULAR PROCESSING                        
STAV00   CLC   RNUM,=C'CM'         FOR CM REPT MULT STAT'S NOT ALLOWED          
         BE    STAVERR                                                          
         LA    R2,IFLD                                                          
         LR    R1,R5               R5 = LEN INPUT                               
COMLOOP1 CLI   0(R2),C'/'          LOOK FOR CABLE DELIMITERS                    
         BE    STAVERR             DISABLE MULT REQUEST WITH CABLE              
         LA    R2,1(R2)            FOR NOW                                      
         BCT   R1,COMLOOP1                                                      
         BAS   RE,MULTSTA                                                       
         STC   R5,5(R4)            RESET ORIGINAL INPUT LENGTH                  
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),STATWRK     RETURN ORIGINAL INPUT                        
*                                                                               
         B     STAVXX                                                           
         SPACE                                                                  
MULTVAL  NTR1                                                                   
****                                                                            
STAVOK   MVI   ROUTSUB,1                     02=ALL 04=XXXX 08=ALL-RN           
*                                  10 SPECIAL FOR SX AND SC                     
*                                  20 SPECIAL FOR SC                            
*                                                                               
         GOTO1 AINITV                        EXTRACT 1ST PART                   
         CLI   FIND,1                                                           
         BL    STAVX                         STA = MISSING                      
         JIF   RNUM,EQ,=C'44',AND,RO1,EQ,C'N',STAVERR,JUMP=N                    
*              CAN'T USE STATION  IF USING MKT GROUPS                           
*                                                                               
         TM    MKTSAVE,X'A0'       SEE IF USING MKTGRPS                         
         BZ    STAV                                                             
         CLC   RNUM,=C'NV'          IF NV REPORT                                
         BNE   STAVI2                                                           
         CLC   =C'ALL-',8(R4)       ALLOW MKTGRP + ALL- OR ALL/                 
         BE    STAV                                                             
         CLC   =C'ALL/',8(R4)                                                   
         BE    STAV                                                             
STAVI2   CLC   RNUM,=C'I2'          IF I2 REPORT                                
         BNE   STAVERR                                                          
         CLC   =C'ALL',8(R4)       ALLOW MKTGRP + ALL                           
         BE    STAVX                                                            
**                                                                              
STAVERR  MVC   FERN,=AL2(FLDINV)                                                
         B     STAVO                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
STAV     CLI   IFLDH+5,3                     1ST PART MUST BE 3/4 CHRS          
         BL    STAVE                                                            
         CLC   =C'ALL-',8(R4)     .. TEST ALL CABLE(R4=TO INPUT FIELD)          
         BNE   *+14                (IFLD TREATS C'-' AS DELIMITER)              
         MVC   IFLD(4),8(R4)       SET IT TO IFLD                               
         B     STAC10                                                           
         ZIC   R1,IFLDH+5         .. AND LOOK FOR CABLE C'/'                    
         LA    RE,IFLD                                                          
         LHI   R0,1                                                             
         LA    RF,8(R4)                                                         
STAC0    CLI   0(RE),C'/'                                                       
         BNE   STAC04                                                           
         CLI   IFLD,C'0'           IF IT'S NUMERIC                              
         BL    *+12                                                             
         MVI   IFLD+4,C'/'         MUST BE / (NOT SURE WHY...                   
         B     STAC10              ... JUST COPYING OLD CODE)                   
*                                                                               
         CHI   R0,5                IF C'/' IS IN USUAL POSITION                 
         BNL   STAC10              LEAVE AS IS                                  
         MVI   0(RE),C' '          CONVERT ANN/BA TO ANN /BA                    
         MVC   1(3,RE),0(RF)                                                    
         AHI   R1,1                INCREASE THE LEN OF THE STRING               
         STC   R1,IFLDH+5                                                       
         B     STAC10                                                           
STAC04   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         AHI   R0,1                                                             
         CR    R0,R1                                                            
         BNH   STAC0                                                            
         CLI   IFLD,C'0'           IF IT'S NUMERIC                              
         BL    STA00                                                            
         CLI   IFLD+4,C'/'         MUST BE /                                    
         BNE   STAVE                                                            
         B     STA00                                                            
*** FOR CABLE STATIONS STAVAL RETURNS STATION AND NETWORK                       
*** WE NEED TO VALIDATE BOTH                                                    
STAC10   DS    0H                                                               
         CLC   RNUM,=C'SC'         IF IT'S SC REPORT                            
         BE    STAVE               CANNOT BE CABLE                              
         CLC   RNUM,=C'CM'         IF IT'S CM REPORT                            
         BNE   *+12                READ NET DEF REC TO VALIDATE                 
         BRAS  RE,CBLVAL                                                        
         B     STAVXX              DONE HERE                                    
*                                                                               
         LA    R5,TEMP             YES/CABLE GOTO STAVAL                        
         XC    TEMP,TEMP                                                        
         USING STABLKD,R5                                                       
*                                                                               
         MVC   STBMED,REQMED       MEDIA                                        
         MVI   STBCTRY,C'U'        COUNTRY                                      
         MVC   STBACOM,ACOMFACS                                                 
         STCM  R4,15,STBADDR       REQUEST INPUT FIELD                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A68'  GET STAVAL ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R5),0                                                 
         CLI   STBERR,0            1=INVALID MEDIA, 2=INVALID STATION           
         BNE   STAVE                                                            
         MVC   RCARD2+12(3),STBNET          SET CABLE NETWORK                   
*        MVC   RSTA,STBSTA                  AND SAVE STATION                    
         MVC   SPTWORK(5),STBSTA                  AND SAVE STATION              
         CLI   STBNET,X'40'        DO WE VALIDATE NETWORK                       
         BNH   STAC20                                                           
         MVI   KEYS,C'0'           YES/READ CABLE NETWORK                       
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   KEYS,C'S'                                                        
         MVC   KEYS+1(1),REQMED                                                 
         MVC   KEYS+2(4),STBNET                                                 
         MVI   KEYS+6,C'T'                                                      
         MVC   KEYS+7(2),AGY                                                    
         GOTO1 ARSTA                                                            
***********************************************                                 
* STAPACK                                                                       
         DROP  R5                                                               
         LA    R5,TEMP+30                                                       
         XC    TEMP+30(40),TEMP+30                                              
         USING STAPACKD,R5                                                      
         MVI   STAPACT,C'P'        VALIDATE                                     
         MVC   STAPAGY,AGY                                                      
         MVC   STAPMED,REQMED                                                   
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'     MARKET                                     
         MVC   STAPQSTA(4),SPTWORK   STATION                                    
         MVC   STAPQNET,RCARD2+12 NETWORK                                       
* READ AGY HEADER FOR COUNTRY                                                   
         MVC   KEYD,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGY                                                     
         GOTO1 ARSPT                                                            
         MVC   STAPCTRY,SPTREC+104   AGYPROF+7                                  
         MVC   KEY,KEYD            RESET KEY                                    
*                                                                               
*                                                                               
*        XC    DMCB(16),DMCB                                                    
*        MVC   DMCB+4(4),=X'D9000A7A'  GET STAPACK                              
*        GOTO1 CALLOV,DMCB                                                      
*        CLI   4(R1),X'FF'                                                      
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        L     RF,DMCB                                                          
         LR    R1,R5                 ADDRESS OF THE BLOCK                       
****     GOTO1 (RF),(R1)                                                        
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    STAC20                                                           
         MVC   FERN,=AL2(607)                                                   
         B     STAVX                                                            
         DROP  R5                                                               
***********************************************                                 
         EJECT                                                                  
*                                                                               
STAC20   CLC   =C'ALL',SPTWORK     ..IF ALL                                     
         BNE   STAC30                                                           
         CLC   =C'NV',RNUM         ...IF RNUM                                   
         BNE   STAC20C                                                          
         CLC   RSTA,SPACES         ...AND NOT SPACES                            
         BNE   *+10                ...DON'T ENTER ALL                           
STAC20C  MVC   RSTA(3),=C'ALL'                                                  
                                                                                
* - IS IT ALL-                                                                  
         MVI   RSTA+4,C'-'                                                      
         CLC   =C'ALL-',8(R4)      R4 = INPUT FIELD HEADER                      
         BE    STAC22                                                           
         CLC   =C'ALL -',8(R4)                                                  
         BE    STAC22                                                           
                                                                                
* - IS IT ALL/                                                                  
         MVI   RSTA+4,C'/'                                                      
         CLC   =C'ALL/',8(R4)                                                   
         BE    *+14                                                             
         CLC   =C'ALL /',8(R4)                                                  
         BNE   STAC20F                                                          
*                                                                               
         CLC   RNUM,=C'B1'                                                      
         BE    STAVE                                                            
         CLC   RNUM,=C'D1'          DISALLOW FOR SPOT BILLING                   
         BE    STAVE                                                            
         B     *+8                                                              
*                                                                               
STAC20F  MVI   RSTA+4,X'40'        SHOULD NEVER GET HERE                        
*                                  BUT KEEP FOR NOW                             
                                                                                
STAC22   OI    FIND,X'02'          ..TURN ON 'ALL' BITS                         
         MVC   FERN,=AL2(FF)                                                    
         B     STAVX               ..THAT'S ALL                                 
                                                                                
         EJECT                                                                  
STAC30   DS    0H                                                               
* STABLKD USES TEMP/ SET UP TEMP TO CONTINUE REGULAR PROCESSING                 
* REGULAR PROCESSING IS A BIT OF A MESS WITH CODE OVER THE YEARS                
* BUT OFF WE GO TO VALIDATE STATION                                             
*        MVC   TEMP(5),RSTA     MOVE STATION TO TEMP                            
         MVC   TEMP(5),SPTWORK  MOVE STATION TO TEMP                            
*        XC    RSTA,RSTA           AND CLEAR RSTA                               
         MVI   TEMP+4,C'T'         CABLE IS ALWAYS C'T'                         
         CLI   CANAGY,C'C'         IF NOT CANADIAN                              
         BE    STAC31              CHECK IF CABLE IS NUMERIC                    
         CLI   IFLD,C'0'                                                        
         BNL   STAC31                                                           
         MVI   IFLD+4,C'T'         IF NOT NUMERIC, GET RID OF '/'               
STAC31   MVI   IFLD,C'T'                                                        
         B     STAV5               RESUME REQULAR PROCESSING                    
*                                                                               
STA00    MVC   TEMP(4),IFLD                  SAVE 1ST PART                      
*                                                                               
         CLI   IFLDH+5,4                                                        
         BH    STAVE                                                            
*                                                                               
         LR    R7,R5                                                            
         MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BH    STAVE                                                            
         BE    STAVIT                                                           
         CLC   RNUM,=C'SC'         2ND PART MISSING                             
         BNE   STAV02              IF SC CHK IF TV/RADIO                        
         CLI   RMED,C'R'                                                        
         BE    STAVE                                                            
STAV02   TM    FIND,X'02'          2ND PART MISSING                             
         BZ    STAV4                AASSUME TV                                  
         MVC   IFLD(3),=C'ALL'                                                  
         B     STAVO                                                            
STAVIT   LA    R7,8(R4,R7)                   DELIMITER MUST BE -                
         CLI   0(R7),C'-'                                                       
         BNE   STAVE                                                            
         TM    FIND,X'02'                                                       
         BZ    STAV3                                                            
         CLI   IFLD,C'R'                     CHECK 2ND PART OF ALL-XXX          
         BNE   STAVE                                                            
         CLI   IFLDH+5,2                                                        
         BH    STAV1                                                            
         LA    R1,REGTAB                                                        
         LA    0,17                                                             
STAV0    CLC   IFLD+1(1),0(R1)                                                  
         BE    STAV0A                                                           
         LA    R1,1(R1)                                                         
         BCT   0,STAV0                                                          
         B     STAVE                                                            
*                                                                               
STAV0A   MVC   RMARK+3(1),IFLD+1                                                
         B     STAV2                                                            
STAV1    CLC   IFLD(3),=C'REG'                                                  
         BNE   STAVE                                                            
         MVI   RMARK+3,C' '                                                     
STAV2    MVC   RMARK(3),=C'GLL'              SET SPECIAL MKT CODE               
         MVC   IFLD(3),=C'ALL'                                                  
         MVI   FIND,X'09'          STA=ALL-RN  OR ALL-REG                       
         B     STAVO                                                            
STAV3    CLI   IFLD,C'L'          CHECK FOR 'LOW POWER' STATION FIRST           
         BE    STAV5                                                            
         CLI   IFLDH+5,2                                                        
         BNE   STAVE                                                            
         CLC   IFLD(2),=C'TV'                                                   
         BE    STAV4                                                            
         CLC   IFLD(2),=C'DV'                                                   
         BE    STAV5                                                            
         CLC   IFLD(2),=C'AM'                                                   
         BE    STAV5                                                            
         CLC   IFLD(2),=C'FM'                                                   
         BE    STAV5                                                            
         CLC   IFLD(2),=C'SM'                                                   
         BE    STAV5                                                            
         CLC   IFLD(2),=C'CM'                                                   
         BE    STAV5                                                            
         B     STAVE                                                            
STAV4    MVI   IFLD,C' '                                                        
         MVC   DUB(1),REQMED       IF SECOND PART BLANK MOVE REQMED             
         B     STAV6                                                            
STAV5    MVC   DUB(1),IFLD                                                      
STAV6    CLI   IFLD+4,C'/'         ,,,IF ITS CABLE SLASH                        
         BNE   STV6AAA                                                          
         CLC   RNUM,=C'SC'         ...AND NOT SC                                
         BE    STAVE               ...CABLE NOTALLOWED                          
         B     STV6AA              ,,,KEEPIT                                    
STV6AAA  MVC   IFLD+4(1),IFLD                                                   
STV6AA   CLC   RNUM,=C'SX'         MKT FIX                                      
         BE    STAV6A                                                           
         CLC   RNUM,=C'SC'         OR STA CALL CHG                              
         BE    STAV6A                                                           
         TM    MKTSAVE,X'16'                                                    
         BNZ   STAVE                         ERROR NON-SPECIFIC MKT             
STAV6A   MVC   IFLD(4),TEMP                                                     
         OI    FIND,X'05'                    STA = XXXX                         
         MVI   KEYS,C'0'                                                        
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   KEYS,C'S'                                                        
         MVC   KEYS+1(1),REQMED                                                 
         MVC   KEYS+2(4),IFLD                                                   
         MVC   KEYS+6(1),DUB                                                    
         MVC   KEYS+7(2),AGY                                                    
         MVC   KEYS+9(3),RCLI                                                   
         CLC   RNUM,=C'SX'                                                      
         BE    STAV6B                                                           
         CLC   RNUM,=C'SC'                                                      
         BE    STAV6B                                                           
         B     STAV6BX                                                          
STAV6B   CLC   RCLI,=C'ALL'        FOR THESE REPORT MUST FIND DEFAULT           
         BNE   STAV6BX             IF ALL CLTS                                  
         MVC   KEYS+9(3),=3C'0'                                                 
STAV6BX  GOTO1 ARSTA                         READ STATION MASTER REC            
*                                                                               
         CLC   RNUM,=C'SC'         SEE IF STA CALL CHG                          
         BNE   STAV6D                                                           
                                                                                
* FOR SC REPORT ENSURE BOTH STATIONS IN SAME MARKET                             
         CLC   COLNUM,=H'49'      AND NEW CALL LETTERS - COL 50                 
         BNE   STAV6D              NO                                           
         CLC   FERN,=AL2(FE)       YES/DEALING WITH NEW STATION                 
         BL    STAV0               DISK ERROR                                   
         BE    STAV6E              2/25/94 LISA C. SAYS CHANGE IT               
*                                  SINCE VICTOR DON'T WORK HERE                 
*                                  NO MORE                                      
                                                                                
*                                  MAY OR MAY NOT FIND STATION                  
*                                  I DON'T CARE - ITS VICTOR'S PROBLEM          
*                                                                               
         CLC   RMARK(4),SPTREC+18  OLD/NEW STA IN SAME MKT ?                    
         BE    *+14                YES                                          
         MVC   FERN,=H'700'        NO/ERROR STA NOT IN SAME MKT                 
         B     STAVO                                                            
         MVI   FIND,X'21'                                                       
         MVC   FERN,=AL2(FF)          SET TO FOUND SO I                         
         B     STAVO               WON'T GET AN ERROR                           
*                                                                               
STAV6D   CLC   FERN,=AL2(FE)                                                    
         BL    STAVO                         DISK ERROR                         
         BH    *+14                                                             
STAV6E   MVC   FERN,=AL2(STANOF)                 STATION NOT ON FILE            
         B     STAVO                                                            
*                                                                               
         CLC   RNUM(2),=C'SX'      MKT FIX                                      
         BE    STAV6F                                                           
         CLC   RNUM,=C'SC'         STATION FIX                                  
         BNE   STAV7               NO - GO DO NORMAL CHECKS                     
STAV6F   DS    0H                                                               
         CLC   SPTREC(12),KEYS     MUST FIND CLT EXECPTION                      
         BNE   STAV6E              OR CLT IF REQUESTED                          
         MVI   FIND,X'11'                                                       
         CLC   RNUM(2),=C'SX'      MKT FIX                                      
         BNE   STAV8                                                            
*                                                                               
STAV7    DS    0H                                                               
         CLC   RNUM(2),=C'44'        NO MKT FOR REQ 44                          
         BE    STAVO                                                            
*                                                                               
         CLC   RNUM(2),=C'C2'      IF C2                                        
         BNE   STAV7A                                                           
         MVC   RMARK(4),=C'0000'   MARKET MUST = 0000                           
         B     STAV7B                                                           
*                                                                               
STAV7A   TM    MKTSAVE,X'08'                 WAS MARKET SPECIFIC                
         BZ    STAV8                         NO                                 
STAV7B   CLC   RMARK(4),SPTREC+18                                               
         BE    STAV9                                                            
         CLC   RNUM(2),=C'I2'      REQ I2/SPECIFIC MKT/STA LEAVE ALONE          
         BE    STAVO                                                            
         CLC   RNUM(2),=C'L2'      REQ L2/SPECIFIC MKT/STA LEAVE ALONE          
         BE    STAVO                                                            
         CLC   RNUM(2),=C'SX'      REQ SX/SPECIFIC MKT/STA LEAVE ALONE          
         BE    STAVO                                                            
*                                                                               
         CLI   CLIEXTRA+2,C'A'     SKIP STA/MKT CHECK IF                        
*******> BL    *+12                                                             
*******> CLI   CLIEXTRA+2,C'F'                                                  
*******> BNH   STAVO                                                            
         BNL   STAVO                                                            
*                                                                               
         MVC   FERN,=AL2(STANIM)       ERROR STATION NOT IN MKT                 
         B     STAVO                                                            
STAV8    MVC   RMARK(4),SPTREC+18                                               
         CLC   RNUM,=C'B1'         IF B1                                        
         BE    STAV8D              SKIP MKTSAVE SETTING                         
         CLC   RNUM,=C'I4'         IF I4                                        
         BE    STAV8D              SKIP MKTSAVE SETTING                         
         CLC   RNUM,=C'I5'         IF I5                                        
         BNE   STAV8C                                                           
*                                                                               
         XR    R5,R5                                                            
         IC    R5,5(R4)                                                         
         BCTR  R5,0                USING FOR BCT LOOP                           
         LR    R7,R4                                                            
         MVI   HALF+1,C' '         MOVE IN A SPACE                              
STAV8B   CLI   8(R7),C'/'          DO WE HAVE A SLASH?                          
         BE    STAV8B3                                                          
         CLI   HALF+1,C'Y'         SLASHY YET?                                  
         BNE   STAV8B5              - NOPE                                      
         MVC   FERN,=AL2(I5NONWRK)   CAN'T HAVE SINGLE NETWORK                  
         B     STAVO                                                            
*                                                                               
STAV8B3  MVI   HALF+1,C'Y'         WE HAVE A SLASH                              
STAV8B5  LA    R7,1(R7)            BUMPY                                        
         BCT   R5,STAV8B                                                        
         B     STAV8D              SKIP MKTSAVE SETTING                         
*                                                                               
STAV8C   CLC   RNUM,=C'JV'         IF JV                                        
         BE    STAV8D              SKIP MKTSAVE SETTING                         
         OI    MKTSAVE,X'08'       SET SPECIFIC MKT INPUT                       
*                                  READ MKT REC TO CHK FOR LIMIT ACC            
STAV8D   CLI   6(R3),C'+'                                                       
         BNE   STAV8F              NO NEED TO CHK                               
         MVC   HALF(1),7(R3)       SAVE MKT ACC                                 
         B     STAV8H                                                           
STAV8F   CLI   8(R3),C'+'                                                       
         BNE   STAV9               NO NEED TO CHK                               
         MVC   HALF(1),9(R3)       SAVE MKT ACC                                 
                                                                                
STAV8H   MVI   KEYS,C'0'                                                        
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   KEYS,C'M'                                                        
         MVC   KEYS+1(1),REQMED                                                 
         MVC   KEYS+2(4),RMARK                                                  
         MVC   KEYS+6(2),AGY                                                    
         GOTO1 ARSTA                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    STAV0                                                            
         BH    *+14                                                             
         MVC   FERN,=AL2(MKTNOF)                                                
         B     STAV0                                                            
*                                                                               
         LA    R7,SPTREC                                                        
         USING MKTRECD,R7                                                       
         LA    R4,MKTLTACC                                                      
         LA    R5,3                                                             
*STAV8B   CLC   0(1,R4),7(R3)                                                   
STAV8K   CLC   0(1,R4),HALF                                                     
         BE    STAV9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,STAV8K                                                        
SACCERR  MVC   FERN,=AL2(ACCERR)                                                
         B     STAVO                                                            
*                                                                               
         DROP  R7                                                               
*                                                                               
STAV9    MVC   SAVEREP(3),SPTREC+63          SAVE REP                           
         B     STAVO                                                            
STAVE    MVC   FERN,=AL2(STAINV)                   INVALID STATION              
STAVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(5,R7),IFLD                                                     
STAVX    MVC   STASAVE,FIND                                                     
STAVXX   B     XXIT                                                             
*                                                                               
*                                                                               
MULTSTA  NTR1                                                                   
         MVC   RMARKSV,RMARK                                                    
         MVC   MKTSV,MKTSAVE                                                    
         MVI   MLTREQSW,C'Y'       SET MULTIPLE REQ SWITCH                      
         LA    R1,STATWRK                                                       
         MVI   0(R1),C' '                                                       
         MVC   1(L'STATWRK-2,R1),0(R1)                                          
         MVC   1(1,R4),STATWRK+99      SET 1(R4) TO REQ FLD TBL ENTRY           
         XC    STATSV,STATSV                                                    
         LA    R2,STATWRK            R2=STATWRK (BUMPED)                        
         SR    R6,R6                 R3=STATION COUNTER                         
         LR    R1,R5                                                            
         LR    R5,R2                 R5=CURRENT FIELD IN STATWRK                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),8(R4)       MOVE TWA INPUT FIELD TO STATWRK              
         ZIC   R1,0(R4)                                                         
         S     R1,=F'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),0(R2)    BLANKS TO TWA INPUT FIELD                       
*                               (R2=STATWRK=BLANKS AT THIS POINT)               
         SR    R1,R1                                                            
MULTLOP  CLI   0(R2),C','          DELIMITER                                    
         BE    MULT10                                                           
         CLI   0(R2),C' '          END OF INPUT                                 
         BE    MULT10                                                           
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         B     MULTLOP                                                          
*                                                                               
MULT10   LTR   R1,R1                                                            
         BZ    STAVE                                                            
         LA    R6,1(R6)            BUMP STATION COUNT                           
         STC   R1,5(R4)            SET LENGTH TO FIELD HEADER                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),0(R5)       SET STATION TO FIELD                         
         LA    R2,1(R2)            BUMP TO START OF NEXT STATION                
         LR    R5,R2                                                            
         BAS   RE,MULTVAL          VALIDATE                                     
         TM    FIND,X'01'                                                       
         BZ    VAL3                                                             
         CLC   FERN,=AL2(FF)                                                    
         BNE   MULTX                ERROR                                       
         B     VAL4                          FLD INPUT VALID                    
VAL3     TM    1(R4),X'01'                   FLD NOT INPUT                      
         BZ    MULTX                ERROR                                       
         B     MULT15                                                           
VAL4     NI    FIND,X'FE'                    FIND=B'XXXXXXX0'                   
         MVC   TEMP(1),1(R4)                 TEMP=B'XXXXXXXX'                   
         NC    TEMP(1),FIND                  IS FLD FORMAT OK FOR REQ           
         BNZ   MULT15                        YES                                
         B     MULTX                ERROR                                       
*                                                                               
MULT15   LA    R1,STATSV                                                        
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVC   0(4,R1),RMARK       SAVE MARKET                                  
         MVC   RMARK(4),RMARKSV                                                 
         MVC   MKTSAVE,MKTSV                                                    
         MVC   4(5,R1),IFLD        SAVE STATION                                 
         SR    R1,R1                                                            
         CLI   0(R2),C' '          END OF STATIONS                              
         BNE   MULTLOP                                                          
         LA    R1,STATSV           YES                                          
         MVC   RMARK(4),0(R1)            SET 1ST MARKET TO REQ REC              
         LH    R7,COLNUM                 SET 1ST STATION TO REQ REC             
         LA    R7,RNUM(R7)                                                      
         MVC   0(5,R7),4(R1)                                                    
         MVC   0(L'STATSV-9,R1),9(R1)    MOVE UP REST OF MARK/STAT              
         BCTR  R6,0                                                             
         STC   R6,MULTNUM          SET COUNTER TO MULTNUM                       
         OI    FIND,X'01'          SET FIELDINPUT                               
*                        THIS BIT NCD OUT ABOVE AND WILL BE CHECKED             
*                        IN 00-WILL RETURN ERROR IF FIELD IS REQUIRED           
         B     MULTX               EXIT                                         
*                                                                               
MULTX    B     XXIT                                                             
         EJECT                                                                  
SEDVAL   NTR1                                START,END DATES -FIND BITS         
         MVI   ROUTSUB,1                     04=YYMM 08=YYMMDD 10=ES            
*              80=START YYMMDD AND NO END                                       
*              40= START YYMM AND NO END                                        
*        20=13/YY            13 MTH13TH PERIOD AND NO END - FOR BILLING         
*                                                                               
         GOTO1 AINITV                                                           
         CLI   5(R4),0                                                          
         BE    SEDVO                         START,END NOT INPUT                
         CLI   IFLDH,1                                                          
         BNE   SEDVE                                                            
         CLC   IFLD(3),=C'13/'                                                  
         BNE   SEDV0                                                            
         CLI   IFLDH+5,5                                                        
         BNE   SEDVE                                                            
         CLI   IFLD+3,C'0'                                                      
         BL    SEDVE                                                            
         CLI   IFLD+4,C'0'                                                      
         BL    SEDVE                                                            
         MVC   RSTRD(2),IFLD+3                                                  
         MVC   RSTRD+2(2),IFLD                                                  
         OI    FIND,X'20'                                                       
*                                  13/YY        FOR BILLING                     
         B     SEDVO                                                            
*                                                                               
SEDV0    DS    0H                                                               
         CLI   IFLDH+5,2                                                        
         BNE   SEDV1                                                            
         CLC   IFLD(2),=C'ES'                                                   
         BNE   SEDVE                                                            
         OI    FIND,X'10'                    START = ES                         
         MVC   RES,IFLD                                                         
         B     SEDV2                                                            
SEDV1    GOTO1 DATVAL,PLIST,(0,IFLD),RSTRD                                      
         OC    PLIST(4),PLIST                                                   
         BE    *+12                                                             
         OI    FIND,X'08'                    START = YYMMDD                     
         B     SEDV2                                                            
         GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    SEDVE                                                            
         OI    FIND,X'04'                    START = YYMM                       
         CLC   RNUM(2),=C'NV'         TEST IF INVOICE REPORT                    
         BE    SEDV1F                     .YES/LEAVE MM BLANK                   
         CLC   RNUM(2),=C'I6'         TEST IF I6 REPORT                         
         BE    SEDV1F                     .YES/LEAVE MM BLANK                   
         MVC   RSTRD+4(2),=C'14'             FIX TO YYMM14                      
                                                                                
* - IF A2 OR YT GET BROADCAST MONTH START ?                                     
         CLC   RNUM(2),=C'A2'                                                   
         BE    *+14                                                             
         CLC   RNUM(2),=C'YT'                                                   
         BNE   SEDV1F                                                           
         LA    R1,RSTRD                                                         
         ST    R1,PLIST                                                         
         MVI   PLIST,1                                                          
         BAS   RE,DATBROAD                                                      
         MVC   RSTRD,SPTWORK       START                                        
* - SET BROADCAST MONTH END IN CASE YYMM AND NO END INPUT                       
         MVC   RENDD,SPTWORK+6     END                                          
                                                                                
SEDV1F   CLC   RNUM(2),=C'50'     * SET DAY TO 01 FOR EST DATE CHECK            
         BNE   *+10                                                             
         MVC   RSTRD+4(2),=C'01'                                                
         EJECT                                                                  
SEDV2    DS    0H                                                               
         CLC   RNUM(2),=C'NV'         FOR NV DON'T ALLOW REQUESTS               
         BNE   SEDV20                 WITH A START DATE PRIOR APR01/02          
         CLC   RAGY,=C'MC'            ONLY FOR MC                               
         BNE   SEDV20                                                           
         CLC   RSTRD(4),=X'FAF2F0F4'                                            
         BL    SEDVE                                                            
*                                                                               
SEDV20   MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BL    SEDV2B                        END NOT INPUT                      
         BH    SEDVE                                                            
         CLI   IFLDH+5,2                                                        
         BNE   SEDV4                                                            
         CLC   IFLD(2),=C'ES'                ALLOW ES OR ES,ES                  
         BNE   SEDVE                                                            
*                                                                               
SEDV2B   DS    0H                  END NOT INPUT                                
         CLC   RNUM(2),=C'NV'          TEST IF INVOICE REPORT                   
         BNE   SEDV3                                                            
         TM    FIND,X'08'               WAS START=YYMMDD                        
         BO    SEDVE                    YES/ERROR                               
*                                                                               
SEDV3    TM    FIND,X'10'                                                       
         BZ    SEDV3A                        END DATE MISSING                   
*        CLC   RNUM,=C'DN'    ALLOW ES AND EST=NNN-NNN FOR REQ DN               
*        BNE   SEDV3AA                                                          
*        TM    ESTSAVE,X'02'  BUT NOT IF EST=ALL                                
*        BNO   SEDVO                                                            
SEDV3AA  TM    ESTSAVE,X'24'                                                    
         BZ    SEDVE                         ES ONLY FOR SPECIFIC EST           
         CLI   ESTDATES,0          SEE IF I HAVE EST DATES                      
         BE    SEDVO               NO                                           
         MVC   RSTRD(12),ESTDATES  YES USE THEM                                 
         MVC   RES(2),=C'  '       BLANK OUT ES                                 
* FOR YT REPORT CHANGE ES DATES TO BROADCAST??                                  
**       CLI   RNUM,=C'YT'         IF YT REPORT                                 
**       BNE   SEDVO                                                            
**       LA    R1,RSTRD                                                         
**       ST    R1,PLIST                                                         
**       MVI   PLIST,1                                                          
**       BAS   RE,DATBROAD                                                      
**       MVC   RSTRD,SPTWORK       START                                        
         B     SEDVO                                                            
*                                                                               
SEDV3A   TM    FIND,X'08'                                                       
         BZ    SEDV3B                                                           
         MVI   FIND,X'81'          YYMMDD AND NO END                            
         B     SEDVO                                                            
*                                                                               
SEDV3B   TM    FIND,X'04'          YYMM AND NO END                              
         BZ    SEDVE                                                            
         CLC   RNUM(2),=C'A2'      IF A2 / END SET                              
         BE    SEDVO                                                            
         CLC   RNUM(2),=C'NV'     * TEST INVOICE REPORT                         
         BNE   SEDV3C             * WHEN ONLY ONE DATE, MAKE THIS               
         MVC   RENDD,RSTRD        * END AND CLEAR START                         
         MVC   RENDD+4(2),=C'16'     YYMM16                                     
         MVC   RSTRD,=6X'40'                                                    
         B     SEDVO                                                            
SEDV3C   MVC   RSTRD+4(2),=C'  '                                                
         MVI   FIND,X'41'          YYMM AND NO END                              
         B     SEDVO                                                            
*                                                                               
SEDV4    TM    FIND,X'0C'                                                       
         BZ    SEDVE                                                            
         GOTO1 DATVAL,PLIST,(0,IFLD),RENDD                                      
         OC    PLIST(4),PLIST                                                   
         BE    *+16                                                             
         TM    FIND,X'08'                                                       
         BZ    SEDVE                                                            
         B     SEDV5                         STR = END = YYMMDD                 
* - YYMM-YYMM INPUT                                                             
         GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    SEDVE                                                            
                                                                                
* - IF A2 GET BROADCAST END DATE                                                
         CLC   RNUM(2),=C'A2'                                                   
         BE    *+14                                                             
         CLC   RNUM(2),=C'YT'                                                   
         BNE   SEDV4D                                                           
         MVC   RENDD+4(2),=C'14'    SET TO MIDDLE OF MONTH                      
         LA    R1,RENDD                                                         
         ST    R1,PLIST                                                         
         MVI   PLIST,1                                                          
         BAS   RE,DATBROAD                                                      
         MVC   RENDD,SPTWORK+6     BROADCAST END DATE                           
         B     SEDV5                                                            
                                                                                
SEDV4D   CLC   RNUM(2),=C'NV'      *INVOICE REPORT                              
         BE    SEDV4DD             *LEAVE MM=BLANK                              
         CLC   RNUM(2),=C'I6'      *I6  REPORT                                  
         BE    SEDV4DD             *LEAVE MM=BLANK                              
         MVC   RENDD+4(2),=C'16'             FIX TO YYMM16                      
SEDV4DD  CLC   RNUM(2),=C'50'      SET DAY TO 31 FOR EST DATE CK                
         BNE   *+10                                                             
         MVC   RENDD+4(2),=C'31'                                                
         TM    FIND,X'04'                                                       
         BZ    SEDVE                         STR = END = YYMM                   
SEDV5    CLC   RSTRD,RENDD                                                      
         BNH   SEDV6                                                            
         MVC   FERN,=AL2(SEDSGE)                   START GT END                 
         B     SEDVXX                                                           
SEDV6    CLI   ESTDATES,0                                                       
         BE    SEDVO                                                            
         CLC   RNUM(2),=C'BT'              FOR BT REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'GT'              FOR GT REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'GM'              FOR GM REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'LO'              FOR LO REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'JW'              FOR JW REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'WB'              FOR WB REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'ZB'              FOR ZB REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'IN'              FOR IN REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'CH'              FOR CH REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
         CLC   RNUM(2),=C'PZ'              FOR PZ REQUEST DATES CAN BE          
         BE    SEDVO                       OUTSIDE ESTIMATE DATES               
*                                                                               
         CLC   ESTDATES(6),RENDD                                                
         BH    *+14                                                             
         CLC   ESTDATES+6(6),RSTRD                                              
         BNL   SEDVO                                                            
         MVC   FERN,=AL2(SEDNIE)            ERROR DATES NOT WITHIN EST          
         B     SEDVO                                                            
SEDVE    MVC   FERN,=AL2(SEDINV)                   DATES INVALID                
SEDVO    EQU   *                                                                
         MVC   SEDSAVE,FIND                                                     
         L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         B     SEDVX                                                            
SEDVX    DS    0H                                                               
         CLI   FIND,0              IF NO INPUT                                  
         BE    SEDVXX              EXIT                                         
         CLC   FERN,=AL2(FF)       IF NO ERROR                                  
         BE    SEDVXX              EXIT                                         
         TM    RFPSTAT,RFPINUSE          SEE IF $RFP IN USE                     
         BZ    SEDVXX                                                           
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         MVI   FIND,X'01'                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   SEDVXX                                                           
         CLC   RNUM,=C'NV'         NV HAS RFP RESTIRCTIONS                      
         BE    SERFP20                                                          
*                                                                               
         CLC   QRFPDICT,=Y(SP#RFPST)    START DATE (YYMMDD) + NO END            
         BE    *+14                                                             
         CLC   QRFPDICT,=Y(NE#RFPST)    START DATE (YYMMDD) + NO END            
         BNE   *+12                                                             
         OI    FIND,X'80'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(SP#RFPSM)    START DATE (YYMM) + NO END              
         BE    *+14                                                             
         CLC   QRFPDICT,=Y(NE#RFPSM)    START DATE (YYMM) + NO END              
         BNE   *+12                                                             
         OI    FIND,X'40'                                                       
         B     START400                                                         
*                                                                               
SERFP20  CLC   QRFPDICT,=Y(SP#RFPRD)    START-END (YYMMDD-YYMMDD)               
         BE    *+14                                                             
         CLC   QRFPDICT,=Y(NE#RFPRD)    START-END (YYMMDD-YYMMDD)               
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     START400                                                         
                                                                                
* - FOR SYMBOLIC WORD 'TODAY'                                                   
         CLC   QRFPDICT,=Y(SP#EODTD)    START-END (YYMMDD-YYMMDD)               
         BE    *+14                                                             
         CLC   QRFPDICT,=Y(NE#EODTD)    START-END (YYMMDD-YYMMDD)               
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(SP#RFPRM)    START-END (YYMM-YYMM)                   
         BE    *+14                                                             
         CLC   QRFPDICT,=Y(NE#RFPRM)    START-END (YYMM-YYMM)                   
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     START400                                                         
*                                                                               
         B     SEDVXX                                                           
*                                                                               
START400 MVC   RSTRD,SPACES             STORE ESCAPE SEQ IN REQCARD             
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,12                                                       
         MVC   FERN,=AL2(FF)                                                    
SEDVXX   B     XXIT                                                             
*                                                                               
* - GETS BROADCAST DATES - EXPECTS 1ST PARAMETER FILLED IN                      
DATBROAD NTR1                                                                   
         L     R2,GETDAY                                                        
         L     R4,ADDAY                                                         
         GOTO1 GETBROAD,PLIST,,SPTWORK,(R2),(R4)                                
         CLI   PLIST,X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     XXIT                                                             
         EJECT                                                                  
TARVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   TARVO                                                            
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    TARVE                                                            
         MVC   IFLD(3),TEMP+1                                                   
         OI    FIND,X'04'        TARAUD=NNN                                     
         B     TARVO                                                            
*                                                                               
TARVE    MVC   FERN,=AL2(DEMINV)                                                
TARVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
TARVX    XIT1                                                                   
         EJECT                                                                  
*              PRD OR PRD GROUP                                                 
*        X'02'=ALL                                                              
*        X'04'=PRD                                                              
*        X'08'=POL                                                              
*        X'10'=PGP=XPOL                                                         
*        X'20'=PGP=XNNN                                                         
*        X'40'=PGP=XALL                                                         
*        X'80'=PGP=XPRD                                                         
*                                                                               
PGRPVAL  NTR1                                                                   
PGRPVAL0 GOTO1 AINITV              ENTER HERE FROM PRMVAL                       
         CLI   FIND,1                                                           
         BL    PGRPXIT             MISSING                                      
         CLC   IFLD(4),=C'PGR='                                                 
         BE    PGRPV2                                                           
*                                                                               
         CLC   =C'DX',RNUM         DX AND CP ALLOW FOR AAA-BBB PRODS            
         BE    PGRPV1                                                           
         CLC   =C'CP',RNUM                                                      
         BE    PGRPV1                                                           
         CLC   =C'K3',RNUM         AND K3                                       
         BE    PGRPV1                                                           
         CLC   =C'RY',RNUM         AND RY                                       
         BE    PGRPV1                                                           
         CLC   =C'RS',RNUM         AND RS                                       
         BE    PGRPV1                                                           
*                                                                               
         CLI   IFLDH+5,3                                                        
         BH    PGRPVE              INPUT TOO LONG                               
PGRPV1   CLC   IFLD(3),=C'NO '     PRD NO INVALID                               
         BE    PGRPVE                                                           
*                                                                               
         B     PROV                SAME AS CODE IN PRDVAL SO GO THERE           
*                                                                               
PGRPV2   CLI   IFLDH+5,6           AT LEAST 6 CHARS                             
         BL    PGRPVE                                                           
*        GET SCHEME CODE                                                        
         MVC   KEYS(13),KEY        SAVE KEY IN KEYS                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),KEYS+1     AGY/MED/CLT                                  
         MVC   KEY+5(1),IFLD+4     SCHEME CODE                                  
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PGRPVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     PGRPVO                                                           
*                                                                               
         MVC   RDIV,IFLD+4         PUT SCHEME IN REQUEST                        
         CLC   IFLD+5(3),=C'POL' CHK PGR=APOLNNN                                
         BNE   PGRPV2A                                                          
         CLI   IFLD+8,C' '                                                      
         BE    PGRPV2A                                                          
         CLI   IFLD+8,C'Z'       CHK NUMERIC                                    
         BH    *+8                                                              
         B     PGRPVE              ERROR/NOT NUMERIC                            
         NI    RDIV,X'BF'          MAKE A  LOW CHARACTER                        
PGRPV2A  CLC   IFLD+5(3),=C'ALL'                                                
         BNE   PGRPV3                                                           
         OI    FIND,X'40'          PGP=XALL                                     
         B     PGRPVZ                                                           
PGRPV3   CLI   IFLD+5,C'Z'                                                      
         BH    PGRPV5              NUMERIC INPUT                                
*                                                                               
*                                                                               
*                      X PRD CODE INPUT SO VALIDATE                             
*                                                                               
         CLC   IFLD+5(3),=C'POL'                                                
         BNE   *+12                                                             
         OI    FIND,X'10'          XPOL  INPUT                                  
         B     PGRPV4                                                           
*                                                                               
         OI    FIND,X'80'          XABC  INPUT                                  
PGRPV4   MVC   KEY(13),KEYS      RESTORE KEY FOR PRD READ                       
         MVC   KEY+4(3),IFLD+5                                                  
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PGRPVO              DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(PRONOF)                                                
         B     PGRPVO                                                           
         MVC   NAME(20),SPTREC+28                                               
         CLI   IFLD+8,C' '         CHK PGR=APOLNNN                              
         BE    PGRPVO              NO/EXIT                                      
         SPACE                                                                  
*                                  YES                                          
* PGR=APOLNNN, SO FUDGE IT TO PGR=ANNN THEN VALIDATE AS USUAL *                 
         MVC   KEYS(13),KEY        SAVE KEY IN KEYS                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),KEYS+1     AGY/MED/CLT                                  
         MVC   KEY+5(1),IFLD+4     SCHEME CODE                                  
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PGRPVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     PGRPVO                                                           
         SPACE                                                                  
         ZIC   R4,IFLDH+5                                                       
         SH    R4,=H'3'            SUBTRACT "POL" LENGTH                        
         STC   R4,IFLDH+5                                                       
         MVC   IFLD+5(3),IFLD+8    MOVE NNN TO POL SO THAT PGR=ANNN             
         SPACE                                                                  
*                                                                               
PGRPV5   LA    R7,SPTREC+24                                                     
         CLI   0(R7),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R4,R4                                                            
         IC    R4,14(R7)                                                        
         SR    R5,R5                                                            
         IC    R5,27(R7)                                                        
         AR    R4,R5                                                            
         IC    R5,40(R7)                                                        
         AR    R4,R5                                                            
         ST    R4,FULL             STORE TOTAL LENGTH                           
*                                                                               
PGRPV6   SR    R7,R7                                                            
         IC    R7,IFLDH+5                                                       
         SH    R7,=H'5'                                                         
         C     R7,FULL                                                          
         BNE   PGRPVE              WRONG LENGTH                                 
*                                                                               
PGRPV7   XC    TEMP(4),TEMP                                                     
         MVI   FULL,0                                                           
         LA    R4,TEMP                                                          
         LA    R5,IFLD+5                                                        
         CLI   0(R5),C'*'          CANNOT START WITH A *                        
         BE    PGRPVE                                                           
PGRPV7A  CLI   0(R5),C'*'                                                       
         BE    PGRPV7X             NO MORE VALIDATION                           
         CLI   0(R5),C'0'                                                       
         BL    PGRPVE                                                           
         CLI   FULL,1              * WAS INPUT BEFORE A NUMBER                  
         BE    PGRPVE              ERROR                                        
         MVC   0(1,R4),0(R5)                                                    
PGRPV7C  LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R7,PGRPV7A          R7 HAS ADJUSTED INPUT LENGTH                 
         B     PGRPV8                                                           
*                                                                               
PGRPV7X  OI    FIND,X'20'                                                       
         OI    FULL,1                                                           
         B     PGRPV7C                                                          
*                                                                               
PGRPV8   CLI   FULL,1                                                           
         BE    PGRPVZ              * INPUT NO FUTHER VALIDATION                 
         PACK  FULL+1(3),TEMP(5)                                                
         MVC   KEY+6(2),FULL+1                                                  
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PGRPVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     PGRPVO                                                           
*                                                                               
         CLC   RNUM,=C'BU'         FOR BILLING CHECK THAT PGR HAS               
         BE    PGRPV10             PRODUCTS IN IT                               
         CLC   RNUM,=C'DU'                                                      
         BE    PGRPV10                                                          
         CLC   RNUM,=C'D1'                                                      
         BE    PGRPV10                                                          
         CLC   RNUM,=C'B1'                                                      
         BNE   PGRPV20                                                          
PGRPV10  MVI   KEY+1,X'81'         PASSIVE POINTER BY PRODUCT                   
         MVC   KEYD,KEY            USE KEYD AS KEYSAVE                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,TEMP,0                  
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TEMP(8),KEYD                                                     
         BE    PGRPV20             THIS PRDGRP HAS AT LEAST ONE PRD             
         MVC   FERN,=AL2(NOPRDPG)  OTHERWISE GIVE ERROR                         
         B     PGRPVO                                                           
*                                                                               
PGRPV20  OI    FIND,X'20'                                                       
         B     PGRPVZ                                                           
*                                                                               
PGRPVE   MVC   FERN,=AL2(FLDINV)                                                
***      B     PGRPVO                                                           
         B     PGRPXIT      ELSE IN SOON ONLY GET SOON ERROR MESSAGE            
*                                                                               
PGRPVZ   MVC   KEY(13),KEYS        RESTORE KEY                                  
*                                                                               
PGRPVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD+5                                                   
         FOUT  (R6),NAME                                                        
*                                                                               
PGRPVX   MVC   PROSAVE,FIND                                                     
         CLC   =C'N2',RNUM         IF N2                                        
         BNE   PGRPN2X                                                          
         TM    FIND,X'20'          AND XABC INPUT?                              
         BNO   PGRPN2X                                                          
         NI    RDIV,X'BF'          MAKE DIVISION LOWER CASE                     
PGRPN2X  EQU   *                                                                
         L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         CLC   =C'SOON',BVROUT                                                  
         BNE   PGRPXIT                                                          
         B     PGRPXIT                                                          
PGRPXIT  XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
*                        MKT OR MKT-GRP VALIDATION                              
*              X'02'=ALL                                                        
*              X'04'=ALLN                                                       
*              X'08'=NNNN                                                       
*              X'20'=MGR=XALL                                                   
*              X'80'=MGR=XNNNN                                                  
*                                                                               
MGRPVAL  NTR1                                                                   
MGRPVALI GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNL   MGRPVAL1                                                         
         CLI   RMED,C'N'          MISSING                                       
         BNE   MKTVO                                                            
         CLC   RNUM,=C'RS'        RS/RY REQUIRE INPUT FOR MEDIA N               
         BE    MGRPVAL0           TO PREVENT ACCIDENTALLY FAX/REPORT            
         CLC   RNUM,=C'RY'        EVERY LOCAL STATION ON EVERY NWK              
         BE    MGRPVAL0           IF OMIT MARKET & DEFAULT ALL USED             
         CLI   CANAGY,C'C'                                                      
         BNE   MKTVO                                                            
         CLC   RNUM,=C'K1'                                                      
         BNE   MKTVO                                                            
MGRPVAL0 L     R4,FLDHADR         SET DEFAULT OF MARKET 0000 INSTEAD            
         MVI   5(R4),4                                                          
         OI    6(R4),X'80'                                                      
         MVC   8(4,R4),=C'0000'                                                 
         B     MGRPVALI           GO AGAIN                                      
MGRPVAL1 DS    0H                                                               
*        CLC   RNUM,=C'K1'                                                      
*        BNE   MGRPVAL3                                                         
*        L     RF,ASAVE               FOR K1 CANADA, MEDIA N, NO MKT            
*        CLI   CANAGY-TWAD(RF),C'C'                                             
*        BNE   MGRPVAL3                                                         
*        CLI   RMED,C'N'                                                        
*        BNE   MGRPVAL3                                                         
*        CLC   IFLD(3),=C'ALL'    ALLOW TO ENTER 'ALL'                          
*        BE    MGRPVAL3                                                         
*        MVC   FERN,=AL2(FLDINV)                                                
*        B     MKTVO                                                            
MGRPVAL3 CLI   CANAGY,C'C'         FOR CANADA MED N K1, ONLY 0000               
         BNE   MGRPVAL4                                                         
         CLI   RMED,C'N'                                                        
         BNE   MGRPVAL4                                                         
         CLC   RNUM,=C'K1'                                                      
         BNE   MGRPVAL4                                                         
         CLC   =C'0000',IFLD                                                    
         BE    MKTV0                                                            
         MVC   FERN,=AL2(FLDINV)                                                
         B     MKTVO                                                            
MGRPVAL4 CLI   FIND,3                                                           
         BE    MGRPVALG                                                         
         CLC   IFLD(4),=C'MGR='                                                 
         BNE   MKTV0                                                            
MGRPVALG GOTO1 =A(MGRPV),DMCB,RR=RELO                                           
MGRX     XIT1                                                                   
         SPACE 3                                                                
*                        VARIOUS AND SUNDRY DATA                                
*              X'04'=OK                                                         
*                                                                               
ANYVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    AOK                    MISSING                                   
         GOTO1 =A(ANYVL),DMCB,RR=RELO                                           
AOK      XIT1                                                                   
         SPACE 3                                                                
*                        ALPHA NUMERIC                                          
*              X'04'=ALPHA, X'08'=NUMERIC                                       
* DIFFERS FROM ALPVAL IN SPREQ04/SETS NUM IN EBCDIC\                            
*              ALPVAL SETS NUM AS HEX                                           
ALPNVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    ALPOK                  MISSING                                   
         GOTO1 =A(APNVL),DMCB,RR=RELO                                           
ALPOK    XIT1                                                                   
         SPACE 3                                                                
*                        PROD-LEN VALIDATION                                    
*              X'04'                                                            
PRDLEN   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    PRDLENX                MISSING                                   
         GOTO1 =A(PRDLENV),DMCB,RR=RELO                                         
PRDLENX  XIT1                                                                   
         EJECT                                                                  
*                           PROD=WEIGHT VALIDATION                              
*                           X'04'                                               
PRDWGT   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    PWTX                NO INPUT                                     
                                                                                
         CLI   IFLDH+5,7           AAA=NNN                                      
         BH    PWTINV                                                           
                                                                                
         XC    STATSV,STATSV                                                    
         XC    PLIST(12),PLIST                                                  
         GOTO1 SCANNER,PLIST,IFLDH,(2,STATSV)                                   
         ZIC   R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BZ    PWTX                                                             
         LA    R2,STATSV                                                        
         CLI   0(R2),3                                                          
         BH    PWTINV                                                           
         CLI   1(R2),3                                                          
         BH    PWTINV                                                           
         CLC   =C'***',12(R2)      ACCEPT C'***'                                
         BE    PWT20                                                            
                                                                                
         MVC   KEY+4(3),12(R2)          VALIDATE PRODUCT                        
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PWTX                                                             
         BH    PWT20                                                            
         MVC   FERN,=AL2(PRONOF)                                                
         B     PWTX                                                             
*                                                                               
PWT20    TM    3(R2),X'80'         2ND HALF MUST BE NUMERIC                     
         BNO   PWTINV                                                           
         OI    FIND,X'04'                                                       
                                                                                
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),12(R2)                                                   
         EDIT  (B4,8(R2)),(3,3(R7)),FILL=0,WRK=STATWRK                          
**       MVC   3(3,R7),22(R2)                                                   
PWTX     XIT1                                                                   
*                                                                               
PWTINV   MVC   FERN,=AL2(FLDINV)                                                
         B     PWTX                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE DUE DAYS AND SET FIND BIT                                     
*        X'04'=NN OR 00                                                         
*                                                                               
DAYDVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   DAYDVO                                                           
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FLDINV)         IF ZERO                                
         BNE   DAYDV2                                                           
         MVC   IFLD(2),=C'00'      ZERO IS OK HERE                              
         MVC   FERN,=AL2(FF)                                                    
         B     DAYDV4                                                           
*                                                                               
DAYDV2   CLC   FERN,=AL2(FF)                                                    
         BNE   DAYDINV                                                          
         MVC   IFLD(2),TEMP+2                                                   
DAYDV4   OI    FIND,X'04'          DUE DAYS = NN OR 00                          
         B     DAYDVO                                                           
*                                                                               
DAYDINV  MVC   FERN,=AL2(FLDINV)                                                
DAYDVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(2,R7),IFLD                                                     
*                                                                               
DAYDVX   XIT1                                                                   
         EJECT                                                                  
*        VALIDATE SHOW AND SET FIND BITS                                        
*        X'04'=NNNN                                                             
*        X'02'=ALL                                                              
*                                                                               
SHWVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLC   RNUM,=C'MY'         ACCEPT ANYTHING FOR MX                       
         BNE   SHWV5                                                            
         CLI   IFLDH+5,3                                                        
         BH    SHWVE                                                            
         CLI   IFLDH+5,2                                                        
         BL    SHWVE                                                            
         OI    FIND,X'04'                                                       
         B     SHWVO                                                            
*                                                                               
SHWV5    CLI   FIND,1                                                           
         BL    SHWVX               MISSING                                      
         BH    SHWVO               ALL                                          
         CLI   IFLDH+5,4                                                        
         BH    SHWVE                                                            
         CLC   RMARK(3),=C'ALL'    NETWORK IS IN MARKET                         
         BE    SHWVE                                                            
         MVC   KEYS(13),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D12'                                                  
         MVC   KEY+2(2),RAGY                                                    
         MVC   KEY+4(4),RMARK                                                   
         CLC   RNUM,=C'DL'         IF DL REPORT                                 
         BNE   *+10                                                             
         MVC   KEY+4(4),RSTA       NETWORK IS IN STATION                        
         MVC   KEY+8(4),IFLD                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    SHWVO               DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     SHWVO                                                            
*                                                                               
         OI    FIND,X'04'                                                       
         MVC   KEY(13),KEYS                                                     
         B     SHWVO                                                            
*                                                                               
SHWVE    MVC   FERN,=AL2(FLDINV)                                                
SHWVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
*                                                                               
SHWVX    XIT1                                                                   
         EJECT                                                                  
*              VALIDATE AMOUNT AND SET FIND BITS                                
*                                                                               
*        X'04'=NNNNNNNN.NN                                                      
*        X'08'= FEE=NNNNNNNN.NN                                                 
*        X'10'= PCT=NNN.NN                                                      
*        X'20'= R=YYMMDD,I=NNNN                                                 
*        X'40'= ONLY PREV  OR  NO PREV                                          
*              OR TIME OR INTEGRATION OR PREV OR OLD OR PKG=NNN                 
*                                                                               
AMTVAL   NTR1                                                                   
         MVC   TEMP(12),=12C' '                                                 
         GOTO1 AINITV                                                           
         CLI   FIND,X'01'                                                       
         BNE   AMTV0               AMT ALL OR MISSING                           
* - D1,DU ALLOWS I=MMMDD/YY                                                     
         CLC   =C'I=',IFLD         FOR B1,D1 REPORT                             
         BNE   AMTV1                                                            
         CLC   RNUM,=C'D1'                                                      
         BE    *+14                                                             
         CLC   RNUM,=C'DU'                                                      
         BNE   AMTV1                                                            
         MVC   TEMP(2),IFLD                                                     
         GOTO1 DATVAL,DMCB,(0,IFLD+2),DUB                                       
         MVC   TEMP+2(6),DUB                                                    
         OI    FIND,X'04'                                                       
         B     AMTV0                                                            
*                                                                               
AMTV1    CLC   IFLD(4),=C'FEE='                                                 
         BE    AMTV10                                                           
         CLC   IFLD(4),=C'PCT='                                                 
         BE    AMTV20                                                           
         CLC   IFLD(2),=C'R='                                                   
         BE    AMTV30                                                           
         CLC   IFLD(12),=CL12'ONLY PREV'                                        
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'NO PREV'                                          
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'TIME'                                             
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'INTEGRATION'                                      
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'PREV'                                             
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'OLD'                                              
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'NON-RETAIL'                                       
         BE    AMTV3                                                            
         CLC   IFLD(4),=C'PKG='                                                 
         BNE   AMTV4                                                            
         CLI   IFLDH+5,7                                                        
         BNE   AMTVE                                                            
         LA    R7,IFLD+4                                                        
         LA    R4,3                                                             
AMTV2    CLI   0(R7),C'0'                                                       
         BL    AMTVE                                                            
         CLI   0(R7),C'9'                                                       
         BH    AMTVE                                                            
         LA    R7,1(R7)                                                         
         BCT   R4,AMTV2                                                         
         B     AMTV3                                                            
*                                                                               
*                                                                               
AMTV3    MVC   TEMP(12),IFLD                                                    
         OI    FIND,X'40'                                                       
         B     AMTV0                                                            
*                                                                               
AMTV4    DS    0H                                                               
         CLC   RNUM,=C'07'         SPOT UNBILLING ?                             
         BE    AMTV40              YES, SLIGHTLY DIFFERENT FORMAT               
         CLC   RNUM,=C'7U'         SAME TRUE FOR NET UNBILLING                  
         BE    AMTV40                                                           
         MVC   TEMP(2),=C'G='      SET TO GROSS =                               
         MVI   TEMP+14,X'04'       VALIDATION BIT                               
         LA    R4,IFLD                                                          
AMTV5    GOTO1 CASHVAL,PLIST,0(R4),(R5)                                         
         CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         L     R7,PLIST+4                                                       
         CVD   R7,DUB                                                           
         UNPK  TEMP+2(10),DUB                                                   
         OI    TEMP+11,X'F0'                                                    
         TM    PLIST+4,X'80'       TEST NEGATIVE                                
         BZ    AMTV8                                                            
         MVI   TEMP+2,C'-'                                                      
         CP    DUB,=P'-999999999'                                               
         BL    AMTVE               CAN'T HANDLE MINUS 10 MILLION                
AMTV8    OC    FIND,TEMP+14                                                     
         B     AMTV0                                                            
*                                                                               
AMTV10   SH    R5,=H'4'                                                         
         MVC   TEMP(2),=C'GF'                                                   
         MVI   TEMP+14,X'08'                                                    
         LA    R4,IFLD+4                                                        
         B     AMTV5                                                            
*                                                                               
AMTV20   SH    R5,=H'4'                                                         
         GOTO1 CASHVAL,PLIST,IFLD+4,(R5)                                        
         CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         L     R7,PLIST+4                                                       
         CVD   R7,DUB                                                           
         UNPK  TEMP+2(5),DUB                                                    
         OI    TEMP+6,X'F0'                                                     
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   AMTVE                                                            
         CP    DUB,=P'10000'                                                    
         BH    AMTVE                                                            
         MVC   TEMP(2),=C'P='                                                   
         MVC   TEMP+7(4),=4C' '                                                 
         OI    FIND,X'10'                                                       
         B     AMTV0                                                            
*                                                                               
AMTV30   DS    0H                                                               
         MVI   ROUTSUB,1           EXTRACT DATE                                 
         GOTO1 AINITV                                                           
         GOTO1 DATVAL,PLIST,(0,IFLD+2),TEMP+12                                  
         OC    PLIST(4),PLIST                                                   
         BE    AMTVDE              DATE ERROR                                   
         MVC   TEMP(2),=C'R='                                                   
         MVC   TEMP+2(6),TEMP+12                                                
         MVI   ROUTSUB,2           GET INV NUMBER                               
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BNE   AMTVE                                                            
         CLC   IFLD(2),=C'I='                                                   
         BNE   AMTVE                                                            
         CLI   IFLDH+5,6                                                        
         BNE   AMTVE                                                            
         XC    DMCB,DMCB                                                        
         GOTO1 =V(SPFMTINO),DMCB,,(C'P',IFLD+2),RR=RELO                         
         L     RF,DMCB+4                                                        
         OC    0(2,RF),0(RF)       IF NULLS -> INVALID                          
         BZ    AMTVE                                                            
         MVC   TEMP+8(4),IFLD+2                                                 
         OI    FIND,X'20'                                                       
         B     AMTV0                                                            
*                                                                               
AMTV40   DS    0H                  FOR SPOT/NET UNBILLIG USE PL8'S              
         LA    R4,IFLD                                                          
         GOTO1 CASHVAL,PLIST,(X'80',0(R4)),(R5)                                 
         CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         MVC   DUB,PLIST+4                                                      
         UNPK  TEMP(12),DUB                                                     
         OI    TEMP+11,X'F0'                                                    
         CP    PLIST+4(8),=P'0'      TEST NEGATIVE                              
         BNL   AMTV45                                                           
         MVI   TEMP,C'-'                                                        
         CP    DUB,=P'-99999999999'                                             
         BL    AMTVE               CAN'T HANDLE MINUS 1 BILLION                 
AMTV45   OI    FIND,X'04'                                                       
         B     AMTV0                                                            
*                                                                               
AMTVE    MVC   FERN,=AL2(FLDINV)                                                
         B     AMTV0                                                            
AMTVDE   MVC   FERN,=AL2(SEDINV)                                                
*                                                                               
AMTV0    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(12,R7),TEMP                                                    
AMTVX    XIT1                                                                   
         EJECT                                                                  
*        VALIDATE WEIGHT OVERRIDES AND SET FIND BIT                             
*        X'04'=XXXXXXX                                                          
*        WHERE X=0-9 OR A                                                       
*                                                                               
WOVAL    NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   WOVX                                                             
         CLI   IFLDH+5,7                                                        
         BL    WOVERR                                                           
         LA    R6,IFLD                                                          
         ZIC   R7,IFLDH+5                                                       
***8     LA    R7,7                FOR BCT                                      
WOV5     CLI   0(R6),C'A'                                                       
         BE    WOV10                                                            
         CLI   0(R6),C'0'                                                       
         BL    WOVERR                                                           
         CLI   0(R6),C'9'                                                       
         BH    WOVERR                                                           
WOV10    LA    R6,1(R6)                                                         
         BCT   R7,WOV5                                                          
         OI    FIND,X'04'          VALID INPUT                                  
         B     WOV0                                                             
*                                                                               
WOVERR   MVC   FERN,=AL2(FLDINV)                                                
WOV0     LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(8,R7),IFLD                                                     
WOVX     XIT1                                                                   
         EJECT                                                                  
*        VALIDATE SPOT LENGHT AND SET FIND BIT                                  
*        X'04'=NNN                                                              
*                                                                               
SLVAL    NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   SLVO                                                             
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FLDINV)         IF ZERO                                
         BE    SLINV                                                            
*                                                                               
SLV2     CLC   FERN,=AL2(FF)                                                    
         BNE   SLINV                                                            
         MVC   IFLD(3),TEMP+1                                                   
SLV4     OI    FIND,X'04'          SPOT LENGHT NNN                              
         B     SLVO                                                             
*                                                                               
SLINV    MVC   FERN,=AL2(FLDINV)                                                
SLVO     LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
*                                                                               
SLVX     XIT1                                                                   
         EJECT                                                                  
*        VALIDATE SORT FIELD  AND SET FIND BIT                                  
*        X'04'=NNNNNN     WHERE N=1-6                                           
*                                                                               
STFVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLC   =C'CR',RNUM                                                      
         BE    STFV20                                                           
         CLI   FIND,1                                                           
         BNE   STFVO                                                            
         CLI   IFLDH+5,6                                                        
         BNE   STFINV                                                           
         LA    R6,IFLD                                                          
         MVC   TEMP(6),=C'123456'                                               
         LA    R7,6                FOR BCT                                      
STFV3    LA    R5,TEMP                                                          
         LA    R4,6                                                             
STFV4    CLC   0(1,R6),0(R5)                                                    
         BE    STFV8               GO ZERO ENTRY IN TEMP                        
         LA    R5,1(R5)                                                         
         BCT   R4,STFV4                                                         
         B     STFINV              NOT IN TABLE OR DUPLICATE                    
*                                                                               
STFV8    MVI   0(R5),0             SO I'LL CATCH DUPS                           
         LA    R6,1(R6)                                                         
         BCT   R7,STFV3                                                         
         OI    FIND,X'04'          VALID INPUT                                  
         B     STFVO                                                            
*                                                                               
STFV20   DS    0H                                                               
         CLI   IFLDH+5,1                                                        
         BNE   STFINV                                                           
         CLI   IFLD,C'P'                                                        
         BE    STFV30                                                           
         CLI   IFLD,C'C'                                                        
         BE    STFV30                                                           
         CLI   IFLD,C'E'                                                        
         BE    STFV30                                                           
         B     STFINV                                                           
*                                                                               
STFV30   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
         OI    FIND,X'04'          VALID INPUT                                  
         B     STFVX                                                            
*                                                                               
*                                                                               
STFINV   MVC   FERN,=AL2(FLDINV)                                                
STFVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),IFLD                                                     
*                                                                               
STFVX    XIT1                                                                   
         EJECT                                                                  
* VALIDATE NEW ESTIMATE FOR KB REPORT                                           
*                                                                               
NEWEST   NTR1                                                                   
         XC    STATSV,STATSV                 CLEAR SAVE AREA                    
*                                                                               
* GET NUMBER OF DAYS IN REQUEST START-END                                       
         GOTO1 PERVERT,PLIST,RSTRD,RENDD     REQ START-END                      
         MVC   STATSV+14(2),8(R1)            SAVE NUMBER OF REQ DAYS            
*                                                                               
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BE    NST10                                                            
         CLI   RNUM+134,X'40'      IF NEW PRODUCT                               
         BNH   NST10                                                            
         MVI   FIND,1              ELSE SPREQ00 IGNORES ERROR MSG               
         B     NSTINV              MUST BE NEW ESTIMATE                         
*                                                                               
NST10    GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    NSTINV                        NON NUMERIC ESTIMATE               
         MVC   IFLD(3),TEMP+1                                                   
*                                                                               
         MVC   KEY+7(1),TEMP                                                    
         MVC   KEY+4(3),RPRO                                                    
*                                                                               
         CLI   RNUM+134,X'40'      IF NEW PRODUCT                               
         BNH   *+10                                                             
         MVC   KEY+4(3),RNUM+134   USE IT                                       
*                                                                               
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    NSTVO            DISK ERROR                                      
         BH    *+8                                                              
         B     NSTINV                                                           
         OI    FIND,X'02'                                                       
                                                                                
* USE STATSV WORK AREA TO SAVE NEW ESTIMATE INFORMATION                         
* STATSV (CL12)   NEW ESTIMATE START-END DATES                                  
* STATSV+12 (CL2) NUMBER OF NEW ESTIMATE DAYS                                   
* STATSV+14 (CL2) NUMBER OF REQUEST START-END DAYS                              
                                                                                
         MVC   STATSV(12),SPTREC+52       NEW ESTIMATE START-END DATES          
         GOTO1 PERVERT,PLIST,STATSV,STATSV+6                                    
         MVC   STATSV+12(2),8(R1)           NUMBER OF NEW EST DAYS              
         CLC   STATSV(2),STATSV+14     NEW EST DAYS < REQ STR-END DAYS?         
         BL    NSTDTINV               YES/ERROR                                 
         B     NSTVO                                                            
*                                                                               
NSTDTINV MVC   FERN,=AL2(420)       EST NOT = REQ DATES                         
         B     NSTVO                                                            
*                                                                               
NSTINV   MVC   FERN,=AL2(ESTINV)                                                
*                                                                               
NSTVO    DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),IFLD                                                     
*                                                                               
NSTVX    XIT1                                                                   
         EJECT                                                                  
* VALIDATE NEW PRODUCT FOR KB  AND MV REPORT                                    
*                                                                               
NEWPRD   NTR1                                                                   
*                                                                               
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   NPDVO                                                            
         GOTO1 ARJN                                                             
         MVC   KEY+4(3),IFLD                                                    
         XC    KEY+7(6),KEY+7                                                   
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    NPDINV           DISK ERROR                                      
         BH    *+8                                                              
         B     NPDINV                                                           
         OI    FIND,X'02'                                                       
         B     NPDVO                                                            
*                                                                               
NPDINV   MVC   FERN,=AL2(15)       INVALID PRODUCT                              
         B     NSTVO                                                            
*                                                                               
NPDVO    DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         CLC   RNUM,=C'MV'                                                      
         BE    NPDX                                                             
         MVC   0(6,R7),IFLD                                                     
*                                                                               
NPDX     XIT1                                                                   
         EJECT                                                                  
* VALIDATE NEW PERIOD START DATE FOR KB REPORT                                  
* STATSV HAS DATA FROM NEW ESTIMATE VALIDATION FOR KB REPORT                    
* CHECK RULE: IF START DATE OF REQUEST NOT = START OF NEW ESTIMATE              
*             THEN MUST ENTER PERIOD START                                      
NEWPER   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BE    PERV00             PERIOD ENTERED                                
                                                                                
*                                 NO PERIOD ENTERED                             
         CLI   STATSV,0           MUST HAVE NEW ESTIMATE                        
         BE    NEWPERR            NO NEW ESTIMATE/ERROR                         
* IF NEW ESTIMATE BUT NO NEW PERIOD                                             
* THEN START DATE OF NEW EST MUST = REQUEST START DATE                          
         CLC   STATSV(6),RSTRD     NEW EST START DATE = REQ START DATE?         
         BE    PERVO               YES/OK                                       
         MVI   FIND,1              NO/ERROR/MUST ENTER PERIOD START             
         MVC   FERN,=AL2(440)                                                   
         B     PERVO                                                            
                                                                                
NEWPERR  MVI   FIND,1                                                           
         MVC   FERN,=AL2(419)                                                   
         B     PERVO                                                            
                                                                                
PERV00   GOTO1 DATVAL,PLIST,(0,IFLD),IFLD                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    PERINV                                                           
         OI    FIND,X'02'                                                       
*                                                                               
* STATSV HAS SAVED DATA FROM NEW ESTIMATE VALIDATION                            
* STATSV (CL12)   NEW ESTIMATE START-END DATES                                  
* STATSV+12 (CL2) NUMBER OF NEW ESTIMATE DAYS                                   
* STATSV+14 (CL2) NUMBER OF REQUEST START-END DAYS                              
* IFLD(6)         NEW PERIOD START DATE                                         
* STATSV+16 (CL6) NEW PERIOD END DATE   (IFLD +STATSV+14)                       
                                                                                
         XC    PLIST+8(4),PLIST+8                                               
         MVC   PLIST+10(2),STATSV+14              NUMBER OF REQ DAYS            
         L     R1,PLIST+8                                                       
         BCTR  R1,0                 -1 DAY FOR ADDITION                         
         ST    R1,PLIST+8                                                       
         GOTO1 ADDAY,PLIST,(C'D',IFLD),STATSV+16                                
*                                                                               
         CLI   STATSV,0            ARE WE DEALING WITH A NEW ESTIMATE?          
         BE    PERV10              NO                                           
         CLC   IFLD(6),STATSV      YES - PER START < EST START?                 
         BL    PERINV                                                           
         CLC   STATSV+16(6),STATSV+6       PER END > EST END?                   
         BH    PERINV                                                           
         B     PERVO                                                            
                                                                                
* NEW START DATE + NUMBER OF DAYS MUST FALL WITHIN OLD ESTIMATE                 
* ESTDATES HAS OLD ESTIMATE START-END DATES                                     
PERV10   CLC   IFLD(6),ESTDATES         PER START < EST START ?                 
         BL    PERINV                                                           
         CLC   STATSV+16(6),ESTDATES+6  PER END > EST END ?                     
         BH    PERINV                                                           
         B     PERVO                                                            
*                                                                               
PERINV   MVC   FERN,=AL2(FLDINV)                                                
*                                                                               
PERVO    DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),IFLD                                                     
*                                                                               
PERVX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CBLVAL   NTR1  BASE=*,LABEL=*  VALIDATE STATION BY READING CBL DEF REC          
         CLI   IFLDH+5,7                                                        
         BH    CBLVERR                                                          
         XC    TEMP,TEMP                                                        
         MVC   TEMP(L'KEY+L'KEYS),KEY  SAVE KEY & KEYS JUST IN CASE             
         XC    KEY,KEY                                                          
         XC    KEYD,KEYD                                                        
K@       USING NDEFRECD,KEY                                                     
         MVC   K@.NDEFKTYP,=X'0D11'                                             
         MVC   K@.NDEFKAGY,RAGY                                                 
         MVC   K@.NDEFKNET,IFLD                                                 
         MVC   KEYD,KEY            USE KEYD AS KEYSAVE                          
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                 
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY,KEYD                                                         
         BNE   CBLVERR                                                          
* TRY FOR CLIENT EXCEPTION REC                                                  
         TM    CLISAVE,X'04'       SPECIFIC CLIENT REQUEST ?                    
         BZ    CBLV2               NO, DON'T LOOK FOR CLT EXCEPTION             
         MVC   TEMP+50(20),KEY     SAVE DEFAULT KEY                             
*     KEEP IN MIND THAT SINCE KEY IS ONLY CL13, I'LL BE TRASHING                
*     THE 'KEYS' FIELD RIGHT AFTER IT, SINCE THE D/A WILL END UP                
*     THERE, BASICALLY I'M TREATING KEY AND KEYS AS ONE FIELD HERE              
         GOTO1 CLPACK,DMCB,RCLI,K@.NDEFKCLT                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEYD,KEY            USE KEYD AS KEYSAVE                          
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                 
         CLI   DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY,KEYD                                                         
         BE    CBLV2               FOUND CLIENT EXCEPTION                       
         MVC   KEY(20),TEMP+50     IF NOT, RESTORE KEY W/ D/A                   
         CLC   RNUM,=C'CM'         CLIENT EXCEPTION MUST EXIST FOR CM           
         BNE   *+14                                                             
         MVC   FERN,=AL2(1253)     CLIENT SPECIFIC CBLMKT MUST EXIST            
         B     CBLV100                                                          
*                                                                               
CBLV2    L     R8,AIORFP           USE THIS IO FOR NETDEF REC                   
         USING NDEFRECD,R8                                                      
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=C'SPTFIL  ',K@.NDEFKDA,(R8), X        
               SPTWORK                                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K@                                                               
         SPACE 1                                                                
* RETRIEVE NTWKID ELEM                                                          
         SPACE 1                                                                
         LA    R6,NDEFEL                                                        
         SR    R0,R0                                                            
CBLV10   CLI   0(R6),X'02'                                                      
         BE    CBLV12                                                           
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   CBLV10                                                           
         DC    H'0'                                                             
*                                                                               
CBLV12   DS    0H                                                               
         CLI   2(R6),1             IS IT A CBLDEF                               
         BNE   CBLVERR             NO : ERROR                                   
*                                                                               
         LA    R6,NDEFEL                                                        
*                                                                               
CBLV20   CLI   0(R6),1                                                          
         BNE   CBLV22                                                           
         USING NDEFEL01,R6                                                      
*                                                                               
         CLC   IFLD+5(2),NDEFMSUF   MATCH MARKET                                
         BE    CBLV24                                                           
*                                                                               
CBLV22   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   CBLV20                                                           
         B     CBLVERR             MKT NOT FOUND, ERROR                         
*                                                                               
CBLV24   SR    R0,R0                                                            
         ICM   R0,3,NDEFMNUM       GET MARKET NUMBER                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RMARK,DUB                                                        
         MVC   RO2(2),IFLD+5       PUT MKT SUFFIX IN QOPT2 AND 3                
*                                                                               
         OI    FIND,X'10'          STA = XXXX                                   
         OI    MKTSAVE,X'08'       SET SPECIFIC MARKET INPUT                    
*                                  READ MKT REC TO CHK FOR LIMIT ACCES          
         CLI   6(R3),C'+'                                                       
         BNE   CBLV30              NO NEED TO CHK                               
         MVC   HALF(1),7(R3)       SAVE MKT ACC                                 
         B     CBLV35                                                           
CBLV30   CLI   8(R3),C'+'                                                       
         BNE   CBLV50              NO NEED TO CHK                               
         MVC   HALF(1),9(R3)       SAVE MKT ACC                                 
                                                                                
CBLV35   MVI   KEYS,C'0'                                                        
         MVC   KEYS+1(L'KEYS-1),KEYS                                            
         MVI   KEYS,C'M'                                                        
         MVC   KEYS+1(1),REQMED                                                 
         MVC   KEYS+2(4),RMARK                                                  
         MVC   KEYS+6(2),AGY                                                    
         GOTO1 ARSTA                                                            
         CLC   FERN,=AL2(FE)                                                    
         BH    *+14                                                             
         MVC   FERN,=AL2(MKTNOF)                                                
         B     CBLV100                                                          
*                                                                               
         LA    R7,SPTREC                                                        
         USING MKTRECD,R7                                                       
         LA    R4,MKTLTACC                                                      
         LA    R5,3                                                             
CBLV40   CLC   0(1,R4),HALF                                                     
         BE    CBLV50                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,CBLV40                                                        
CBLACER  MVC   FERN,=AL2(ACCERR)                                                
         B     CBLV100                                                          
*                                                                               
         DROP  R7                                                               
*                                                                               
CBLV50   MVC   SAVEREP(3),SPTREC+63          SAVE REP                           
         B     CBLV100                                                          
CBLVERR  MVC   FERN,=AL2(STAINV)                   INVALID STATION              
CBLV100  LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
         MVC   STASAVE,FIND                                                     
CBLVX    MVC   KEY(L'KEY+L'KEYS),TEMP    RESTORE KEY                            
         XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
FLDINV   EQU   2                                                                
DDSFMT   EQU   X'04'                                                            
FMTNAV   EQU   05                            FORMAT NOT AVAILABLE               
CLINOF   EQU   40                            CLI NOT ON FILE                    
CLIINV   EQU   14                            CLI INVALID                        
PRONOF   EQU   41                            PRO NOT ON FILE                    
PROINV   EQU   15                            PRO INVALID                        
PRMNOF   EQU   27                            PRO NOT ON FILE                    
PRMINV   EQU   26                            PRO MODE INVALID                   
ESTNOF   EQU   42                            EST NOT ON FILE                    
ESTINV   EQU   16                            EST INVALID                        
EST1G2   EQU   16                            1ST EST GT 2ND EST                 
MKTNOF   EQU   44                            MKT NOT ON FILE                    
MKTINV   EQU   17                            MKT INVALID                        
STANOF   EQU   43                            STA NOT ON FILE                    
STANIM   EQU   78                            STA NOT IN MKT                     
STAINV   EQU   18                            STA INVALID                        
SEDSGE   EQU   80                            START DATE GT END DATE             
SEDNIE   EQU   79                            DATES NOT WITHIN EST               
SEDINV   EQU   20                            DATE INVALID                       
DEMINV   EQU   111                                                              
ACCERR   EQU   207                 LIMITED ACCESS ERROR                         
MGNPG    EQU   97                  MGROUP NEEDS PGROUP                          
PERREQ   EQU   419                 PERIOD REQUIRED                              
NOPRDPG  EQU   1255                NO PRODUCTS IN THIS PGROUP                   
I5NONWRK EQU   1323                CAN'T HAVE INDIVIDUAL NETWORKS IN I5         
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM.                           
*                                                                               
ROUTADRT DC    A(0)                          00 - N/D                           
         DC    A(0)                          01 - N/D                           
         DC    A(CLIVAL)                     02 - CLIENT                        
         DC    A(PROVAL)                     03 - PRODUCT                       
         DC    A(PRMVAL)                     04 - PRODUCT,MODE                  
         DC    A(ESTVAL)                     05 - ESTIMATE                      
         DC    A(MKTVAL)                     06 - MARKET                        
         DC    A(STATVAL)                    07 - STATION                       
         DC    A(SEDVAL)                     08 - START,END DATES               
         DC    A(TARVAL)                     09 - TARGET AUDIENCE               
         DC    A(PGRPVAL)               10- PRD OR PRD GRP                      
         DC    A(MGRPVAL)               11- MKT OR MKT GRP                      
         DC    A(DAYDVAL)          12 - DUE DAYS                                
         DC    A(AMTVAL)           13 - AMOUNT (NEW BILLING)                    
         DC    A(SHWVAL)           14 - CANADIAN SHOW                           
         DC    A(WOVAL)            15 - WEIGHT OVERRIDE                         
         DC    A(SLVAL)            16 - SPOT LENGHT                             
         DC    A(STFVAL)           17 - SORT FIELD                              
         DC    A(ANYVAL)           18 - VARIOUS AND SUNDRY ITEMS                
         DC    A(ALPNVAL)          19 - VARIOUS AND SUNDRY ITEMS                
         DC    A(PRDLEN)           20 - PROD-LEN VALIDATION                     
         DC    A(PRDWGT)           21 - PROD-WEIGHT VALIDATION                  
         DC    A(NEWEST)           22 - NEW EST VALIDATION (KB REPORT)          
         DC    A(NEWPER)           23 - NEW PERIOD STRTDAT (KB REPORT)          
         DC    A(NEWPRD)           24 - NEW PRODUCT (KB REPORT)                 
         DC    A(0)                25 - N/D                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  R8                                                               
         EJECT                                                                  
VALGROUP CSECT                                                                  
         NMOD1 0,VALGRP                                                         
         USING REQTEMP,R9                                                       
         USING TWAD,R3                                                          
*                                                                               
         MVC   TEMP(13),KEY        SAVE KEY                                     
         MVC   HALF1(1),KEY+1      SAVE AGY/MED                                 
         LA    R4,KEY                                                           
         USING GRPRECD,R4          CLIENT STATION PASSIVE POINTER               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   GRPPTYP,GRPPTYPQ       RECORD TYPE                               
         MVI   GRPPSTYP,GRPPCTYQ      CLIENT GROUP                              
         MVC   GRPPAGMD(1),HALF1      AGENCY/MEDIA                              
         MVC   GRPPVAL(3),IFLD        CLIENT                                    
         OC    GRPPVAL,SPACES         BLANK PADDED                              
         MVC   GRPPID(1),7(R3)        GROUP ID                                  
*                                                                               
         MVC   KEYD(13),KEY         USE KEYD AS KEYSAVE                         
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR  ',KEY,KEY,0                 
         CLC   KEY(10),KEYD                                                     
         BNE   VALGRPX                                                          
*                                                                               
         MVC   FULL,SPACES                                                      
         MVC   FULL(2),8(R3)       GROUP CODE                                   
         OC    FULL,=C'0000'       REPLACE BLANKS WITH X'F0'                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRL   R0,4                GET RID OF SIGN NIBLE                        
         STCM  R0,3,HALF           LEFT-JUSTIFIED, PWOS                         
*                                                                               
         CLC   HALF,GRPPCODE       GROUP CODE MATCH?                            
*                                  CC= OK   CC NOT= ERROR                       
         MVC   KEY(13),TEMP        RESTORE KEY                                  
VALGRPX  XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                        MKT OR MKT-GRP VALIDATION                              
*              X'02'=ALL                                                        
*              X'04'=ALLN                                                       
*              X'08'=NNNN                                                       
*              X'20'=MGR=XALL                                                   
*              X'80'=MGR=XNNNN                                                  
*                                                                               
MGRPV    CSECT                                                                  
         NMOD1 0,MGRPV                                                          
         USING REQTEMP,R9                                                       
         USING TWAD,R3                                                          
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    MGRPVX              MISSING                                      
         BE    MGRPV2                                                           
         CLI   6(R3),C'+'          CK FOR MKT LIMIT ACCESS                      
         BE    MGACCERR                                                         
         CLI   8(R3),C'+'          CK FOR MKT LIMIT ACCESS FOR WESTERN          
         BE    MGACCERR                                                         
         B     MGRPVO                                                           
*                                                                               
MGRPV2   DS    0H                                                               
         CLC   IFLD(4),=C'MGR='                                                 
         BE    MGRPV2A                                                          
         DC    H'0'           **   SAME AS CODE IN MKTVAL  **                   
*                                  SO GO TO PORTION OF MKTVAL                   
*                                  (SHOULD BE CAUGHT IN MGRPVAL)                
         SPACE                                                                  
MGRPV2A  CLI   IFLDH+5,6                                                        
         BL    MGRPVE                                                           
*                                                                               
         CLC   RNUM,=C'M6'             M6,M2 ALLOW MGR=0(-7)ALL                 
         BE    MGR00                      AND SKIP ANY TESTS                    
         CLC   RNUM,=C'M2'                                                      
         BNE   V2B                                                              
*                                                                               
MGR00    DS    0H                                                               
         CLC   IFLD+5(3),=C'ALL'                                                
         BNE   V2B                                                              
         CLI   IFLD+4,C'0'                                                      
         BL    V2B                                                              
         CLI   IFLD+4,C'7'                                                      
         BH    V2B                                                              
         OI    FIND,X'20'                                                       
         MVC   RDIST,IFLD+4                                                     
         MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
         B     MGRPVX                                                           
*                                                                               
V2B      DS    0H                                                               
         MVC   HALF2,SPACES                                                     
         MVC   HALF2(1),IFLD+4                                                  
         MVC   TEMP+30(4),IFLD+5                                                
         CLI   IFLD+5,C'0'         IS IT 2 CHAR MGRP CODE?                      
         BL    *+12                                                             
         CLI   IFLD+5,C'9'                                                      
         BNH   V2BB                                                             
         CLC   =C'ALL',IFLD+5                                                   
         BE    V2BB                                                             
         MVC   HALF2+1(1),IFLD+5                                                
         MVC   TEMP+30(4),IFLD+6                                                
V2BB     CLI   TEMP+30,C'*'        (TO AVOID MGRP=A**** ERROR)                  
         BE    MGRPVE                                                           
         LA    RF,SPMGRTAB                                                      
         LHI   RE,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   HALF2,0(RF)                                                      
         BE    V2BC                                                             
         AHI   RF,L'SPMGRTAB                                                    
         BCT   RE,*-14                                                          
         B     MGRPVE                                                           
*                                                                               
V2BC     XC    HALF2,HALF2                                                      
         MVC   HALF2(1),2(RF)      PUT IN TRANSLATION VALUE                     
         CLI   1(RF),C' '          IS IT 2 CHAR MKTGRP                          
         BNH   *+8                                                              
         MVI   HALF2+1,2           TURN ON FLAG TO INDIC 2 CHARS                
*                                                                               
*2B      CLI   IFLD+4,C'A'                                                      
*        BL    MGRPVE                                                           
*        CLI   IFLD+4,C'N'         ACCEPT ANY ALPHA                             
*        BE    MGRPVE              EXCEPT N                                     
*        CLI   IFLD+4,C'Y'                                                      
*        BE    MGRPVE              OR Y                                         
*        CLI   IFLD+4,C'Z'                                                      
*        BH    MGRPVE                                                           
*        CLI   IFLD+5,C'*'        (TO AVOID MGRP=A**** ERROR)                   
*        BE    MGRPVE                                                           
*                             TRY TO READ SCHEME RECORD                         
         MVC   KEYS(13),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),KEYS+1         AGY/MED/CLT                              
         CLI   HALF2,C' '                                                       
         BL    *+12                                                             
         CLI   HALF2,C'G'              A-F REQUIRE CLIENT                       
         BL    *+10                                                             
*                                                                               
         XC    KEY+3(2),KEY+3           CLEAR CLT FOR MGRPS  G-Z                
*                                                                               
         MVC   KEY+8(1),HALF2                                                   
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    MGRPVO              DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     MGRPVO                                                           
*                                                                               
         MVC   RDIST,HALF2                                                      
         LA    R7,SPTREC+24                                                     
         CLI   0(R7),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKGEL01,R7          SEE IF SCHEME SAYS                           
         CLI   MKGPGA,C'Y'         IF PGROUP REQUIRED                           
         BNE   MGRPV3                                                           
         DROP  R7                                                               
         CLI   RDIV,C' '           SEE IF PGROUP SPECIFIED                      
         BNE   MGRPV3                                                           
         MVC   FERN,=AL2(MGNPG)      ERROR 97 MKTGROUP NEEDS PRDGROUP           
         B     MGRPVO                                                           
MGRPV3   CLC   TEMP+30(3),=C'ALL'                                               
         BNE   MGRPV4                                                           
         CLI   6(R3),C'+'          CK FOR MKT LIMIT ACCESS                      
         BE    MGACCERR                                                         
         CLI   8(R3),C'+'          CK FOR MKT LIMIT ACCESS FOR WESTERN          
         BE    MGACCERR                                                         
         OI    FIND,X'20'                                                       
         MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
         B     MGRPVZ                   DONE                                    
MGRPV4   SR    R4,R4                                                            
         IC    R4,14(R7)           R7 POINTS TO 01 ELEM                         
         SR    R5,R5                                                            
         IC    R5,27(R7)                                                        
         AR    R4,R5                                                            
         IC    R5,40(R7)                                                        
         AR    R4,R5                                                            
         ST    R4,FULL             SAVE TOTAL LENGTH                            
         IC    R5,IFLDH+5          LEN OF WHOLE ENTRY                           
         SH    R5,=H'5'            MINUS LEN OF OVERHEAD                        
         CLI   HALF2+1,0           2 CHAR MGRP?                                 
         BNH   *+6                                                              
         BCTR  R5,0                YES, SUBTRACT 1                              
         C     R5,FULL                                                          
         BNE   MGRPVE                                                           
*                                                                               
MGRPV7   XC    TEMP(4),TEMP                                                     
         MVI   FULL,0                                                           
         LA    R4,TEMP                                                          
         LA    R7,TEMP+30                                                       
MGRPV7A  CLI   0(R7),C'*'                                                       
         BE    MGRPV7X                                                          
         CLI   0(R7),C'0'                                                       
         BL    MGRPVE                                                           
         CLI   FULL,1              * WAS INPUT BEFORE A NUMBER                  
         BE    MGRPVE              ERROR                                        
         MVC   0(1,R4),0(R7)                                                    
MGRPV7C  LA    R4,1(R4)                                                         
         LA    R7,1(R7)                                                         
         BCT   R5,MGRPV7A                                                       
         B     MGRPV8                                                           
*                                                                               
MGRPV7X  OI    FIND,X'80'               XI***                                   
         OI    FULL,1                                                           
         B     MGRPV7C                                                          
*                                                                               
MGRPV8   CLI   FULL,1                                                           
         BNE   MGRPV9              * INPUT - NO FUTHER VALIDATION               
         MVC   RSTA(4),TEMP+30                                                  
         MVC   RMARK(3),=C'ALL'                                                 
         B     MGRPVZ                                                           
*                                                                               
MGRPV9   PACK  FULL+1(3),TEMP(5)                                                
         MVC   KEY+9(2),FULL+1                                                  
MGRPVA   GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    MGRPVO                                                           
         BH    MGRPVC                                                           
         OC    KEY+3(2),KEY+3                                                   
         BNZ   MGRPVB                                                           
         MVC   KEY+3(2),KEYS+2          TRY WITH CLT IN KEY                     
         OC    KEYS+2(2),KEYS+2        SEE IF CLT SPECIFIED                     
         BNZ   MGRPVA              YES                                          
         MVC   FERN,=AL2(53)             MGRP NOT FOUND                         
         B     MGRPVO                                                           
*                                                                               
MGRPVB   DS    0H                                                               
         MVC   FERN,=AL2(FF)                                                    
         TM    PROSAVE,X'F0'         WAS PGRP SPECIFIED                         
         BNZ   MGRPVC              YES - THEN MAY NOT FIND MGRP                 
         MVC   FERN,=AL2(53)             NOT FOUND                              
         B     MGRPVO                                                           
*                                                                               
MGRPVC   OI    FIND,X'80'                                                       
         CLI   6(R3),C'+'          CK FOR LIMIT MKT ACCESS                      
         BNE   *+14                                                             
         MVC   HALF(1),7(R3)       SAVE MKT LIMIT                               
         B     MGRPVC0                                                          
         CLI   8(R3),C'+'          CK FOR LIMIT MKT ACCESS                      
         BNE   MGRPVC4                                                          
         MVC   HALF(1),9(R3)       SAVE MKT LIMIT ACC                           
                                                                                
MGRPVC0  LA    R7,SPTREC+24                                                     
MGRPVC1  CLI   0(R7),X'20'         FIND MKTGRP LIMIT ACCESS ELEM                
         BE    MGRPVC2                                                          
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0             END OF REC                                   
         BE    MGACCERR                                                         
         B     MGRPVC1                                                          
*                                                                               
MGRPVC2  DS    0H                                                               
         LA    R4,2(R7)                                                         
         LA    R5,3                                                             
*MGRPVC3  CLC   0(1,R4),7(R3)                                                   
MGRPVC3  CLC   0(1,R4),HALF                                                     
         BE    MGRPVC4                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,MGRPVC3                                                       
         B     MGACCERR                                                         
*                                                                               
MGRPVC4  DS    0H                                                               
         MVC   RSTA(4),TEMP+30                                                  
         MVC   RMARK(3),=C'ALL'                                                 
         B     MGRPVZ                                                           
*                                                                               
MGRPVE   MVC   FERN,=AL2(MKTINV)                                                
         B     MGRPVO                                                           
MGACCERR MVC   FERN,=AL2(ACCERR)                                                
MGRPVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
         B     MGRPVX                                                           
MGRPVZ   MVC   KEY(13),KEYS        RESTORE KEY                                  
MGRPVX   FOUT  (R6),NAME                                                        
         MVC   MKTSAVE,FIND                                                     
         L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         CLC   =C'SOON',BVROUT                                                  
         BNE   MGRVPXX                                                          
         B     MGRVPXX                                                          
MGRVPXX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
ANYVL    CSECT                       VARIOUS AND SUNDRY X'04'=OK                
         NMOD1 0,ANYVL                                                          
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
*                                                                               
         CLC   RNUM,=C'MC'                                                      
         BE    ANYV10                                                           
         CLC   RNUM,=C'MD'                                                      
         BE    ANYV10                                                           
         CLC   RNUM,=C'48'                                                      
         BE    ANYV20                                                           
         CLC   RNUM,=C'DN'                                                      
         BE    ANYV30                                                           
         CLC   RNUM,=C'C2'                                                      
         BE    ANYV40                                                           
         CLC   RNUM,=C'MV'                                                      
         BE    ANYV40                                                           
         CLC   RNUM,=C'EB'                                                      
         BE    ANYV50                                                           
         CLC   RNUM,=C'EX'         XML - SAME AS EB                             
         BE    ANYV50                                                           
         CLC   RNUM,=C'LO'                                                      
         BE    ANYV50                                                           
         CLC   RNUM,=C'BT'                                                      
         BE    ANYV50                                                           
         CLC   RNUM,=C'JV'                                                      
         BE    ANYV60                                                           
         CLC   RNUM,=C'JW'                                                      
         BE    ANYV50                                                           
ANYX     XIT1                                                                   
*                                                                               
ANYV10   DS    0H                                                               
         MVC   RCARD2+20(8),IFLD   MC/MD                                        
         LA    R3,8                                                             
         LA    R4,RCARD2+20                                                     
ANYV12   CLI   0(R4),X'40'                                                      
         BE    ANYVOK                                                           
         CLI   0(R4),C'A'                                                       
         BL    ANYINV                                                           
         CLI   0(R4),C'9'                                                       
         BH    ANYINV                                                           
         LA    R4,1(R4)                                                         
         BCT   R3,ANYV12                                                        
         B     ANYVOK                                                           
*                                                                               
*                                                                               
ANYV20   CLI   RO1,C'S'                                                         
         BE    ANYV22                                                           
         CLI   RO1,X'40'                                                        
         BH    ANYINV                                                           
ANYV22   MVC   RCARD2+20(4),IFLD                                                
         B     ANYVOK                                                           
*                                                                               
ANYV30   GOTO1 ARJN               RIGHT JUSTIFIES/CHECK NUM 0-255               
         CLC   FERN,=AL2(FF)                                                    
         BL    ANYINV              NON-NUMERIC                                  
         MVC   RO1(3),TEMP+1                                                    
         TM    ESTSAVE,X'04'       ONLY IF EST = NNN                            
         BNO   ANYINV                                                           
         B     ANYVOK                                                           
*                                                                               
ANYV40   DS    0H                                                               
         GOTO1 AINITV                                                           
*                                                                               
         CLI   IFLD,0                                                           
         BE    ANYINV                                                           
         CLC   =C'ALL',IFLD                                                     
         BE    ANYINV                                                           
*                                                                               
         GOTO1 CLPACK,DMCB,IFLD,KEY+2                                           
         CLI   DMCB,0                                                           
         BNE   ANYINV                                                           
         MVC   KEYD,KEY           **SAVE KEY IN DEM KEY                         
         XC    KEY+4(9),KEY+4                                                   
         GOTO1 ARSPT                         READ CLIENT HEADER                 
         MVC   KEY(13),KEYD       **RESTORE KEY                                 
         CLC   FERN,=AL2(FE)                                                    
         BL    ANYINV                        DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(CLINOF)                   CLIENT NOT ON FILE           
         B     ANYX                                                             
         OI    FIND,X'04'                                                       
         LA    R7,SPTREC                                                        
         USING CLTHDRD,R7                                                       
         MVC   NAME(20),CNAME                                                   
         MVC   RCARD2+20(3),IFLD                                                
         FOUT  (R6),NAME                                                        
         DROP  R7                                                               
         XIT1                                                                   
*                                                                               
ANYV50   DS    0H                                                               
         MVC   TEMP(3),=C'000'                                                  
         XC    TEMP+3(3),TEMP+3                                                 
         MVZ   TEMP+3(3),IFLD+1     1ST MIGHT BE ALPHA                          
         CLC   TEMP(3),TEMP+3       NUMERIC?                                    
         BNE   ANYINV                                                           
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(4,R7),IFLD                                                     
         B     ANYVOK                                                           
*                                                                               
ANYV60   DS    0H                                                               
         CLI   IFLDH+5,3                                                        
         BH    ANYINV                                                           
         LHI   R0,3                                                             
         LA    R1,IFLD+2                                                        
         XC    TEMP(3),TEMP                                                     
         LA    RE,TEMP+2                                                        
ANYV63   CLI   0(R1),C' '                                                       
         BE    ANYV65                                                           
         CLI   0(R1),C'0'                                                       
         BL    ANYINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ANYINV                                                           
         MVC   0(1,RE),0(R1)                                                    
         BCTR  RE,0                                                             
ANYV65   BCTR  R1,0                                                             
         BCT   R0,ANYV63                                                        
         OC    TEMP(3),=3C'0'                                                   
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),TEMP                                                     
         B     ANYVOK                                                           
*                                                                               
ANYVOK   OI    FIND,X'04'                                                       
         B     ANYX                                                             
*                                                                               
ANYINV   MVC   FERN,=AL2(FLDINV)                                                
         B     ANYX                                                             
         LTORG                                                                  
         EJECT                                                                  
PRDLENV  CSECT                       X'04'                                      
         NMOD1 0,PRDLNV                                                         
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
*                                                                               
         CLI   IFLDH+5,3                                                        
         BNE   PLV00                                                            
         CLC   =C'POL',IFLD                                                     
         BNE   PLV00                                                            
         MVC   KEY+4(3),IFLD            VALIDATE PRODUCT                        
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PLVX                                                             
         BH    *+14                                                             
         MVC   FERN,=AL2(PRONOF)                                                
         B     PLVX                                                             
         OI    FIND,X'08'                                                       
         OI    PROSAVE,X'08'                                                    
         MVC   RPRO,IFLD                                                        
         B     PLVX                                                             
*                                                                               
PLV00    XC    STATSV,STATSV                                                    
         GOTO1 SCANNER,PLIST,IFLDH,(2,STATSV),C',=/-'                           
         ZIC   R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BZ    PLVX                                                             
         LA    R4,RNUM+11                                                       
         LA    R2,STATSV                                                        
         CLI   0(R2),3                                                          
         BH    PLVINV                                                           
         CLI   1(R2),3                                                          
         BH    PLVINV                                                           
PLV10    CLC   =C'POL',12(R2)                                                   
         BE    PLVINV                                                           
         CLC   =C'UNA',12(R2)                                                   
         BNE   PLV12                                                            
         MVC   0(3,R4),=C'POL'                                                  
         B     PLV15                                                            
PLV12    MVC   KEY+4(3),12(R2)          VALIDATE PRODUCT                        
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    PLVX                                                             
         BH    *+14                                                             
         MVC   FERN,=AL2(PRONOF)                                                
         B     PLVX                                                             
         MVC   0(3,R4),12(R2)                                                   
PLV15    TM    FIND,X'04'          IS THIS 2ND TIME THROUGH/THENDONE            
         BO    PLV30                                                            
         OI    FIND,X'04'                                                       
         CLI   1(R2),0             SECOND HALF OF DIVIDED FILED                 
         BE    PLV30                                                            
         LA    R4,RNUM+49                                                       
         LA    R2,10(R2)                                                        
         B     PLV10                                                            
PLV30    LA    R4,RNUM+52                                                       
         LA    R2,STATSV                                                        
         LA    R2,32(R2)                                                        
         CLI   0(R2),0                                                          
         BE    PLVX                                                             
         TM    2(R2),X'80'         MUST BE NUMERIC                              
         BNO   PLVINV                                                           
         ZIC   R1,7(R2)            CHK BINARY VALUE                             
         BAS   RE,CHKBIN                                                        
         BE    PLVINV                                                           
PLV40    MVC   0(3,R4),12(R2)                                                   
         CLI   1(R2),0                                                          
         BE    PLVX                                                             
         TM    3(R2),X'80'                                                      
         BNO   PLVINV                                                           
         ZIC   R1,11(R2)           CHK 2ND BINARY VALUE                         
         BAS   RE,CHKBIN                                                        
         BE    PLVINV                                                           
         LA    R4,RNUM+55                                                       
         MVC   0(3,R4),22(R2)                                                   
PLVX     XIT1                                                                   
*                                                                               
PLVINV   MVC   FERN,=AL2(FLDINV)                                                
         B     PLVX                                                             
*                                                                               
CHKBIN   NTR1                                                                   
         STC   R1,FULL                                                          
         L     R1,SLNTAB                                                        
         LH    RE,0(R1)              GET ENTRY LENGTH                           
         L     RF,2(R1)              DISPL TO EOT                               
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                  POINT TO FIRST ENTRY                       
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'R'                                                          
         CLI   RMED,C'R'                                                        
         BE    CB10                                                             
         CLI   RMED,C'X'                                                        
         BE    CB10                                                             
         LA    R0,C'T'                                                          
*                                                                               
CB10     CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    CB15                                                             
         CLC   RAGY,0(R1)          MATCH AGY                                    
         BNE   *+12                                                             
CB15     CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    CB20                                                             
*                                                                               
         BXLE  R1,RE,CB10          NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
CB20     AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,FULL             GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         B     PLVX                EXIT WITH CC SET                             
         LTORG                                                                  
         EJECT                                                                  
APNVL    CSECT                       ALPHA=X'04',NUM=X'08'                      
         NMOD1 0,ANPVL                                                          
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
*                                                                               
         CLI   IFLD,C'A'                                                        
         BL    ANPINV                                                           
         CLI   IFLD,C'Z'                                                        
         BNH   ANPOKA                                                           
         CLI   IFLD,C'0'                                                        
         BL    ANPINV                                                           
         CLI   IFLD,C'9'                                                        
         BH    ANPINV                                                           
         OI    FIND,X'08'          NUMERIC                                      
         B     ANPOK                                                            
*                                                                               
ANPOKA   OI    FIND,X'04'          ALPHA                                        
*                                                                               
ANPOK    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
ANPX     XIT1                                                                   
*                                                                               
ANPINV   MVC   FERN,=AL2(FLDINV)                                                
         B     ANPX                                                             
         LTORG                                                                  
         EJECT                                                                  
CLTGRP   CSECT                       X'40'=C'CGR='                              
         NMOD1 0,CLGRP                                                          
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
*                                                                               
***->    CLI   IFLDH+5,6            AT LEAST 6 CHARACTERS  7/24/98              
         CLI   IFLDH+5,5            AT LEAST 5 CHARACTERS                       
         BL    CLGINV                                                           
*                                   IF 2 CHAR CGRP, CONVERT TO ONE              
         MVC   HALF2,SPACES                                                     
         MVC   HALF2(1),IFLD+4                                                  
         MVC   TEMP+30(4),IFLD+5                                                
         CLI   IFLD+5,C'0'          IS IT 2 CHAR MGRP CODE?                     
         BNL   CLTGRB               NO, DON'T DO ANYTHING                       
         CLC   =C'ALL',IFLD+5                                                   
         BE    CLTGRB                                                           
         MVC   HALF2,IFLD+4         MOVE IN 2 CHAR CGRP                         
         MVC   TEMP+30(4),IFLD+6                                                
*                                                                               
CLTGRB   LA    RF,SPCGRTAB                                                      
         LHI   RE,(SPCGRTBX-SPCGRTAB)/L'SPCGRTAB                                
         CLC   HALF2,0(RF)                                                      
         BE    CLTGRD                                                           
         AHI   RF,L'SPCGRTAB                                                    
         BCT   RE,*-14                                                          
         B     CLGINV                                                           
                                                                                
CLTGRD   XC    HALF2,HALF2                                                      
         MVC   HALF2(1),2(RF)      PUT IN TRANSLATION VALUE                     
         CLI   1(RF),C' '          IS IT 2 CHAR MKTGRP                          
         BNH   *+8                                                              
         MVI   HALF2+1,2           TURN ON FLAG TO INDIC 2 CHARS                
*                                                                               
* VALIDATE CLIENT GROUP                                                         
         MVC   IFLDCNT(13),KEY        SAVE KEY                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D04'                                                  
         MVC   KEY+2(1),IFLDCNT+1  AGY/MED                                      
         MVC   KEY+3(1),HALF2      GROUP ID                                     
*                                                                               
         CLC   =C'BU',RNUM           IS IT NETWORK                              
         BE    CLTGRNET                                                         
         CLC   =C'DU',RNUM           IS IT NETWORK                              
         BE    CLTGRNET                                                         
         CLC   =C'I6',RNUM         IF I6                                        
         BNE   CLTGR01                                                          
         CLI   RMED,C'N'           AND NETWORK                                  
         BNE   CLTGR01                                                          
*                                                                               
CLTGRNET XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D06'       SET NETWORK KEY                            
         MVC   KEY+2(1),IFLDCNT+1    AGY/MED                                    
*****    MVC   KEY+2(1),KEYS+1                                                  
         MVC   KEY+3(1),HALF2                                                   
CLTGR01  MVC   KEYD(4),KEY                                                      
*                                                                               
GTCLTRC  GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    CLGX                                                             
         BH    *+14                                                             
CLTGR03  MVC   FERN,=AL2(53)                                                    
         B     CLGX                                                             
         CLC   =C'ALL',TEMP+30                                                  
         BE    CLTGR20                                                          
         UNPK  DUB(5),SPTREC+4(3)                                               
*****    CLC   RNUM,=C'BU'             IF NETWORK                               
*****    BNE   CLTGR04                                                          
*****    UNPK  DUB(5),SPTREC+6(3)       DIFFERENT KEY                           
CLTGR04  LA    RF,DUB                                                           
         LA    RE,TEMP+30                                                       
         LA    R0,4                                                             
CLTGR05  CLI   0(RE),X'C1'         IF A LETTER OR NUMBER                        
         BL    CLTGR07                                                          
         CLC   0(1,RE),0(RF)       MUST MATCH                                   
         BNE   CLTGR10                                                          
CLTGR07  LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CLTGR05                                                       
         B     CLTGR20                                                          
CLTGR10  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEYD(4),KEY                                                      
         BNE   CLTGR03                                                          
         B     GTCLTRC                                                          
* PUT SCHEME IN REQUEST                                                         
CLTGR20  CLC   RAGY,=C'GZ'         IS IT GM ?                                   
*        BE    *+14                                                             
*        CLC   RAGY,=C'*B'         DDSB FOR TESTING                             
         BNE   CLTGR25                                                          
         CLC   =C'B1',RNUM                                                      
         BE    *+14                                                             
         CLC   =C'D1',RNUM                                                      
         BNE   CLTGR25                                                          
         CLC   =C'ALL',TEMP+30     FOR ALL GROUPS IN A SCHEME?                  
         BE    CLGINV              INVALID FOR GM BILLING                       
         MVC   DEMS(4),SPTREC+2    SAVE A/M AND GROUP FOR L8R                   
*                                                                               
CLTGR25  MVC   RCLI,=C'ALL'                                                     
         MVC   RCARD2+4(1),HALF2                                                
         ZIC   R1,IFLDH+5                                                       
         S     R1,=F'6'            OVERHEAD                                     
         CLI   HALF2+1,2           2 CHAR CGRP ?                                
         BNE   *+6                                                              
         BCTR  R1,0                1 MORE BYTE OF OVERHEAD                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RCARD2+5(0),TEMP+30                                              
         OI    FIND,X'40'                                                       
         MVC   KEY(13),IFLDCNT     RESTORE KEY                                  
         B     CLGX                                                             
*                                                                               
NETCGRP  DS    0H                 NETWORK CLIENT GROUP                          
*                                                                               
CLGOK    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
CLGX     XIT1                                                                   
*                                                                               
CLGINV   MVC   FERN,=AL2(FLDINV)                                                
         B     CLGX                                                             
         LTORG                                                                  
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
CNVOFF   CSECT                     CONVERT 2 CHAR OFFICE CODE TO 1 HEX          
         NMOD1 0,CNOFF                                                          
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
         CLI   IFLD,C'*'           OFFICE CODE REQUEST ?                        
         BE    CNOFF30                                                          
         CLC   =C'ALL-',IFLD       OLD OFFICE REQUEST?                          
         BNE   CNOFFX                                                           
         CLI   IFLDH+5,6                                                        
         BH    OFFINV                                                           
         MVC   TEMP(6),IFLD                                                     
         MVC   IFLD,SPACES         CLEAR IFLD                                   
         MVI   IFLD,C'*'           CONVERT ALL-X TO *X                          
         MVC   IFLD+1(2),TEMP+4    OFFICE                                       
         MVI   IFLDH+5,2                                                        
         CLI   IFLD+2,C' '         IS IT 2CHAR OFFICE?                          
         BE    *+8                                                              
         MVI   IFLDH+5,3           YES, LENGTH IS 3                             
*                                                                               
CNOFF30  CLC   =C'*-',IFLD         EXCLUDING AN OFFICE CODE? *-X                
         BNE   CNOFF50                                                          
         CLI   IFLDH+5,4                                                        
         BH    OFFINV                                                           
         MVC   TEMP(2),IFLD+2                                                   
         BAS   RE,OFFCNV                                                        
         BNE   OFFNOF                                                           
         MVI   IFLDH+5,3                                                        
         MVC   IFLD+2(2),TEMP                                                   
         B     CNOFFX                                                           
*                                                                               
CNOFF50  CLI   IFLDH+5,3           IS IT *X?                                    
         BH    CNOFF70             HAS TO BE *X-X SITUATION                     
         MVC   TEMP(2),IFLD+1      GET 2 CHAR OFFICE CODE                       
         BAS   RE,OFFCNV                                                        
         BNE   OFFNOF                                                           
         MVI   IFLDH+5,2                                                        
         MVC   IFLD+1(2),TEMP                                                   
         B     CNOFFX                                                           
*                                                                               
CNOFF70  CLI   IFLDH+5,6           *XX-XX                                       
         BH    OFFINV                                                           
         MVC   TEMP(2),IFLD+1                                                   
         CLI   IFLD+2,C'-'         IS 1ST OFFICE 1CHAR?                         
         BNE   *+12                                                             
         MVI   TEMP+1,C' '                                                      
         B     *+12                                                             
*                                                                               
         CLI   IFLD+3,C'-'         IS 1ST OFFICE 2CHAR ?                        
         BNE   OFFINV              NO, INVALID ENTRY                            
*                                                                               
         BAS   RE,OFFCNV                                                        
         BNE   OFFNOF                                                           
         MVC   HALF(1),TEMP                                                     
*                                                                               
         CLI   IFLD+2,C'-'         IS 2ND OFFICE 1CHAR?                         
         BNE   *+14                                                             
         MVC   TEMP(2),IFLD+3                                                   
         B     *+10                                                             
*                                                                               
         MVC   TEMP(2),IFLD+4                                                   
*                                                                               
         BAS   RE,OFFCNV                                                        
         BNE   OFFNOF                                                           
         MVC   HALF+1(1),TEMP                                                   
         MVI   IFLDH+5,3                                                        
         MVC   IFLD+1(2),HALF                                                   
         MVC   IFLD+3(3),SPACES                                                 
*                                                                               
CNOFFX   CR    RB,RB               SET CC EQ                                    
CNOFFXX  XIT1                                                                   
*                                                                               
OFFCNV   NTR1                      TEMP HAS 2 CHAR OFFICE                       
         XC    TEMP+2(OFCLENQ),TEMP+2   LENGTH OF OFFICED IS 48 BYTES           
         LA    R5,TEMP+2                                                        
         USING OFFICED,R5                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,6(R3)       ID AUTH VALUE                                
         MVC   OFCLMT,6(R3)                                                     
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC2,TEMP                                                     
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         CLI   0(R1),0                                                          
         BNE   CNOFFXX            SET CC NEQ                                    
         MVC   TEMP(1),OFCOFC     SAVE OFF 1 BYTE INTERNAL OFFICE CODE          
         MVI   TEMP+1,C' '        CLEAR 2ND CHAR                                
         B     CNOFFX                                                           
         DROP  R5                                                               
*                                                                               
OFFINV   MVC   FERN,=AL2(FLDINV)                                                
         LTR   RE,RE              SET CC NEQ                                    
         B     CNOFFXX                                                          
*                                                                               
OFFNOF   MVC   FERN,=AL2(OFFNFL)                                                
         LTR   RE,RE              SET CC NEQ                                    
         B     CNOFFXX                                                          
*                                                                               
OFFNFL   EQU   1273               OFFICE NOT ON FILE                            
         LTORG                                                                  
         EJECT                                                                  
ESTVL    CSECT                               ESTIMATE - FIND BITS               
         NMOD1 0,ESTVL                                                          
         USING TWAD,R3                                                          
         USING REQTEMP,R9                                                       
*                                                                               
         MVI   ROUTSUB,1                     02=ALL 04=NNN 08=NNN-NNN           
         GOTO1 AINITV                        10=NO 20=NNN 40=NNN-NNN            
*                                  80=NO,XXX   FILTERS                          
*                                                                               
         CLI   IFLD,C'E'           IF B1 REPORT BEGINS EST WITH E               
         BNE   CHKBSFLT                                                         
         CLC   RNUM,=C'B1'                                                      
         BE    ESTEOPT                                                          
         CLC   RNUM,=C'D1'         OR D1                                        
         BNE   ESTVV1                                                           
ESTEOPT  MVI   RO1,C'E'            SET E IN QOPT1                               
         L     R4,FLDHADR          AND FUDGE FIELD                              
         ZIC   R5,5(R4)                                                         
         C     R5,=F'2'                                                         
         BL    ESTVV1                                                           
         A     R5,=F'-2'           .SET NEW LENGTH                              
         STC   R5,5(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),10(R4)      .MOVE TO START OF FIELD                      
         GOTO1 AINITV              .AND START ALL OVER AGAIN                    
*                                                                               
CHKBSFLT DS    0H                                                               
*        CLC   RNUM,=C'A2'                                                      
*        BNE   ESTVV1                                                           
         CLC   IFLD(4),=C'ALL='    ALL=BAR OR ALL=STW                           
         BE    *+14                                                             
         CLC   IFLD(3),=C'NO='     SAME TRUE FOR NO=                            
         BNE   ESTVV1                                                           
         L     R4,FLDHADR                                                       
         ZIC   R5,5(R4)                                                         
         SHI   R5,3                                                             
         LA    R1,8+3(R4)          POINT TO ACTUAL FILTER (PAST =)              
         CLC   IFLD(3),=C'ALL'                                                  
         BNE   *+10                                                             
         LA    R1,1(R1)                                                         
         BCTR  R5,0                R5 HOLDS LEN OF ENTRY PAST = SIGN            
         CHI   R5,3                HAS TO BE 3 (BAR OR STW)                     
         BE    CHKBS20                                                          
         CHI   R5,1                OR 1 --> '*'                                 
         BNE   ESTVE                                                            
         CLI   0(R1),C'*'                                                       
         BNE   ESTVE                                                            
         MVI   RCARD2+16,C'*'                                                   
         BCTR  R1,0                BACK OUT TO = SIGN                           
         MVC   0(2,R1),SPACES                                                   
         ZIC   R5,5(R4)                                                         
         SHI   R5,2                OVERHEAD                                     
         STC   R5,5(R4)                                                         
         B     CHKBS50                                                          
CHKBS20  CLC   =C'BAR',0(R1)                                                    
         BNE   *+12                                                             
         MVI   RCARD2+16,C'B'                                                   
         B     *+18                                                             
         CLC   =C'STW',0(R1)                                                    
         BNE   ESTVE                                                            
         MVI   RCARD2+16,C'S'                                                   
         BCTR  R1,0                BACK OUT TO = SIGN                           
         MVC   0(4,R1),SPACES                                                   
         ZIC   R5,5(R4)                                                         
         SHI   R5,4                OVERHEAD                                     
         STC   R5,5(R4)                                                         
CHKBS50  GOTO1 AINITV                                                           
*                                                                               
ESTVV1   XC    ESTDATES,ESTDATES                                                
         CLI   FIND,1                                                           
         BNE   ESTVO                         EST = ALL OR MISSING               
         CLI   IFLDH+5,3                                                        
         BH    ESTVE                                                            
         CLC   IFLD(3),=C'NO '                                                  
         BNE   ESTV1                                                            
         OI    FIND,X'10'                    EST = NO                           
         B     ESTVO                                                            
ESTV1    GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    ESTVE                         NON NUMERIC ESTIMATE               
         MVC   IFLD(3),TEMP+1                                                   
         TM    PROSAVE,X'0C'                                                    
         BNZ   ESTV1A                                                           
*                                                                               
*                      ATTEMPT TO READ POL EST TO SAVE ITS DATES                
*                                                                               
         OI    FIND,X'20'                    EST = NNN & NON SPEC PRO           
         CLC   RNUM(2),=C'X2'      DON'T SAVE POL DATE FOR X2                   
         BE    ESTVO                                                            
         CLC   RNUM(2),=C'X1'      DON'T SAVE POL DATE FOR X1                   
         BE    ESTVO                                                            
         CLC   RNUM(2),=C'MN'      DON'T SAVE POL DATE FOR MN                   
         BE    ESTVO                                                            
*                                                                               
*                                                                               
         MVC   KEY+7(1),TEMP                                                    
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    ESTVO            DISK ERROR                                      
         BH    SKIP12                                                           
***      GOTO1 =A(MEDTEST),DMCB,RR=RELO                                         
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           2=SPOT,3=NET    CC=EQUAL IF NET              
         BE    ESTVE                                                            
         CLC   RNUM,=C'SL'         FOR SYNDICATE PROGRAM LIST                   
         BE    ESTVE               ERROR IF NO POL ESTIMATE                     
         CLC   RNUM,=C'C2'         FOR C2                                       
         BE    ESTVE               ERROR IF NO POL ESTIMATE                     
         DROP  RF                                                               
*                                                                               
         MVC   FERN,=AL2(FF)         POL EST NOT FOUND - NO ERROR               
         B     ESTV2A                                                           
SKIP12   MVC   ESTDATES(12),SPTREC+52                                           
         OI    ODDMNTS,X'01'       POL EST FOUND                                
         B     ESTV2A                                                           
*                                                                               
ESTV1A   MVC   KEY+7(1),TEMP                                                    
         GOTO1 ARSPT                         READ ESTIMATE HEADER               
         CLC   FERN,=AL2(FE)                                                    
         BL    ESTVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(ESTNOF)                   EST NOT ON FILE              
         B     ESTVXIT                                                          
         MVC   ESTDATES(12),SPTREC+52                                           
         MVC   NAME(20),SPTREC+24                                               
         OI    FIND,X'04'                    EST = NNN                          
*                                                                               
         CLC   RNUM,=C'CW'         IF CW                                        
         BNE   ESTV2A                                                           
         LA    R2,SPTREC                                                        
         USING ESTHDR,R2                                                        
         OC    ECOST2,ECOST2                                                    
         BNZ   ESTVE                                                            
*                                                                               
*                                  ***  TEST FOR JWT CLIENTS ****               
ESTV2A   CLC   RNUM,=C'M8'                                                      
         BE    ESTVO                                                            
         CLC   RNUM,=C'M9'                                                      
         BE    ESTVO                                                            
         CLI   CLIPROF+11,C'Y'     FORCE EST SERIES REQ ?                       
         BNE   ESTVO               NO                                           
*                                                                               
         TM    PROSAVE,X'02'       PRD = ALL ?                                  
         BO    ESTVO               YES - SKIP BELOW                             
*                                                                               
*   IF PRODUCT IS ALL, EXPECTING TO HAVE A SINGLE ESTIMATE RECORD               
*   AVAILABLE IN SPTREC, WITH SOME KIND OF ESTIMATE-RANGE OVERRIDE,             
*   IS ILLOGICAL SO THE CODE BELOW IS SKIPPED.                                  
*                                                                               
         TM    FIND,X'24'          EST = NNN ?                                  
         BNO   ESTVO               NO - SKIP BELOW                              
*                                                                               
*   IF NOT SINGLE ESTIMATE, SKIP THE CODE BELOW                                 
*                                                                               
         LA    R2,SPTREC                                                        
         USING ESTHDR,R2                                                        
         OC    EREQLO(2),EREQLO                                                 
         BE    ESTVO                                                            
         ZIC   R0,EREQLO                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REST,DUB                                                         
         ZIC   R0,EREQHI                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REST1,DUB                                                        
         B     ESTVX                                                            
         DROP  R2                                                               
*                                 *** RETURN TO REGULAR PROCESSING              
ESTVE    MVC   FERN,=AL2(ESTINV)                 INVALID ESTIMATE               
         B     ESTVXIT                                                          
ESTVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
ESTV3    CLC   5(1,R4),IFLDH+5               ONLY ONE ESTIMATE                  
         BE    ESTVX                         YES THEN OK SO FAR                 
**       TM    FIND,X'34'                    WAS 1ST EST NNN                    
**       BZ    ESTVE1                       ALL CANT HAVE 2ND EST               
**       TM    FIND,X'34'                                                       
**       BNZ   ESTV3A                                                           
**       CLC   RNUM,=C'NV'                  EXCEPT FOR NV REPORT                
**       BNE   ESTVE1                                                           
ESTV3A   NI    FIND,X'CB'                                                       
*                                                                               
*              CHK FOR NO,XXX   FILTERS                                         
*                      AND ALL,XXX FOR NV REPORT                                
*                                                                               
         CLC   0(2,R7),=C'NO'                                                   
         BE    ESTV3B                                                           
         CLC   0(3,R7),=C'ALL'                                                  
         BNE   ESTV3K                                                           
ESTV3B   MVI   ROUTSUB,0           SET TO REEDIT FIELD                          
         GOTO1 AINITV                                                           
*                                  CHECK FILTERS                                
         CLI   IFLDH+5,6                                                        
         BL    ESTVE1                                                           
         CLI   IFLDH+5,9                                                        
         BH    ESTVE1              MAX IS -X-X-X                                
         ZIC   R1,IFLDH+5          SAVE INPUT LENGHT                            
         SH    R1,=H'3'            ADJUST FOR NO,                               
         CLC   0(3,R7),=C'ALL'     OR ALL,                                      
         BNE   ESTV3BB                                                          
         BCTR  R1,0                                                             
ESTV3BB  LA    R4,IFLD+3          BUMP PAST NO,                                 
         CLC   0(3,R7),=C'ALL'                                                  
         BNE   *+8                                                              
         LA    R4,1(R4)            BUMP PAST ALL,                               
         LA    R5,3                FOR BCT                                      
         XC    TEMP(4),TEMP                                                     
         LA    R6,TEMP+1                                                        
ESTV3C   EQU   *                                                                
         CLI   0(R4),C'-'          SEE IF NEGATIVE FILTER                       
         BNE   ESTV3E              NO                                           
         MVI   TEMP,1                                                           
         LA    R4,1(R4)            PAST -                                       
         BCTR  R1,0                DECREMENT COUNTER                            
ESTV3E   MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   ESTV3F                                                           
         CLI   TEMP,0                                                           
         BNE   ESTVE1              -* IS INVALID                                
         B     ESTV3G                                                           
*                                                                               
ESTV3F   CLI   0(R6),C'A'                                                       
         BL    ESTVE1                                                           
         CLI   0(R6),C'9'                                                       
         BH    ESTVE1                                                           
         CLI   TEMP,1            SEE IF NEGATIVE FILETR                         
         BNE   *+8                                                              
         NI    0(R6),X'BF'         SET OFF X'40'                                
ESTV3G   LA    R6,1(R6)                                                         
         MVI   TEMP,0            ZERO NEGATIVE INDICATOR                        
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
         BCT   R5,ESTV3C                                                        
         LTR   R1,R1                                                            
         BNZ   ESTVE1              SHOULD HAVE NO MORE INPUT                    
         OI    FIND,X'80'          EST=NO,XXX                                   
         B     ESTV5                                                            
*                                                                               
ESTV3K   EQU   *                                                                
         MVI   ROUTSUB,2                     GET 2ND EST NUM                    
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BNE   ESTVE1                                                           
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    ESTVE1                        NON NUMERIC EST                    
         CLC   RNUM,=C'96'                                                      
         BNE   ESTV4                                                            
*             READ SECOND EST ONLY FOR REQ 96                                   
         MVC   KEY+7(1),TEMP                                                    
         MVC   IFLD(3),TEMP+1                                                   
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BL    ESTVO           DISK ERROR                                       
         BH    *+14                                                             
         MVC   FERN,=AL2(ESTNOF)         NOT FOUND                              
         B     ESTVXIT                                                          
         OI    FIND,X'08'     EST=NNN-NNN                                       
         MVC   RO1(3),IFLD         COL 62-64                                    
         B     ESTVX                                                            
ESTV4    TM    PROSAVE,X'0C'                                                    
         BNZ   *+18                                                             
         OI    FIND,X'40'                    EST = NNN-NNN & NON SPEC           
         XC    ESTDATES,ESTDATES                                                
         B     *+8                                                              
         OI    FIND,X'08'                    EST = NNN-NNN                      
ESTV5    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   3(3,R7),TEMP+1                                                   
*        CLC   RNUM,=C'DN'         NNN-NNN IS EST-LINE FOR REQ DN               
*        BE    ESTVX                                                            
         CLC   0(2,R7),=C'NO'                                                   
         BE    ESTVX               2ND FLD IS FILTERS FOR EST=NO                
         CLC   0(3,R7),=C'ALL'                                                  
         BE    ESTVX               2ND FLD IS FILTERS FOR EST=ALL               
         CLC   0(3,R7),3(R7)                 1ST EST MUST BE LT 2ND EST         
         BL    ESTVX                                                            
         MVC   FERN,=AL2(EST1G2)                                                
         B     ESTVXIT                                                          
ESTVE1   MVC   FERN,=AL2(ESTINV)                INVALID 2ND ESTIMATE            
         B     ESTVXIT                                                          
ESTVX    MVC   ESTSAVE,FIND                                                     
*                                                                               
         CLC   RNUM,=C'B1'         B1 REPORTS FOR SINGLE ESTIMATES              
         BE    ESTVXE              NOW CHECK FOR E TYPE ESTIMATES               
         CLC   RNUM,=C'D1'                                                      
         BNE   ESTVXBS                                                          
ESTVXE   CLI   RO1,C'E'            IF REQUESTED TYPE E ESTIMATES                
         BNE   ESTVX3                                                           
         L     R4,FLDHADR          RESET E, AT START OF FIELD                   
         ZIC   R5,5(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+50(0),8(R4)                                                 
         MVC   8(2,R4),=C'E,'                                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R4),TEMP+50                                                 
         LA    R5,3(R5)            AND RESET ORIGINAL LENGTH                    
         STC   R5,5(R4)                                                         
         TM    ESTSAVE,X'04'       IS IT SINGLE ESTIMATE                        
         BNO   ESTVX5                                                           
         LA    R2,SPTREC                                                        
         USING ESTHDR,R2                                                        
         TM    ECONTROL,EBILESTQ   IF IT IS E TYPE                              
         BO    ESTVX5              OK                                           
         B     ETYPERR                                                          
*                                                                               
*                                  E TYPE NOT REQUESTED                         
ESTVX3   TM    ESTSAVE,X'04'       IS IT SINGLE ESTIMATE                        
         BNO   ESTVXBS                                                          
         LA    R2,SPTREC           YES/CHECK IF E TYPE                          
         USING ESTHDR,R2                                                        
         TM    ECONTROL,EBILESTQ   AND IT IS                                    
         BNO   ESTVXBS             SET ERROR                                    
ETYPERR  MVC   FERN,=AL2(ESTINV)                                                
         B     ESTVXIT                                                          
         DROP  R2                                                               
*                                                                               
ESTVXBS  DS    0H                                                               
*        CLC   RNUM,=C'A2'                                                      
*        BNE   ESTVX5                                                           
         CLI   RCARD2+16,C' '      ANYTHING IN 17TH COL?                        
         BNH   ESTVX5              NO SKIP                                      
         L     R4,FLDHADR          RESTOR ALL/NO = BAR/STW                      
         CLC   =C'ALL',8(R4)       IS IT ALL                                    
         BE    *+14                                                             
         CLC   =C'NO',8(R4)        IS IT NO                                     
         BNE   ESTVE                                                            
         ZIC   R5,5(R4)                                                         
         LA    R1,8(R5,R4)         POINT PAST ENTRY                             
         CLI   RCARD2+16,C'B'                                                   
         BNE   *+14                                                             
         MVC   0(4,R1),=C'=BAR'                                                 
         B     *+18                                                             
         CLI   RCARD2+16,C'S'                                                   
         BNE   ESTVXBSC                                                         
         MVC   0(4,R1),=C'=STW'                                                 
         AHI   R5,4                ADJUST LENGTH                                
         STC   R5,5(R4)                                                         
         B     ESTVX5                                                           
ESTVXBSC CLI   RCARD2+16,C'*'                                                   
         BNE   ESTVE                                                            
         MVC   0(2,R1),=C'=*'                                                   
         AHI   R5,2                ADJUST LENGTH                                
         STC   R5,5(R4)                                                         
         B     ESTVX5                                                           
*                                                                               
ESTVX5   TM    TWAAUTH,X'80'    CHECK AUTHORIZATION BIT (JWT/BELL)              
         BNO   ESTVX6                                                           
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BE    ESTVX6              SKIP TEST IF NET                             
         DROP  RF                                                               
* - MUST BE SINGLE CLIENT                                                       
         TM    CLISAVE,X'04'                                                    
         BO    ESTVX5B                                                          
         MVI   ROUTNUM,2                                                        
         B     EAUTHERR                                                         
* - MUST BE SINGLE ESTIMATE                                                     
ESTVX5B  TM    ESTSAVE,X'04'                                                    
         BNO   EAUTHERR                                                         
* - AND CHECK POL ESTIMATE FOR OK                                               
         MVC   KEY(13),SPTREC                                                   
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 ARSPT                                                            
         LA    R2,SPTREC                                                        
         USING ESTHDR,R2                                                        
         TM    EFLAG1,EF1REQ       DOES POL EST GIVE OK                         
         BO    ESTVX6              YES                                          
EAUTHERR MVC   FERN,=AL2(583)      NO                                           
         B     ESTVXIT                                                          
                                                                                
ESTVX6   L     R7,ASAVE                                                         
         USING T208FFD,R7                                                       
         CLC   =C'SOON',BVROUT                                                  
         BNE   ESTVXIT                                                          
         B     ESTVXIT                                                          
ESTVXIT  XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPREQSAVE                                                      
       ++INCLUDE SPREQTEMP                                                      
       ++INCLUDE FLDIND                                                         
         SPACE 2                                                                
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPREQFFBD                                                      
       ++INCLUDE DDOFFICED                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
       ++INCLUDE NEDDEQUS                                                       
       ++INCLUDE SPSTAPACKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'173SPREQ03   04/28/17'                                      
         END                                                                    
