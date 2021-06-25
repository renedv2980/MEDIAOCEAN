*          DATA SET REREI02    AT LEVEL 005 AS OF 04/27/07                      
*PHASE T82602A                                                                  
*INCLUDE RANDOM                                                                 
***********************************************************************         
*                                                                               
*  TITLE: T82602 - DISPLAY INVOICE HEADER INFO                                  
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T82600), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     REREIFD (T826FD) -- MAINTENANCE                                 
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - WORK                                                            
*          R8 - SECOND BASE                                                     
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
         TITLE 'REREI02 - REP INVOICE MAINT OVERLAY'                            
T82602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82602*,R8,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         BAS   RE,SETPFTBL                                                      
         GOTOR MODSCRN                                                          
*                                                                               
         TM    CTLRFLG1,CF1CKOFF          SAVE OFFSET OF SELECTED LINE?         
         BZ    MAIN10                                                           
         MVC   SVDOFFST,SELOFFST          YES                                   
         NI    CTLRFLG1,X'FF'-CF1CKOFF    DON'T OVERWRITE NEXT TIME             
         MVC   LSTQSTA,DISSTA                                                   
         MVC   LSTQPER,DISMON                                                   
         MVC   LSTQINV,DISINV                                                   
*                                                                               
MAIN10   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VREC                                                             
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
         MVI   CURRSYS,C'R'        ALWAYS START IN REP                          
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKSTA00  LA    R2,DISSTAH                                                       
         TM    CTLRFLG1,X'10'      LOCAL STATION                                
         BZ    VKSTA02                                                          
         MVC   8(L'DISSTA,R2),SVSIGNON                                          
         OI    6(R2),X'80'         RESTRANSMIT                                  
         OI    1(R2),X'20'                                                      
         MVI   5(R2),L'DISSTA      LENGTH                                       
*                                                                               
         LA    R6,DISSTA                                                        
         AHI   R6,L'DISSTA-1       FIND THE LAST CHARACTER                      
         CLI   0(R6),X'40'                                                      
         BH    *+8                                                              
         BCT   R6,*-8                                                           
*                                                                               
         CLI   0(R6),C'L'          NO L FOR STATION                             
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
*                                                                               
VKSTA02  CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALISTA                                                          
         BNE   INVLFLD                                                          
*                                                                               
         MVC   DISSTNM,MKTNM       SHOW MARKET NAME                             
         OI    DISSTNMH+6,X'80'                                                 
*                                                                               
VKSTAX   DS    0H                                                               
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE PERIOD = MONTH                                                   
*****                                                                           
VKPER00  LA    R2,DISMONH                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 PERVAL,DMCB,(DISMONH+5,DISMON),PERVALST                          
         TM    DMCB+4,X'01'                                                     
         BO    INVLFLD                                                          
*                                                                               
         LA    R3,PERVALST                                                      
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALASD                  START DAY ASSUMED?             
         BZ    DATEERR                           YES...ERROR                    
*                                                                               
         TM    PVALASSM,PVALAED+PVALAEM+PVALAEY  ALL OF END ASSUMED?            
         BNO   INVLFLD                           NO                             
         MVC   BMOS,PVALCSTA                                                    
         MVC   BMOSFF,PVALCSTA                                                  
         XC    BMOSFF,=X'FFFF'                                                  
*                                                                               
         MVC   PVALESTA+4(2),=C'15'                                             
         GOTO1 GETBROAD,DMCB,(1,PVALESTA),BRDDATES,GETDAY,ADDAY                 
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                DATE SHOULDN'T BE INVALID                    
*                                                                               
         GOTO1 DATCON,DMCB,(X'10',BRDDATES),(2,WORK)                            
         MVC   BRDCSDAT,WORK                                                    
         MVC   BRDCEDAT,WORK+3                                                  
*                                                                               
         OI    DISMNDTH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(X'12',BRDCSDAT),(8,DISMNDT)                         
*                                                                               
VKPERX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
         DROP  R3                                                               
*****                                                                           
* VALIDATE THE CONTRACT                                                         
*****                                                                           
VKCON00  LA    R2,DISCONH                                                       
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
*        QSNVRFLG=Contract(Reppak)/Rep Contract(MO)/Station Ord#                
*                                                                               
         MVI   QSNVRFLG,0          clear flag                                   
         XC    BCONT99,BCONT99                                                  
         NI    CTLRFLG1,X'FF'-CF1MOREP-CF1MOSTA-CF1MOINV                        
*                                                                               
         TM    CTLRFLG1,X'10'      IS THIS A LOCAL REP?                         
         BZ    *+20                NO                                           
         MVC   QSTAORD#,8(R2)                                                   
         OC    QSTAORD#,=10C' '    PADDED WITH SPACE, THIS IS HOW               
*                                  IT IS STORED IN THE RECORD                   
         B     VKCONX                                                           
*                                                                               
         CLI   DISTYPE,C'C'        C = CONTRACT NUMBER?                         
         BE    VKCON01             YES                                          
         OC    DISTYPE,DISTYPE     ALL # = CONTRACT NUMBER?(DEFAULT)            
         BZ    VKCON01             YES                                          
         CLI   DISTYPE,C' '        SPACE = CONTRACT NUMBER?(DEFAULT)            
         BE    VKCON01             YES                                          
*                                                                               
* DISTYPE MUST CONTAIN A CHARACTER THAT IS NOT 'C'                              
*                                                                               
         MVC   QORD#,8(R2)                                                      
         OC    QORD#,=10C' '                                                    
*                                                                               
         CLI   DISTYPE,C'R'                                                     
         BNE   *+12                                                             
         MVI   QSNVRFLG,C'R'       SET FLAG TO MO CONTRACT#                     
         OI    CTLRFLG1,CF1MOINV+CF1MOREP                                       
*                                                                               
         CLI   DISTYPE,C'S'                                                     
         BNE   *+12                                                             
         OI    CTLRFLG1,CF1MOINV+CF1MOSTA                                       
         MVI   QSNVRFLG,C'S'       SET FLAG TO STATION ORD#                     
*                                                                               
         LA    RE,QORD#                                                         
         LA    RF,WORK                                                          
         LA    R1,L'QORD#                                                       
         XC    WORK(L'QORD#),WORK                                               
VKCON00A CLI   0(RE),X'40'                                                      
         BNH   VKCON00B                                                         
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
VKCON00B LA    RE,1(RE)                                                         
         BCT   R1,VKCON00A                                                      
         MVC   QORD#,WORK                                                       
         OC    QORD#,=10C' '                                                    
         B     VKCONX                                                           
*                                                                               
VKCON01  LA    RE,8(R2)            RE-FORMAT CONTRACT NUMBER                    
         LA    RF,WORK+20          CONTRACT# NOW HAS A 'C' AT THE END           
         LA    R1,L'DISCON         BEFORE READING THE CONTRACT KEY,             
         SR    R0,R0               NEED TO CHOP OFF THE 'C',AND ALSO            
VKCON03  CLI   0(RE),X'40'         CALCULATE THE REAL LENGTH OF THE #           
         BNH   VKCON05                                                          
         CLI   0(RE),C'C'                                                       
         BE    VKCON05                                                          
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
         AHI   R0,1                COUNT HOW MANY CHARACTERS                    
VKCON05  AHI   RE,1                                                             
         BCT   R1,VKCON03                                                       
*                                                                               
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(5),WORK+20(0)                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK(5)                                                
         SRP   WORK+5(5),1,0                                                    
         MVC   BCONT99,WORK+5                                                   
*                                                                               
         GOTO1 GETCONT             GET CONTRACT REC                             
         BNE   RECNTFND                     NO, RECORD NOT FOUND                
*                                                                               
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   VKCON10                                                          
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    VKCON10             YES                                          
         CLC   QOFF,TWAACCS+2      NO, MUST MATCH RESTRICTED OFFICE             
         BNE   NOACCESS            NOT AUTH'D FOR DATA                          
*                                  SHOW OFFICE FOR THIS CONTRACT                
VKCON10  MVC   DISOFF,QOFF                                                      
         OI    DISOFFH+6,X'80'                                                  
*                                                                               
VKCONX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VKINV00  LA    R2,DISINVH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKINV02                                                          
         TM    CTLRFLG1,X'10'      LOCAL STATION?                               
         BO    VKINVX              INVOICE NOT MANDATORY                        
         B     MISSFLD                                                          
*                                                                               
VKINV02  MVC   QINVOICE,8(R2)                                                   
         OC    QINVOICE,=CL10' '                                                
VKINVX   OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
VKKEY    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING SNVKEY,R3                                                        
*                                                                               
         TM    CTLRFLG1,X'10'      IS THIS A LOCAL REP?                         
         BZ    VKKEY06             NO                                           
*                                                                               
         MVI   SNVLTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVLSUB,SNVLSUBQ    X'B3'                                        
         MVC   SNVLID,TWAORIG                                                   
         MVC   SNVLSTA,QSTA                                                     
         MVC   SNVLMOS,BMOSFF                                                   
         MVC   SNVLSORD,QSTAORD#                                                
         MVC   SNVLPOW,TWAAGY                                                   
         B     VKKEY08                                                          
*                                                                               
VKKEY06  MVI   SNVRTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVRSUB,SNVRSUBQ    X'A3'                                        
         MVC   SNVREP,AGENCY                                                    
         MVC   SNVRSTA,QSTA                                                     
         MVC   SNVRMOS,BMOSFF                                                   
         MVC   SNVRCON,BCONT99                                                  
         MVC   SNVRFLG,QSNVRFLG                                                 
         MVC   SNVRINV,QORD#                                                    
         CLI   SNVRFLG,0                                                        
         BNE   *+10                                                             
         MVC   SNVRINV,QINVOICE                                                 
         DROP  R3                                                               
*                                                                               
VKKEY08  CLI   CURRSYS,C'S'                                                     
         BE    VKKEY10                                                          
         GOTO1 SPTSYS                                                           
VKKEY10  GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),INVDIR,KEY,AIO                   
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLC   SNVRMAST,KEY        YES, SAME RECORD?                            
         BNE   RECNTFND                     NO, RECORD NOT FOUND                
*                                                                               
         MVC   SVMASTKY(L'SNVRMAST),SNVRMAST                                    
*                                                                               
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   VKX                 THEN NO D/A                                  
         MVC   CONHED2(4),=C'D/A='                                              
         GOTO1 HEXOUT,DMCB,SNVDDA,CONHED2+4,L'SNVDDA                            
         OI    CONHED2H+6,X'80'                                                 
*                                                                               
VKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VREC     DS    0H                                                               
         GOTO1 INITMNIO                                                         
VRX      B     DREC                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DREC     DS    0H                                                               
         TWAXC DISDATEH,DISCDTSH   CLEAR THE SCREEN                             
*                                                                               
* GET THE HEADER ELEMENT                                                        
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    X'10'                                        
         BAS   RE,MINIOHI                                                       
         BE    DREC10                                                           
         DC    H'0'                                                             
*                                                                               
DREC10   L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ    X'10'                                        
         BE    *+6                                                              
         DC    H'0'                DIE IF WE'RE MISSING HEADER ELEMENT          
*                                                                               
         GOTO1 DATCON,DMCB,(2,SNVHDIDT),(8,DISDATE)                             
         OI    DISDATEH+6,X'80'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,SNVHDDDT),(8,DISDDAT)                             
         OI    DISDDATH+6,X'80'                                                 
*                                                                               
         XC    DISPAID,DISPAID                                                  
         TM    SNVHDCTL,SNVHDPDQ   INVOICE IS PAID?                             
         BZ    *+10                                                             
         MVC   DISPAID(8),=C'**PAID**'                                          
         OI    DISPAIDH+6,X'80'                                                 
***                                                                             
* REVISION NUMBER                                                               
***                                                                             
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVRTELQ    X'24'                                        
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVRTELD,R6                                                      
         CLI   SNVRTEL,SNVRTELQ    X'24'                                        
         BE    *+6                                                              
         DC    H'0'                DIE IF WE'RE MISSING INVOICE ELEM            
*                                                                               
         EDIT  (B2,SNVRCT),(4,DISRVSN),ZERO=NOBLANK,ALIGN=LEFT                  
         OI    DISRVSNH+6,X'80'                                                 
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    X'10'                                        
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEL,SNVHDELQ    X'10'                                        
         BE    *+6                                                              
         DC    H'0'                DIE IF WE'RE MISSING HEADER ELEMENT          
*                                                                               
* DISPLAY THE TOTAL DOLLARS AND SPOTS                                           
*                                                                               
         ZAP   PCKOF16B,=P'0'                                                   
         OC    SNVHDTCS,SNVHDTCS   JUST IN CASE NO VALUE                        
         BNZ   *+10                                                             
         ZAP   SNVHDTCS,=P'0'                                                   
         ZAP   PCKOF16B,SNVHDTCS                                                
         EDIT  (P16,PCKOF16B),(15,DISTOTL),2,ZERO=NOBLANK,ALIGN=LEFT            
         OI    DISTOTLH+6,X'80'                                                 
*                                                                               
         EDIT  (B2,SNVHDTSP),(4,DISSPTS),ZERO=NOBLANK,ALIGN=LEFT                
         OI    DISSPTSH+6,X'80'                                                 
*                                                                               
         TM    CTLRFLG1,CF1LOCAL+CF1MOINV   LOCAL/MO INVOICE                    
         BZ    DREC16              SKIP CONTRACT INFORMATION                    
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVRIELQ    X'25'                                        
         BAS   RE,MINIOHI                                                       
         BNE   DROPTX                                                           
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVRINVD,R6                                                      
         CLI   SNVRIEL,SNVRIELQ    X'25'                                        
         BNE   DROPTX                                                           
*                                                                               
         MVC   DISAGY,SNVRIAGN     AGENCY                                       
         OI    DISAGYH+6,X'80'                                                  
         MVC   DISAGYN,SNVRIAGN                                                 
         OI    DISAGYNH+6,X'80'                                                 
*                                                                               
         MVC   DISBUYR(3),=C'N/A'  BUYER - SUPPRESS FOR LOCAL                   
         OI    DISBUYRH+6,X'80'                                                 
*                                                                               
         MVC   DISADV,SNVRIANM     ADV                                          
         OI    DISADVH+6,X'80'                                                  
         MVC   DISADVN,SNVRIANM                                                 
         OI    DISADVNH+6,X'80'                                                 
*                                                                               
         MVC   DISPROD,SNVRIPNM    PRODUCT                                      
         OI    DISPRODH+6,X'80'                                                 
*                                                                               
         MVC   DISSAL,SNVRISNM     SALESPERSON                                  
         OI    DISSALH+6,X'80'                                                  
         MVC   DISSALN,SNVRISNM                                                 
         OI    DISSALNH+6,X'80'                                                 
*                                                                               
         OC    SNVRISSD,SNVRISSD                                                
         BZ    DREC12                                                           
         GOTO1 DATCON,DMCB,(0,SNVRISSD),(11,WORK)                               
*                                                                               
DREC12   OC    SNVRISED,SNVRISED                                                
         BZ    DROPTX                                                           
         GOTO1 (RF),(R1),(0,SNVRISED),(11,WORK+9)                               
         MVI   WORK+8,C'-'         DATE                                         
         MVC   DISCDTS,WORK                                                     
         OI    DISCDTSH+6,X'80'                                                 
         B     DROPTX              DONE                                         
         DROP  R6                                                               
*                                                                               
***********************************                                             
*        AGENCY                                                                 
***********************************                                             
DREC16   MVC   DISAGY,QAGY                                                      
         LA    R1,DISAGY+4                                                      
         CLI   DISADV+3,C' '                                                    
         BH    *+8                                                              
         LA    R1,DISAGY+3                                                      
         MVI   0(R1),C'-'                                                       
         MVC   1(2,R1),QAGYOFF                                                  
         OI    DISAGYH+6,X'80'                                                  
         GOTO1 GETAGYN                                                          
         MVC   DISAGYN,AGYNM                                                    
         OI    DISAGYNH+6,X'80'                                                 
***********************************                                             
*        BUYER                                                                  
***********************************                                             
         MVC   DISBUYR,QBUYER                                                   
         OI    DISBUYRH+6,X'80'                                                 
***********************************                                             
*        ADVERTISER                                                             
***********************************                                             
         MVC   DISADV,QADV                                                      
         OI    DISADVH+6,X'80'                                                  
         GOTO1 GETADVN                                                          
         MVC   DISADVN,ADVNM                                                    
         OI    DISADVNH+6,X'80'                                                 
***********************************                                             
*        PRODUCT                                                                
***********************************                                             
         MVC   DISPROD,QPROD                                                    
         CLC   QPROD,=C'   '                                                    
         BH    *+10                                                             
         MVC   DISPROD,QPRODNM                                                  
         OI    DISPRODH+6,X'80'                                                 
***********************************                                             
*        SALESPERSON                                                            
***********************************                                             
         MVC   DISSAL,QSAL                                                      
         OI    DISSALH+6,X'80'                                                  
         GOTO1 GETSALN                                                          
         MVC   DISSALN,SALNM                                                    
         OI    DISSALNH+6,X'80'                                                 
***********************************                                             
*        DATES                                                                  
***********************************                                             
         GOTO1 DATCON,DMCB,(3,QDATES),(11,WORK)                                 
         GOTO1 (RF),(R1),(3,QDATES+3),(11,WORK+9)                               
         MVI   WORK+8,C'-'                                                      
         MVC   DISCDTS,WORK                                                     
         OI    DISCDTSH+6,X'80'                                                 
***********************************                                             
* DISPLAY THE OPTIONS                                                           
***********************************                                             
DROPT00  DS    0H                  NO OPTIONS?                                  
DROPTX   DS    0H                                                               
         EJECT                                                                  
***********************************                                             
* GET THE DETAIL ORDER ELEMENT                                                  
***********************************                                             
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVDTELQ    X'20'                                        
         BAS   RE,MINIOHI                                                       
         BE    DREC20                                                           
         CLI   MINERR,MINEEOF      ANY DETAIL ORDER ELEMENT?                    
         BE    DREC30                                                           
         DC    H'0'                                                             
*                                                                               
DREC20   L     R6,MINELEM                                                       
         USING SNVDTELD,R6                                                      
         CLI   SNVDTEL,SNVDTELQ    X'20'                                        
         BNE   DREC30                                                           
*                                  NOT DISPLAYING DETAILS                       
***********************************                                             
* SEE IF WE HAVE ANY DETAILS                                                    
***********************************                                             
DREC30   XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ    X'40'                                        
         BAS   RE,MINIOHI                                                       
         BE    DREC40                                                           
         CLI   MINERR,MINEEOF      ANY DETAIL ORDER ELEMENT?                    
         BE    DREC60                                                           
         DC    H'0'                                                             
*                                                                               
DREC40   L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   SNVIDEL,SNVIDELQ    X'40'                                        
         BNE   DREC60                                                           
         OI    MISCFLG1,MF1GTDTL   WE HAVE INVOICE DETAILS                      
         DROP  R6                                                               
*                                                                               
DREC60   DS    0H                                                               
*                                                                               
DRX      CLI   CURRSYS,C'R'        SWITCH BACK TO REP ON EXIT                   
         BE    DRXX                                                             
         GOTO1 REPSYS                                                           
DRXX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         B     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    YES                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
***********************************************************************         
MINIODEL NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
***********************************************************************         
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* GENERAL INFORMATIONAL MESSAGES                                                
***********************************************************************         
PLSENTER MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
***********************************************************************         
* REGULAR ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
NOACCESS MVC   GERROR,=AL2(NOTAUTHD)                                            
         B     ERREXIT                                                          
*                                                                               
DATEERR  MVC   GERROR,=AL2(1138)   INVALID DATE FORMAT                          
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         NI    MNIOFLAG,X'FF'-X'80'  DON'T SAVE CHANGES ON ERRORS               
         B     MYERRXIT                                                         
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
INFRTEXT LA    R1,INFEXIT                                                       
         B     *+8                                                              
ERRRTEXT LA    R1,ERREXIT                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSYS,GETMSYS                                                   
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         BR    R1                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REREIFEQTB        FIELD EQUATE TABLE                           
         EJECT                                                                  
***********************************************************************         
* VALID OPTIONS TABLE                                                           
*                                                                               
* BYTE  0      - FLAG BIT                                                       
* BYTES 1-8    - TEXT EQUIVALENT                                                
***********************************************************************         
OPTNTABL DS    0CL9                                                             
         DC    AL1(SNVHDNTQ),CL8'NET'                                           
         DC    AL1(SNVHDDNQ),CL8'NET'  IGNORED IN BRLIDATING                    
         DC    AL1(SNVHDMCQ),CL8'MCT'                                           
         DC    AL1(SNVHDMTQ),CL8'MILITARY'                                      
*                                                                               
         DC    X'00'                                                            
***********************************************************************         
* DETAIL FIELDS WE CAN REMOVE TABLE                                             
*                                                                               
* BYTE  0      - FIELD EQUATE                                                   
* BYTE  1      - NUMBER OF BYTES INTO DETAIL ELEMENT                            
* BYTE  2      - NUMBER OF BYTES TO TEST WITH 'OC'                              
***********************************************************************         
PURGETBL DS    0XL3                                                             
         DC    AL1(FLDNFILM),AL1(SNVIDCML-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNCOST),AL1(SNVIDCST-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNESTM),AL1(SNVIDEST-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNRCNT),AL1(SNVIDRSP-SNVIDELD),AL1(L'SNVIDEST)             
         DC    AL1(FLDNPFLM),AL1(SNVIDCM2-SNVIDELD),AL1(L'SNVIDEST)             
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
MODSCRN  TM    CTLRFLG1,X'10'      LOCAL REP SIGN ON?                           
         BZ    MODSX                                                            
         MVC   DISCONA,=C'Sta-Ord#'                                             
         OI    DISCONAH+6,X'80'                                                 
         MVC   DISMSG,=C'** Invoice  Information **'                            
         OI    DISMSGH+6,X'80'                                                  
MODSX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL NTR1                                                                   
*                                                                               
         XC    DISPF12,DISPF12                                                  
         OI    DISPF12H+6,X'80'                                                 
         CLI   CALLSP,0                                                         
         BE    *+10                                                             
         MVC   DISPF12,=C'PF12=Return'                                          
*                                                                               
         SR    R2,R2                                                            
         CLI   PFKEY,3                                                          
         BE    STPF50                                                           
         CLI   PFKEY,4                                                          
         BE    STPF50                                                           
         CLI   PFKEY,12                                                         
         BNE   STPFX                                                            
STPF50   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR                                                
*                                                                               
STPF100  GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPFX    XIT1                                                                   
*                                                                               
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE DISPLAY                                   
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* DETAIL  DISPLAY                                                               
         DC    AL1(SPF04X-*,04,PFTCPROG,(SPF04X-SPF04)/KEYLNQ,0)                
         DC    CL3' ',CL8'DETAIL  ',CL8'DISPLAY '                               
SPF04    DC    AL1(KEYTYTWA,L'DISSTA-1),AL2(DISSTA-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DISMON-1),AL2(DISMON-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DISCON-1),AL2(DISCON-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DISTYPE-1),AL2(DISTYPE-T826FFD)                   
         DC    AL1(KEYTYTWA,L'DISINV-1),AL2(DISINV-T826FFD)                     
SPF04X   EQU   *                                                                
* INVOICE LIST                                                                  
         DC    AL1(SPF03X-*,03,PFTCPROG,(SPF03X-SPF03)/KEYLNQ,0)                
         DC    CL3' ',CL8'INVOICE ',CL8'LIST    '                               
SPF03    DC    AL1(KEYTYTWA,L'DISSTA-1),AL2(DISSTA-T826FFD)                     
         DC    AL1(KEYTYTWA,L'DISMON-1),AL2(DISMON-T826FFD)                     
SPF03X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* INCLUDES AND DSECTS BELOW                                                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE REREIWORKD        SYSTEM AREAS                                 
         EJECT                                                                  
       ++INCLUDE SPGENSNVN         INVOICE RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE REREIFFD          BASE SCREEN FOR SYSTEM                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REREIFDD          OUR MAINTENANCE SCREEN                       
         EJECT                                                                  
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAFACTS                                                                       
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDGLVXCTLD                                                                    
* DDMINBLK                                                                      
* FAGETTXTD                                                                     
* SPGETBUBLD                                                                    
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE FAGETTXTD                                                      
         DSECT                                                                  
       ++INCLUDE SPGETBUBLD                                                     
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
VGLOBBER DS    A                   A(GLOBBER)                                   
*                                                                               
SVDOFFST DS    XL1                 SAVED OFFSET IF REC IS SELECTED              
*                                                                               
MISCFLG1 DS    X                                                                
MF1KYCHG EQU   X'80'               KEY FIELD CHANGED                            
MF1GTDTL EQU   X'40'               THIS INVOICE HAS DETAILS                     
MF1RPCHG EQU   X'20'               REPORT CHANGE POSSIBLE                       
MF1RESPN EQU   X'10'               RESPONSE SWITCH                              
MF1CKBIT EQU   X'08'               USED FOR TESTING IN DELETING DETAIL          
*                                                                               
SVAREA   DS    0C                                                               
SVPRDCOD DS    XL1                 SAVED PRODUCT CODE                           
SVQPRDCD DS    CL3                       EBCDIC PRODUCT CODE                    
SVPR2COD DS    XL1                       PIGGY PRODUCT CODE                     
SVQPR2CD DS    CL3                       EBCDIC PIGGYBACK PRODUCT CODE          
SVESTNUM DS    XL1                       ESTIMATE                               
SVCONNUM DS    CL12                      CONTRACT NUMBER                        
SVTOTCOS DS    PL8                       TOTAL COST                             
SVTOTSPT DS    XL2                       TOTAL NUMBER OF SPOTS                  
SVINVDAT DS    XL2                       INVOICE DATE                           
SVDUEDAT DS    XL2                       DUE DATE                               
SVCTLBYT DS    XL1                       CONTROL BYTE                           
SVFLDLST DS    XL(MAXFLDEQ)              FIELD NUMBER EQUATE LIST               
SVFLDCNT DS    XL1                       FIELD COUNT                            
SVREP    DS    CL3                       REP                                    
SVAREAX  DS    0C                                                               
*                                                                               
LSTQMED  DS    CL1                 IF WE CAME FROM THE LIST THEN THESE          
LSTQCLT  DS    CL3                    FIELDS SHOULD BE FILLED IN                
LSTQSTA  DS    CL6                                                              
LSTQPER  DS    CL6                                                              
LSTQINV  DS    CL10                                                             
*                                                                               
SVTPRD   DS    XL1                 SAVED REQUEST PRODUCT CODE                   
SVTPRDN  DS    CL3                               PRODUCT NAME                   
SVTPRD2  DS    XL1                               PIGGY PRODUCT CODE             
SVTPRD2N DS    CL3                               PB PRODUCT NAME                
SVTEST   DS    XL1                               ESTIMATE                       
SVTESTN  DS    CL3                               ESTIAMTE CODE (EBCDIC)         
SVTIDNUM DS    XL1                               FILM ID NUMBER                 
*                                                                               
INTESTSD DS    CL6                 INTERSECTED ESTIMATE START DATE              
INTESTED DS    CL6                                      END DATE                
*                                                                               
BRDDATES DS    CL12                BROADCAST DATES                              
BRDCSDAT DS    XL2                 BROADCAST COMPRESSED START DATE              
BRDCEDAT DS    XL2                 BROADCAST COMPRESSED END DATE                
*                                                                               
PERDSYMD DS    CL6                 PERIOD'S START YYMMDD                        
PERDEYMD DS    CL6                          END   YYMMDD                        
*                                                                               
TEMPDATE DS    CL6                 TEMPORARY DATE FIELD                         
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE                               
*                                                                               
PCKOF16B DS    PL16                                                             
*                                                                               
PROF0I2  DS    CL16                I2  PROFILE                                  
P0I2BC   EQU   PROF0I2+9           BROADCAST/CALENDAR MONTHS                    
P0I2DTOR EQU   PROF0I2+14          DETAIL ORDER SCHEME                          
*                                                                               
PROFI2R  DS    CL16                I2R PROFILE                                  
PI2RAUTO EQU   PROFI2R+0           AUTO U2 FOR $INV                             
PI2RPOL  EQU   PROFI2R+2           POL/ALL IF PRD NOT STATED                    
PI2RHUT  EQU   PROFI2R+3           HUT ADJUSTMENT FOR AUTO BOOK LOOKUP          
*                                                                               
PROFI2X  DS    CL16                I2X PROFILE                                  
PI2XERQ  EQU   PROFI2X+14          ESTIMATE REQUIRED                            
*                                                                               
PROF0TI  DS    CL16                TI  PROFILE                                  
P0TIFCA  EQU   PROF0TI+0           FILM CODES ACCEPTED                          
*                                                                               
KEYLINE  DS    CL(L'WORK)          KEYLINE FOR THE DETAIL SCREEN                
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
*                                                                               
CHGFLAG  DS    XL1                 ESTIMATE OR PRODUCT WAS CHANGED              
XPKEY    DS    XL64                                                             
XSVPKEY  DS    XL64                                                             
ORIGEST  DS    X                                                                
ORIGPRD  DS    X                                                                
ORIGPR2  DS    X                                                                
SVKEY    DS    XL13                                                             
REPRECD  DS    XL15                                                             
REPAGY   DS    CL2                                                              
SAVEADDR DS    CL2                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREI02   04/27/07'                                      
         END                                                                    
