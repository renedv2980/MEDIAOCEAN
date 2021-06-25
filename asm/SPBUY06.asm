*          DATA SET SPBUY06    AT LEVEL 018 AS OF 04/02/13                      
*PHASE T21106C                                                                  
         TITLE 'SPBUY06 - SPOTPAK BUY - DELETE/UNDELETE'                        
T21106   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21106                                                         
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21106+4096,R9                                                   
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         XC    FLEN,FLEN                                                        
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         GOTO1 FLDVAL                                                           
*                                                                               
         CLI   BUTRCODE,C'D'       DELETE                                       
         BE    B400                                                             
         CLI   BUTRCODE,C'U'       UNDELETE                                     
         DC    H'0'                                                             
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
DELERR   MVC   NERRCD,=AL2(DELMAX)                                              
         MVI   ERRCD,NEWERRS                                                    
         B     BUYERR                                                           
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
B400     MVI   UPNDX,SDELLINQ                                                   
         OC    SVNDEF(16),SVNDEF                                                
         BNZ   B500                                                             
* SOME REASON CODE SCHEMES DON'T ALLOW DELETE IF ORIG BUYS EXIST                
         L     RE,=A(SVB0PROF-BUYSAVE)                                          
         AR    RE,RA                                                            
         CLI   7(RE),C'1'          TEST RC SCHEME 1                             
         BNE   B402                                                             
         CLI   SVANYLOK,C'Y'                                                    
         BNE   B402                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NODELLK)                                               
         B     BUYERR                                                           
*                                                                               
B402     MVI   ERRCD,INVACTN                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   *+12                                                             
         CLI   BUYMD,C'N'                                                       
         BE    BUYERR              MAY NOT DELETE EXPL NTWK BUY                 
*                                                                               
         CLC   =C'DM',0(R4)        TEST MULT DEL                                
         BE    B410                                                             
         CLC   =C'DEL',0(R4)                                                    
         BE    B406                                                             
B404     MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(HOWTODEL)                                            
         B     BUYERR                                                           
*                                                                               
B406     MVI   ERRCD,NORECALL                                                   
         TM    UPSW,UPON+UPCHA     TEST UPLOADING                               
         BO    B408                YES-BUY ALREADY IN STORAGE                   
         L     RE,ADRSVKEY                                                      
         OC    14(4,RE),14(RE)                                                  
         BZ    BUYERR                                                           
*                                                                               
         MVC   KEY,0(RE)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DELERR1                                                          
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
B408     BAS   RE,DELTEST                                                       
         BNE   DELERR2                                                          
*                                                                               
         BAS   RE,DELBUY                                                        
         BAS   RE,DELPKG                                                        
         BAS   RE,DELMOV           DELETE BUY MOVE REFERENCE                    
         B     B425                                                             
         EJECT                                                                  
* DELETE MULTIPLE                                                               
*                                                                               
* READ FOR LI=                                                                  
B410     MVI   ERRCD,2                                                          
         MVI   FSTOPS+1,C'='                                                    
         GOTO1 FLDVAL                                                           
         CLC   =C'LI=',0(R4)                                                    
         BNE   BUYERR                                                           
* READ LINE NUMBERS                                                             
         MVI   BYTE,0              COUNT NUMBER OF LINE WANT TO DELETE          
         XC    BUDEMS,BUDEMS                                                    
         LA    R6,BUDEMS                                                        
         MVI   FSTOPS+1,0                                                       
B412     MVI   ERRCD,BADLINE                                                    
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CLI   FLEN+1,3                                                         
         BH    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         LA    RE,MAXBUYS                                                       
         CLI   SV1OR2,2                                                         
         BE    *+8                                                              
         LA    RE,255                                                           
         CR    R0,RE                                                            
         BH    BUYERR                                                           
* CHECK FOR DUPLICATE                                                           
         LA    RE,BUDEMS                                                        
B413     CH    R0,0(RE)                                                         
         BE    BUYERR                                                           
         LA    RE,2(RE)                                                         
         OC    0(2,RE),0(RE)                                                    
         BNZ   B413                                                             
         STH   R0,0(R6)                                                         
         LA    R6,2(R6)                                                         
         LLC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         CLI   BYTE,MAXDEL                                                      
         BH    DELERR                                                           
         CLI   FSTOP,C','                                                       
         BE    B412                                                             
*                                                                               
         LA    R6,BUDEMS                                                        
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
B415     MVC   KEY(13),SVKEY                                                    
         MVC   KEY+11(2),0(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DELERR1                                                          
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVPOLPRD,0          TEST BRAND POL BY BRAND                      
         BE    B417                NO                                           
         CLC   SVPOLPRD,BDMASPRD   MAKE SURE BRANDS MATCH                       
         BE    B417                                                             
         MVI   ERRCD,PRDERR                                                     
         B     DELERR2                                                          
*                                                                               
B417     BAS   RE,DELTEST                                                       
         BNE   DELERR2                                                          
*                                                                               
         LA    R6,2(R6)                                                         
         OC    0(2,R6),0(R6)                                                    
         BNZ   B415                                                             
         EJECT                                                                  
* OK TO DELETE                                                                  
         LA    R6,BUDEMS                                                        
B420     MVC   KEY(13),SVKEY                                                    
         MVC   KEY+11(2),0(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,DELBUY                                                        
*                                                                               
         BAS   RE,DELPKG                                                        
         BAS   RE,DELMOV           DELETE BUY MOVE REFERENCE                    
         LA    R6,2(R6)                                                         
         OC    0(2,R6),0(R6)                                                    
         BNZ   B420                                                             
*                                                                               
B425     MVC   BUYMSG(21),=C'** RECORDS DELETED **'                             
         CLC   =C'DM',BUTRCODE                                                  
         BE    *+8                                                              
         MVI   BUYMSG+9,C' '                                                    
         XC    SVKEY+14(4),SVKEY+14  CLEAR SO NO MORE UPDATES                   
         B     EXIT                                                             
B450     DC    H'0'                                                             
         EJECT                                                                  
* CANAD NTWK DELETE                                                             
*                                                                               
B500     MVI   ERRCD,NORECALL                                                   
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    BUYERR                                                           
*                                                                               
         CLC   =C'DEL',0(R4)                                                    
         BNE   B404                TELL THEM HOW TO DELETE                      
*                                                                               
B502     MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DELERR1                                                          
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,DELTEST                                                       
         BNE   DELERR2                                                          
*                                                                               
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   B504                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(UNREF1ST)                                            
         B     BUYERR                                                           
*                                                                               
* MOVE REC TO REC2                                                              
*                                                                               
B504     MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC2      TO                                           
         GOTO1 MOVEREC                                                          
* NOW DELETE EXPLODED BUYS                                                      
         L     R7,AREC2                                                         
         LA    R7,24(R7)                                                        
B510     ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    B520                                                             
         CLI   0(R7),X'68'                                                      
         BNE   B510                                                             
         MVC   KEY,SVKEY           RESTORE THE KEY STUPID                       
         MVC   KEY+4(5),2(R7)      MOVE MKT/STA                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   B510                WHY FIGHT - IT'S NOT THERE                   
         GOTO1 GETREC                                                           
         BAS   RE,DELTEST                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DELBUY                                                        
         B     B510                                                             
*                                                                               
* NOW DELETE NTWK BUY                                                           
*                                                                               
B520     MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         BAS   RE,DELBUY                                                        
         B     B425                                                             
         EJECT                                                                  
DELBUY   NTR1                                                                   
         BAS   RE,TSTDARE                                                       
         BAS   RE,TSTLOCK                                                       
         BE    DELBUY2                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DATALOCK)                                              
         B     BUYERR                                                           
*                                                                               
DELBUY2  OC    SVPASSWD,SVPASSWD   TEST PASSWORD PROTECT ACTIVE                 
         BZ    DELBUY10                                                         
         GOTO1 VDATCON,DMCB,(5,0),(3,SVTODAYB)                                  
* CREATE OR UPDATE ACTIVITY ELEMENT                                             
         L     R6,AREC                                                          
         MVI   ELCDLO,X'99'                                                     
         MVI   ELCDHI,X'99'                                                     
         LA    R6,24(R6)                                                        
         BAS   RE,NEXTEL                                                        
         BE    DELBUY8                                                          
* CREATE NEW ELEM                                                               
         MVI   DUB,X'99'           DUB IS AVAILABLE                             
         MVI   DUB+1,12             WILL CLEAR AFTER INSERTION                  
         GOTO1 VRECUP,DMCB,AREC,DUB,(R6)                                        
         XC    2(10,R6),2(R6)                                                   
         MVC   2(2,R6),SVPASSWD                                                 
         MVC   4(3,R6),SVTODAYB                                                 
         B     DELBUY10                                                         
DELBUY8  DS    0H                                                               
* UPDATE EXISTING ELEM                                                          
         MVC   7(2,R6),SVPASSWD                                                 
         MVC   9(3,R6),SVTODAYB                                                 
*                                                                               
DELBUY10 DS    0H                                                               
         L     R6,AREC                                                          
         TM    BDCIND2,X'02'       TEST CTA ACTIVE                              
         BO    *+12                                                             
         TM    BDSTAT2,X'20'       TEST NON-CTA TRADE                           
         BZ    DELBUY12                                                         
         GOTO1 GOGETCTA,DMCB,('CIBCPYQ',AREC)                                   
*                                                                               
DELBUY12 DS    0H                                                               
         GOTO1 HIGH                READ SPTDIR POINTER                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        SET DELETED IND                              
* UPLOAD CLIENTS CANNOT PURGE DELETED RECORDS                                   
         TM    SVCOPT1,X'02'                                                    
         BZ    *+8                                                              
         OI    KEY+13,X'20'        SET DO NOT PURGE FLAG                        
         MVI   GBYACT,C'W'                                                      
         GOTO1 DIR                                                              
* NOW SET FILE RECORD DELETED                                                   
         L     RE,AREC                                                          
         OC    15(1,RE),KEY+13     COPY DIRECTORY BITS TO FILE                  
         GOTO1 PUTREC                                                           
         OI    SVUPDATE,X'80'      SET BUY CHANGE FLAG                          
*                                                                               
         TM    BDCIND2,X'02'       TEST CTA ACTIVE                              
         BO    *+12                                                             
         TM    BDSTAT2,X'20'       TEST NON-CTA TRADE                           
         BZ    DELBUY14                                                         
         GOTO1 GOGETCTA,DMCB,('CIBCHGQ',AREC)                                   
*                                                                               
DELBUY14 DS    0H                                                               
         GOTO1 VBLDQLST            BUILD REQ PRD LIST                           
         B     EXIT                                                             
         SPACE 2                                                                
DELERR1  MVC   BUYMSG(30),=C'* ERROR * BUY RECORD NOT FOUND'                    
         LA    R4,BUYMSG+32                                                     
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         B     DELERRX                                                          
*                                                                               
DELERR2  MVC   BUYMSG(39),=C'* ERROR * LINE=     CANNOT BE DELETED -'           
         LA    R4,BUYMSG+10                                                     
         MVC   BUYMSG+41(7),=C'MATCHED'                                         
         CLI   ERRCD,MATCHED                                                    
         BNE   DELERR10                                                         
         CLI   ERRAREA,X'72'       TEST SPECIAL IND FOR COUNTS PRESENT          
         BNE   DELERRX                                                          
         MVC   BUYMSG+41(14),=C'COUNTS PRESENT'                                 
         B     DELERRX                                                          
*                                                                               
DELERR10 MVC   BUYMSG+41(9),=C'MADE-GOOD'                                       
         CLI   ERRCD,MADEGOOD                                                   
         BE    DELERRX                                                          
         MVC   BUYMSG+41(9),=C'MAKE-GOOD'                                       
         CLI   ERRCD,NODELMG                                                    
         BE    DELERRX                                                          
         MVC   BUYMSG+41(11),=C'BILLED/PAID'                                    
         CLI   ERRCD,BLLDPAID                                                   
         BE    DELERRX                                                          
         MVC   BUYMSG+41(13),=C'WRONG PRODUCT'                                  
         CLI   ERRCD,PRDERR                                                     
         BE    DELERRX                                                          
         MVC   BUYMSG+41(14),=C'PKG/REF MASTER'                                 
         CLI   ERRCD,PKGMSTR                                                    
         BE    DELERRX                                                          
         MVC   BUYMSG+41(15),=C'DARE MG PENDING'                                
         CLI   ERRCD,DARMGPND                                                   
         BE    DELERRX                                                          
         MVC   BUYMSG+41(19),=C'REFD BY NEXT PKG LN'                            
         CLI   ERRCD,NODELIND                                                   
         BE    DELERRX                                                          
         MVC   BUYMSG+41(19),SPACES                                             
         MVC   BUYMSG+41(7),=C'UNKNOWN'                                         
*                                                                               
DELERRX  MVC   0(5,R4),=C'LINE='                                                
         SR    R0,R0                                                            
         ICM   R0,3,KEY+11                                                      
         EDIT  (R0),(4,5(R4)),ALIGN=LEFT                                        
         MVI   ERRAREA,X'FF'                                                    
         OI    6(R2),X'40'         SET CURSOR POSN                              
* SQUASH OUTPUT                                                                 
         MVC   DMCB+4(4),=X'D9000A0D'   SQUASHER                                
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),BUYMSG,60                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE BUY IS LOCKED OUT BY DARE                   
*                                                                               
* ON ENTRY:    AREC                A(BUY RECORD)                                
***********************************************************************         
TSTDARE  NTR1                                                                   
         MVI   SVDRFLG2-SVDARED(RE),0    RESET NO DARE TEST FLAG                
         L     R6,AREC                                                          
         CLI   0(R6),X'10'         MAKE SURE THIS IS A BUY                      
         BL    TDAREX                                                           
*                                                                               
TDARE10  MVI   ELCODE,X'06'        MUST USE BUY ELEMENTS                        
         MVI   ELCODE,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
TDARE20  BAS   RE,NEXTEL                                                        
         BNE   TDAREX                                                           
         L     RE,ASVDARE          TEST ELEM IN LOCKED DARE FLIGHT              
         ST    R6,SVDRELEM-SVDARED(RE)                                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
         B     TDARE20                                                          
*                                                                               
TDAREX   L     R4,ASVDARE                                                       
         USING SVDARED,R4                                                       
         OI    SVDRFLG2,SVDRFLG2_IGN     SUPPRESS DARE TEST ON EXIT             
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
DELTEST  NTR1                                                                   
*                                                                               
         TM    BDSTAT,X'01'        TEST NETPAK                                  
         BZ    DELTEST0                                                         
* TEST PKG FROZEN                                                               
         MVI   ELCDLO,X'94'                                                     
         MVI   ELCDHI,X'94'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ERRCD,PKGFRZN                                                    
         TM    2(R6),X'A0'                                                      
         BNZ   BUYERR                                                           
DELTEST0 DS    0H                                                               
         TM    SVESTFL1,EF1NMG     TEST NEW MAKEGOODS ALLOWED                   
         BNO   DELTST0A            NO                                           
         MVI   ERRCD,NODELMG                                                    
         CLI   BDMGDATE,X'C0'      TEST THIS IS A NEW MAKEGOOD                  
         BH    NEQXIT              YES - DON'T ALLOW                            
*                                                                               
DELTST0A MVI   ELCDLO,X'05'                                                     
         MVI   ELCDHI,X'05'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   DELTEST1                                                         
         MVI   ERRCD,PKGMSTR                                                    
         TM    2(R6),X'01'                                                      
         BO    NEQXIT                                                           
         MVI   ERRCD,NODELMG                                                    
         CLI   2(R6),8             TEST MAKE-GOOD SLAVE                         
         BE    NEQXIT                                                           
         BAS   RE,TESTREF          TEST FOR INDIRECT REFERENCES                 
         BNE   NEQXIT                                                           
DELTEST1 DS    0H                                                               
*                                                                               
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
DELTEST2 BAS   RE,NEXTEL                                                        
         BNE   EQXIT                                                            
*                                                                               
         MVI   ERRCD,BLLDPAID                                                   
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BNZ   NEQXIT                                                           
         MVI   ERRCD,DARMGPND                                                   
         TM    6(R6),X'10'         TEST DARE MG PENDING                         
         BO    NEQXIT                                                           
         BAS   RE,TESTMTCH                                                      
         BNE   NEQXIT                                                           
         MVI   ERRCD,MADEGOOD                                                   
         TM    6(R6),X'02'                                                      
         BO    NEQXIT                                                           
         B     DELTEST2                                                         
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE BUY IS LOCKED                               
* ON ENTRY:    AREC                A(BUY RECORD)                                
***********************************************************************         
TSTLOCK  NTR1                                                                   
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         MVC   L.LOCKSTA,QSTA                                                   
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
* TEST ALLOCATION KEYS                                                          
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         SR    R0,R0                                                            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LOCKEST,DUB                                                    
         MVC   DUB(3),L.LOCKEST                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         CLI   SVAPROF+7,C'C'      FOR CAN, MAKE SURE MED C NOT LOCKED          
         BNE   TSTLKEQ                                                          
         CLI   BUYMD,C'T'                                                       
         BE    *+12                                                             
         CLI   BUYMD,C'N'                                                       
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         MVC   L.LOCKEST,DUB                                                    
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
*                                                                               
TSTIT2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REMOVE THIS LINE FROM PACKAGE/REF MASTER LIST                                 
***********************************************************************         
*                                                                               
DELPKG   NTR1                                                                   
*                                                                               
         MVC   HALF(2),KEY+11      SAVE SLAVE LINE NUMBER                       
         XC    KEY,KEY                                                          
         MVC   KEY(11),SVKEY                                                    
* FIND MASTER LINE NUM                                                          
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
DELPKG1  SR    R0,R0                                                            
         LLC   R0,3(R6)            GET 1-BTYE LINE NUM                          
         TM    2(R6),X'10'         TEST ELEM HAS 2-BYTE LINENUMS                
         BZ    *+8                                                              
         ICM   R0,3,3(R6)          GET 2-BYTE LINE NUM                          
         STCM  R0,3,KEY+11         MASTER LINE NUM                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
* FIND PKG MASTER ELEM                                                          
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    2(R6),X'01'         TEST THIS IS A MASTER LINE                   
         BZ    DELPKG1             NO - READ AGAIN FOR MASTER                   
         XC    ELEM,ELEM           MOVE DATA TO ELEM                            
         LLC   RE,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6) *EXECUTED*                                         
*                                                                               
         LA    R7,ELEM+3           FIRST SLAVE                                  
*                                                                               
DELPKG2  LLC   R0,0(R7)            GET 1-BYTE LINE NUMBER                       
                                                                                
         BZ    *+8                                                              
         ICM   R0,3,0(R7)                                                       
         CH    R0,HALF                                                          
         BE    DELPKG4                                                          
         LA    R7,1(R7)                                                         
         TM    ELEM+2,X'10'                                                     
         BZ    *+8                                                              
         LA    R7,1(R7)                                                         
         B     DELPKG2                                                          
*                                                                               
DELPKG4  TM    ELEM+2,X'10'        TEST 2-BYTE LINENUMS IN ELEM                 
         BO    DELPKG10                                                         
*                                                                               
         MVC   0(1,R7),1(R7)       MOVE NEXT OVER IT                            
         LA    R7,1(R7)                                                         
         CLI   0(R7),0             TEST FOR MORE                                
         BNE   DELPKG4                                                          
         IC    R0,ELEM+1           GET OLD LEN                                  
         BCTR  R0,0                                                             
         STC   R0,ELEM+1                                                        
         B     DELPKG20                                                         
*                                                                               
DELPKG10 MVC   0(2,R7),2(R7)       MOVE NEXT OVER IT                            
         LA    R7,2(R7)                                                         
         OC    0(2,R7),0(R7)       TEST FOR MORE                                
         BNZ   DELPKG10                                                         
         IC    R0,ELEM+1           GET OLD LEN                                  
         AHI   R0,-2                                                            
         STC   R0,ELEM+1                                                        
*                                                                               
DELPKG20 GOTO1 VRECUP,DMCB,BUYREC,(R6)   DELETE OLD ELEM                        
*                                                                               
         CLI   ELEM+1,3            TEST NEED TO INSERT NEW ELEM                 
         BNH   DELPKG22                                                         
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
DELPKG22 GOTO1 PUTREC                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REMOVE BUY MOVE FROM X'92' ELEM IF THIS HAS BUY MOVED TO X'93' ELEM           
***********************************************************************         
*                                                                               
*  NOT CONCERNED WITH DELPKG OVERWRITING BUYREC                                 
*                                                                               
DELMOV   NTR1                                                                   
         MVI   HALF,0                                                           
         L     R6,AREC             DELETED BUY                                  
         MVC   HALF,10(R6)         SAVE DELETED LINE NUMBER                     
*                                                                               
         MVI   ELCDLO,X'93'        BUY MOVED TO THIS BUY?                       
         MVI   ELCDHI,X'93'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                NOPE GET OUT                                 
*                                                                               
         MVC   WORK(MOVLENQ),0(R6) SAVE X'93' ELEM                              
*                                                                               
         USING MOVELEM,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         ST    RF,VCLPACK          WASN'T SET ?                                 
*                                                                               
         GOTO1 VCLPACK,DMCB,MOVQCLT,KEY+1                                       
         MVI   KEY+3,X'FF'                                                      
         MVC   KEY+9(1),MOVBEST                                                 
         MVI   KEY+10,0                                                         
         MVI   KEY+11,0            ONLY SUPPORT 1-BYTE LINENUMS                 
         MVC   KEY+12(1),MOVLIN                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                MOVED FROM BUY IS GONE!                      
*                                                                               
         GOTO1 GETREC                                                           
* FIND MOVED FROM ELEM                                                          
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'92'        BUY MOVED FROM ELEM                          
         MVI   ELCDHI,X'92'                                                     
DELMOV10 BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING MOVELEM,R6          CHECK RIGHT FROM ELEM                        
         GOTO1 VCLPACK,DMCB,MOVQCLT,WORK                                        
         CLC   WORK(2),SVKEY+1     RIGHT CLIENT                                 
         BNE   DELMOV10                                                         
*                                                                               
         CLC   SVKEY+9(1),MOVBEST  RIGHT ESTIMATE                               
         BNE   DELMOV10                                                         
*                                                                               
         CLC   MOVLIN,HALF+1       TEST RIGHT LINE                              
         BNE   DELMOV10            * LINE NOT IN SVKEY FOR DM                   
*                                                                               
DELMOV30 GOTO1 VRECUP,DMCB,BUYREC,(R6)      DELETE ELEM                         
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*===================================================================*           
* READ OLD PACKAGE MASTER LINE TO TEST FOR REFERENCES TO THIS LINE. *           
* IF FOUND, TELL USER TO DELETE REFERENCES FIRST                    *           
* ON ENTRY ELCDLO=ELCDHI=X'05' AND R6 POINTS TO PKGEL IN SLAVE LINE *           
*===================================================================*           
         SPACE 1                                                                
TESTREF  NTR1                                                                   
         MVC   WORK(24),KEY       SAVE SLAVE LINE KEY/DA                        
TESTREF2 SR    R0,R0               MASTER LINE NUM                              
         ICM   R0,1,3(R6)          <<<                                          
         STCM  R0,3,KEY+11                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
* FIND PKG EL                                                                   
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    2(R6),X'01'         TEST MASTER                                  
         BZ    TESTREF2            NO                                           
* FIND THIS SLAVE LINE IN MASTER PKGEL                                          
         LLC   R0,1(R6)                                                         
         AHI   R0,-3                                                            
         LA    R6,3(R6)                                                         
         SR    RE,RE                                                            
TESTREF3 ICM   RE,1,0(R6)          <<< GET LINE NUMBER                          
         CLM   RE,3,WORK+11                                                     
         BE    TESTREF4                                                         
         LA    R6,1(R6)            <<<                                          
         BCT   R0,TESTREF3                                                      
         DC    H'0'                                                             
*                                                                               
TESTREF4 AHI   R0,-1               ADJUST LINES LEFT IN PACKAGE                 
         BZ    TESTREF6            NO - OURS IS LAST                            
                                                                                
* MORE LINES - SEE IF NEXT ONE POINTS TO US                                     
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)          <<<                                          
         STCM  R0,3,KEY+11                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,1,3(R6)  <<<                                                  
         CLM   R0,3,WORK+11                                                     
         BNE   TESTREF6                                                         
         MVC   KEY(20),WORK        RESTORE KEY FOR ERROR ROUTINE                
         GOTO1 GETREC                                                           
         MVI   ERRCD,NODELIND                                                   
         B     NEQXIT                                                           
*                                                                               
TESTREF6 MVC   KEY(20),WORK       RESTORE KEY/DA                                
         GOTO1 HIGH                RESET DIRECTORY                              
         GOTO1 GETREC                                                           
         B     EQXIT                                                            
         EJECT                                                                  
*===============================================================*               
* TEST FOR AFFIDS FOR ELEM AT 0(R6).                                            
* ASSUME ELCDLO AND ELCDHI CONTAIN REGEL ARGUMENTS                              
*===============================================================*               
         SPACE 1                                                                
TESTMTCH NTR1                                                                   
         MVI   ERRCD,MATCHED                                                    
         LR    R5,R6               SAVE EL ADDRESS                              
TMTCH2   BAS   RE,NEXTEL                                                        
         BNE   TMTCH4                                                           
         CLC   2(2,R5),2(R6)       TEST SAME DATE                               
         BE    TMTCH2                                                           
TMTCH4   LR    R7,R6               R7 IS END OF SRCH                            
         BCTR  R7,0                                                             
TMTCH6   CLI   0(R5),X'10'         TEST AFFID                                   
         BE    NEQXIT              EXIT WITH CC NOT EQ IF MATCHED               
         ZIC   R6,1(R5)                                                         
         BXLE  R5,R6,TMTCH6                                                     
         B     EQXIT                                                            
         LTORG                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
MAXDEL   EQU   24                  MAX N'LINES CAN DELETE AT ONCE               
       ++INCLUDE SPBUYWORK                                                      
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
 END                                                                            
