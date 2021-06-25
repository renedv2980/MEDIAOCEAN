*          DATA SET SPOMS0C    AT LEVEL 003 AS OF 01/03/07                      
*PHASE T2340CA                                                                  
*INCLUDE COVAIL                                                                 
T2340C   TITLE 'SPOMS0C - ACTIVITY REPORT'                                      
T2340C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2340C*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
         MVC   DATADISP,=H'24'                                                  
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A7D',0  TSAR                                
         CLI   4(R1),X'FF'         ERROR GETTING A(TSAROFF)                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAROFF,0(R1)                                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BE    PR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         XC    FILTRFLG,FILTRFLG                                                
****                                                                            
* VALIDATE MEDIA                                                                
****                                                                            
VKMED    DS    0H                                                               
         LA    R2,ORAMEDH                                                       
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0             NEED THE MEDIA                               
         BE    NEEDFLDS                                                         
         GOTO1 VALIMED                                                          
VKMEDX   OI    4(R2),X'20'                                                      
*                                                                               
         BAS   RE,GETAGYMD         GET SVAGYMD INFO                             
****                                                                            
* VALIDATE DATE FILTER                                                          
****                                                                            
VKDATE   DS    0H                                                               
         LA    R2,ORADATH                                                       
*                                                                               
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    NEEDFLDS                                                         
*                                                                               
         LA    R3,8(R2)                                                         
         ICM   R3,8,5(R2)                                                       
         LA    R4,PERVALST                                                      
         GOTO1 PERVAL,DMCB,(R3),(R4)                                            
*                                                                               
         TM    DMCB+4,X'03'                                                     
         BNZ   INVLDATE                                                         
         TM    DMCB+4,X'04'                                                     
         BNO   VKDAT40                                                          
         USING PERVALD,R4                                                       
*                                                                               
* ONLY 1 DATE INPUT                                                             
*                                                                               
*KDAT10  MVC   8(8,R2),PVALCPER    TO MUCH TROUBLE WRITING TO SCREEN            
VKDAT10  MVI   5(R2),8             MUST FORCE LENGTH                            
         OI    6(R2),X'80'                                                      
         MVC   ENDDATE,=6X'FF'                                                  
         MVC   STTDATE,PVALESTA                                                 
         B     VKDATX                                                           
*                                                                               
*        MVC   ORADAT,PVALCPER     TO MUCH TROUBLE WRITING TO SCREEN            
VKDAT40  DS    0H                                                               
         MVI   5(R2),17            MUST FORCE LENGTH                            
         OI    6(R2),X'80'                                                      
         MVC   STTDATE,PVALESTA                                                 
         MVC   ENDDATE,PVALEEND                                                 
*                                                                               
VKDATX   OI    4(R2),X'20'                                                      
****                                                                            
* VALIDATE CLIENT                                                               
****                                                                            
VKCLT    DS    0H                                                               
         LA    R2,ORACLTH                                                       
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKCLTX                                                           
*                                                                               
         CLI   9(R2),C'-'          CLIENT GROUP?                                
         BE    VKCLT20                                                          
         CLI   10(R2),C'-'                                                      
         BE    VKCLT20             YES                                          
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   FLTCLT,BCLT                                                      
         OI    FILTRFLG,FFLGCLT                                                 
         B     VKCLTX                                                           
*                                                                               
VKCLT20  LA    R4,KEY              VALIDATE ID/SCHEME                           
         XC    KEY,KEY                                                          
         USING GRPKEY,R4                                                        
         MVI   GRPKTYP,GRPKTYPQ    X'0D' RECORD TYPE                            
         MVI   GRPKSTYP,GRPKCTYQ   X'04' CLIENT GROUP SUBTYPE                   
         MVC   GRPKAGMD,SVBAGYMD                                                
*                                                                               
         MVC   FLTGRPID,SPACES                                                  
         MVC   FLTGRPID(1),8(R2)      SAVE SCHEME                               
VKCLT30  MVC   FLTGRPI1,8(R2)                                                   
         MVC   GRPKID,FLTGRPI1     PUT ID IN KEY                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'GRPKEY),KEYSAVE                                            
         BNE   RECNTFND            ERROR: RECORD NOT FOUND                      
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,GRPBK2LN                                                
         XC    BREAK3LN,BREAK3LN                                                
*                                                                               
         XC    FLTGRPCD,FLTGRPCD                                                
         LA    R3,2                                                             
         CLI   FLTGRPID+1,C' '                                                  
         BNH   *+8                                                              
         LA    R3,1(R3)            INC BY 1                                     
         CLM   R3,1,5(R2)          INPUT CGROUP SCHEME ONLY?                    
         BE    VKCLT50             YES                                          
         ZIC   R1,BREAK1LN                                                      
         ZIC   R0,BREAK2LN                                                      
         AR    R1,R0                                                            
         AR    R1,R3                                                            
         CLM   R1,1,5(R2)                                                       
         BNE   INVLFLD             MUST BE SPECIFIC CGROUP                      
         LA    R3,8(R3,R2)                                                      
         MVC   FULL,0(R3)          GROUP CODES ARE LEFT-JUSTIFIED, PWOS         
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    INVLFLD             AND MUST BE NON-ZERO                         
         STCM  R0,3,FLTGRPCD                                                    
         MVC   GRPKCODE,FLTGRPCD                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(GRPKMSQL),KEYSAVE                                            
         BNE   INVLFLD                                                          
VKCLT50  OI    FILTRFLG,FFLGCGRP                                                
*                                                                               
VKCLTX   OI    4(R2),X'20'                                                      
****                                                                            
* VALIDATE MARKET                                                               
****                                                                            
VKMKT    DS    0H                                                               
         LA    R2,ORAMKTH                                                       
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKMKTX                                                           
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    VKMKT20             NO, CHECK MGROUP                             
*                                                                               
         XR    R1,R1               SAVE THE MARKET NUMBER                       
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,FLTMKT                                                      
         OI    FILTRFLG,FFLGMKT                                                 
         B     VKMKTX                                                           
*                                                                               
VKMKT20  MVC   HALF,8(R2)                                                       
         CLI   5(R2),1             ONLY ENTERED A SCHEME?                       
         BE    VKMKT24                                                          
         CLI   HALF+1,C'A'                                                      
         BL    INVLFLD                                                          
         CLI   HALF+1,C'Z'                                                      
         BNH   VKMKT25                                                          
VKMKT24  MVI   HALF+1,C' '                                                      
VKMKT25  LA    RE,SPMGRTAB                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
VKMKT30  CLC   HALF,0(RE)                                                       
         BE    VKMKT40                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,VKMKT30                                                       
         B     INVLFLD                                                          
*                                                                               
VKMKT40  TM    FILTRFLG,FFLGCGRP   DO WE HAVE A CGROUP FILTER?                  
         BNZ   INVLFLD             CANNOT HAVE BOTH                             
         MVC   FLTGRPID,0(RE)                                                   
         MVC   FLTGRPI1,2(RE)                                                   
*                                                                               
         TM    FILTRFLG,FFLGCLT    CLIENT FILTER?                               
         BNZ   VKMKT50                                                          
         CLI   FLTGRPID,C'G'       CLT 'ALL', MGRID LESS THAN 'G'?              
         BL    INVLFLD             ONLY INPUT 'ALL' IF MGRID > G                
         B     VKMKT60                                                          
VKMKT50  CLI   FLTGRPID,C'F'       ONLY A-F IF CLIENT INPUT                     
         BH    INVLFLD                                                          
*                                                                               
VKMKT60  LA    R4,KEY              VALIDATE ID/SCHEME                           
         XC    KEY,KEY                                                          
         USING MKGKEY,R4                                                        
         MVC   MKGKTYP,=X'0D02'    TYPE/SUBTYPE                                 
         MVC   MKGKAGMD,SVBAGYMD                                                
         TM    FILTRFLG,FFLGCLT    CLIENT FILTER?                               
         BZ    *+10                                                             
         MVC   MKGKCLT,FLTCLT      YES                                          
         MVC   MKGKMID,FLTGRPI1    PUT ID IN KEY                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   RECNTFND            ERROR: RECORD NOT FOUND                      
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        MKTGRP BREAK DESCRIPTION                     
         BAS   RE,GETEL                                                         
         USING MKGEL01,R6                                                       
         MVC   BREAK1LN,MKGBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,MKGBK2LN                                                
         MVC   BREAK3LN,MKGBK3LN                                                
*                                                                               
         XC    FLTGRPCD,FLTGRPCD                                                
         LA    R3,1                                                             
         CLI   FLTGRPID+1,C' '                                                  
         BNH   *+8                                                              
         LA    R3,1(R3)                                                         
         CLM   R3,1,5(R2)          INPUT CGROUP SCHEME ONLY?                    
         BE    VKMKT80             YES                                          
         ZIC   R1,BREAK1LN                                                      
         ZIC   R0,BREAK2LN                                                      
         AR    R1,R0                                                            
         ZIC   R0,BREAK3LN                                                      
         AR    R1,R0                                                            
         AR    R1,R3                                                            
         CLM   R1,1,5(R2)                                                       
         BNE   INVLFLD             MUST BE SPECIFIC CGROUP                      
         LA    R3,8(R3,R2)                                                      
         MVC   FULL,0(R3)          GROUP CODES ARE LEFT-JUSTIFIED, PWOS         
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    INVLFLD             AND MUST BE NON-ZERO                         
         STCM  R0,3,FLTGRPCD                                                    
         MVC   MKGKMGRP,FLTGRPCD                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   INVLFLD                                                          
VKMKT80  OI    FILTRFLG,FFLGMGRP                                                
*                                                                               
VKMKTX   OI    4(R2),X'20'                                                      
****                                                                            
* VALIDATE BUYER                                                                
****                                                                            
*&&DO                                                                           
VKBYR    DS    0H                                                               
         LA    R2,ORABYRH                                                       
         TM    4(R2),X'20'         DID THIS FIELD CHANGE?                       
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKBYRX                                                           
*                                                                               
         GOTO1 VALIBUYR,DMCB,8(R2) VALIDATE IT                                  
         BNE   INVLFLD                                                          
         MVC   FLTBUYR,8(R2)                                                    
         OC    FLTBUYR,SPACES                                                   
*                                                                               
VKBYRX   OI    4(R2),X'20'                                                      
*&&                                                                             
*                                                                               
VKXIT    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
*                                                                               
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 TOTSAR,TSRINIQ      INITIALIZE TSAR                              
* INIT                                                                          
         XC    CURCLT,CURCLT                                                    
         XC    CURMKT,CURMKT                                                    
         XC    CURCGPCD,CURCGPCD                                                
         XC    TOTBUYS,TOTBUYS                                                  
         XC    TOTORDRS,TOTORDRS                                                
         XC    TOTTRDFL,TOTTRDFL                                                
         XC    TOTBYORD,TOTBYORD                                                
         XC    GRPBUYS,GRPBUYS                                                  
         XC    GRPORDRS,GRPORDRS                                                
         XC    GRPBYORD,GRPBYORD                                                
         XC    AGTBUYS,AGTBUYS                                                  
         XC    AGTORDRS,AGTORDRS                                                
         XC    AGTBYORD,AGTBYORD                                                
         XC    NUMCCG,NUMCCG                                                    
*                                                                               
         MVI   RCSUBPRG,0                                                       
*                                                                               
         TM    FILTRFLG,FFLGMGRP   MGROUP FILTER?                               
         BZ    PR001                                                            
         BAS   RE,RDMGROUP                                                      
         MVI   RCSUBPRG,2                                                       
*                                                                               
PR001    TM    FILTRFLG,FFLGCGRP   CGROUP FILTER?                               
         BZ    PR002                                                            
         BAS   RE,RDCGROUP                                                      
         MVI   RCSUBPRG,1                                                       
*                                                                               
PR002    LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         USING BUYRECD,R2                                                       
         MVC   BUYKAM,SVBAGYMD                                                  
         MVC   BUYKPRD,X'FF'       ONLY READ POL PRODUCTS                       
         TM    FILTRFLG,FFLGCLT    CLIENT FILTER?                               
         BZ    PR004                                                            
         MVC   BUYKCLT,FLTCLT                                                   
*                                                                               
PR004    TM    FILTRFLG,FFLGCGRP   CGROUP FILTER?                               
         BZ    PRRDHI                                                           
         L     R3,AGROUP                                                        
         MVC   BUYKCLT,0(R3)                                                    
*                                                                               
PRRDHI   MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
PR005    CLC   BUYKAM,SVBAGYMD     DID THE MEDIA CHANGE?                        
         BNE   PRTSRRDV            YES, WRITE TO TSAR, SPOOL, THEN EXIT         
*                                                                               
         CLC   BUYKCLT,KEYSAVE+BUYKCLT-BUYRECD CLIENT CHANGED?                  
         BE    PR010               NO                                           
         TM    FILTRFLG,FFLGCLT    CLIENT FILTER?                               
         BNZ   PRTSRRDV            YES, SPOOL AND EXIT                          
         TM    FILTRFLG,FFLGCGRP+FFLGCGRP                                       
         BZ    PRTSRRDX                                                         
*                                                                               
PR010    TM    FILTRFLG,FFLGCGRP   CGROUP FILTER?                               
         BZ    PR020               NO                                           
         OC    CURCLT,CURCLT       YES, DID WE JUST CLEAR IT?                   
         BNZ   PRTSRRDX            NO, SPOOL                                    
*                                                                               
         TM    FILTRFLG,FFLGCGRP   CGROUP FILTER?                               
         BZ    PR020               NO, SKIP TEST                                
         XC    WORK,WORK                                                        
         MVC   WORK(2),BUYKCLT                                                  
*                                                                               
         GOTO1 BINSRCH,CMLPAR1,(0,WORK),AGROUP                                  
         CLI   0(R1),1             TEST RECORD FOUND                            
         BE    PR030               NO, NEXT CLIENT                              
         MVC   ACURCODE,0(R1)      SAVE ADDRESS                                 
*                                                                               
PR020    CLI   BUYKPRD,X'FF'       DO WE HAVE A POL PRODUCT?                    
         BE    PR025               YES                                          
         MVI   BUYKPRD,X'FF'       READ THE POL PRODUCT AND                     
         XC    BUYKMSTA(9),BUYKMSTA  CLEAR EVERYTHING BEYOND PRODUCT            
         B     PRRDHI                                                           
*                                                                               
PR025    MVC   CURCLT,BUYKCLT      SAVE THE CURRENT CLIENT                      
*                                                                               
         TM    MISCFLG1,MF1BDEST                                                
         BO    PR040                                                            
         MVC   SVBUYKEY,KEY                                                     
         BAS   RE,BLDEST           BUILD TABLE OF VALID CURCLT/PRD/EST          
         MVC   KEY,SVBUYKEY                                                     
         OC    ESTTAB,ESTTAB       ANY VALID ESTIMATES?                         
         BNZ   PR035                                                            
PR030    MVC   BUYKPRD(6),=6X'FF'  READ NEXT CLIENT                             
         B     PRTSRRDX            NONE, NEXT KEY                               
*                                                                               
PR035    GOTO1 HIGH                RESTORE THE BUY SEQUENCE                     
         OI    MISCFLG1,MF1BDEST   DON'T BUILD AGAIN UNTIL NEW CLIENT           
         B     PR042               AND MARKET DEFINITELY CHANGE, SKIP           
*                                                                               
PR040    CLC   BUYKMKTN,KEYSAVE+BUYKMKTN-BUYRECD DID THE MARKET CHANGE?         
         BE    PR060                                                            
*                                                                               
PR042    TM    FILTRFLG,FFLGMGRP   MGROUP FILTER?                               
         BNZ   PR055               YES, CHECK IF JUST CAME BACK?                
*                                                                               
PR045    TM    FILTRFLG,FFLGMKT    MARKET FILTER?                               
         BZ    PR055                                                            
         CLC   BUYKMKTN,FLTMKT     DO THEY MATCH?                               
         BE    PR060                                                            
         BH    PR030               NO, ITS HIGH READ THE NEXT CLIENT            
         MVC   BUYKMKTN,FLTMKT     MARKET LOW, READ CURR PRD/MKT                
         XC    BUYKSTAC,BUYKSTAC   CLEAR STATION                                
         B     PRRDHI              READ HIGH                                    
*                                                                               
PR055    OC    CURMKT,CURMKT       YES, BUT DID WE CLEAR IT?                    
         BNZ   PRTSRRDH            READ HI TSAR                                 
*                                                                               
PR060    TM    FILTRFLG,FFLGMGRP   MGROUP FILTER?                               
         BZ    PR080               NO                                           
         XC    WORK,WORK           YES, CHECK MGROUP LIST                       
         MVC   WORK(2),BUYKMKTN                                                 
*                                                                               
         GOTO1 BINSRCH,CMLPAR1,(0,WORK),AGROUP                                  
         CLI   0(R1),1             TEST MARKET FOUND?                           
         BNE   PR070               YES                                          
PR065    MVC   BUYKSTAC,=3X'FF'    READ NEXT MARKET                             
         B     PRRDHI              READ HIGH                                    
PR070    MVC   ACURCODE,0(R1)      SAVE ADDRESS OF CURRENT ENTRY                
*                                                                               
PR080    MVC   CURMKT,BUYKMKTN     SAVE THE CURRENT MARKET                      
*                                                                               
         CLI   BUYKSTAC,X'E8'      CABLE BINARY?                                
         BNL   PR065               SKIP THESE                                   
*                                                                               
         LA    RF,ESTTAB                                                        
         ZIC   RE,BUYKEST                                                       
         LA    RE,0(RE,RF)         POINT TO N'TH ENTRY                          
         CLC   BUYKEST,0(RE)       VALID ESTIMATE?                              
         BNE   PRRDHISQ                                                         
*                                                                               
         MVC   SVBUYKEY,KEY        SAVE CURRENT BUY KEY                         
*                                                                               
         LA    R0,PRDLIST          INITIALIZE PRDLIST                           
         ST    R0,APRDLIST                                                      
         LH    R1,=Y(L'PRDLIST)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    PRDNUM,PRDNUM                                                    
*                                                                               
PR085    GOTO1 GETREC                                                           
         BAS   RE,GETPRDS                                                       
         GOTO1 SEQ                                                              
         CLC   KEY(10),SVBUYKEY    A/M/CLT/X'FF'/MKT/STA/EST                    
         BE    PR085                                                            
*                                                                               
         L     R1,PRDNUM           NUMBER OR PRD1-PRD2 INCL UNALLOCATED         
         L     R0,TOTBUYS                                                       
         AR    R0,R1               ADD NUMBER OF PRD1-PRD2                      
         ST    R0,TOTBUYS          STORE TOTAL BUYS                             
*                                                                               
         BAS   RE,READORD          READ THE ORDER RECORD                        
         MVC   KEY,SVBUYKEY                                                     
*                                                                               
PRRDHISQ MVI   RDUPDATE,C'N'                                                    
         MVC   BUYKBUY,=3X'FF'     READ NEXT ESTIMATE                           
         B     PRRDHI                                                           
*                                                                               
PRTSRRDV OI    MISCFLG1,MF1RPORT   PRINT THE REPORT AND EXIT                    
*                                                                               
PRTSRRDX NI    MISCFLG1,X'FF'-MF1BDEST                                          
*                                                                               
PRTSRRDH OC    TOTBUYS,TOTBUYS                                                  
         BZ    PRSPOOL                                                          
*                                                                               
         XC    TSRKCODE,TSRKCODE                                                
         TM    FILTRFLG,FFLGCGRP+FFLGMGRP  CGROUP OR MGROUP FILTER?             
         BZ    PRTRH05                                                          
         L     R3,ACURCODE                                                      
         MVC   TSRKCODE,2(R3)      CODE                                         
PRTRH05  MVC   TSRKCLT,CURCLT      CLIENT                                       
         MVC   TSRKMKT,CURMKT      MARKET                                       
         GOTO1 TOTSAR,TSRRDHIQ     READ HIGH FOR THE ENTRY                      
         BNE   PRTSRADD            NOT FOUND, ADD IT                            
*                                                                               
         L     R0,TSRBUYS                                                       
         L     R1,TOTBUYS                                                       
         AR    R0,R1               ADD TOTAL BUYS                               
         ST    R0,TSRBUYS                                                       
*                                                                               
         L     R0,TSRORDRS                                                      
         L     R1,TOTORDRS                                                      
         AR    R0,R1               ADD TOTAL ORDERS                             
         ST    R0,TSRORDRS                                                      
*                                                                               
         L     R0,TSRTRDFL                                                      
         L     R1,TOTTRDFL                                                      
         AR    R0,R1               ADD TOTAL FLIGHTS AND TRADE                  
         ST    R0,TSRTRDFL                                                      
*                                                                               
         L     R0,TSRBYORD                                                      
         L     R1,TOTBYORD                                                      
         AR    R0,R1               ADD TOTAL BUYS WITH ORDERS                   
         ST    R0,TSRBYORD                                                      
*                                                                               
         GOTO1 TOTSAR,TSRWRTQ      WRITE IT BACK                                
         B     PRSPOOL                                                          
*                                                                               
PRTSRADD XC    TSRKCODE,TSRKCODE                                                
         TM    FILTRFLG,FFLGCGRP+FFLGMGRP  CGROUP OR MGROUP FILTER?             
         BZ    PRTA05                                                           
         L     R3,ACURCODE                                                      
         MVC   TSRKCODE,2(R3)      CODE                                         
PRTA05   MVC   TSRKCLT,CURCLT                                                   
         MVC   TSRKMKT,CURMKT                                                   
         MVC   TSRBUYS,TOTBUYS                                                  
         MVC   TSRTRDFL,TOTTRDFL                                                
         MVC   TSRORDRS,TOTORDRS                                                
         MVC   TSRBYORD,TOTBYORD                                                
         GOTO1 TOTSAR,TSRADDQ      ADD                                          
*                                                                               
PRSPOOL  TM    MISCFLG1,MF1RPORT   PRINT THE REPORT?                            
         BNZ   PRSP05              YES                                          
         XC    CURCLT,CURCLT                                                    
         XC    CURMKT,CURMKT                                                    
         XC    TOTBUYS,TOTBUYS                                                  
         XC    TOTORDRS,TOTORDRS                                                
         XC    TOTTRDFL,TOTTRDFL                                                
         XC    TOTBYORD,TOTBYORD                                                
         B     PRRDHI                                                           
**************                                                                  
* SPOOL WHATEVER IS IN TSAR                                                     
**************                                                                  
PRSP05   DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1PALL                                           
         XC    TSRKCODE,TSRKCODE                                                
         XC    TSRKCLT,TSRKCLT                                                  
         XC    TSRKMKT,TSRKMKT                                                  
         GOTO1 TOTSAR,TSRRDHIQ     GET FIRST RECORD                             
         OC    TSRKCLT,TSRKCLT     DID WE GET ANYTHING?                         
         BZ    PRSP70              NO                                           
*                                                                               
         LA    R4,P                                                             
         USING PLINED,R4                                                        
PRSP10   OC    TSRKCODE,TSRKCODE      DO WE HAVE CODE?                          
         BZ    PRSP30                 NO                                        
         CLC   CURCGPCD,TSRKCODE      DID CODE CHANGE?                          
         BE    PRSP20                 NO                                        
*                                                                               
* PRINT GROUP TOTALS                                                            
*                                                                               
PRSP11   OC    GRPBUYS,GRPBUYS        ANY GROUP TOTAL?                          
         BZ    PRSP15                 NO                                        
*                                                                               
         BAS   RE,FORCELIN                                                      
*                                                                               
         MVC   PGROUP,SAVECODE                                                  
         EDIT  GRPBUYS,PTOTBUYS,ALIGN=LEFT                                      
*                                                                               
         ICM   R3,15,GRPBYORD                                                   
         BZ    PRSP13                                                           
         EDIT  GRPORDRS,PTOTORDR,ALIGN=LEFT                                     
         EDIT  GRPTRDFL,PTRDFLT,ALIGN=LEFT                                      
         EDIT  GRPBYORD,PBUYORDR,ALIGN=LEFT                                     
         SR    R2,R2                                                            
         MHI   R3,100                                                           
         L     R1,GRPBUYS                                                       
         DR    R2,R1                                                            
PRSP13   EDIT  (R3),PBYORD,ZERO=NOBLANK,ALIGN=LEFT,TRAIL=C'%'                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    MISCFLG1,MF1PALL       PRINT ALL?                                
         BO    PRSP50                                                           
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK ON NEXT GROUP               
         XC    GRPBUYS,GRPBUYS                                                  
         XC    GRPORDRS,GRPORDRS                                                
         XC    GRPBYORD,GRPBYORD                                                
*                                                                               
PRSP15   MVC   CURCGPCD,TSRKCODE      SAVE NEW CODE                             
         ICM   R1,B'1100',TSRKCODE                                              
         SRL   R1,12                  DD DD ?? ??  =>  00 0D DD D?              
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'           00 0D DD DS                               
         UNPK  CODECHAR(5),FULL+1(3)               =>  Z0 ZD ZD ZD ZD           
*                                                                               
         MVC   SAVECODE(2),FLTGRPID                                             
         LA    R3,SAVECODE+1                                                    
         CLI   0(R3),C' '             2 CHAR SCHEME?                            
         BNH   *+8                    NO                                        
         LA    R3,1(R3)               INC BY 1                                  
*                                                                               
         ZIC   R1,BREAK1LN            L'BREAK CODES                             
         ZIC   R0,BREAK2LN                                                      
         AR    R1,R0                  L'WHOLE GROUP CODE                        
         ZIC   R0,BREAK3LN                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),CODECHAR+1                                               
PRSP20   MVC   PGROUP,SAVECODE                                                  
*                                                                               
* PRINT CURRENT LINE TOTALS                                                     
*                                                                               
PRSP30   EDIT  TSRKMKT,PMKT,FILL=0                                              
*                                                                               
         XC    KEY,KEY            *GET MKT NAME                                 
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),PMKT       11(R3) HAS MKT NUM                           
         MVC   KEY+6(2),14(RA)                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     R6,AIO                                                           
         USING MKTREC,R6                                                        
         MVC   PMKTNM(13),=C'** UNKNOWN **'                                     
         CLC   KEY(8),0(R6)                                                     
         BNE   PRSP35                                                           
         MVI   PDASH,C'-'                                                       
         MVC   PMKTNM,MKTNAME     MKTNAME TO SCREEN                             
         DROP  R6                                                               
*                                                                               
PRSP35   GOTO1 CLUNPK,DMCB,TSRKCLT,PCLT                                         
         EDIT  TSRBUYS,PTOTBUYS,ALIGN=LEFT                                      
*                                                                               
         ICM   R3,15,TSRBYORD                                                   
         BZ    PRSP40                                                           
         EDIT  TSRORDRS,PTOTORDR,ALIGN=LEFT                                     
         EDIT  TSRTRDFL,PTRDFLT,ALIGN=LEFT                                      
         EDIT  TSRBYORD,PBUYORDR,ALIGN=LEFT                                     
         SR    R2,R2                                                            
         MHI   R3,100                                                           
         L     R1,TSRBUYS                                                       
         DR    R2,R1                                                            
PRSP40   EDIT  (R3),PBYORD,ZERO=NOBLANK,ALIGN=LEFT,TRAIL=C'%'                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R1,AGTBUYS                                                       
         L     R0,TSRBUYS                                                       
         AR    R1,R0                                                            
         ST    R1,AGTBUYS          SAVE AGENCY TOTAL BUYS                       
*                                                                               
         L     R1,AGTORDRS                                                      
         L     R0,TSRORDRS                                                      
         AR    R1,R0                                                            
         ST    R1,AGTORDRS         SAVE AGENCY TOTAL ORDERS                     
*                                                                               
         L     R1,AGTTRDFL                                                      
         L     R0,TSRTRDFL                                                      
         AR    R1,R0                                                            
         ST    R1,AGTTRDFL         SAVE AGENCY TOTAL FLIGHTS AND TRADE          
*                                                                               
         L     R1,AGTBYORD                                                      
         L     R0,TSRBYORD                                                      
         AR    R1,R0                                                            
         ST    R1,AGTBYORD         SAVE AGENCY TOTAL BUYS W/ORDERS              
*                                                                               
         L     R1,GRPBUYS                                                       
         L     R0,TSRBUYS                                                       
         AR    R1,R0                                                            
         ST    R1,GRPBUYS          SAVE GROUP TOTAL BUYS                        
*                                                                               
         L     R1,GRPORDRS                                                      
         L     R0,TSRORDRS                                                      
         AR    R1,R0                                                            
         ST    R1,GRPORDRS         SAVE GROUP TOTAL ORDERS                      
*                                                                               
         L     R1,GRPTRDFL                                                      
         L     R0,TSRTRDFL                                                      
         AR    R1,R0                                                            
         ST    R1,GRPTRDFL         SAVE GROUP TOTAL FLIGHTS AND TRADE           
*                                                                               
         L     R1,GRPBYORD                                                      
         L     R0,TSRBYORD                                                      
         AR    R1,R0                                                            
         ST    R1,GRPBYORD         SAVE GROUP TOTAL BUYS W/ORDERS               
*                                                                               
         GOTO1 TOTSAR,TSRNXTQ      ANY MORE?                                    
         BE    PRSP10              YES, GO BACK AND SPOOL                       
*                                                                               
         TM    FILTRFLG,FFLGCGRP+FFLGMGRP    CLIENT/MARKET GROUP?               
         BZ    PRSP50              NO, DON'T PRINT GROUP TOTAL THEN             
         OI    MISCFLG1,MF1PALL    YES, PRINT GROUP THEN TOTAL                  
         B     PRSP11                                                           
*                                                                               
* PRINT ALL TOTALS                                                              
*                                                                               
PRSP50   BAS   RE,FORCELIN                                                      
         MVC   PGROUP(3),=C'ALL'                                                
         EDIT  AGTBUYS,PTOTBUYS,ALIGN=LEFT                                      
         ICM   R3,15,AGTBYORD                                                   
         BZ    PRSP60                                                           
         EDIT  AGTORDRS,PTOTORDR,ALIGN=LEFT                                     
         EDIT  AGTTRDFL,PTRDFLT,ALIGN=LEFT                                      
         EDIT  AGTBYORD,PBUYORDR,ALIGN=LEFT                                     
         SR    R2,R2                                                            
         MHI   R3,100                                                           
         L     R1,AGTBUYS                                                       
         DR    R2,R1                                                            
PRSP60   EDIT  (R3),PBYORD,ZERO=NOBLANK,ALIGN=LEFT,TRAIL=C'%'                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R4                                                               
*                                                                               
PRSP70   TM    FILTRFLG,FFLGCGRP+FFLGMGRP    CLIENT/MARKET GROUP?               
         BZ    XIT                                                              
*                                                                               
         L     R2,AGROUP                                                        
         SHI   R2,4                                                             
         L     R3,0(R2)                                                         
         ST    R3,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'FREE',(R2)                                     
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*===============================================================                
* GET THE AGYENCY/MEDIA INFO                                                    
*===============================================================                
GETAGYMD NTR1                                                                   
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING AGYHDR,R4                                                        
         MVI   AGYKTYPE,AGYKTYPQ   X'06'                                        
         MVC   AGYKAGY,14(RA)                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE      VALID ALPHAID?                               
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG!!!                      
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AGYMEDEQ     X'02''    MEDIA CODE ELEMENT                 
         BAS   RE,GETEL                                                         
GTAID10  BE    GTAID20                                                          
         DC    H'0'                                                             
*                                                                               
         USING AGYMEDEL,R6                                                      
GTAID20  CLC   AGYMEDCD,ORAMED                                                  
         BE    GTAID30                                                          
         BAS   RE,NEXTEL                                                        
         B     GTAID10                                                          
*                                                                               
GTAID30  MVC   SVBAGYMD,AGYMEDBT                                                
         B     XIT                                                              
         DROP  R6                                                               
*===============================================================                
* GET THE PRODUCTS AND INCREMENT TOTAL BUYS                                     
*===============================================================                
GETPRDS  NTR1                                                                   
*                                                                               
         XC    HALF,HALF                                                        
         LA    R4,HALF                                                          
         USING PRDLSTD,R4                                                       
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
GTPRD10  CLI   0(R6),0             END OF RECORD?                               
         BE    GETPRDX                                                          
*                                                                               
         CLI   0(R6),X'0B'         POOL ORIGINAL ELEMENT                        
         BE    GETPRD30                                                         
         CLI   0(R6),X'0C'         POOL OTO ELEMENT                             
         BE    GETPRD20                                                         
GETPRD15 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GTPRD10                                                          
*                                                                               
         USING REGELEM,R6                                                       
GETPRD20 TM    RSTATUS,RSMINUSQ    IS IT A +OTO?                                
         BO    GETPRD15            NO, SKIP IT                                  
*                                                                               
GETPRD25 CLI   RPPRD,0             IS IT ALLOCATED?                             
         BNE   GETPRD30                                                         
GETPRD28 MVC   HALF,=X'FFFF'       NOT ALLOCATED COUNTS AS 1 PRODUCT            
         B     GETPRD40                                                         
*                                                                               
GETPRD30 XC    HALF,HALF                                                        
         MVC   PRDL1,RPPRD         SAVE FIRST PRODUCT                           
         CLI   1(R6),14            TEST PIGGYBACK SPOT?                         
         BL    GETPRD28            NOT ALLOCATED                                
         BE    GETPRD40                                                         
         MVC   PRDL2,RPPRD+4       SAVE THE PIGGY                               
         CLC   RPPRD,RPPRD+4       IS IT SORTED CORRECTLY?                      
         BL    GETPRD40            YES                                          
GETPRD35 MVC   PRDL1,RPPRD+4       NO, SWITCH THEM                              
         MVC   PRDL2,RPPRD                                                      
         DROP  R6,R4                                                            
*                                                                               
GETPRD40 GOTO1 BINSRCH,PRDPAR1,(X'01',HALF),APRDLIST                            
         CLC   0(4,R1),=4X'00'     RECORD FULL?                                 
         BNE   GETPRD15                                                         
         DC    H'0'                INC PRDLSTMX=MAX NUMBER OF REC               
*                                                                               
GETPRDX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* READ THE ORDER RECORD                                                         
*===============================================================                
READORD  NTR1                                                                   
         LA    R2,PRDLIST                                                       
         USING PRDLSTD,R2                                                       
B        USING BUYRECD,SVBUYKEY                                                 
D        USING DOKEY,KEY                                                        
         NI    MISCFLG1,X'FF'-MF1BYORD                                          
*                                                                               
         CLC   0(2,R2),=X'FFFF'    UNALLOCATED SPOTS?                           
         BE    READORDX            YES, LAST ENTRY SO EXIT                      
*                                                                               
RORD05   XC    KEY,KEY             CLEAR IT                                     
         MVI   D.DCKTYPE,DCKTYPQ   READ THE ORDER RECORD(CLT PASSIVE)           
         MVI   D.DCKSUBTY,DCKSTYPQ                                              
         MVC   D.DCKAGMD,SVBAGYMD                                               
         MVC   D.DCKCLT,B.BUYKCLT                                               
         MVC   D.DCKPRD,PRDL1      MOVE FIRST PRODUCT                           
         MVC   D.DCKEST,B.BUYKEST                                               
         MVC   D.DCKSTA,B.BUYKSTAC                                              
         MVC   D.DCKPRD2,PRDL2     MOVE SECOND PRODUCT OR NULL                  
         DROP  B,R2                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
RORD10   CLC   KEY(DCKFLTNM-DOKEY),KEYSAVE  SAME CLT/PRD/EST/STA/PRD2?          
         BNE   RORD40              NO                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,DOSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   RORD30                                                           
*                                                                               
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,QEMPTY                                                  
         BE    RORD30                                                           
*                                                                               
         OI    MISCFLG1,MF1BYORD                                                
*                                                                               
         CLI   D.DCKFLTNM,0        FLIGHTED?                                    
         BNE   RORD24                                                           
         TM    D.DCKFLAG,DCKFTRDE                                               
         BNZ   RORD24                                                           
         MVC   SVDARKEY,KEY                                                     
         B     RORD25                                                           
*                                                                               
RORD24   L     R0,TOTTRDFL                                                      
         AHI   R0,1                                                             
         ST    R0,TOTTRDFL                                                      
         TM    D.DCKFLAG,DCKFTRDE                                               
         BNZ   RORD25                                                           
         CLC   SVDARKEY(11),KEY    IS THERE A ZERO CASH FLIGHT?                 
         BNE   RORD25                                                           
         XC    SVDARKEY,SVDARKEY   YES, ADD ONLY 1 MORE FLIGHT FOR IT           
         B     RORD24                                                           
         DROP  D                                                                
*                                                                               
RORD25   L     R0,TOTORDRS                                                      
         AHI   R0,1                                                             
         ST    R0,TOTORDRS                                                      
*                                                                               
RORD30   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     RORD10                                                           
*                                                                               
RORD40   TM    MISCFLG1,MF1BYORD                                                
         BZ    RORD50                                                           
         NI    MISCFLG1,X'FF'-MF1BYORD                                          
*                                                                               
         L     R0,TOTBYORD                                                      
         AHI   R0,1                                                             
         ST    R0,TOTBYORD                                                      
*                                                                               
RORD50   LA    R2,PRDKEYL(R2)      BUMP TO NEXT ENTRY                           
         CLC   0(2,R2),=X'0000'    END OF TABLE?                                
         BNE   RORD05              NO                                           
*                                                                               
READORDX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF ESTIMATES THAT ARE WITHIN DATE RANGE                           
***********************************************************************         
BLDEST   NTR1                                                                   
*                                                                               
         LA    R4,KEY              READ ESTIMATE RECORD                         
         USING ESTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,SVBAGYMD                                                  
         MVC   EKEYCLT,CURCLT                                                   
         MVC   EKEYPRD,=C'POL'                                                  
         MVI   EKEYEST,1                                                        
         MVC   AIO,AIO3                                                         
*                                                                               
         XC    ESTTAB,ESTTAB                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
BLDEST40 CLC   KEY(EKEYEST-EKEY),KEYSAVE   DID I GET MY RECORD L=7?             
         BNE   BLDESTX                                                          
*                                                                               
BLDEST50 MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO3                                                          
         CLC   STTDATE,EEND                                                     
         BH    BLDEST60                                                         
         CLC   ENDDATE,ESTART                                                   
         BL    BLDEST60                                                         
*                                                                               
         LA    R3,ESTTAB                                                        
         ZIC   R1,EKEYEST          R1 = ESTIMATE NUMBER                         
         LA    R3,0(R1,R3)         POINT TO N'TH ENTRY                          
         MVC   0(1,R3),EKEYEST     MOVE N'TH ESTIMATE NUMBER THERE              
*                                                                               
BLDEST60 GOTO1 SEQ                                                              
         B     BLDEST40                                                         
*                                                                               
BLDESTX  DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
NODATARQ EQU   812                 NO DATA TO REPORT                            
*                                                                               
RCNDSPLQ EQU   136                 RECORD(S) XX THRU XX OF XX DISPLAY..         
RDBKDSPQ EQU   137                 READ ORDERS BACK TO MM/DD/YY                 
MORRCDQ  EQU   138                 MORE RECORDS TO READ                         
TOOMANYQ EQU   139                 TOO MANY RECORDS TO DISPLAY                  
ENDLSTQ  EQU   140                 END OF LIST - ENTER TO RETURN TO 1ST         
SELDSPQ  EQU   64                  SELECTION DISPLAYED                          
***********************************************************************         
* READ THE CLIENT GROUP RECORD(S) AND CREATE CLIENT/CGROUP LIST                 
***********************************************************************         
RDCGROUP NTR1                                                                   
*                                                                               
         LA    R3,GRPRECL          RECORD LENGTH                                
         LHI   RE,GROUPDMX         MAX RECORDS                                  
         MR    R2,RE               MAX N'RECORDS * L'ENTRY                      
         LA    R3,4(R3)                                                         
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'  GET THE STORAGE                          
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,0(RE)            +0 = L'TABLE                                 
         S     R3,=F'4'                                                         
         LA    RE,4(RE)                                                         
         ST    RE,AGROUP                                                        
         XCEF  (RE),(R3)           CLEAR TABLE                                  
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING GRPKEY,R4                                                        
         MVI   GRPKTYP,GRPKTYPQ    X'0D'                                        
         MVI   GRPKSTYP,GRPKCTYQ   X'04' CLIENT GROUP                           
         MVC   GRPKAGMD,SVBAGYMD                                                
         MVC   GRPKID,FLTGRPID                                                  
         MVI   GRPKCODE+1,1        DON'T READ CDEF RECORD                       
         OC    FLTGRPCD,FLTGRPCD                                                
         BZ    *+10                                                             
         MVC   GRPKCODE,FLTGRPCD                                                
*                                                                               
         LA    R3,WORK                                                          
         USING GROUPD,R3                                                        
*                                                                               
RCG05    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
RCG10    CLC   KEY(GRPKCODE-GRPKEY),KEYSAVE  CLIENT SHEME CHANGED?              
         BNE   RCGX                          YES, NO MORE                       
         OC    FLTGRPCD,FLTGRPCD   CODE FILTER?                                 
         BZ    RCG20               NO                                           
         CLC   GRPKCODE,FLTGRPCD   CODE MATCHES?                                
         BNE   RCGX                                                             
*                                                                               
RCG20    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,GRPVALCQ     X'30'                                        
         BAS   RE,GETEL                                                         
         BNE   RCG50               READ THE NEXT RECORD                         
         USING GRPVALD,R6                                                       
*                                                                               
RCG30    GOTO1 CLPACK,DMCB,GRPVALUE,GCLTMKT                                     
*                                                                               
         MVC   GRPCODE,GRPKCODE                                                 
         DROP  R6,R3                                                            
*                                                                               
         GOTO1 BINSRCH,CMLPAR1,(X'01',WORK),AGROUP                              
         CLC   0(4,R1),=4X'00'     RECORD FULL?                                 
         BNE   *+6                                                              
         DC    H'0'                INC GROUPDMX=MAX NUMBER OF REC               
         MVC   NUMCCG,DMCB+10      UPDATE NUMBER OF RECORDS                     
*                                                                               
         BAS   RE,NEXTEL           GOT NEXT CLIENT                              
         BE    RCG30               YES                                          
*                                                                               
RCG50    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 READ NEXT CGROUP RECORD                      
         B     RCG10                                                            
*                                                                               
RCGX     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     NO                                                               
         DROP  R4                                                               
*                                                                               
***********************************************************************         
* READ THE MARKET GROUP RECORD(S) AND CREATE CLIENT/MGROUP LIST                 
***********************************************************************         
RDMGROUP NTR1                                                                   
*                                                                               
         LA    R3,GRPRECL          RECORD LENGTH                                
         LHI   RE,GROUPDMX         MAX RECORDS                                  
         MR    R2,RE               MAX N'RECORDS * L'ENTRY                      
         LA    R3,4(R3)                                                         
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 =V(COVAIL),DMCB,C'GET'  GET THE STORAGE                          
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,0(RE)            +0 = L'TABLE                                 
         S     R3,=F'4'                                                         
         LA    RE,4(RE)                                                         
         ST    RE,AGROUP                                                        
         XCEF  (RE),(R3)           CLEAR TABLE                                  
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R4,KEY              VALIDATE ID/SCHEME                           
         XC    KEY,KEY                                                          
         USING MKGKEY,R4                                                        
         MVC   MKGPTYP,=X'0D82'    PASSIVE TYPE/SUBTYPE                         
         MVC   MKGPAGMD,SVBAGYMD                                                
         TM    FILTRFLG,FFLGCLT    CLIENT FILTER?                               
         BZ    *+10                                                             
         MVC   MKGPCLT,FLTCLT      YES                                          
         MVC   MKGPMID,FLTGRPI1    PUT ID IN KEY                                
         OC    FLTGRPCD,FLTGRPCD                                                
         BZ    *+10                                                             
         MVC   MKGPMGRP,FLTGRPCD                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
RMG10    GOTO1 HIGH                                                             
*                                                                               
RMG15    CLC   KEY(MKGPMGRP-MKGKEY),KEYSAVE                                     
         BNE   RMGX                YES, EXIT                                    
*                                                                               
         TM    FILTRFLG,FFLGCLT    CLIENT FILTER?                               
         BZ    RMG20                                                            
         CLC   MKGPCLT,FLTCLT      DID THE CLIENT CHANGE?                       
         BNE   RMGX                                                             
         OC    MKGPPID,MKGPPID     NO, PRGROUP IS ALL?                          
         BNZ   RMGX                NO, EXIT                                     
         B     RMG25               YES                                          
*                                                                               
RMG20    OC    MKGKCLT,MKGKCLT     DID WE GET A CLIENT?                         
         BNZ   RMGX                EXIT, MUST BE ALL CLIENT                     
         CLI   MKGPPID,X'0'        PGROUP IS ALL?                               
         BE    RMG25               YES                                          
         B     RMGX                NO, EXIT                                     
*                                                                               
RMG25    CLC   MKGPMID,FLTGRPI1    DID SCHEME CHANGE?                           
         BNE   RMGSEQ              YES, READ SEQ                                
         OC    FLTGRPCD,FLTGRPCD   NO, DO WE HAVE A CODE FILTER?                
         BZ    RMG30               NO                                           
         CLC   MKGPMGRP,FLTGRPCD   YES, DID IT CHANGE?                          
         BNE   RMGSEQ              YES, READ SEQ                                
*                                                                               
RMG30    LA    R3,WORK                                                          
         USING GROUPD,R3                                                        
         MVC   GCLTMKT,MKGPMKT                                                  
         MVC   GRPCODE,MKGPMGRP                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 BINSRCH,CMLPAR1,(X'01',WORK),AGROUP                              
         CLC   0(4,R1),=4X'00'     RECORD FULL?                                 
         BNE   *+6                                                              
         DC    H'0'                INC GROUPDMX=MAX NUMBER OF REC               
         MVC   NUMCCG,DMCB+10      UPDATE NUMBER OF RECORDS                     
*                                                                               
RMGSEQ   GOTO1 SEQ                                                              
         B     RMG15                                                            
*                                                                               
RMGX     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     NO                                                               
         DROP  R4                                                               
***********************************************************************         
* HEAD HOOK ROUTINE                                                             
***********************************************************************         
HDHOOK   NTR1                                                                   
                                                                                
         MVC   H4+9(L'QMED),QMED                                                
         MVC   H4+14(L'MEDNM),MEDNM                                             
         MVC   H4+39(L'ORADAT),ORADAT                                           
         MVC   H1+50(20),=C'ORDER SUMMARY REPORT'                               
         MVI   H2+50,X'BF'         UNDERLINE CHARACTER                          
         MVC   H2+51(19),H2+50                                                  
                                                                                
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
                                                                                
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+09,C'C'                                                  
         MVI   BOXCOLS+43,C'C'                                                  
         MVI   BOXCOLS+52,C'C'                                                  
         MVI   BOXCOLS+65,C'C'                                                  
         MVI   BOXCOLS+78,C'C'                                                  
         MVI   BOXCOLS+87,C'C'                                                  
         MVI   BOXCOLS+103,C'C'                                                 
         MVI   BOXCOLS+117,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
HDHOOKX  B     XIT                                                              
         SPACE 1                                                                
*                                                                               
* IF BOXCOLS CHANGE IN HDHOOK, CHANGE IT HERE TO MATCH!                         
*                                                                               
FORCELIN NTR1                                                                   
         MVI   P+1,X'BF'                                                        
         MVC   P+2(PLINLNQ),P+1                                                 
         MVI   P,X'EB'                                                          
         MVI   P+09,X'8F'                                                       
         MVI   P+43,X'8F'                                                       
         MVI   P+52,X'8F'                                                       
         MVI   P+65,X'8F'                                                       
         MVI   P+78,X'8F'                                                       
         MVI   P+87,X'8F'                                                       
         MVI   P+103,X'8F'                                                      
         MVI   P+117,X'EC'                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
HEDSPECS SPROG 0,1,2                                                            
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,86,REPORT                                                     
         SSPEC H1,105,REQUESTOR                                                 
         SSPEC H2,86,RUN                                                        
         SSPEC H2,112,PAGE                                                      
         SSPEC H4,1,C'MEDIA :'                                                  
         SSPEC H4,32,C'DATE :'                                                  
*                                                                               
         SSPEC H7,12,C'Market - Market Name'                                    
         SSPEC H7,46,C'Client'                                                  
         SSPEC H7,55,C'Total Buys'                                              
         SSPEC H6,68,C'Buys with'                                               
         SSPEC H7,68,C'Orders'                                                  
         SSPEC H6,81,C'% Sent'                                                  
         SSPEC H7,81,C'via OM'                                                  
         SSPEC H6,90,C'Trade and'                                               
         SSPEC H7,90,C'Flight Orders'                                           
         SSPEC H6,106,C'Total'                                                  
         SSPEC H7,106,C'Orders Sent'                                            
*                                                                               
         SPROG 1,2                                                              
         SSPEC H7,3,C'Group'                                                    
         SPROG 1                                                                
         SSPEC H6,3,C'Client'                                                   
         SPROG 2                                                                
         SSPEC H6,3,C'Market'                                                   
HEDSPECX DC    X'00'                                                            
         EJECT                                                                  
                                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
TOOMANY  MVI   GERROR1,TOOMANYQ    TOO MANY RECORDS TO DISPLAY                  
         J     UGERROR1                                                         
*                                                                               
INVLDATE MVI   GERROR1,INVDATE                                                  
         J     UGERROR1                                                         
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         J     UGERROR1                                                         
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
*                                                                               
UGERROR1 MVI   GERROR,0           IN CASE THERE WAS GARBAGE FROM BEFORE         
         J     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    MISSING INPUT FIELD                          
*                                                                               
INFERR1  MVI   GERROR,0                                                         
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
RELO     DS    A                                                                
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
*                                                                               
CMLPAR1  DC    A(0)                                                             
CMLPAR2  DC    A(0)                  A(TABLE)                                   
CMLPAR3  DC    F'0'                RECORD COUNT                                 
CMLPAR4  DC    A(GRPRECL)          RECORD LENGTH                                
CMLPAR5  DC    A(GRKEYL)           KEYDSPL/KEYLEN                               
CMLPAR6  DC    A(GROUPDMX)         MAX NUMBER OF RECORDS                        
*                                                                               
PRDPAR1  DC    A(0)                                                             
PRDPAR2  DC    A(0)                  A(TABLE)                                   
PRDNUM   DC    F'0'                RECORD COUNT                                 
PRDPAR4  DC    A(PRDRECL)          RECORD LENGTH                                
PRDPAR5  DC    A(PRDKEYL)          KEYDSPL/KEYLEN                               
PRDPAR6  DC    A(PRDLSTMX)         MAX NUMBER OF RECORDS                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO TSAR                                                             
***********************************************************************         
TOTSAR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
TSRINIQ  EQU   1                   INITIALIZE                                   
TSRADDQ  EQU   2                   ADD                                          
TSRRDHIQ EQU   3                   READHI BY KEY                                
TSRWRTQ  EQU   4                   WRITE BY KEY                                 
TSRNXTQ  EQU   5                   READ NEXT RECORD BY NUMBER                   
*                                                                               
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     TSRINI                     INITIALIZE                            
         B     TSRADD                     ADD                                   
         B     TSRRDH                     READHI BY KEY                         
         B     TSRWRT                     WRITE BY KEY                          
         B     TSRNXT                                                           
*                                                                               
TSRINI   DS    0H                                                               
         ICM   R0,15,=A(TSARBUFL)  GET LENGTH OF BUFFER                         
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)  GET 31BIT STORAGE                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF         SAVE BUFFER ADRESS                           
*                                                                               
         XC    TSARBLK(TSARDL),TSARBLK       CLEAR PARAMATER BLOCK              
         LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI     SET ACTION = INIT                            
         MVC   TSABUF,TSARBUFF     SET BUFFER ADDRESS                           
         MVC   TSAREC,=A(TSARBUFL) ON INIT CALL, SET BUFFLEN HERE               
         LA    R0,TSRKEYL                                                       
         STC   R0,TSKEYL                                                        
         LA    R0,TSRRECL                                                       
         STH   R0,TSRECL                                                        
         GOTO1 VTSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         JE    YES                                                              
         DC    H'0'                                                             
*                                                                               
TSRADD   LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSAADD     SET TO ADD RECORD                            
         LA    RE,TSRREC                                                        
         ST    RE,TSAREC                                                        
         GOTO1 VTSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         JE    YES                                                              
         DC    H'0'                                                             
*                                                                               
TSRRDH   LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSARDH     SET TO READ HIGH                             
         LA    R0,TSRREC                                                        
         ST    R0,TSAREC                                                        
         GOTO1 VTSAROFF,(R2)                                                    
         TM    TSERRS,TSERNF       RECORD NOT FOUND?                            
         JO    NO                  YES, RETURN NO-CC                            
         CLI   TSERRS,0                                                         
         JE    YES                                                              
         DC    H'0'                                                             
*                                                                               
TSRWRT   LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSAWRT     SET TO WRITE                                 
         LA    R0,TSRREC                                                        
         ST    R0,TSAREC                                                        
         GOTO1 VTSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         JE    YES                                                              
         DC    H'0'                                                             
*                                                                               
TSRNXT   LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSANXT     SET TO READ NEXT                             
         LA    R0,TSRREC                                                        
         ST    R0,TSAREC                                                        
         GOTO1 VTSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       TEST RECORD NOT FOUND                        
         JO    NO                                                               
         J     YES                                                              
*                                                                               
         EJECT                                                                  
* DDBIGBOX                                                                      
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
* FAFACTS                                                                       
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* CTGENEDICT                                                                    
* FATIOB                                                                        
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENEST                                                                      
* SPGENMKT                                                                      
* SPGENPRD                                                                      
* SPADAVCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTGENEDICT                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPADAVCOM                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSFCD          (REPORT SCREEN)                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE SPGENGRP          (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE SPGENMKG          (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
VTSAROFF DS    V                                                                
ATSAROFF DS    A                   ADDRESS OF TSAROFF                           
AGROUP   DS    A                                                                
*                                                                               
BREAK1LN DS    X                   BREAK 1 LENGTH FROM DEFINITION REC           
BREAK2LN DS    X                   BREAK 2 LENGTH FROM DEFINITION REC           
BREAK3LN DS    X                   BREAK 3 LENGTH FROM DEFINITION REC           
*                                                                               
FILTRFLG DS    X                                                                
FFLGCLT  EQU   X'80'               CLIENT FILTER                                
FFLGMKT  EQU   X'40'               MARKET FILTER                                
FFLGBUYR EQU   X'20'               BUYER FILTER                                 
FFLGCGRP EQU   X'10'               CLIENT GROUP FILTER                          
FFLGMGRP EQU   X'08'               MARKET GROUP FILTER                          
*                                                                               
SVBUYKEY DS    XL(L'BUYKEY)        SAVE BUYER KEY                               
SVDARKEY DS    XL(L'DOKEY)         SAVE ORDER KEY                               
SVBAGYMD DS    X                   BINARY AGENCY/MEDIA CODE                     
*                                                                               
FLTCLTG  DS    0XL2                CLIENT GROUP FILTER (BINARY)                 
FLTCLT   DS    XL2                 CLIENT FILTER (BINARY)                       
FLTGRPID DS    CL2                 GROUP ID                                     
FLTGRPI1 DS    X                   1 BYTE GROUP ID                              
FLTGRPCD DS    XL2                 GROUP CODE (PWOS)                            
FLTMKT   DS    XL2                 MARKET FILTER (BINARY)                       
FLTBUYR  DS    CL3                 BUYER FILTER (CHAR)                          
FFLTSDAT DS    XL3                 START DATE FILTER                            
FFLTEDAT DS    XL3                 END DATE FILTER                              
*                                                                               
CURCLT   DS    XL2                 CURRENT CLIENT IN KEY                        
CURMKT   DS    XL2                 CURRENT MARKET IN KEY                        
CURCGPCD DS    XL2                 CURRENT CLIENT GROUP CODE                    
CODECHAR DS    CL5                 GROUP CODE CHARACTER                         
SAVECODE DS    CL6                                                              
ACURCODE DS    F                                                                
*                                                                               
TOTBUYS  DS    F                   TOTAL BUYS FOR THIS CLIENT-MARKET            
TOTORDRS DS    F                   TOTAL ORDERS FOR THIS CLIENT-MARKET          
TOTTRDFL DS    F                   TOTAL ORDERS WITH TRADE AND FLIGHTS          
TOTBYORD DS    F                   TOTAL BUYS WITH ORDERS                       
*                                                                               
GRPBUYS  DS    F                   GROUPTOTAL BUYS                              
GRPORDRS DS    F                   GROUPTOTAL ORDERS                            
GRPTRDFL DS    F                   GROUPTOTAL FLIGHTS AND TRADE                 
GRPBYORD DS    F                   GROUPTOTAL BUYS WITH ORDERS                  
*                                                                               
AGTBUYS  DS    F                   AGENCY TOTAL BUYS                            
AGTORDRS DS    F                   AGENCY TOTAL ORDERS                          
AGTTRDFL DS    F                   AGENCY TOTAL FLIGHTS AND TRADE               
AGTBYORD DS    F                   AGENCY TOTAL BUYS THAT HAVE ORDERS           
*                                                                               
MISCFLG1 DS    C                   MISCELLANEOUS FLAG 1                         
MF1KYCHG EQU   X'80'               - KEY HAS CHANGED                            
MF1RPORT EQU   X'40'               - PRINT THE REPORT                           
MF1BYORD EQU   X'20'               - THE BUY HAS AN ORDER                       
MF1BDEST EQU   X'10'               - CALL BLDEST                                
MF1PALL  EQU   X'08'               - PRINT ALL TOTALS                           
*                                                                               
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE AREA                          
*                                                                               
ESTTAB   DS    XL256               ESTIMATE NUMBER TABLE                        
*                                                                               
NUMCCG   DS    XL2                 NUMBER IN CLIENT/CGROUP TABLE                
*                                                                               
STTDATE  DS    CL6                 START OF ESTIMATE DATE (EBCDIC)              
ENDDATE  DS    CL6                 END OF ESTIMATE DATE (EBCDIC)                
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL60                                                             
*                                                                               
TSARBUFF DS    A                   ADDRESS OF GETMAIN'D BUFFER                  
TSARBUFL EQU   TSRRECL*GROUPDMX    BUFFER LENGTH FOR 5000 RECORDS               
*                                                                               
TSRREC   DS    0D                                                               
*                                                                               
TSRKEY   DS    0C                  KEY                                          
TSRKCODE DS    XL2                 PWOS CODE                                    
TSRKMKT  DS    XL2                 TSARKEY MARKET                               
TSRKCLT  DS    XL2                 TSARKEY CLIENT                               
TSRKEYL  EQU   *-TSRKEY                                                         
*                                                                               
TSRBUYS  DS    F                   TSARREC TOTAL BUYS                           
TSRORDRS DS    F                   TSARREC TOTAL ORDERS                         
TSRTRDFL DS    F                   TSARREC TOTAL FLIGHTS AND TRADE              
TSRBYORD DS    F                   TOTAL BUYS THAT HAVE ORDERS                  
TSRRECL  EQU   *-TSRREC                                                         
*                                                                               
TSARBLK  DS    XL(TSARDL)          AREA TO BE COVERED BY TSARD                  
*                                                                               
APRDLIST DS    A                                                                
PRDLIST  DS    XL(PRDLRECL*PRDLSTMX)                                            
*                                                                               
***********************************************************************         
* DSECT TO COVER CLIENT/CGROUP TABLE                                            
***********************************************************************         
GROUPD   DSECT                                                                  
GRPDKEY  DS    0D                                                               
GCLTMKT  DS    XL2                                                              
GRKEYL   EQU   *-GRPDKEY                                                        
*                                                                               
GRPCODE  DS    XL2                                                              
GRPRECL  EQU   *-GROUPD                                                         
*                                                                               
GROUPDMX EQU   5000                                                             
         EJECT                                                                  
*                                                                               
PRDLSTD  DSECT                                                                  
PRDLKEY  DS    0D                                                               
PRDL1    DS    X                                                                
PRDL2    DS    X                                                                
PRDKEYL  EQU   *-PRDLKEY                                                        
*                                                                               
PRDRECL  EQU   *-PRDLSTD                                                        
*                                                                               
PRDLRECL EQU   *-PRDLSTD                                                        
PRDLSTMX EQU   506                 MAXIMUM NUMBER OF PRODUCT ENTRIES            
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PGROUP   DS    CL6                                                              
         DS    CL3                                                              
PMKT     DS    CL4                                                              
         DS    C                                                                
PDASH    DS    C                                                                
         DS    C                                                                
PMKTNM   DS    CL24                                                             
         DS    CL3                                                              
PCLT     DS    CL5                                                              
         DS    CL4                                                              
PTOTBUYS DS    CL10                                                             
         DS    CL3                                                              
PBUYORDR DS    CL10                                                             
         DS    CL3                                                              
PBYORD   DS    CL4                                                              
         DS    CL5                                                              
PTRDFLT  DS    CL10                                                             
         DS    CL6                                                              
PTOTORDR DS    CL10                                                             
PLINLNQ  EQU   *-PLINED                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPOMS0C   01/03/07'                                      
         END                                                                    
