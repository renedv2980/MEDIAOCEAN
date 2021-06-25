*          DATA SET NEMED61    AT LEVEL 084 AS OF 05/01/02                      
*PHASE T31E61A,+0                                                               
*INCLUDE BINSRCH2                                                               
         TITLE 'T31E61 - NETWORK UPDATE '                                       
         PRINT NOGEN                                                            
************************************************************                    
* NETWORK UPDATE  LIST/REPORT                                                   
*                                                                               
* THIS PROGRAM CHANGES AND LISTS THOSE                                          
* PACKAGES AND UNITS AFFECTED BY THE                                            
*  - NEW UNIVERSE CODE                                                          
*  - NEW INTEGRATION FIGURE                                                     
*                                                                               
* GLOBALS: RA - A(TWA)                                                          
*          RB - BASE REG                                                        
*          RC - GEND                                                            
*          R9 - NETWORK SYSTEM DSECT                                            
*          R8 - A(DSECT FOR SPOOL PRINTING)                                     
*          R7 - WORKING STORAGE                                                 
*                                                                               
* INPUTS:  NEWUNIVS ELEM/NEW UNIVCODE/NEW INTG                                  
*          (RETURNED IN W/S AREA1 BY EDIT OVLAY)                                
*                                                                               
**************************************************************                  
T31E61   CSECT                                                                  
         NMOD1 0,**UPDT**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         LA    R2,FROZEN                                                        
         XC    0(200,R2),0(R2)                                                  
         LA    R2,200(R2)                                                       
         XC    0(200,R2),0(R2)                                                  
         LA    R2,200(R2)                                                       
         XC    0(100,R2),0(R2)                                                  
         MVI   FRZNEND,X'FF'                                                    
         EJECT                                                                  
         SPACE                                                                  
** INITIALIZE BINSRCH PARAMETERS **                                             
*                                                                               
UNV05    SR    R0,R0               A OF REC TO BE ADDED                         
         L     R1,=A(BTBLAREA)     A OF BINTBL                                  
         A     R1,RELO                                                          
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,BINRCLNE         LENGTH OF REC                                
         LA    R4,BINKLNE          DISP OF KEY INTO REC                         
         LA    R5,BNALNE/BINRCLNE   BINAREA=10K                                 
         STM   R0,R5,BINDMCB                                                    
         SPACE                                                                  
         LR    RE,R1               CLEAR BINTBL AREA                            
         LR    RF,R5               R5 HAS BINTBL LENGTH                         
         XCEF                                                                   
         SPACE 2                                                                
         MVI   NBDATA,C'B'         SELECT BOTH RECORDS                          
         LA    R2,MAINLINE         SET NBHOOK WITH MAINLINE                     
         ST    R2,NBHOOK                                                        
         SPACE                                                                  
** NETIO GETS RECORD AND GOES TO MAINLINE THROUGH NBHOOK **                     
*                                                                               
         MVI   NBRESUME,NBPROCPK   START WITH 1ST PACKAGE                       
UNV10    NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         B     CKEOF                                                            
         SPACE                                                                  
*                                                                               
MAINLINE NTR1                                                                   
         MVI   NBUPUNIT,0          SET ELEM SWITCH IN NETIO                     
         CLI   NBMODE,NBPROCPK     IF A PACKAGE RECORD                          
         BNE   *+8                                                              
         BAS   RE,PAKRTN                                                        
         CLI   NBMODE,NBPROCUN    IF A UNIT RECORD                              
         BNE   *+8                                                              
         BAS   RE,UNTRTN                                                        
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
CKEOF    CLI   NBMODE,NBREQLST     IS IT EOF                                    
         BNE   UNV10                                                            
         BAS   RE,PRINTRTN                                                      
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*****************************************************                           
*** THIS ROUTINE HANDLES THE UNIT RECORDS           *                           
***                                                 *                           
*** SETS UP ARGUMENT FOR BINSRCH/GOTO BINSRCH       *                           
*** REPLACES OLD WITH NEW UNIV CODE                 *                           
*** DELETES ESTIMATED UNIV ELEM /PUTS NEW ONE       *                           
*** GOES TO NETIO TO PUTREC NEW UNIT RECORD         *                           
*                                                   *                           
**  INPUT: UNIT REC IN NBAIO RETURNED BY NETIO      *                           
**         NETBLOCK SET UP BY NETIO                 *                           
**         NEWUNIVS/ NEWUNCD/ NEWINTG IN W/S        *                           
**                                                  *                           
** OUTPUT: NEW ELEM IN UNIT REC WITH NEW UNIVERSES  *                           
**         NEW UNIV CODE IN UNIT REC                *                           
**         NEW INTG NUMB IN UNIT REC                *                           
**         LISTING/UPDATE IN BINSRCH TABLE          *                           
**         NETIO PUTSREC NEW UNIT RECORD            *                           
*****************************************************                           
         SPACE 2                                                                
** SET UP FOR BINSRCH/GOTO BINSRCH **                                           
UNTRTN   DS    0H                                                               
         LA    R5,FROZEN           TEST IF ANY FROZEN PACKAGES                  
         CLI   0(R5),0                                                          
         BE    UNT02               IF NOT/SKIP FROZEN UNIT TEST                 
         MVC   FRZNTST(4),NBACTNET  MOVE UNIT(NET+PKG) TO TEST AREA             
         MVC   FRZNTST+4(1),NBPACK                                              
UNT01    CLC   FRZNTST,0(R5)       COMPARE TO FROZEN PKG                        
         BER   RE                  IF =,SKIP UNIT                               
         LA    R5,5(R5)                                                         
         CLI   0(R5),X'FF'         EOF FROZEN                                   
         BNE   UNT01               NO/CONTINUE CHECK                            
         SPACE 2                                                                
UNT02    NTR1                       PROCESS UNIT REC                            
         L     R3,ANETWS2              USE ANETWS2 TO SET UP BINREC             
         XC    0(BINRCLNE,R3),0(R3)      CLEAR IT                               
         USING BINTBL,R3                                                        
         MVC   BINNET,NBACTNET     * NETWORK          (*=KEY)                   
         MVC   BINPKCD,NBPACK      * PACKAGE CODE                               
         MVC   BINPRGCD,NBACTPRG   * PROGRAM CODE                               
         CLI   UNVFLG,C'Y'                                                      
         BNE   *+10                                                             
         MVC   BINOUNV,NBUNCODE    * OLD UNIVERSE CODE                          
         CLI   INTGFLG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   BINOINTG,NBINTEG    * OLD INTG NUMB                              
         MVC   BINPRGNM,NBPROGNM     PROGRAM NAME                               
         SPACE                                                                  
* GO GET PACKAGE NAME FROM BINTBL PACKAGE RECORD *                              
         L     R4,=A(BTBLAREA)                                                  
         A     R4,RELO                                                          
UNT03    CLC   BINNET(5),0(R4)     NTWK+PAK CODE (*** HARD CODED ***)           
         BE    UNT05                                                            
         LA    R4,BINRCLNE(R4)                                                  
         CLC   0(3,R4),=3X'0'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     UNT03                                                            
UNT05    MVC   BINPKNM,BINPKNM-BINTBL(R4)       PACKAGE NAME                    
*                                               FROM BINTBL                     
         SPACE                                                                  
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R3))                                     
         CLC   0(2,R1),=X'0000'                                                 
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL. BOMB.                            
         DROP  R3                                                               
         L     R2,0(R1)                                                         
         USING BINTBL,R2                                                        
         L     R1,BINUNTOT         INCREMENT TOTAL UNITS COUNT                  
         LA    R1,1(R1)                                                         
         ST    R1,BINUNTOT                                                      
         SPACE 2                                                                
** DELETE EST UNIV ELEM, ADD NEW UNIV ELEM **                                   
**                           NEW UNIV CODE **                                   
*                                                                               
         CLI   UNVFLG,C'Y'         TEST NEW UNIV                                
         BNE   UNT10               IF NO/SKIP UNIV PROC                         
         SPACE                                                                  
         L     R6,NBAIO            GET ADDRESS OF UNITREC                       
         USING NURECD,R6                                                        
         MVC   NUUNCODE,NEWUNCD    SET NEW UNIV CODE                            
         SPACE                                                                  
         CLI   UNSTFLG,C'Y'        CHK NUUNST2 FLG                              
         BNE   UNT07                                                            
         OI    NUUNST2,X'08'                                                    
         SPACE                                                                  
UNT07    MVI   ELCODE,X'31'        EST UNIV ELEM CODE                           
         L     R6,NBAIO            NEED R6-POINT TO REC FOR DELELM              
         BAS   RE,DELELM           DELETE EST UNIV ELEM                         
         LA    R4,NEWUNIVS         SET UP NEW ELELM                             
         BAS   RE,PUTELM           PUT NEW EST UNIV ELEM                        
         BAS   RE,PASSNAD          UPDATE THE NAD INFORMATION                   
         SPACE                                                                  
UNT10    CLI   INTGFLG,C'Y'        TEST NEW INTEGRATION NUM                     
         BNE   UNTX                IF NO/SKIP INTG PROC                         
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
         MVC   NUINTEG,NEWINTG     SET NEW INTEG NUMB                           
         SPACE                                                                  
UNTX     DS     0H                                                              
         MVI   NBUPUNIT,C'Y'       SET ON PUTREC SWITCH FOR NETIO               
         MVI   NBNOWRIT,C'Y'       SET ON WRITE SWITCH FOR NETIO                
         B     XIT                 (XIT1)                                       
         EJECT                                                                  
****************************************************                            
*** THIS ROUTINE PROCESSES REMOVES THE CURRENT   ***                            
*** NAD INFORMATION, AND UPDATES THE UNITS WITH  ***                            
*** THE CURRENT NAD INFORMATION                  ***                            
*                                                                               
PASSNAD  NTR1                                                                   
*                                                                               
*  DELETE ALL NAD UNIVERSE DEMOS                                                
         USING NUOVD,R3                                                         
         L     R2,NBAIO                                                         
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'DD',(R2)),0                         
         CLI   12(R1),0                                                         
         BNE   PSN100                                                           
         L     R3,12(R1)                                                        
         B     PSN20                                                            
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
PSN10    ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'DD'                                                      
         BNE   PSN100                                                           
*                                                                               
PSN20    TM    NUOVFLG,X'80'        IS ELEMENT A NAD DEMO                       
         BZ    PSN10                                                            
         CLI   NUOVMOD,C'U'         IS IT UNIVERSE ELEMENT                      
         BNE   PSN10                                                            
         MVI   NUOVZER,X'FF'        SET FOR DELETE                              
         B     PSN10                                                            
*                                                                               
PSN100   GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'DD',(R2)),(1,=X'FF')                
         DROP  R3                                                               
*                                                                               
*  MOVE ALL THE NAD DEMO INFO THAT IS ON THE ESTIMATE                           
*  FROM THE PROGRAM RECORD TO THE UNIT RECORD.                                  
*                                                                               
PSN200   L     R2,ANETWS3           DEMO BLOCK                                  
         USING NDDEMBLK,R2                                                      
         L     R4,ANETWS4           UNIVERSE RECORD                             
         USING NUNRECD,R4                                                       
*                                                                               
         L     R3,NBAIO             BUY RECORD                                  
******   GOTO1 =V(PRNTBL),DMCB,=C'BEFR',0(R3),C'DUMP',1500,=C'1D'               
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'DD',(R4)),0                         
         CLI   12(R1),0                                                         
         BNE   PSNEX                                                            
         L     R6,12(R1)                                                        
         B     PSN220                                                           
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
PSN210   ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'DD'                                                      
         BNE   PSNEX                                                            
*                                                                               
*  CHECK TO SEE IF DEMO CATEGORY IS ON THE ESTIMATE                             
*                                                                               
PSN220   LA    RE,NDDEMOS                                                       
         LA    RF,20                                                            
*                                                                               
PSN240   CLC   0(1,RE),3(R6)        CHECK NAD CATEGORY                          
         BNE   PSN260                                                           
         CLC   2(1,RE),5(R6)        CHECK DEMO CODE                             
         BE    PSN300                                                           
PSN260   LA    RE,3(RE)                                                         
         BCT   RF,PSN240                                                        
         B     PSN210                                                           
*                                                                               
*  WRITE THE ELEMENT OUT                                                        
*                                                                               
PSN300   GOTO1 HELLO,DMCB,(C'P',UNTFILE),(X'DD',(R3)),(R6),0                    
         B     PSN210                                                           
*                                                                               
******   GOTO1 =V(PRNTBL),DMCB,=C'AFTR',0(R3),C'DUMP',1500,=C'1D'               
PSNEX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
****************************************************                            
*** THIS ROUTINE PROCESSES THE PACKAGE RECORDS   ***                            
*** SETS UP ARGUMENTS FOR BINSRCH/GOTO BINSRCH   ***                            
*** SETS NEW UNIV CODE IN PACKAGE REC            ***                            
*** GOES TO NETIO TO PUTREC                      ***                            
*** GOES TO UNV10 FOR NXT REC                    ***                            
***                                              ***                            
** INPUT: FROM NETBLOCK BY NETIO                  **                            
**             NBACTNET CL4 NETWORK               **                            
**             NBACTPAK CL1 PACKAGE NUMBER        **                            
**             NBPAKNAM CL16 PACKAGE NAME         **                            
**             NBUNCODE CL2  UNIVERSE CODE        **                            
**        PACKAGE REC IN NBAIO RETURNED BY NETIO  **                            
**        NEWUNCD/NEWINTG                         **                            
**                                                **                            
** OUTPUT: NEW UNIV CODE IN PACKAGE REC           **                            
**         LISTING IN BINSRCH TABLE               **                            
****************************************************                            
         SPACE 2                                                                
PAKRTN   DS    0H                                                               
         L     R6,NBAIO                                                         
         USING NPRECD,R6                                                        
         CLI   FROZ,C'Y'           INCLUDE FROZEN PACKAGES                      
         BE    PAK05               YES                                          
         TM    NPAKSTAT,X'80'      NO/IS IT FROZEN                              
         BNO   PAK05                                                            
         LA    R5,FROZEN                                                        
PAK02    CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL BOMB.                             
         CLI   0(R5),0                                                          
         BNE   PAK03                                                            
         MVC   0(4,R5),NBACTNET    YES/SAVE FROZEN NET+PGK                      
         MVC   4(1,R5),NBACTPAK                                                 
         BR    RE                      AND SKIP RECORD                          
PAK03    LA    R5,5(R5)                                                         
         B     PAK02                                                            
         SPACE 2                                                                
PAK05    NTR1                                                                   
         L     R3,ANETWS2                                                       
         XC    0(BINRCLNE,R3),0(R3)                                             
         USING BINTBL,R3                                                        
         MVC   BINNET,NBACTNET     * NETWORK   (*=KEY)                          
         MVC   BINPKCD,NBACTPAK    * PACKAGE CODE                               
         CLI   UNVFLG,C'Y'                                                      
         BNE   *+10                                                             
         MVC   BINOUNV,NBUNCODE    * OLD UNIVERSE NUMBER                        
         CLI   INTGFLG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   BINOINTG,NPAKINT    * OLD INTG NUMB                              
         MVC   BINPKNM,NBPAKNAM      PACKAGE NAME                               
         SPACE                                                                  
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R3))                                     
         CLC   0(2,R1),=X'0000'                                                 
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL. BOMB.                            
         DROP  R3                                                               
         L     R2,0(R1)                                                         
         USING BINTBL,R2                                                        
         L     R1,BINUNTOT         UPDATE TOTALS                                
         LA    R1,1(R1)                                                         
         ST    R1,BINUNTOT                                                      
         SPACE                                                                  
         CLI   UNVFLG,C'Y'         TEST NEW UNIV                                
         BNE   PAK10                                                            
         MVC   NPAKUNCD,NEWUNCD   SET NEW UNIV CODE                             
         SPACE                                                                  
PAK10    CLI   INTGFLG,C'Y'        TEST INTEGR NUM                              
         BNE   PAKX                                                             
         MVC   NPAKINT,NEWINTG     SET NEW INTG NUM                             
         SPACE                                                                  
PAKX     DS    0H                                                               
         MVI   NBUPUNIT,C'Y'       SET ON PUTREC SWITCH FOR NETIO               
         MVI   NBNOWRIT,C'Y'       SET ON WRITE SWITCH FOR NETIO                
         B     XIT                (XIT1)                                        
         EJECT                                                                  
*                                                                               
PRINTRTN DS    0H                                                               
         NTR1                                                                   
         L     R3,=A(BTBLAREA)                                                  
         A     R3,RELO                                                          
         USING BINTBL,R3                                                        
         L     R4,BRECNUM       R4=NUM OF RECS IN BINTBL/FOR BCT LOOP           
         LTR   R4,R4                                                            
         BP    PRNT10                                                           
         MVC   P+5(21),=C'*** NO BUYS FOUND ***'                                
         LA    R4,1                FOR BCT                                      
         B     PRNTX                                                            
PRNT10   DS    0H                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PLNET,BINNET                                                     
         EDIT  (B1,BINPKCD),(3,PLPKCDE)                                         
         MVC   PLPKNM,BINPKNM                                                   
         MVC   PLPRGCDE,BINPRGCD                                                
         MVC   PLPRGNM,BINPRGNM                                                 
         EDIT  (B4,BINUNTOT),(5,PLUNTOT)                                        
         LA    R1,ENDP                                                          
         AR    R2,R1               POINT TO BEG OF NXT P SPACE                  
         CLI   INTGFLG,C'Y'        TEST NEW INTG NUM                            
         BNE   PRNT15                                                           
         USING INTGD,R2                                                         
         EDIT  (B4,BINOINTG),(10,PLOINTG),2,ALIGN=LEFT                          
         MVC   PLNINTG,NEWINTGP                                                 
         LA    R1,INTGLE           POINT TO NXT P SPACE                         
         AR    R2,R1                                                            
PRNT15   CLI   UNVFLG,C'Y'         TEST NEW UNIV                                
         BNE   PRNTX                                                            
         USING UNIVD,R2                                                         
         UNPK  WORK(5),BINOUNV(3)     OLD UNIV IS PWOS(CL2)                     
         EDIT  (C4,WORK),(4,PLOUNV),ZERO=BLANK                                  
         MVC   PLNUNV,NEWUNVP                                                   
         LA    R1,UNIVLE                                                        
         AR    R2,R1               POINT TO NXT P SPACE                         
         SPACE                                                                  
PRNTX    GOTO1 SPOOL,DMCB,(R8)     PRINT                                        
         A     R3,BRECLN                                                        
         BCT   R4,PRNT10                                                        
         B     XIT                 (XIT1)                                       
         EJECT                                                                  
*                                                                               
HOOK     DS 0H                                                                  
         NTR1                                                                   
         MVC   H3+10(3),NBSELCLI                                                
         MVC   H3+15(20),UVLCLIN                                                
         LA    R3,H4+10                                                         
         EDIT  NBSELEST,(4,(R3)),ALIGN=LEFT                                     
         MVC   H4+15(24),UVLESTN                                                
         MVC   H5+15(4),NBSELNET                                                
         CLC   NBSELNET,=4X'0'                                                  
         BNE   *+10                                                             
         MVC   H5+15(3),=C'ALL'                                                 
         CLI   NBSELPAK,0                                                       
         BNE   HK03                                                             
         MVC   H6+15(3),=C'ALL'                                                 
         B     HK05                                                             
HK03     LA    R3,H6+15                                                         
         EDIT  (B1,NBSELPAK),(4,(R3)),ALIGN=LEFT                                
*                                                                               
*                                                                               
HK05     LA    R2,H9                                                            
         USING PLINED,R2                                                        
         MVC   PLPKCDE(21),=C'-------PACKAGE-------'                            
         MVC   PLPRGCDE(21),=C'-------PROGRAM-------'                           
         MVC   PLUNTOT(5),=C'TOTAL'                                             
         LA    R1,ENDP                                                          
         AR    R2,R1               POINT TO NXT P SPACE                         
         CLI   INTGFLG,C'Y'                                                     
         BNE   HK10                                                             
         USING INTGD,R2                                                         
         MVC   PLOINTG(22),=C'-----INTEGRATION------'                           
         LA    R1,INTGLE                                                        
         AR    R2,R1                                                            
HK10     CLI   UNVFLG,C'Y'                                                      
         BNE   HK50                                                             
         USING UNIVD,R2                                                         
         MVC   PLOUNV(10),=C'---UNIV---'                                        
         LA    R1,UNIVLE                                                        
         AR    R2,R1                                                            
         B     HK50                                                             
*                                                                               
HK50     LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PLNET(4),=C'NTWK'                                                
         MVC   PLPKCDE(4),=C'CODE'                                              
         MVC   PLPKNM(4),=C'NAME'                                               
         MVC   PLPRGCDE(4),=C'CODE'                                             
         MVC   PLPRGNM(4),=C'NAME'                                              
         MVC   PLUNTOT(5),=C'UNITS'                                             
         LA    R1,ENDP                                                          
         AR    R2,R1                                                            
         CLI   INTGFLG,C'Y'                                                     
         BNE   HK60                                                             
         USING INTGD,R2                                                         
         MVC   PLOINTG(3),=C'OLD'                                               
         MVC   PLNINTG(3),=C'NEW'                                               
         LA    R1,INTGLE                                                        
         AR    R2,R1                                                            
HK60     CLI   UNVFLG,C'Y'                                                      
         BNE   HKX                                                              
         USING UNIVD,R2                                                         
         MVC   PLOUNV(3),=C'OLD'                                                
         MVC   PLNUNV(3),=C'NEW'                                                
         LA    R1,UNIVLE                                                        
         AR    R2,R1                                                            
*                                                                               
HKX      B     XIT                 (XIT1)                                       
         EJECT                                                                  
         SPACE                                                                  
* SUB-ROUTINE TO DELETE ELMENT (R6 POINTS TO RECORD)                            
*                                                                               
DELELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(ELCODE,(R6)),0                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TP PUT ELEMENT (R4 POINTS TO ELEMENT)                             
*                                                                               
PUTELM   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),(ELCODE,(R6)),(R4)                     
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
         GETEL (R6),NBDTADSP,ELCODE                                             
         SPACE 2                                                                
*                                                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE                                                                  
UNTFILE  DC    CL8'UNTFIL'                                                      
SPTFILE  DC    CL8'SPTFIL'                                                      
TRAPERR  EQU   ERREX                                                            
         SPACE 2                                                                
*                                                                               
BTBLAREA CSECT              ***FOR USE BY BINTABLE***                           
         DS    10CL10000                                                        
BNALNE   EQU   *-BTBLAREA                                                       
         EJECT                                                                  
         SPACE                                                                  
*                                                                               
*** BINSRCH TABLE DESECT ***                                                    
         SPACE                                                                  
BINRECD  DSECT                                                                  
BINTBL   DS    0H                                                               
BINNET   DS    CL4                 NETWORK                                      
BINPKCD  DS    CL1                 PACK CODE                                    
BINPRGCD DS    CL6                 PROGRAM CODE                                 
BINOUNV  DS    CL2                 OLD UNIVERSE NUMBER                          
BINOINTG DS    CL4                 OLD INTG NUMBER                              
*                                                                               
BINKLNE  EQU   *-BINTBL            DISP INTO KEY LENGTH                         
BINPKNM  DS    CL16                PACK NAME                                    
BINPRGNM DS    CL16                PROGRAM NAME                                 
BINUNTOT DS    F                   TOTAL UNITS                                  
BINRCLNE EQU   *-BINTBL            BINREC LENGTH                                
         SPACE 3                                                                
*                                                                               
*** PRINT LINE DESECT ***                                                       
         SPACE                                                                  
PLINED   DSECT                                                                  
PLNET    DS    CL4                 NETWORK                                      
         DS    CL2                                                              
PLPKCDE  DS    CL3                 PACKAGE CODE                                 
         DS    CL2                                                              
PLPKNM   DS    CL16                PACKAGE NAME                                 
         DS    CL2                                                              
PLPRGCDE DS    CL6                 PROGRAM CODE                                 
         DS    CL2                                                              
PLPRGNM  DS    CL16                PROGRAM NAME                                 
         DS    CL2                                                              
PLUNTOT  DS    CL5                 TOTAL UNITS                                  
         DS    CL2                                                              
ENDP     EQU   *-PLNET                                                          
*                                                                               
INTGD    DSECT                                                                  
PLOINTG  DS    CL10                OLD INTEGRATION                              
         DS    CL2                                                              
PLNINTG  DS    CL10                NEW INTEGRATION                              
         DS    CL2                                                              
INTGLE   EQU   *-INTGD                                                          
*                                                                               
UNIVD    DSECT                                                                  
PLOUNV   DS    CL4                 OLD UNIVERSE                                 
         DS    CL2                                                              
PLNUNV   DS    CL4                 NEW UNIVERSE                                 
UNIVLE   EQU   *-UNIVD                                                          
         DS    CL46                SPARE                                        
         EJECT                                                                  
         SPACE                                                                  
**** PASDATA STORAGE (IN W/S AREA1 FROM EDIT OVERLAY) ***                       
WORKD    DSECT                                                                  
         DS    0F                                                               
NEWUNIVS DS    CL200                                                            
*                                                                               
NEWINTG  DS    F                   NEW INTEGRATION                              
NEWINTGP DS    CL10                INTG IN PRINT FORMAT                         
NEWUNCD  DS    CL2                 NEW UNIV CODE                                
NEWUNVP  DS    CL4                 UNIV IN PRINT FORMAT                         
UNVFLG   DS    CL1                 UNIVERSE FLAG                                
INTGFLG  DS    CL1                 INTEGRATION FLAG                             
UNSTFLG  DS    CL1                 NUUNST2 FLG                                  
FROZ     DS    CL1                 INCLUDE FROZEN PKGS                          
*                                                                               
*                                                                               
*** WORKING STORAGE SET UP IN THIS OVERLAY ***                                  
RELO     DS    F                                                                
*                                                                               
BINDMCB  DS    0F                  *** BINSRCH PARAMETERS ***                   
BADDADR  DS    F                                                                
BTBLADR  DS    F                                                                
BRECNUM  DS    F                                                                
BRECLN   DS    F                                                                
BKEYDSP  DS    F                                                                
BRECMAX  DS    F                                                                
*                                                                               
FROZEN   DS    CL500                                                            
FRZNEND  DS    CL1                                                              
FRZNTST  DS    CL5                                                              
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE1D                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENUNIV                                                      
       ++INCLUDE NETDEMOD                                                       
         EJECT                                                                  
DOMFACSD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084NEMED61   05/01/02'                                      
         END                                                                    
