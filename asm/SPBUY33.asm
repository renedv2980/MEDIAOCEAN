*          DATA SET SPBUY33    AT LEVEL 006 AS OF 04/10/13                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041861.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*========= THIS PROGRAM IS DEAD!  MHER 4/4/13 ==========*                       
*========= THIS PROGRAM IS DEAD!  MHER 4/4/13 ==========*                       
*========= THIS PROGRAM IS DEAD!  MHER 4/4/13 ==========*                       
*========= THIS PROGRAM IS DEAD!  MHER 4/4/13 ==========*                       
*========= THIS PROGRAM IS DEAD!  MHER 4/4/13 ==========*                       
*========= THIS PROGRAM IS DEAD!  MHER 4/4/13 ==========*                       
*PHASE T21133A                                                                  
*&&      SET   TRC=N                                                            
         TITLE 'SPBUY33 - COKE BUYS FROM WORKER FILES'                          
T21133   CSECT                                                                  
         PRINT NOGEN                                                            
**********************************************************************          
*        R0   -                                                                 
*        R1   -                                                                 
*        R2   -                                                                 
*        R3   - TWA                                                             
*        R4   - WORKER FILE RECORD / OBJECT SAVE AREA(AMGWORK)                  
*        R5   -                                                                 
*        R6   - SPOT RECORD / NEXTEL                                            
*        R7   -                                                                 
*        R8   - BASE 2                                                          
*        R9   - WORKING STORAGE(BASE 1)                                         
*        RA   - BUYSAVE                                                         
*        RB   - BASE 1                                                          
*        RC   - SPBUYWKD                                                        
*        RD   - SYSTEM                                                          
*        RE   - SYSTEM                                                          
*        RF   - SYSTEM                                                          
**********************************************************************          
         EJECT                                                                  
         NMOD1 WORKLENQ,T21133,R8,RR=R7,CLEAR=YES                               
         LR    R9,RC                                                            
         USING WORKD,R9                                                         
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         ST    RD,SVPOP                SAVE THE WAY OUT                         
         ST    R7,RELO33                                                        
*                                                                               
         GOTO1 =A(INIT),RR=RELO33                                               
         BE    NEXT                                                             
*                                                                               
FATAL    MVC   BUYMSG+50(6),=C'FATAL'                                           
         MVC   ERRCNT,=X'FFFFFFFF'                                              
         GOTO1 AWRKEOF                                                          
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
****************************************************************                
* READ DATA RECORDS FROM WORKER FILE                                            
****************************************************************                
         SPACE 1                                                                
NEXT     LA    R4,OBJBUF                                                        
         USING SBUYD,R4                                                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    NEXT5                                                            
         CLI   8(R1),X'90'         EOF?                                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         GOTO1 AWRKEOF             CLOSE WRKF AND EXIT PROGRAM                  
*                                                                               
NEXT5    CLC   SBUYTYPE,=C'EOE*'    SKIP END OF EST OBJECTS                     
         BE    NEXT                                                             
         CLC   SBUYTYPE,=C'DEL*'    TEST DELETE OBJECT                          
         BE    NEXT                 IGNORE                                      
         CLC   SBUYTYPE,=C'BUY*'    TEST CURRENT OBJECT IS A BUY                
         BE    NEXT10                                                           
         CLC   SBUYTYPE,=C'EBY*'    THIS SHOULDN'T HAPPEN, BUT IT DO            
         BE    NEXT                                                             
         CLC   SBUYTYPE,=C'HDR*'    HEADER OBJECT?                              
         BE    *+6                                                              
         DC    H'0'                 NOT AN ACCEPT OBJ TYPE                      
*                                                                               
         GOTO1 AENDSTA              FINISH UP LAST STA IF NECESSARY             
         GOTO1 APROCHDR             PROCESS HEADER                              
         BE    NEXT                                                             
         B     NEXT5                ON ERROR, NEXT HDR* HAS BEEN READ           
         EJECT                                                                  
*==================================================================*            
* PROCESS BUY* OBJECT                                                           
*==================================================================*            
         SPACE 1                                                                
NEXT10   GOTO1 ACLRINP                                                          
         CLC   SBUYSTA,MYSTA       TEST CHANGE OF STATION                       
         BE    NEXT30              NO - CONTINUE                                
*                                                                               
NEXT20   GOTO1 ASETIN,DMCB,BUYSTH,SBUYSTA,L'SBUYSTA                             
         XC    BUYOP,BUYOP         CLEAR BUY OPTIONS FIELD                      
         MVI   BUYOPH+5,0                                                       
         MVC   MYSTA,SBUYSTA                                                    
         MVC   MYACN,SPACES         CLEAR SAVED ACN NUMBER                      
         MVC   MYORDN,SBUYUID       SAVED ORDER NUMBER (4)                      
*                                                                               
         GOTO1 VCALLBAS             VALIDATE STATION                            
         OC    BMGEERR,BMGEERR      TEST ERRORS                                 
         BNZ   NEXT25               NOT OK                                      
         GOTO1 ARDBUYS              BUILD BUY TABLE                             
         B     NEXT30                                                           
*                                                                               
NEXT25   DS    0H                                                               
         CLC   SACNNUM,SPACES      TEST FOR OVERRIDE ACN NUMBER                 
         BNH   NEXT26              NO                                           
         CLC   BMGEERR,=AL2(SPACNER6)                                           
         BE    NEXT30                                                           
                                                                                
NEXT26   GOTO1 AERR                 PROCESS ERROR                               
         GOTO1 ASKIPSTA             SKIP TO NEXT STATION                        
         B     NEXT5                                                            
*                                                                               
NEXT30   XC    MYWORK,MYWORK                                                    
         LA    R7,MYWORK                                                        
         CLC   MYACN,SACNNUM       TEST SAME AS LAST TIME (OR NONE)             
         BE    NEXT50                                                           
         CLC   SACNNUM,SPACES      TEST FOR OVERRIDE ACN NUMBER                 
         BNH   NEXT50              NO                                           
         MVC   MYACN,SACNNUM       SAVE NEW ACN NUMBER                          
         CLC   SVID(5),SACNNUM     TEST SAME AS DEFAULT                         
         BE    NEXT50              ACN# OK                                      
         MVC   0(3,R7),=C'ID='                                                  
         MVC   3(5,R7),SACNNUM                                                  
         LA    R7,8(R7)                                                         
*                                                                               
NEXT40   LA    RE,MYWORK                                                        
         SR    R7,RE                                                            
         GOTO1 ASETIN,DMCB,BUYOPH,MYWORK,(R7)                                   
*                                                                               
         GOTO1 VCALLBAS             VALIDATE IT                                 
         OC    BMGEERR,BMGEERR      TEST ERRORS                                 
         BZ    NEXT45                                                           
         GOTO1 AERR                                                             
         GOTO1 ASKIPSTA             FORGET THIS STATION                         
         B     NEXT5                                                            
*                                                                               
NEXT45   DS    0H                                                               
         GOTO1 ARDBUYS             NEED TO READ BUYS IN NEW MKT                 
*                                                                               
NEXT50   XC    BUYREC(256),BUYREC                                               
         MVI   NUMCOMS,0            CLEAR # OF COMMENTS PROCESSED               
         MVI   UPDSW,0              CLEAR PUTREC FLAG                           
         XC    BUYCOUNT,BUYCOUNT    CLEAR BUY COUNTER                           
         BAS   RE,NEWBUY                                                        
         B     NEXT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*===============================================================                
* ADD A NEW BUY                                                                 
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
NEWBUY   NTR1                                                                   
         LA    R4,OBJBUF                                                        
         USING SBUYD,R4                                                         
         L     R5,AMGWORK                                                       
         USING SVOBJD,R5                                                        
*                                                                               
         XC    SVBUY(4),SVBUY      CLEAR SAVED OBJECTS                          
         XC    SVDEM(4),SVDEM                                                   
         XC    SVSKD(4),SVSKD                                                   
         XC    SVCOM1(4),SVCOM1                                                 
         XC    SVCOM2(4),SVCOM2                                                 
         XC    SVCOM3(4),SVCOM3                                                 
         XC    SVCOM4(4),SVCOM4                                                 
*                                                                               
NB2      LA    R0,SVBUY                                                         
         CLC   SBUYTYPE,=C'BUY*'   FIND THE SAVE AREA FOR THIS TYPE             
         BE    NB20                                                             
*                                                                               
         LA    R0,SVDEM                                                         
         CLC   SBUYTYPE,=C'DEM*'                                                
         BE    NB20                                                             
*                                                                               
         LA    R0,SVSKD                                                         
         CLC   SBUYTYPE,=C'SKD*'                                                
         BE    NB20                                                             
*                                                                               
         CLC   SBUYTYPE,=C'COM*'                                                
         BE    NB15                                                             
*                                                                               
         CLC   SBUYTYPE,=C'EBY*'   END OF THIS BUY?                             
         BE    NB100               YES, CREATE IT                               
         B     NB25                IGNORE UNKWOWN OBJECT TYPE                   
*                                                                               
NB15     DS    0H                  COMMENT RECORD                               
         CLC   14(20,R4),SPACES    ALLOW FOR HEADER + COM*                      
         BNH   NB25                COMMENT CAN'T START WITH 20 SPACES           
         MVC   BMGEERR,=AL2(MCOMERR)                                            
         CLI   NUMCOMS,4           TOO MANY COMMENTS?                           
         BNL   NBERR                                                            
*                                                                               
         ZIC   R1,NUMCOMS                                                       
         MHI   R1,L'SVCOM1                                                      
         LA    R0,SVCOM1                                                        
         AR    R0,R1               POINT TO CORRECT COMMENT SAVE                
         ZIC   R1,NUMCOMS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NUMCOMS                                                       
*                                                                               
NB20     LH    R1,SBUYLEN          THE LENGTH OF THE DEST                       
         LA    R1,2(R1)            INCLUDE 2 LENGTH BYTES                       
         LR    RE,R4               ADDRESS OF SOURCE                            
         LR    RF,R1               LENGTH OF SOURCE                             
         MVCL  R0,RE                                                            
*                                                                               
NB25     DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    NB2                                                              
         DC    H'0'                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*===============================================================                
         SPACE 1                                                                
NB100    LA    R7,MYWORK                                                        
         L     R4,AMGWORK                        OBJECT SAVE AREA               
         USING SVOBJD,R4                                                        
*                                                                               
         MVC   BMGEERR,=AL2(MHDRERR)                                            
         OC    SVBUY(4),SVBUY                    BUY OBJECT FOUND?              
         BZ    NBERR                             YES                            
*                                                                               
         CLI   SVBUY+(SBUYDEL-SBUYD),C'Y'                                       
         BE    NBXX                              DELETE BUY,IGNORE LINE         
*                                                                               
         MVC   BMGEERR,=AL2(MSKDERR)                                            
         OC    SVSKD(4),SVSKD                    SKD OBJECT FOUND?              
         BZ    NBERR                                                            
*                                                                               
NB102    MVC   0(2,R7),=C'B,'                                                   
         LA    R7,2(R7)                                                         
*--------------------------------------------------------------------           
         LA    R2,SVSKD+(SSKDSDT-SSKDD)          START DATE                     
         OC    0(6,R2),=C'000000'                GUARANTEE NUMERIC              
         CLC   0(6,R2),=C'000000'                TEST DATE PRESENT              
         BH    NB104                             YUP                            
         MVC   0(6,R2),SVHDRSDT                  USE HDR START DATE             
         OC    0(6,R2),=C'000000'                GUARANTEE NUMERIC              
         CLC   0(6,R2),=C'000000'                TEST DATE PRESENT              
         BH    NB104                             YUP                            
         MVC   0(6,R2),SVSTART                   USE EST START DATE             
NB104    DS    0H                                                               
         MVC   BMGEERR,=AL2(MSTDTLK)                                            
         GOTO1 VDATCON,DMCB,(0,(R2)),(3,WORK)                                   
         CLC   WORK(3),SVAGYLK                COMPARE START & LOCK DATE         
         BL    NBERR                                                            
         MVC   BMGEERR,=AL2(MI5ENDT)                                            
         CLC   WORK(3),SVI5ENDT               COMPARE WITH I5 END DATE          
         BNH   NBERR                                                            
         GOTO1 VGETDAY,DMCB,(R2),WORK            VALIDATE IT                    
         MVC   SVSTDATE,0(R2)                    SAVE START DATE                
         MVC   BYTE,SVBUY+(SBUYRDAY-SBUYD)       ROTATION START DAY             
         NI    BYTE,X'0F'                        MAKE IT BINARY                 
         CLC   BYTE,0(R1)                        DATE=ROTATION START?           
         BE    NB105                             YES                            
         ZIC   R0,BYTE                           ROTATION START                 
         ZIC   R5,0(R1)                          DAY OF DATE                    
         SR    R0,R5                             DAYS TO ADD TO DATE            
         GOTO1 VADDAY,DMCB,(R2),SVSTDATE,(R0)                                   
NB105    GOTO1 VDATCON,DMCB,SVSTDATE,(11,(R7))                                  
         LA    R7,5(R7)                                                         
*--------------------------------------------------------------------           
         LA    R1,SVSKD+(SSKDCNTR-SSKDD)         # OF WEEKS                     
         LA    R1,26(R1)                         LAST ENTRY IN TABLE            
         LA    R2,14                             MAX # OF WEEKS                 
NB110    CLC   0(2,R1),=C'00'                                                   
         BH    NB115                                                            
         SH    R1,=H'2'                                                         
         BCT   R2,NB110                                                         
         LA    R2,1                              DEFAULT = 1 WEEK               
NB115    MVI   0(R7),C'-'                                                       
         LA    R7,1(R7)                                                         
         STC   R2,NUMWKS                         SAVE # OF WEEKS                
         EDIT  (R2),(4,(R7)),ALIGN=LEFT                                         
         AR    R7,R0                                                            
         MVI   0(R7),C'W'                                                       
         MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
*--------------------------------------------------------------------           
         LA    R1,XROTDAYS                       ROTATION DAYS                  
         LA    R2,SVBUY+(SBUYROT-SBUYD)                                         
         LA    R0,L'XROTDAYS                                                    
NB120    MVI   0(R7),C'.'                        ASSUME NOT TODAY               
         CLI   0(R2),C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(1,R7),0(R1)                     MOVE IN THE DAY                
         LA    R7,1(R7)                                                         
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,NB120                                                         
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
*--------------------------------------------------------------------           
         LA    R1,SVSKD+(SSKDCNTR-SSKDD)         NUMBER PER WEEK                
         ZIC   R2,NUMWKS                         # WEEKS IN TABLE               
         MVC   WORK(2),0(R1)                     INITIAL NPW                    
         NI    MYFLAGS,X'FF'-SKEDCHG             SET TO ALL SPOTS EQUAL         
NB130    CLC   WORK(2),0(R1)                                                    
         BE    NB132                                                            
         OI    MYFLAGS,SKEDCHG                    SET NEED 'C,SKED='            
         BH    NB132                                                            
         MVC   WORK(2),0(R1)                                                    
NB132    LA    R1,2(R1)                          NEXT ENTRY                     
         BCT   R2,NB130                                                         
         MVC   0(2,R7),WORK                      GREATEST NPW                   
         CLC   0(2,R7),=C'00'                    DO NOT SET NPW = 0             
         BNE   *+14                                                             
         MVC   0(2,R7),=C'01'                                                   
         OI    MYFLAGS,SKEDCHG                    SET NEED 'C,SKED='            
         MVI   2(R7),C','                                                       
         LA    R7,3(R7)                                                         
*--------------------------------------------------------------------           
         XC    0(11,R7),0(R7)                    PROGRAM TIME                   
         LA    R2,SVBUY+(SBUYSTIM-SBUYD)                                        
         PACK  DUB,0(4,R2)                                                      
         CVB   R1,DUB                                                           
         STH   R1,FULL                                                          
         PACK  DUB,4(4,R2)                                                      
         CVB   R2,DUB                                                           
         STH   R2,FULL+2                                                        
         CLC   FULL(2),FULL+2                                                   
         BNE   *+10                                                             
         XC    FULL+2(2),FULL+2                                                 
         GOTO1 VUNTIME,DMCB,FULL,0(R7)                                          
         LA    R7,10(R7)                                                        
NB140    CLI   0(R7),C' '                        LAST SIGN. CHAR?               
         BH    NB142                             YES                            
         BCTR  R7,0                                                             
         B     NB140                                                            
NB142    LA    R7,1(R7)                          NEXT WRITABLE CHAR             
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
*--------------------------------------------------------------------           
         MVC   0(1,R7),SVBUY+(SBUYDPT-SBUYD)     DAYPART                        
         CLI   0(R7),C'Z'                                                       
         BE    NB153                                                            
*                                                                               
         LA    R0,L'SVMENU-1       R0=MAX N'DAYPARTS                            
         LA    R1,SVMENU                                                        
*                                                                               
NB150    CLC   0(1,R7),0(R1)                                                    
         BE    NB155                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,NB150                                                         
NB153    MVI   0(R7),C'Q'          DEFAULT DAYPART                              
*                                                                               
NB155    MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
*--------------------------------------------------------------------           
         LA    R2,SVBUY+(SBUYSLEN-SBUYD)         SPOT LENGTH                    
         MVC   0(3,R7),0(R2)                                                    
         MVI   3(R7),C','                                                       
         LA    R7,4(R7)                                                         
*--------------------------------------------------------------------           
*                                                PROGRAM NAME                   
         MVC   0(L'BDPROGRM,R7),SVBUY+(SBUYPROG-SBUYD)                          
         LA    R7,L'BDPROGRM-1(R7)                                              
NB170    CLI   0(R7),C' '                        LAST SIGN. CHAR?               
         BH    NB172                             YES                            
         BCTR  R7,0                                                             
         B     NB170                                                            
NB172    LA    R7,1(R7)                          NEXT WRITABLE CHAR             
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
*--------------------------------------------------------------------           
         LA    R6,SVSKD+(SSKDCOST-SSKDD)         COST                           
         CLC   0(L'SSKDCOST,R6),SPACES           IN SKED?                       
         BNE   *+8                               NO                             
         LA    R6,SVBUY+(SBUYCOST-SBUYD)                                        
         PACK  DUB,0(L'SBUYCOST,R6)                                             
         CVB   R1,DUB                                                           
         EDIT  (R1),(10,(R7)),2,ZERO=NOBLANK,ALIGN=LEFT                         
         AR    R7,R0                             # CHARS INTO R1                
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
*--------------------------------------------------------------------           
         LA    RE,MYWORK                         DONE WITH 1ST LINE             
         SR    R7,RE                             LENGTH OF INPUT                
         GOTO1 ASETIN,DMCB,BUYINP1H,MYWORK,(R7)                                 
         EJECT                                                                  
*===============================================================                
         SPACE 1                                                                
         XC    MYWORK,MYWORK                                                    
         OC    SVDEM(4),SVDEM                    DEMO OBJECT?                   
         BZ    NB250                             NO                             
*                                                                               
         LA    R7,MYWORK                                                        
         SR    R6,R6                                                            
         IC    R6,NUMDEMS                        NUMBER OF EST. DEMOS           
         LA    R5,DEMODSPS                       DEMO MATCHED TABLE             
NB200    DS    0H                                                               
         CLI   0(R5),X'FF'                       END OF DEMOS                   
         BE    NB215                                                            
         LA    R2,SVDEM+(SDEMDEM-SDEMD)                                         
         ZIC   R1,0(R5)                          LOAD MATCHING DEMO #           
         LTR   R1,R1                             IS THERE A MATCH?              
         BZ    NB210                             NO                             
         BCTR  R1,0                                                             
         MH    R1,=AL2(L'SDEMDEM)                INDEX INTO LIST                
         LA    R2,0(R1,R2)                                                      
*                                                                               
         PACK  DUB,0(L'SDEMDEM,R2)                                              
         CVB   R1,DUB                                                           
         EDIT  (R1),(10,0(R7)),1,ZERO=NOBLANK,ALIGN=LEFT                        
         AR    R7,R0                             # CHARS INTO R1                
*                                                                               
NB210    MVI   0(R7),C'/'                                                       
         LA    R7,1(R7)                                                         
         LA    R5,1(R5)                          NEXT EST. DEMO                 
         BCT   R6,NB200                                                         
*                                                                               
NB215    BCTR  R7,0                              REMOVE TRAILING /'S            
         CLI   0(R7),C'/'                                                       
         BNE   *+12                                                             
         MVI   0(R7),C' '                                                       
         B     NB215                                                            
         LA    R7,1(R7)                                                         
*--------------------------------------------------------------------           
         LA    R1,SVBUY+(SBUYMAS-SBUYD)          MASTER PRODUCT                 
         CLC   0(L'SBUYMAS,R1),SPACES            OMITTED?                       
         BE    NB230                             YES                            
*                                                                               
         MVC   0(3,R7),=C',M='                                                  
         MVC   3(L'SBUYMAS,R7),0(R1)                                            
         LA    R7,L'SBUYMAS+3(R7)                                               
*--------------------------------------------------------------------           
NB230    LA    RE,MYWORK                         DONE WITH SECOND LINE          
         SR    R7,RE                             LENGTH OF INPUT                
         GOTO1 ASETIN,DMCB,BUYINP2H,MYWORK,(R7)                                 
         EJECT                                                                  
*===============================================================                
         SPACE 1                                                                
NB250    MVC   UPUID(8),SVBUY+(SBUYUID-SBUYD)                                   
*                                                                               
         OI    WRKRUPSW,WRKRUPSW_NODMER                                         
         GOTO1 VCALLBAS            SEND TO SPBUY                                
         OC    BMGEERR,BMGEERR                                                  
         BNZ   NBERR                                                            
         L     RE,BUYCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,BUYCOUNT                                                      
*                                                                               
*  WRITE EBY OBJECT BACK AFTER CLEARING ERROR CODES                             
         GOTO1 AERR                                                             
*&&TRC                                                                          
       GOTO1 AWRTSCR                           DEBUG                            
*&&                                                                             
NB252    DS    0H                                                               
         EJECT                                                                  
*===============================================================*               
* SEARCH BUY TABLE TO SEE IF THIS UNIQUE ID IS ON FILE ALREADY                  
* IF IT IS, WRITE IT BACK TO THE SAME LINE NUMBER                               
* IF IT ISN'T, USE THE LOWEST AVAILABLE LINE NUMBER                             
*===============================================================*               
         SPACE 1                                                                
         GOTO1 AFNDBUY                                                          
         CLI   MYBUYLIN,0          TEST NO MORE LINES TO WRITE OVER             
         BE    NB270                                                            
*                                                                               
NB260    L     R5,MYBUYADR         POINT TO TABLE ENTRY                         
         MVI   1(R5),C'P'          SET SLOT USED FLAG                           
*                                                                               
NB262    XC    KEY,KEY                                                          
         L     R2,AREC                                                          
         MVC   KEY(10),0(R2)           GET THE KEY                              
         MVC   KEY+11(1),MYBUYLIN      THE LINE #                               
         MVC   BUYREC+10(1),MYBUYLIN                                            
         MVC   KEY+14(4),4(R5)         GET THE DISK ADDR                        
         MVC   AREC,AREC2              READ TO AREC2                            
         OC    KEY+14(4),KEY+14        TEST RUNNING W/O FILE UPDATE             
         BZ    NB264                                                            
         GOTO1 GETREC                                                           
*                                                                               
NB264    MVC   AREC,AREC1              RESTORE AREC1 ADDRESS                    
         MVI   UPDSW,C'Y'              SET PUTREC REQUIRED                      
         B     NB290                                                            
*                                                                               
NB270    GOTO1 ADDREC                  ADD RECORD WITH EXISING LIN NUM          
         OC    KEY+14(4),KEY+14        TEST RUNNING W/O FILE UPDATE             
         BZ    NB272                                                            
         GOTO1 GETREC                  IN CASE OF CHANGE                        
*                                                                               
NB272    L     R5,ABUYTAB              ADD TO BUY TABLE                         
         SR    R0,R0                                                            
         IC    R0,KEY+11               GET BUYLINE NUMBER                       
         BCTR  R0,0                                                             
         MH    R0,=AL2(L'BUYTAB)                                                
         AR    R5,R0                   SLOT FOR THIS BUYLINE                    
         MVC   0(1,R5),KEY+11          MOVE LINE NUMBER TO SLOT                 
         MVI   1(R5),C'A'              SET SLOT USED                            
         MVC   4(4,R5),KEY+14          MOVE DISK ADDRESS                        
         MVC   8(8,R5),SVBUY+(SBUYUID-SBUYD)  MOVE UNIQID                       
         EJECT                                                                  
         SPACE 1                                                                
NB290    DS    0H                                                               
         CLI   NUMCOMS,0           ANY COMMENTS?                                
         BE    NB400               NO                                           
*                                                                               
         MVI   UPDSW,C'Y'          NEED PUTREC                                  
         GOTO1 ACLRINP             CLEAR INPUT LINES                            
         LA    R2,1                COMMENT COUNT                                
         LA    R5,SVCOM1                                                        
         USING SCOMD,R5                                                         
         LA    R6,BUYINP1H                                                      
*                                                                               
NB300    DS    0H                                                               
         CH    R2,=H'5'            ALL 4 COMMENTS DONE?                         
         BE    NB320               YES                                          
         OC    SCOMLEN,SCOMLEN     END OF COMMENTS?                             
         BZ    NB320               YES                                          
*                                                                               
         LA    R7,MYWORK                                                        
         MVC   0(8,R7),=C'C,COM=1-'                                             
         STC   R2,6(R7)                                                         
         OI    6(R7),X'F0'          MAKE EBCDIC                                 
         LA    R2,1(R2)             NEXT COMMENT NUMBER                         
         LA    R7,8(R7)                                                         
*                                                                               
         LA    R1,SCOMDATA+L'SCOMDATA                                           
NB310    CLI   0(R1),C' '           FIND LAST CHAR                              
         BH    *+8                                                              
         BCT   R1,NB310                                                         
*                                                                               
         LA    RE,SCOMDATA                                                      
         SR    R1,RE                             LENGTH OF DATA-1               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),SCOMDATA                                                 
         LA    R7,1(R1,R7)                                                      
*                                                                               
         LA    RE,MYWORK                                                        
         SR    R7,RE                             LENGTH OF INPUT                
         GOTO1 ASETIN,DMCB,(R6),MYWORK,(R7)                                     
         LA    R6,BUYINP2H-BUYINP1H(R6)                                         
         LA    R5,L'SVCOM1(R5)                                                  
         B     NB300                                                            
         DROP  R5                                                               
*                                                                               
NB320    GOTO1 VCALLBAS                          SEND TO SPBUY                  
         OC    BMGEERR,BMGEERR                                                  
         BNZ   NBERR                                                            
         EJECT                                                                  
*===============================================================                
         SPACE 1                                                                
NB400    TM    MYFLAGS,SKEDCHG                   SKED CHANGE?                   
         BZ    NBX                               NO                             
*                                                                               
         MVI   UPDSW,C'Y'          NEED PUTREC                                  
         GOTO1 ACLRINP                                                          
         LA    R7,MYWORK                                                        
         MVC   0(4,R7),=C'C,SK'                                                 
         GOTO1 VDATCON,DMCB,SVSTDATE,(7,4(R7))   GET MMMDD                      
         MVI   9(R7),C'='                                                       
         LA    R7,10(R7)                                                        
         LA    R1,SVSKD+(SSKDCNTR-SSKDD)                                        
         ZIC   R2,NUMWKS                         # WEEKS IN TABLE               
*                                                                               
NB410    MVC   0(2,R7),0(R1)                                                    
         MVI   2(R7),C'/'                                                       
         LA    R7,3(R7)                                                         
         LA    R1,2(R1)                          NEXT SKED                      
         BCT   R2,NB410                                                         
*                                                                               
         BCTR  R7,0                              REMOVE LAST '/'                
         MVI   0(R7),C' '                                                       
         LA    RE,MYWORK                                                        
         SR    R7,RE                             LENGTH OF INPUT                
         GOTO1 ASETIN,DMCB,BUYINP1H,MYWORK,(R7)                                 
*                                                                               
         GOTO1 VCALLBAS                          SEND TO SPBUY                  
*&&TRC                                                                          
       GOTO1 AWRTSCR                           DEBUG                            
*&&                                                                             
         OC    BMGEERR,BMGEERR                                                  
         BNZ   NBERR                                                            
         SPACE 1                                                                
NBX      DS    0H                                                               
         CLI   UPDSW,C'Y'                                                       
         BNE   NBXX                                                             
         OC    KEY+14(4),KEY+14    TEST HAVE NO DISK ADDRESS                    
         BZ    NBXX                RIGHT - MUST BE TEST SYSTEM U=N              
         GOTO1 PUTREC                                                           
* MAKE SURE DIRECTORY POINTER IS NOT DELETED                                    
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    KEY+13,X'80'                                                     
         BZ    NBXX                                                             
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
* NEED TO UNDELETE BRAND POINTER TOO                                            
         MVC   KEY+3(1),SVPOLPRD                                                
         MVI   KEY+10,X'FF'                                                     
         L     RE,AREC1                                                         
         MVI   KEY+11,0                                                         
         MVC   KEY+12(1),10(RE)    LINE NUMBER                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    KEY+13,X'80'                                                     
         BZ    NBXX                                                             
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
         B     NBXX                                                             
*                                                                               
NBERR    GOTO1 AERR                                                             
*                                                                               
NBXX     XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* WRITE THE TWA OUT TO TMPSTR                                                   
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
WRTSCR   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*WRTSCR*'                                                    
*                                                                               
         OI    BUYINP1H+6,X'40'    CURSOR KLUDGE                                
         LA    R1,64(R3)                                                        
WSCR10   CLI   0(R1),0             END OF TWA??                                 
         BE    WSCR20              YES                                          
         ZIC   R2,0(R1)            LENGTH OF FIELD                              
         AR    R1,R2               NEXT FIELD                                   
         B     WSCR10                                                           
*                                                                               
WSCR20   SR    R1,R3                                                            
         LR    R0,R1                                                            
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'TSTRCVR',(R0),(R3)                    
         XC    BUYMSG,BUYMSG       CLEAR MESSAGE                                
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*NEXTEL*'                                                    
NEXTEL1  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL1                                                          
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL1                                                          
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         XIT1  REGS=(R6)                                                        
         EJECT                                                                  
**********************************************************************          
* WORKER EOF ENCOUNTERED CLEAN UP AND EXIT(EVENTUALLY)                          
**********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
WRKEOF   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*WRKEOF'                                                     
*                                                                               
         GOTO1 AENDSTA             CLEAN UP FOR LAST STATION                    
* CLOSE WRKF FILE                                                               
         GOTO1 VDATAMGR,DMCB,DMCLOSE,WRKFIL,0,RECBUF,VTIA                       
* SET FINAL STATUS OF FILE TO HOLD OR PROCESSED                                 
         LA    R0,=CL8'HOLD'                                                    
         OC    ERRCNT,ERRCNT                                                    
         BNZ   *+8                                                              
         LA    R0,=CL8'PROC'                                                    
         GOTO1 VDATAMGR,DMCB,(R0),WRKFIL,WINDEX,RECBUF,VTIA                     
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(19),=C'END OF WORKER FILE'                                
         MVC   BUYMSG+20(6),RECBUF+8    MOVE LAST SEQ NUM PROCESSED             
*                                                                               
         L     RD,SVPOP                                                         
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* HANDLE ERRORS                                                                 
****************************************************************                
         SPACE 1                                                                
         DS    0H                                                               
ERR      NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**ERR***'                                                    
*                                                                               
         LA    R4,OBJBUF                                                        
         USING SEBYD,R4                                                         
*                                                                               
         OC    BMGEERR,BMGEERR                                                  
         BNZ   ERR1                                                             
         OC    SEBYODAT(L'BMGEERR),SEBYODAT                                     
         BZ    ERRX                                                             
*                                                                               
ERR1     CLC   SEBYTYPE,=C'EBY*'   TEST AT EBY ALREADY                          
         BE    ERR5                                                             
         GOTO1 AENDBUY             IF NOT, GET IT NOW                           
*                                                                               
ERR5     MVC   SEBYODAT(L'BMGEERR),BMGEERR                                      
         OC    BMGEERR,BMGEERR                                                  
         BZ    ERR10                                                            
*                                                                               
         LA    R5,BUYMSG+L'BUYMSG-12                                            
         MVC   0(6,R5),RECBUF+8    MOVE OBJ SEQ NUM                             
         LA    R5,BUYMSG+L'BUYMSG-5                                             
         EDIT  (B2,SEBYODAT),(4,(R5)),ALIGN=LEFT                                
*&&TRC                                                                          
       GOTO1 AWRTSCR                           DEBUG                            
*&&                                                                             
*                                                                               
ERR10    GOTO1 VDATAMGR,DMCB,DMWRITE,WRKFIL,WKEY,RECBUF,VTIA                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    BMGEERR,BMGEERR                                                  
         BZ    ERRX                                                             
         GOTO1 ACLRINP                                                          
         L     RE,ERRCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,ERRCNT                                                        
*                                                                               
ERRX     XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PLACE HEADER DATA ON SCREEN FOR NEW ESTIMATE.                                 
* AND BUILD THE DEMO TABLE(EVENTUALLY)                                          
*                                                                               
* IF AN ERROR OCCURS SET THE ERROR IN THE FILE AND TRY THE NEXT                 
* HEADER                                                                        
**********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
PROCHDR  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*PROCHDR'                                                    
PROCH2   GOTO1 ACLEAR              CLEAR THE SCREEN                             
         MVI   MYFLAGS,0           CLEAR FLAGS                                  
         XC    MYSTA,MYSTA         CLEAR SAVED STATION                          
*                                                                               
         LA    R4,OBJBUF                                                        
         USING SHDRD,R4            FILL IN HEADER FIELDS                        
*                                                                               
         LA    R1,SHDRSTPD         HDR START DATE                               
         LA    R2,12               6 FOR START AND END                          
PROCH4   CLI   0(R1),C'0'                                                       
         BL    PROCH6              NOT GOOD                                     
         CLI   0(R1),C'9'                                                       
         BH    PROCH6              NOT GOOD                                     
         LA    R1,1(R1)                                                         
         BCT   R2,PROCH4                                                        
         B     PROCH8              IT'S ALL GOOD                                
*                                                                               
PROCH6   MVC   BMGEERR,=AL2(MHDRDATE)   INVALID HDR DATE                        
         GOTO1 AHERR                                                            
         B     PROCHNQX                                                         
*                                                                               
PROCH8   MVC   SVHDRSDT,SHDRSTPD   SAVE HDR START DATE                          
         GOTO1 VDATCON,DMCB,SHDRSTPD,(3,SVHDRBST)    SAVE 3 BYTE FMT            
         GOTO1 (RF),(R1),SHDRENPD,(3,SVHDRBND)                                  
* MEDIA                                                                         
         GOTO1 ASETIN,DMCB,BUYMDH,SHDRMED,L'SHDRMED                             
* CLIENT                                                                        
         GOTO1 ASETIN,DMCB,BUYCLH,SHDRCLT,L'SHDRCLT                             
* PRODUCT                                                                       
         GOTO1 ASETIN,DMCB,BUYPRH,SHDRPRD,L'SHDRPRD                             
* ESTIMATE                                                                      
         GOTO1 ASETIN,DMCB,BUYESH,SHDREST,L'SHDREST                             
*                                                                               
         MVI   WORK,C'='           (PUT '=' IN FRONT OF BUYER FIELD)            
         LA    R5,SHDRBID                                                       
         LA    R6,L'SHDRBID-1                                                   
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),SHDRBID                                                
         CLI   SHDRBID,C' '                                                     
         BH    *+10                                                             
         MVC   WORK+1(6),=C'UPLOAD'                                             
         GOTO1 ASETIN,DMCB,BUYBUH,WORK,L'SHDRBID+1                              
* VALIDATE HEADER DATA - SHOULD RETURN WITH STATION ERROR SINCE NONE            
         GOTO1 VCALLBAS                                                         
         TM    BUYSTH+6,X'40'      TEST CURSOR ON STATION FIELD                 
         BZ    *+14                NO - SERIOUS ERROR                           
         CLC   BMGEERR,=AL2(1)     TEST MISSING INPUT FIELD MESSAGE             
         BE    PROCH10                                                          
         GOTO1 AHERR                                                            
         B     PROCHNQX                                                         
*                                                                               
PROCH10  CLC   SHDRDEMO(7),SPACES VALIDATE DEMOS                                
         BH    PROCH14                                                          
         MVC   BMGEERR,=AL2(MMISDEMS) MISSING DEMO ERROR                        
         GOTO1 AHERR                                                            
         B     PROCHNQX                                                         
*                                                                               
PROCH14  LA    R0,14                                                            
         LA    R1,SHDRDEMO                                                      
         LA    RE,MYWORK+8         BUILD DUMMY TWA FIELD CONTAIN                
         XC    MYWORK,MYWORK       DEMOS FOR DEMOVAL                            
*                                                                               
PROCH15  CLC   0(7,R1),SPACES                                                   
         BNH   PROCH20                                                          
         MVC   0(7,RE),0(R1)                                                    
         LA    RE,6(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C','                                                       
         LA    RE,2(RE)                                                         
         LA    R1,7(R1)                                                         
         BCT   R0,PROCH15                                                       
*                                                                               
PROCH20  BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         LA    RF,MYWORK+8                                                      
         SR    RE,RF                                                            
         STC   RE,MYWORK+5                                                      
         LA    RE,8(RE)                                                         
         STC   RE,MYWORK                                                        
*                                                                               
         L     R7,ADBLOCK                                                       
         USING DBLOCKD,R7                                                       
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'       ASSUMES NOT CANADA                           
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,VCOMFACS                                                
*                                                                               
         LHI   R2,UPDEMS-BUYSAVE                                                
         AR    R2,RA                                                            
         XC    0(L'UPDEMS,R2),0(R2)                                             
*        XC    UPDEMS,UPDEMS                                                    
*                                                                               
         L     RF,VDEMOVAL                                                      
         LA    R1,DMCB                                                          
         GOTO1 (RF),(R1),(1,MYWORK),(14,0(R2)),(C'S',DBLOCK),USRDEMS            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)          RE=#DEMOS                                    
         BNZ   PROCH25                                                          
         MVC   BMGEERR,=AL2(MBADDEMS) MISSING DEMO ERROR                        
         GOTO1 AHERR                                                            
         B     PROCH2                                                           
*                                                                               
         DROP  R7                                                               
*                                                                               
PROCH25  LR    R1,R2               REPLACE END-OF-LIST WITH 3X'0                
*ROCH25  LA    R1,UPDEMS           REPLACE END-OF-LIST WITH 3X'0                
         MH    RE,=H'3'                                                         
         LA    R1,0(RE,R1)                                                      
         XC    0(3,R1),0(R1)                                                    
         GOTO1 =A(BDEMTAB),RR=RELO33                                            
         BNZ   PROCH30             NON-ZERO MEANS OK                            
         MVC   BMGEERR,=AL2(MDEMERR)                                            
         GOTO1 AHERR                                                            
         B     PROCH2                                                           
*                                                                               
PROCH30  MVC   MYSRCE,SHDRSRCE     SAVE SOURCE OF UPLOAD                        
         MVC   SVUPDTFL,SHDRUPDO   SAVE UPDATES FLAG                            
*                                                                               
         XC    BMGEERR,BMGEERR                                                  
         GOTO1 AHERR                                                            
PROCHEQX CR    RB,RB               SET CC EQ                                    
         B     *+6                                                              
PROCHNQX LTR  RB,RB               SET CC NEQ                                    
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* HDR ERRORS                                                                    
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
HERR     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**HERR**'                                                    
*                                                                               
         LA    R4,OBJBUF                                                        
         USING SHDRD,R4                                                         
*                                                                               
         OC    BMGEERR,BMGEERR                                                  
         BNZ   HERR03                                                           
         OC    SHDRERNO,SHDRERNO                                                
         BZ    HERRX                                                            
         MVC   SHDRERNO,BMGEERR                                                 
         B     HERR05                                                           
*                                                                               
HERR03   MVC   SHDRERNO,BMGEERR                                                 
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(28),=C'HEADER ERROR XXXX SEQ=000000'                      
*                                                                               
         L     RE,ERRCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,ERRCNT                                                        
*                                                                               
         LA    R5,BUYMSG+13                                                     
         LH    R0,SHDRERNO                                                      
         N     R0,=X'00007FFF'                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R5),DUB                                                      
         MVC   BUYMSG+22(6),RECBUF+8   MOVE CURRENT SEQUENCE NUMBER             
*&&TRC                                                                          
         GOTO1 AWRTSCR              DEBUG                                       
*&&                                                                             
* WRITE ERROR TO WRKF                                                           
HERR05   GOTO1 VDATAMGR,DMCB,DMWRITE,WRKFIL,WKEY,RECBUF,VTIA                    
         CLI   8(R1),0                                                          
         BE    HERR10                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
MCOMERR  EQU   X'8001'             COMMENT COUNT ERROR(#COMS > 4)               
MDELERR  EQU   X'8002'             DELETE ERROR                                 
MPRODERR EQU   X'8003'             PRODUCT NOT IN CLIENT TABLE                  
MHDRERR  EQU   X'8004'             HDR OBJECT NOT FOUND                         
MMISDEMS EQU   X'8005'             MISSING DEMOS                                
MBADDEMS EQU   X'8006'             BAD DEMO NAMES                               
MINVERR  EQU   X'8007'             INVOICE DATA FOUND                           
MDEMERR  EQU   X'8008'             NO UPLOAD DEMOS IN ESTHDR                    
MACNERR  EQU   X'8009'             ACN VALIDATION ERROR                         
MBUYNFND EQU   X'800A'             BUY TO DELETE NOT FOUND - SPBUY36            
MROTERR  EQU   X'800B'             ROT OBJECT > 100 - SPBUY36                   
MSKDERR  EQU   X'8010'             SKED OBJECT NOT FOUND                        
MSTDTLK  EQU   X'8011'             START DATE < LOCK DATE                       
MI5ENDT  EQU   X'8012'             START DATE < I5 END DATE                     
MHDRDATE EQU   X'8013'             INVALID HDR DATE                             
* DON'T FORGET TO UPDATE ERRTAB IN SPREPCF02 WHEN ADDING NEW ERRORS             
         EJECT                                                                  
*==================================================================             
* READ TILL NEXT HDR* OBJECT                                                    
*==================================================================             
         SPACE 1                                                                
HERR10   DS    0H                                                               
         OC    BMGEERR,BMGEERR                                                  
         BZ    HERRX                                                            
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    HERR12                                                           
         CLI   8(R1),X'90'         TEST EOF                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AWRKEOF                                                          
*                                                                               
HERR12   CLC   SHDRTYPE,=C'HDR*'                                                
         BNE   HERR10                                                           
HERRX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
* ENDBUY - PROCESS OBJECTS UNTIL EBY                                            
****************************************************************                
         SPACE 1                                                                
         DS    0H                                                               
ENDBUY   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*ENDBUY*'                                                    
         LA    R4,OBJBUF                                                        
         USING SBUYD,R4                                                         
*                                                                               
ENDB10   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SBUYTYPE,=C'EBY*'                                                
         BNE   ENDB10                                                           
*                                                                               
ENDBX    XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* SKIPSTA - READ FILE UNTIL BUY* WITH NEW STATION                               
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
SKIPSTA  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*SKIPSTA'                                                    
         LA    R4,OBJBUF                                                        
         USING SBUYD,R4                                                         
*                                                                               
SKIPS10  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    SKIPS20                                                          
         CLI   8(R1),X'90'         EOF?                                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         XC    MYSTA,MYSTA         DON'T NEED THIS ANYMORE                      
         GOTO1 AWRKEOF                                                          
*                                                                               
SKIPS20  CLC   SBUYTYPE,=C'HDR*'                                                
         BE    SKIPS30                                                          
         CLC   SBUYTYPE,=C'BUY*'                                                
         BE    SKIPS22                                                          
         CLC   SBUYTYPE,=C'DEL*'                                                
         BNE   SKIPS10                                                          
*                                                                               
SKIPS22  CLC   SBUYSTA,MYSTA                                                    
         BE    SKIPS10                                                          
*                                                                               
SKIPS30  XC    MYSTA,MYSTA         INHIBIT FURTHER PROC THIS STA                
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* READ BUYS FOR THIS PRODUCT/ESTIMATE/STATION                                   
* TABLE FORMAT IS +0  LINENUM  (1)                                              
*                 +1  FLAG     (1)                                              
*                 +2  SPARE    (2)                                              
*                 +4  DISKADDR (4)                                              
*                 +8  UNIQID   (8)                                              
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
RDBUYS   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*RDBUYS*'                                                    
* CLEAR PREVIOUS BUY TABLE                                                      
         L     R0,ABUYTAB                                                       
         LH    R1,=AL2(BUYTABX-BUYTAB)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVI5ENDT,SVI5ENDT   END DATE FOR AUTO I5                         
*                                                                               
         MVC   AREC,AREC1                                                       
         L     R2,AREC                                                          
         USING BUYREC,R2                                                        
*                                                                               
         MVI   DMINBTS,X'08'       GET DELETED RECORDS                          
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       A-M/CLT/PRD/MKT/STA/EST                      
         CLI   SVPOLPRD,0          TEST BRAND POL UPLOAD                        
         BE    *+10                NO - READ ALL POL BUYS                       
         MVC   KEY+3(1),SVPOLPRD   ELSE USE THIS PRD ONLY                       
         GOTO1 HIGH                                                             
         B     RDB12                                                            
*                                                                               
RDB10    GOTO1 SEQ                                                              
*                                                                               
RDB12    CLC   KEY(10),KEYSAVE                                                  
         BNE   RDBX                                                             
*                                                                               
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    RDB10                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         TM    BUYREC+15,X'80'     TEST DELETED                                 
         BO    RDB10                                                            
         CLC   BDSTART,SVAGYLK     COMPARE START & LOCK DATE                    
         BL    RDB10                SO DON'T DEL BUYS BEFORE LOCK DATE          
         CLC   BDSTART,SVHDRBND    BUY START AFTER HEADER END                   
         BH    RDB10               YES - IGNORE                                 
         CLC   BDEND,SVHDRBST      BUY END BEFORE HEADER START                  
         BL    RDB10               YES - IGNORE                                 
* IGNORE ADJUSTMENT BUYS                                                        
         CLC   =C'ADJ',BDPROGRM                                                 
         BE    RDB10                                                            
* SAVE END DATE OF BROADCAST MONTH OF END DATE OF AUTO I5 TO MAKE SURE          
* IT IS BEFORE THE START DATE OF BUYS                                           
         CLC   =C'AUTO - I5',BDPROGRM                                           
         BE    *+14                                                             
         CLC   =C'SI5',BDPROGRM                                                 
         BNE   RDB15                                                            
         GOTO1 VDATCON,DMCB,(3,BDEND),(0,WORK)                                  
         GOTO1 VGETBROD,DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                     
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATCON,DMCB,(0,WORK+12),(3,WORK)                                
         CLC   SVI5ENDT,WORK       MAKE SURE GET LATEST END DATE                
         BNL   RDB10                                                            
         MVC   SVI5ENDT,WORK                                                    
         B     RDB10               DON'T PUT I5'S IN TABLE                      
* CHECK FOR INVOICE DATA PRESENT                                                
RDB15    LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         GOTO1 ANEXTEL                                                          
         BE    RDBNEX              INVOICE DATA FOUND                           
* TEST IF ANY PAID SPOTS                                                        
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
RDB20    GOTO1 ANEXTEL                                                          
         BNE   RDB30               DONE WITH '0B' S                             
         OC    4(2,R6),4(R6)       SPOT PAID?                                   
         BNZ   RDBNEX              YES                                          
         B     RDB20                                                            
*                                                                               
RDB30    L     R5,ABUYTAB                                                       
         SR    R0,R0                                                            
         IC    R0,BUYREC+10        GET BUYLINE NUMBER                           
         BCTR  R0,0                                                             
         MH    R0,=AL2(L'BUYTAB)                                                
         AR    R5,R0               POINT TO SLOT FOR THIS BUYLINE               
*                                                                               
         LA    R6,BDELEM           NOW FIND UNIQUE ID ELEMENT                   
         MVI   ELCDLO,X'95'        GET THE BUY DESCRIPTION ELEMENT              
         MVI   ELCDHI,X'95'                                                     
         GOTO1 ANEXTEL                                                          
         BNE   RDB10                                                            
         LA    R6,BUPUID-BUPELEM(R6)  POINT TO UNIQID                           
         CLC   MYSRCE,=C'MM'       IF IT'S AN MMPLUS UPLOAD                     
         BNE   *+14                                                             
         CLC   MYORDN,0(R6)        COMPARE FIRST 4 NUMBERS (FILE REF #)         
         BNE   RDB10                                                            
         MVC   0(1,R5),BUYREC+10      MOVE LINE NUMBER                          
         MVC   4(4,R5),KEY+14         MOVE DISK ADDRESS                         
         MVC   8(L'BUPUID,R5),0(R6)   MOVE ID TO TABLE ENTRY                    
         B     RDB10                                                            
*                                                                               
RDBX     CR    RB,RB                                                            
         B     *+6                                                              
RDBNEX   LTR   RB,RB                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------                
*  RECALL BUY LINE FROM BUY TABLE                                               
*  INPUT:                                                                       
*        SVBUY       ADDRESS OF LATEST BUY* OBJECT                              
*  OUTPUT:                                                                      
*        MYBUYLIN    BUY LINE NUMBER IF BUY FOUND                               
*        MYBUYADR    ADDRESS OF THE LINE IN THE TABLE                           
*                                                                               
*---------------------------------------------------------------                
         SPACE 1                                                                
         DS    0H                                                               
FNDBUY   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*FNDBUY*'                                                    
*                                                                               
         XC    MYBUYADR,MYBUYADR   CLEAR OUTPUT AREA                            
         MVI   MYBUYLIN,0                                                       
*                                                                               
         L     R5,AMGWORK                                                       
         USING SVOBJD,R5                                                        
         LA    R4,SVBUY            POINT TO SAVED BUY* OBJECT                   
         USING SBUYD,R4                                                         
*                                                                               
         L     R1,ABUYTAB                                                       
         LA    R0,256                                                           
*                                                                               
FNDBUY10 CLC   SBUYUID(8),8(R1)    MATCH UNIQUE ID                              
         BE    FNDBUY30                                                         
         LA    R1,L'BUYTAB(R1)                                                  
         BCT   R0,FNDBUY10                                                      
         B     FNDBUYX                                                          
*                                                                               
FNDBUY30 ST    R1,MYBUYADR         RETURN ADDRESS OF SLOT                       
         MVC   MYBUYLIN,0(R1)        AND LINE NUMBER                            
*                                                                               
FNDBUYX  DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* AT END OF STATION DELETE ALL UNUSED BUY ENTRIES IN TABLE                      
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
ENDSTA   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*ENDSTA*'                                                    
*                                                                               
         CLC   MYSTA,SPACES        ARE WE PROCESSING A STATION                  
         BNH   ENDSTAX                                                          
*                                                                               
         NI    WRKRUPSW,X'FF'-WRKRUPSW_NOIO   RESET NOIO SWITCH                 
*                                                                               
         GOTO1 AREQBGL             GENERATE REPORT REQUEST                      
*                                                                               
         CLI   SVUPDTFL,C'Y'       UPDATES ONLY?                                
         BE    ENDSTAX             DON'T DELETE BUYS                            
*                                                                               
         L     R5,ABUYTAB                                                       
         LA    R6,256                                                           
*                                                                               
ENDSTA2  CLI   0(R5),0             TEST DATA PRESENT                            
         BE    ENDSTA10            NO - LINE DOES NOT EXIST                     
         CLI   1(R5),0             TEST LINE NUMBER ADDED/CHANGED               
         BNE   ENDSTA10                                                         
* LINE NOT USED - RECALL AND DELETE IT                                          
         GOTO1 ACLRINP                                                          
         ZIC   R0,0(R5)            LINE NUMBER                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MYWORK(3),DUB                                                    
         GOTO1 ASETIN,DMCB,BUYINP1H,MYWORK,3                                    
*                                                                               
         MVC   MYWORK(3),=C'DEL'                                                
         GOTO1 ASETIN,DMCB,BUYINP2H,MYWORK,3                                    
*                                                                               
         GOTO1 VCALLBAS                                                         
         OC    BMGEERR,BMGEERR                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ENDSTA10 LA    R5,L'BUYTAB(R5)                                                  
         BCT   R6,ENDSTA2                                                       
*                                                                               
ENDSTAX  OI    WRKRUPSW,WRKRUPSW_NOIO   SET NOIO SWITCH                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------                
* BUILD DEMO LOOKUP TABLE                                                       
*---------------------------------------------------------------                
         SPACE 1                                                                
         DS    0H                                                               
BDEMTAB  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*BDEMTAB'                                                    
*                                  IGNORE ERRORS                                
BDTAB2   LA    R6,SVBRDEMS         BRAND TABLE                                  
         OC    SVBRDEMS,SVBRDEMS                                                
         BNZ   *+8                                                              
         LA    R6,SVDEMOS          POOL TABLE                                   
*                                                                               
         LA    R7,DEMODSPS                                                      
         SR    R2,R2               EST. DEMO COUNT                              
         XC    DEMODSPS(14),DEMODSPS                                            
*                                                                               
BDTAB5   LHI   R5,UPDEMS-BUYSAVE   NEW WAY OF CALLING UPDEMS                    
         AR    R5,RA                                                            
*DTAB5   LA    R5,UPDEMS           OLD WAY                                      
         LA    R2,1(R2)            ONE MORE EST. DEMO                           
         LA    R1,1                                                             
         OC    0(3,R6),0(R6)       END OF EST. DEMOS                            
         BZ    BDTABEX                                                          
*                                                                               
BDTAB12  OC    0(3,R5),0(R5)       END OF UPDEMS                                
         BZ    BDTAB14                                                          
         CLC   0(3,R5),0(R6)       MATCH UPDEMS TO EST. DEMOS                   
         BE    BDTAB16                                                          
         LA    R5,3(R5)            NEXT UPDEM                                   
         LA    R1,1(R1)                                                         
         B     BDTAB12                                                          
*                                                                               
BDTAB14  SR    R1,R1               NOT IN UPDEMS                                
BDTAB16  STC   R1,0(R7)            STORE MATCHED DISP                           
         LA    R7,1(R7)                                                         
         LA    R6,3(R6)            NEXT EST. DEMO                               
         B     BDTAB5                                                           
*                                                                               
BDTABEX  STC   R2,NUMDEMS          SAVE NUMBER OF DEMOS                         
         MVI   0(R7),X'FF'         END OF DEMODISP TAB                          
         LTR   R2,R2               EXIT WITH CC ZERO ON ERROR                   
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* BUILD THE DEMO TABLE TO CHECK FOR CHANGES                                     
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
BLDDEMT  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*BLDDEM*'                                                    
         LA    R7,MYWORK                                                        
         EDIT  SVLNNUM,(3,0(R7)),ALIGN=LEFT                                     
         AR    R7,R0                                                            
         MVI   0(R7),C'D'          RECALL BUY(SKED FORMAT)                      
*                                                                               
         GOTO1 VCALLBAS            BUILD SV(BRD)DEMOS                           
         CLC   BMGEERR,=H'1'       MISSING INPUT ERROR OK                       
         BNE   BDEMNEX                                                          
*                                                                               
         LA    R7,BUYINP4H                                                      
         LA    R6,SVDEMT                                                        
*                                                                               
BDEMT5   ZIC   R1,0(R7)            FIND NEXT UNPROTECTED FIELD                  
         AR    R7,R1               NEXT FIELD                                   
         CLI   0(R7),0             END OF SCREEN?                               
         BE    BDEMT20             YES                                          
         TM    1(R7),X'20'         PROTECTED?                                   
         BO    BDEMT5              YES                                          
*                                                                               
         CLI   0(R7),9             1 BYTE UNPROTECTED IS END OF SCREEN          
         BE    BDEMT20                                                          
*                                                                               
         XC    0(6,R6),0(R6)       CLEAR THIS ENTRY                             
         LA    R1,8(R7)            BEGINING OF DEMO VALUE                       
         CLI   0(R1),C' '          FIND END OF DATA                             
         BNH   *+12                                                             
         LA    R1,1(R1)            NEXT CHAR                                    
         B     *-12                                                             
*                                                                               
         LA    R0,8(R7)                                                         
         SR    R1,R0                                                            
         LR    R0,R1                                                            
         GOTO1 VCASHVAL,DMCB,(2,8(R7)),(R0)                                     
         MVC   0(4,R6),4(R1)                                                    
*                                                                               
         LA    R6,L'SVDEMT(R6)     NEXT DEM SAVE                                
         B     BDEMT5                                                           
*                                                                               
BDEMT20  MVI   0(R6),X'FF'         END OF TABLE                                 
         B     BDEMTX                                                           
*                                                                               
BDEMTX   CR    RB,RB                                                            
         B     *+6                                                              
BDEMNEX  LTR   RB,RB                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* INITIALIZE                                                                    
* WORKER FILE CONTAINS THE FOLLOWING                                            
* 2101SCRIPTSPCOKUPL   ...                                                      
* 2102000001SIGNON INFORMATION  COKEAT                                          
* 2102000002                    @#$%NN  WHERE NN IS FILE NUMBER                 
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
INIT     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**INIT**'                                                    
*                                                                               
         LH    RE,=AL2(BUYTAB-WORKD)                                            
         AR    RE,R9               ADD LOCAL W/S REG                            
         ST    RE,ABUYTAB                                                       
         SH    RE,=H'8'                                                         
         MVC   0(8,RE),=CL8'*BUYTAB*'                                           
*                                                                               
         MVC   SVUID,10(R3)        SAVE USERID FROM TWA                         
         XC    SVSTAT,SVSTAT                                                    
         OI    WRKRUPSW,WRKRUPSW_ISME   TELL SPBUY WHO IT IS                    
         OI    WRKRUPSW,WRKRUPSW_NOIO   AND TO SUPPRESS I/O                     
*                                                                               
         MVC   ZEROS,=C'000000000'                                              
         MVC   DMOPEN,=CL8'OPEN'                                                
         MVC   DMCLOSE,=CL8'CLOSE'                                              
         MVC   DMREAD,=CL8'READ'                                                
         MVC   DMWRITE,=CL8'WRITE'                                              
         MVC   DMSEQ,=CL8'SEQ'                                                  
         MVC   DMINDEX,=CL8'INDEX'                                              
         MVC   WRKFIL,=CL8'WRKFILE'                                             
         MVC   XROTDAYS,=C'MTWTFSS'                                             
*                                                                               
         LA    R1,ADCONS                                                        
         LA    R0,(ADCONX-ADCONS)/4                                             
         LA    RE,ASETIN                                                        
*                                                                               
INIT2    L     RF,0(R1)                                                         
         A     RF,RELO33                                                        
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,INIT2                                                         
         B     INIT10                                                           
*                                                                               
ADCONS   DS    0F                                                               
         DC    A(SETIN)                                                         
         DC    A(WRKEOF)                                                        
         DC    A(PROCHDR)                                                       
         DC    A(CLEAR)                                                         
         DC    A(CLEARINP)                                                      
         DC    A(ERR)                                                           
         DC    A(HERR)                                                          
         DC    A(FNDBUY)                                                        
         DC    A(ENDBUY)                                                        
         DC    A(ENDSTA)                                                        
         DC    A(RDBUYS)                                                        
         DC    A(WRTSCR)                                                        
         DC    A(NEXTEL)                                                        
         DC    A(SKIPSTA)                                                       
         DC    A(REQBGL)                                                        
ADCONX   EQU   *                                                                
         EJECT                                                                  
INIT10   DS    0H                                                               
* TELL FACPAK TO LET TRANSACTION EXCEED MAXIOS                                  
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'80',0),F#MIOST                                      
*                                                                               
         GOTO1 VSCANNER,DMCB,(0,BUYBUH),(2,SLINES)                              
         TM    SLINES+2,X'80'                                                   
         BZ    INVWRKR                                                          
         MVC   FILNO,SLINES+6                                                   
*                                                                               
* OPEN WORKER FILE                                                              
*                                                                               
INIT20   XC    WINDEX,WINDEX                                                    
         LA    R2,WINDEX                                                        
         USING UKRECD,R2                                                        
         MVC   UKUSRID,SVUID                                                    
         MVC   UKFILENO,FILNO                                                   
         OI    UKFLAG,X'80'                                                     
         GOTO1 VDATAMGR,DMCB,DMINDEX,WRKFIL,WINDEX,RECBUF,VTIA                  
         NI    UKFLAG,X'FF'-X'80'                                               
         CLI   8(R1),0                                                          
         BE    INIT30                                                           
         CLI   8(R1),X'90'         EOF?                                         
         BE    INVWRKR              YES                                         
         DC    H'0'                                                             
*                                                                               
**  SKIP WORKER FILE HEADER RECORD - SCRIPT NAME, ETC. >>                       
*                                                                               
INIT30   CLC   UKUSRID,SVUID       CHECK IF GOT BACK SAME WORKER FILE           
         BNE   INVWRKR                                                          
         CLC   UKFILENO,FILNO                                                   
         BNE   INVWRKR                                                          
         DROP  R2                                                               
*                                                                               
INIT35   GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    INIT40                                                           
         CLI   8(R1),X'90'         EOF?                                         
         BE    INEQXIT              YES                                         
         DC    H'0'                                                             
*                                                                               
**  SKIP WORKER FILE SIGNON DATA                       >>                       
*                                                                               
INIT40   GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    INIT50                                                           
         CLI   8(R1),X'90'         EOF?                                         
         BE    INEQXIT              YES                                         
         DC    H'0'                                                             
*                                                                               
**  SKIP ANOTHER RECORD                                                         
*                                                                               
INIT50   GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    IEQXIT                                                           
         CLI   8(R1),X'90'         EOF?                                         
         BE    INEQXIT              YES                                         
         DC    H'0'                                                             
IEQXIT   CR    RB,RB                                                            
         B     IEXIT                                                            
INEQXIT  LTR   RB,RB                                                            
IEXIT    XIT1                                                                   
         SPACE 2                                                                
INVWRKR  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(INVWRKNO)                                            
         GOTO1 ERROR                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* ROUTINE MOVES DATA TO A TWA FIELD AND SETS APPROPRIATE HEADER                 
* ON ENTRY, PARAMETER 1 = A(FIELD HEADER)                                       
*           PARAMETER 2 = A(DATA FIELD)                                         
*           PARAMETER 3 = L'DATA FIELD                                          
*===============================================================                
         SPACE 1                                                                
         DS    0H                                                               
SETIN    NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**SETIN*'                                                    
         L     R2,0(R1)            R2=A(FIELD HEADER)                           
         L     R5,4(R1)            R5=A(DATA FIELD)                             
         L     R6,8(R1)            R6=L'DATA FIELD                              
*                                                                               
         LR    RF,R5               A(DATA)                                      
         ZIC   R0,0(R2)                                                         
         SH    R0,=H'8'                                                         
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R0,=H'8'                                                         
*                                                                               
         LR    R1,R0                                                            
         BCTR  R1,0                CLEAR THE FIELD                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
         CR    R6,R0               COMPARE L'DATA TO L'FIELD                    
         BNH   *+6                                                              
         LR    R6,R0                HIGH - USE L'FIELD                          
*                                                                               
         AR    RF,R6               POINT TO END OF FIELD                        
         BCTR  RF,0                RF POINTS TO LAST BYTE THAT FITS             
*                                                                               
         CLI   0(RF),C' '          SCAN BACKWARDS FOR NON-SPACE                 
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R6,*-10                                                          
         B     SETINX              NO INPUT                                     
*                                                                               
         STC   R6,5(R2)            INPUT LENGTH                                 
         NI    4(R2),X'DF'         UNSET PREVIOUSLY VALIDATED                   
         OI    6(R2),X'80'         TRANSMIT BIT                                 
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R5)       MOVE THE DATA                                
         LA    R6,1(R6)            RESET R6                                     
*                                                                               
SETIN2   CLI   0(RF),C'0'          TEST NUMERIC                                 
         BL    SETINX                                                           
         CLI   0(RF),C'9'                                                       
         BH    SETINX                                                           
         BCTR  RF,0                                                             
         BCT   R6,SETIN2                                                        
         OI    4(R2),X'08'         SET VALID NUMERIC                            
*                                                                               
SETINX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* ROUTINE TO CLEAR ALL UNPROTECTED FIELDS IN THE TWA                            
****************************************************************                
         SPACE 1                                                                
         DS    0H                                                               
CLEAR    NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**CLEAR*'                                                    
         LA    R2,64(R3)                                                        
         SR    RF,RF                                                            
*                                                                               
CLEAR2   ZIC   RF,0(R2)            TOTAL FIELD LENGTH                           
         LTR   RF,RF                                                            
         BZ    CLEARX              0=END OF SCREEN                              
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    CLEAR4                                                           
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8              CLEAR THE FIELD                              
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         NI    6(R2),X'BF'         UNSET THE CURSOR                             
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
*                                                                               
CLEAR4   ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         B     CLEAR2                                                           
*                                                                               
CLEARX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* ROUTINE TO CLEAR ALL BUY INPUT LINES                                          
****************************************************************                
         SPACE 1                                                                
         DS    0H                                                               
CLEARINP NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*CLRINP*'                                                    
         XC    BUYINP1,BUYINP1                                                  
         OI    BUYINP1H+6,X'80'         TRANSMIT                                
         NI    BUYINP1H+6,X'BF'         UNSET THE CURSOR                        
         MVI   BUYINP1H+4,0             CLEAR INPUT INDICATORS                  
         MVI   BUYINP1H+5,0             LENGTH 0                                
*                                                                               
         XC    BUYINP2,BUYINP2                                                  
         OI    BUYINP2H+6,X'80'         TRANSMIT                                
         NI    BUYINP2H+6,X'BF'         UNSET THE CURSOR                        
         MVI   BUYINP2H+4,0             CLEAR INPUT INDICATORS                  
         MVI   BUYINP2H+5,0             LENGTH 0                                
*                                                                               
         XC    BUYINP3,BUYINP3                                                  
         OI    BUYINP3H+6,X'80'         TRANSMIT                                
         NI    BUYINP3H+6,X'BF'         UNSET THE CURSOR                        
         MVI   BUYINP3H+4,0             CLEAR INPUT INDICATORS                  
         MVI   BUYINP3H+5,0             LENGTH 0                                
*                                                                               
         XC    BUYINP4,BUYINP4                                                  
         OI    BUYINP4H+6,X'80'         TRANSMIT                                
         NI    BUYINP4H+6,X'BF'         UNSET THE CURSOR                        
         MVI   BUYINP4H+4,0             CLEAR INPUT INDICATORS                  
         MVI   BUYINP4H+5,0             LENGTH 0                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*================================================================               
* GENERATE A BUYING GUIDELINES REQUEST                                          
*================================================================               
         SPACE 1                                                                
         DS    0H                                                               
REQBGL   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*REQBGL*'                                                    
*                                                                               
         OC    BUYCOUNT,BUYCOUNT   TEST ANY BUYS SUCCESSFUL                     
         BZ    REQBGLX             NO - NORMAL UPLOAD SUCCESS RATE              
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+26                                                       
         MVI   0(R4),C' '          EVAN SAYS TO USE SF SPACES                   
         MVC   1(79,R4),0(R4)                                                   
         MVC   0(2,R4),=C'BG'      JCL ID (BUYING GUIDELINES)                   
         MVC   2(2,R4),AGYALPHA                                                 
*                          1.2  .3  ..5                                         
         MVC   4(24,R4),=C'*.BGL.REP..OV,CCX.......'                            
         MVC   28(1,R4),BUYMD                                                   
         MVI   29(R4),C'.'                                                      
         MVC   30(3,R4),QCLT                                                    
         MVI   33(R4),C'.'                                                      
         MVC   34(3,R4),QPRD                                                    
         MVI   37(R4),C'.'                                                      
         ZIC   R0,SVKEY+9          ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  38(3,R4),DUB                                                     
         MVI   41(R4),C'.'                                                      
         ICM   R0,3,SVKEY+4        MARKET                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  42(4,R4),DUB                                                     
         LA    R4,46(R4)           POINT BEYOND MARKET                          
* NOTE THAT DATES DEFAULT TO 'ES'                                               
         MVC   0(2,R4),=C'.*'                                                   
         XC    DUB,DUB                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',DUB,ELEM                     
         EJECT                                                                  
* GENERATE A SPOT C9 REQUEST TOO                                                
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+26                                                       
         MVI   0(R4),C' '                                                       
         MVC   1(79,R4),0(R4)                                                   
         MVC   0(2,R4),=C'C9'      PROG ID                                      
         MVC   2(2,R4),AGYALPHA                                                 
*                                                                               
         MVC   4(1,R4),BUYMD                                                    
         MVC   5(3,R4),QCLT                                                     
         MVC   11(3,R4),QPRD                                                    
*                                                                               
         ICM   R0,3,SVKEY+4        MARKET                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  14(4,R4),DUB                                                     
*                                                                               
         MVC   18(5,R4),QSTA       STATION                                      
         CLI   22(R4),C' '                                                      
         BH    *+8                                                              
         MVI   22(R4),C'T'                                                      
*                                                                               
         ZIC   R0,SVKEY+9          ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
*                                                                               
         MVC   31(2,R4),=C'ES'                                                  
         XC    DUB,DUB                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',DUB,ELEM                     
REQBGLX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* WORKING STORAGE                                                               
*===============================================================                
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
ZEROS    DS    9C'0'                                                            
DMOPEN   DS    CL8'OPEN'                                                        
DMCLOSE  DS    CL8'CLOSE'                                                       
DMREAD   DS    CL8'READ'                                                        
DMWRITE  DS    CL8'WRITE'                                                       
DMSEQ    DS    CL8'SEQ'                                                         
DMINDEX  DS    CL8'INDEX'                                                       
WRKFIL   DS    CL8'WRKFILE'                                                     
XROTDAYS DS    CL7                                                              
         EJECT                                                                  
RELO33   DS    A                                                                
ASETIN   DC    A(SETIN)                                                         
AWRKEOF  DC    A(WRKEOF)                                                        
APROCHDR DC    A(PROCHDR)                                                       
ACLEAR   DC    A(CLEAR)                                                         
ACLRINP  DC    A(CLEARINP)                                                      
AERR     DC    A(ERR)                                                           
AHERR    DC    A(HERR)                                                          
AFNDBUY  DC    A(FNDBUY)                                                        
AENDBUY  DC    A(ENDBUY)                                                        
AENDSTA  DC    A(ENDSTA)                                                        
ARDBUYS  DC    A(RDBUYS)                                                        
AWRTSCR  DC    A(WRTSCR)                                                        
ANEXTEL  DC    A(NEXTEL)                                                        
ASKIPSTA DC    A(SKIPSTA)                                                       
AREQBGL  DC    A(AREQBGL)                                                       
*                                                                               
SVRE     DS    A                                                                
SVPOP    DS    A                   TOP OF MY D-CHAIN                            
ABUYTAB  DS    A                                                                
         EJECT                                                                  
BUYCOUNT DS    F                                                                
ERRCNT   DS    F                                                                
SVBDELEM DS    A                   SAVE THE ADDRESS OF BDELEM                   
SVBCOST  DS    F                   SAVE BUY COST                                
SVHDRSDT DS    CL6                 START DATE FROM HDR*                         
SVHDRBST DS    XL3                 BINARY START DATE                            
SVHDRBND DS    XL3                 BINARY END DATE                              
MYBUYLIN DS    0CL1                                                             
MYBUYADR DS    A                                                                
FILNO    DS    H                   FILE NUMBER                                  
SVUID    DS    XL2                 USER ID FROM TWA + 10                        
MYSTA    DS    CL(L'SBUYSTA)       SAVE OF STATION STRING                       
MYACN    DS    CL5                                                              
MYORDN   DS    CL4                 FIRST 4 BYTES OF UNIQUE BUY ID               
SVI5ENDT DS    XL3                 END DATE OF BROADCAST MONTH FOR I5           
MYSRCE   DS    CL2                 SOURCE OF UPLOAD (MM=MMPLUS)                 
SVUPDTFL DS    C                   UPDATES ONLY FLAG                            
MYFLAGS  DS    X                                                                
SKEDCHG  EQU   X'80'                                                            
MBADSTA  EQU   X'40'                                                            
PERCHG   EQU   X'20'                                                            
SVLNNUM  DS    X                   SAVE BUY LINE # FROM BUYTAB                  
SVSKDT   DS    15XL2               SAVE SKED INFO FROM RECALL                   
SVDEMT   DS    15XL6               SAVE DEMO INFO FROM RECALL                   
SVBROT   DS    CL(L'SBUYROT)       SAVE ROTATION DAYS                           
SVBRDAY  DS    C                   SAVE ROTAION START DAY                       
SVSTDATE DS    CL6                                                              
NUMWKS   DS    X                                                                
NUMCOMS  DS    X                   NUMBER OF COMMENTS                           
MYWORK   DS    XL256                                                            
USRDEMS  DS    CL35                STUPID DEMOVAL                               
*                                                                               
DEMODSPS DS    14C                 DEMO DISPLACEMENTS                           
NUMDEMS  DS    X                                                                
*                                                                               
SLINES   DS    3CL32               SCANNER                                      
         SPACE 2                                                                
         SPACE 2                                                                
WINDEX   DS    CL32                                                             
         ORG   WINDEX                                                           
WKEY     DS    CL8                                                              
         ORG                                                                    
         DS    0D                                                               
RECBUF   DS    4096C                                                            
         ORG   RECBUF                                                           
         DS    CL14                WORKER FILE KEY                              
OBJBUF   DS    0C                                                               
         ORG                                                                    
         DS    0D                                                               
BUYTABID DS    CL8                                                              
BUYTAB   DS    256CL16                                                          
BUYTABX  EQU   *                                                                
WORKLENQ EQU   *-WORKD                                                          
         EJECT                                                                  
*********************************************************************           
* SAVE OBJECTS FOR NEWBUYS (USE TIA)                                            
*********************************************************************           
         SPACE                                                                  
SVOBJD   DSECT                                                                  
* 2 EXTRA BYTES BECAUSE LEN OF LEN FIELD NOT INCLUDED IN LEN EQU                
SVBUY    DS    CL(SBUYLENQ+2)                                                   
SVSKD    DS    CL(SSKDLENQ+2)                                                   
SVDEM    DS    CL(SDEMLENQ+2)                                                   
SVCOM1   DS    CL(SCOMLENQ+2)                                                   
SVCOM2   DS    CL(SCOMLENQ+2)                                                   
SVCOM3   DS    CL(SCOMLENQ+2)                                                   
SVCOM4   DS    CL(SCOMLENQ+2)                                                   
SVOEND   DS    0C                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
*        EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
* DEDBLOCK                                                                      
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFK                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPBUY33   04/10/13'                                      
         END                                                                    
