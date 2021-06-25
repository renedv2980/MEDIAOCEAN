*          DATA SET SPBUY36    AT LEVEL 183 AS OF 04/10/13                      
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
*PHASE T21133A                                                                  
*==========  THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*==========  THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*==========  THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*==========  THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*==========  THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
*==========  THIS PROGRAM IS DEAD  MHER 4/4/13 ===========*                     
         TITLE 'SPBUY36 - UPLOAD BUYS FROM WORKER FILE'                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* THIS IS NOT THE COKE UPLOAD. THAT IS SPBUY33.                       *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
T21136   CSECT                                                                  
         PRINT NOGEN                                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        R0   -                                                       *         
*        R1   -                                                       *         
*        R2   -                                                       *         
*        R3   - TWA                                                   *         
*        R4   - WORKER FILE RECORD / OBJECT SAVE AREA(AMGWORK)        *         
*        R5   -                                                       *         
*        R6   - SPOT RECORD / NEXTEL                                  *         
*        R7   -                                                       *         
*        R8   - BASE 2                                                *         
*        R9   - WORKING STORAGE(BASE 1)                               *         
*        RA   - BUYSAVE                                               *         
*        RB   - BASE 1                                                *         
*        RC   - SPBUYWKD                                              *         
*        RD   - SYSTEM                                                *         
*        RE   - SYSTEM                                                *         
*        RF   - SYSTEM                                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
         NMOD1 WORKLENQ,T21136,R8,RR=R7,CLEAR=YES                               
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
         ST    R7,RELO36                                                        
*                                                                               
         GOTO1 =A(INIT),RR=RELO36                                               
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* READ DATA RECORDS FROM WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* PROCESS BUY* OBJECT                                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
NEXT10   GOTO1 ACLRINP                                                          
         CLC   SBUYSTA,MYSTA       TEST CHANGE OF STATION                       
         BE    NEXT30              NO - CONTINUE                                
         CLC   MYSTA,SPACES        TEST FIRST TIME                              
         BNH   NEXT20                                                           
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
         GOTO1 AERR                 PROCESS ERROR                               
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ADD A NEW BUY                                                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         DS    0H                                                               
NEWBUY   NTR1                                                                   
         LA    R4,OBJBUF                                                        
         USING SBUYD,R4                                                         
         L     R5,AMGWORK                                                       
         USING SVOBJD,R5                                                        
         LA    R7,NWROTS                                                        
         USING NWROTABD,R7                                                      
*                                                                               
         MVI   SVNPW,0                                                          
         MVI   NUMROTS,0                                                        
         XC    SVRDATE,SVRDATE                                                  
         XC    SVBUY(4),SVBUY      CLEAR SAVED OBJECTS                          
         XC    SVDEM(4),SVDEM                                                   
         XC    SVSKD,SVSKD                                                      
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
***      BE    NB20                                                             
         BNE   *+12                                                             
         OI    MYFLAGS,SKEDDATA    SKED DATA PRESENT                            
         B     NB20                                                             
*                                                                               
         CLC   SBUYTYPE,=C'ROT*'                                                
         BE    NB22                                                             
*                                                                               
         CLC   SBUYTYPE,=C'COM*'                                                
         BE    NB15                                                             
*                                                                               
         CLC   SBUYTYPE,=C'EBY*'   END OF THIS BUY?                             
         BE    NB100               YES, CREATE IT                               
         B     NB25                IGNORE UNKHOWN OBJECT TYPE                   
*                                                                               
NB15     DS    0H                  COMMENT RECORD                               
         MVC   BMGEERR,=AL2(MCOMERR)                                            
         CLI   NUMCOMS,4           TOO MANY COMMENTS?                           
         BNL   NBERR                                                            
*                                                                               
         ZIC   R1,NUMCOMS                                                       
         MH    R1,=AL2(L'SVCOM1)                                                
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
         B     NB25                                                             
         DROP  R4                                                               
*                                                                               
NB22     DS    0H                  ROTATION RECORD                              
         USING SROTD,R4                                                         
         PACK  DUB,SROTSPTS        NUMBER OF SPOTS                              
         CVB   R6,DUB                                                           
*                                                                               
NB22AA   MVC   BMGEERR,=AL2(MROTERR)                                            
         CLI   NUMROTS,20          TOO MANY ROTATIONS?                          
         BNL   NBERR                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(0,SROTDATE),(2,NWRDATE)                            
         MVC   SVLASTDT,NWRDATE                                                 
         MVC   BMGEERR,=AL2(MROTORDR)   ROTS OUT OF ORDER                       
         CLC   SVRDATE,NWRDATE     DATE CHANGED?                                
         BH    NBERR                                                            
         BE    *+8                                                              
         MVI   SVRSEQ,0                                                         
         ZIC   R1,SVRSEQ                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SVRSEQ                                                        
         STC   R1,NWRSEQ                                                        
*                                                                               
         CLC   SVRSEQ,SVNPW        SAVE HIGHEST NUMBER PER WEEK                 
         BNH   *+10                                                             
         MVC   SVNPW,SVRSEQ                                                     
*                                                                               
         MVC   SVRDATE,NWRDATE     SAVE ROTATION DATE                           
         PACK  DUB,SROTCOST        COST                                         
         CVB   R1,DUB                                                           
         STCM  R1,7,NWRCOST                                                     
*                                                                               
         GOTO1 AGTBPRD,DMCB,SROTPRD1                                            
         MVC   BMGEERR,=AL2(MIVPRDRO)                                           
         CLI   DMCB,0                                                           
         BE    NBERR                                                            
         MVC   NWRPRD1,DMCB                                                     
*                                                                               
         PACK  DUB,SROTLEN1                                                     
         CVB   R1,DUB                                                           
         STC   R1,NWRLEN1                                                       
*                                                                               
         CLC   SROTPRD2,SPACES                                                  
         BE    NB22A                                                            
         GOTO1 AGTBPRD,DMCB,SROTPRD2                                            
         MVC   BMGEERR,=AL2(MIVPRDRO)                                           
         CLI   DMCB,0                                                           
         BE    NBERR                                                            
         MVC   NWRPRD2,DMCB                                                     
*                                                                               
         ZIC   R1,SVBUY+(SBUYSLEN-SBUYD)         TOTAL SPOT LENGTH              
         ZIC   RE,NWRLEN1                                                       
         SR    R1,RE                                                            
         STC   R1,NWRLEN2                        LENGTH 2                       
*                                                                               
NB22A    ZIC   R1,NUMROTS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NUMROTS                                                       
         LA    R7,NWROTLNQ(R7)                                                  
         BCT   R6,NB22AA                                                        
*                                                                               
NB25     DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFIL,WKEY,RECBUF,VTIA                     
         CLI   8(R1),0                                                          
         BE    NB2                                                              
         DC    H'0'                                                             
         DROP  R4,R5,R7                                                         
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
NB100    MVI   0(R7),X'FF'                                                      
         LA    R7,MYWORK                                                        
         L     R4,AMGWORK                        OBJECT SAVE AREA               
         USING SVOBJD,R4                                                        
*                                                                               
         MVC   BMGEERR,=AL2(MHDRERR)                                            
         OC    SVBUY(4),SVBUY                    BUY OBJECT FOUND?              
         BZ    NBERR                             YES                            
*                                                                               
         MVC   BMGEERR,=AL2(MSKDERR)                                            
         OC    SVSKD(4),SVSKD                    SKD OBJECT FOUND?              
         BZ    NBERR                                                            
*                                                                               
         NI    MYFLAGS,X'FF'-CHNGBUY                                            
                                                                                
         LA    R1,BUYINP1H                                                      
         ST    R1,BUYLNH                                                        
         XC    MYWORK,MYWORK                                                    
         GOTO1 AFNDBUY                                                          
*                                                                               
         CLI   MYBUYLIN,0                        HAVE THE LINE NO?              
         BNE   NB110                             YES                            
*                                                                               
         CLI   SVBUY+(SBUYDEL-SBUYD),C'Y'        DELETE THE BUY?                
         BNE   PER20                             NO, ADDING A NEW BUY           
         MVC   BMGEERR,=AL2(MBUYNFND)            YES, BUT DON'T HAVE            
         B     NBERR                              BUYLINE NO.                   
*                                                                               
* RECALL THE BUYLINE                                                            
NB110    L     R1,MYBUYADR                       POINT TO TABLE ENTRY           
         MVI   1(R1),C'P'                        SET SLOT USED FLAG             
         EDIT  (1,MYBUYLIN),(3,(R7)),ALIGN=LEFT  BUYLINE NUMBER                 
         LR    R7,R0                             LENGTH OF INPUT                
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         XC    MYWORK,MYWORK                                                    
*                                                                               
         CLI   SVBUY+(SBUYDEL-SBUYD),C'Y'        DELETE THE BUY?                
         BNE   PER10                             NO, JUST CHANGING IT           
         MVI   MYWORK,C'D'                       DELETE BUYLINE                 
         LA    R7,1                                                             
         MVI   CALLB,C'Y'                                                       
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         B     NB251B                                                           
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE PERIOD:                                 *         
*      C,PER=JAN16-6W,.....S.                                         *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE PERIOD:          *         
*      B,JAN16-6W,.....S.,  (TO BE CONT. IN NEXT SECTION)             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PER10    OI    MYFLAGS,CHNGBUY                                                  
         MVC   MYWORK(6),=C'C,PER='              CHANGE PERIOD                  
         LA    R7,MYWORK+6                                                      
         B     PER30                                                            
*                                                                               
PER20    MVC   0(2,R7),=C'B,'                    (ADDING BUY)                   
         LA    R7,2(R7)                                                         
*                                                                               
PER30    LA    R2,SVSKD+(SSKDSDT-SSKDD)          START DATE                     
         TM    MYFLAGS,SKEDDATA                  SKED DATA PRESENT?             
         BNO   PER40                             NO                             
         OC    0(6,R2),=C'000000'                GUARANTEE NUMERIC              
         CLC   0(6,R2),=C'000000'                TEST DATE PRESENT              
         BH    PER50                             YUP                            
PER40    MVC   0(6,R2),SVSTART                   USE EST START DATE             
PER50    MVC   BMGEERR,=AL2(MSTDTLK)                                            
         GOTO1 VDATCON,DMCB,(0,(R2)),(3,WORK)                                   
         CLC   WORK(3),SVAGYLK                COMPARE START & LOCK DATE         
         BL    NBERR                                                            
         GOTO1 VGETDAY,DMCB,(R2),WORK            VALIDATE IT                    
         MVC   BYTE,SVBUY+(SBUYRDAY-SBUYD)       ROTATION START DAY             
         NI    BYTE,X'0F'                        MAKE IT BINARY                 
         CLC   BYTE,0(R1)                        DATE=ROTATION START?           
         BE    PER60                             YES                            
         ZIC   R0,BYTE                           ROTATION START                 
         ZIC   R5,0(R1)                          DAY OF DATE                    
         SR    R0,R5                             DAYS TO ADD TO DATE            
         GOTO1 VADDAY,DMCB,(R2),WORK,(R0)                                       
         LA    R2,WORK                           POINT TO NEW DATE              
PER60    GOTO1 VDATCON,DMCB,(0,(R2)),(11,(R7))                                  
         LA    R7,5(R7)                                                         
*                                                                               
         CLI   NUMROTS,0                                                        
         BE    PER70                                                            
         LA    R6,ROTTAB                                                        
         USING PERVALD,R6                                                       
         LA    R2,SVSKD+(SSKDSDT-SSKDD)        START DATE                       
         GOTO1 VDATCON,DMCB,(0,(R2)),(5,WORK)                                   
         MVI   WORK+8,C'-'                                                      
         GOTO1 VDATCON,DMCB,(2,SVLASTDT),(5,WORK+9)                             
         L     RF,VCOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(17,WORK),(R6)                                         
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R2,PVALNWKS+1                                                    
         B     PER90                                                            
         DROP  R6                                                               
*                                                                               
PER70    LA    R1,SVSKD+(SSKDCNTR-SSKDD)         # OF WEEKS                     
         LA    R1,26(R1)                         LAST ENTRY IN TABLE            
         LA    R2,14                             MAX # OF WEEKS                 
PER80    CLC   0(2,R1),=C'00'                                                   
         BH    PER90                                                            
         SH    R1,=H'2'                                                         
         BCT   R2,PER80                                                         
         LA    R2,1                              DEFAULT = 1 WEEK               
PER90    MVI   0(R7),C'-'                                                       
         LA    R7,1(R7)                                                         
         STC   R2,NUMWKS                         SAVE # OF WEEKS                
         EDIT  (R2),(4,(R7)),ALIGN=LEFT                                         
         AR    R7,R0                                                            
         MVI   0(R7),C'W'                                                       
         MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
*                                                                               
         LA    R1,XROTDAYS                       ROTATION DAYS                  
         LA    R2,SVBUY+(SBUYROT-SBUYD)                                         
         LA    R0,L'XROTDAYS                                                    
PER100   MVI   0(R7),C'.'                        ASSUME NOT TODAY               
         CLI   0(R2),C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(1,R7),0(R1)                     MOVE IN THE DAY                
         LA    R7,1(R7)                                                         
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,PER100                                                        
*                                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BO    PER110                                                           
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         B     NPW10                                                            
*                                                                               
PER110   LA    RE,MYWORK                                                        
         SR    R7,RE                                                            
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE NPW:                                    *         
*      C,NPW=01                                                       *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE NPW:             *         
*      B,JAN16-6W,.....S.,01, (THIS IS THE BUYLINE SO FAR)            *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,NPW='              CHANGE NPW NEXT                
         LA    R7,MYWORK+6                                                      
*                                                                               
NPW10    CLI   NUMROTS,0                         NEW BUY W/ ROTS                
         BE    NPW20                                                            
         TM    MYFLAGS,CHNGBUY                   CHANGE NPW LATER               
         BO    TIM10                              WITH SKED                     
         EDIT  SVNPW,(2,(R7)),FILL=0             GREATEST NPW                   
         B     NPW50                                                            
*                                                                               
NPW20    LA    R1,SVSKD+(SSKDCNTR-SSKDD)         NUMBER PER WEEK                
         ZIC   R2,NUMWKS                         # WEEKS IN TABLE               
         MVC   WORK(2),0(R1)                     INITIAL NPW                    
         NI    MYFLAGS,X'FF'-SKEDCHG             SET TO ALL SPOTS EQUAL         
NPW30    CLC   WORK(2),0(R1)                                                    
         BE    NPW40                                                            
         BH    NPW35                                                            
         MVC   WORK(2),0(R1)                                                    
NPW35    OI    MYFLAGS,SKEDCHG                   SET NEED 'C,SKED='             
NPW40    LA    R1,2(R1)                          NEXT ENTRY                     
         BCT   R2,NPW30                                                         
         MVC   0(2,R7),WORK                      GREATEST NPW                   
         CLC   0(2,R7),=C'00'                    DO NOT SET NPW = 0             
         BNE   *+10                                                             
         MVC   0(2,R7),=C'01'                                                   
*                                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BO    NPW60                                                            
NPW50    MVI   2(R7),C','                                                       
         LA    R7,3(R7)                                                         
         B     TIM20                                                            
*                                                                               
NPW60    LA    R7,6+2                                                           
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
*                                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE TIME:                                   *         
*      C,TIM=2-4P                                                     *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE TIME             *         
*      2-4P, (TO BE CONT. IN NEXT SECTION)                            *         
*      B,JAN16-6W,.....S.,01,2-4P, (THIS IS THE BUYLINE SO FAR)       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
TIM10    XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,TIM='                  CHANGE TIME                
         LA    R7,MYWORK+6                                                      
         B     TIM30                                                            
*                                                                               
TIM20    XC    0(11,R7),0(R7)                    PROGRAM TIME                   
TIM30    LA    R2,SVBUY+(SBUYSTIM-SBUYD)                                        
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
TIM40    CLI   0(R7),C' '                        LAST SIGN. CHAR?               
         BH    TIM50                             YES                            
         BCTR  R7,0                                                             
         B     TIM40                                                            
*                                                                               
TIM50    LA    R7,1(R7)                          NEXT WRITABLE CHAR             
         TM    MYFLAGS,CHNGBUY                                                  
         BO    TIM60                                                            
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         B     DPT10                                                            
*                                                                               
TIM60    LA    RE,MYWORK                                                        
         SR    R7,RE                                                            
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE DAYPART:                                *         
*      C,DPT=E                                                        *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE DAYPART:         *         
*      B,JAN16-6W,.....S.,01,2-4P,E, (THIS IS THE BUYLINE SO FAR)     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,DPT='              CHANGE DAYPART NEXT            
         LA    R7,MYWORK+6                                                      
*                                                                               
DPT10    MVC   0(1,R7),SVBUY+(SBUYDPT-SBUYD)     DAYPART                        
*                                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BO    DPT20                                                            
         MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
         B     SLN10                                                            
*                                                                               
DPT20    LA    R7,6+1                                                           
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE SPOT LENGTH:                            *         
*      C,SLN=030                                                      *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE SPOT LENGTH:     *         
*      B,JAN16-6W,.....S.,01,2-4P,E,030, (THIS IS THE BUYLINE SO FAR) *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,SLN='                  CHANGE TIME                
         LA    R7,MYWORK+6                                                      
*                                                                               
SLN10    LA    R2,SVBUY+(SBUYSLEN-SBUYD)         SPOT LENGTH                    
         MVC   0(3,R7),0(R2)                                                    
*                                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BO    SLN20                                                            
         MVI   3(R7),C','                                                       
         LA    R7,4(R7)                                                         
         B     PRO10                                                            
*                                                                               
SLN20    LA    R7,6+3                                                           
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE PROGRAM NAME:                           *         
*      C,PRO=MOVIE                                                    *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE PROGRAM NAME:    *         
*      B,JAN16-6W,.....S.,01,2-4P,E,030,MOVIE                         *         
*      (THIS IS THE BUYLINE SO FAR)                                   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,PRO='              CHANGE PROGRAM                 
         LA    R7,MYWORK+6                                                      
*                                                                               
PRO10    MVC   0(L'BDPROGRM,R7),SVBUY+(SBUYPROG-SBUYD)                          
         LA    R7,L'BDPROGRM-1(R7)                                              
PRO20    CLI   0(R7),C' '                        LAST SIGN. CHAR?               
         BH    PRO30                             YES                            
         BCTR  R7,0                                                             
         B     PRO20                                                            
*                                                PROGRAM NAME                   
PRO30    LA    R7,1(R7)                          NEXT WRITABLE CHAR             
         TM    MYFLAGS,CHNGBUY                                                  
         BO    PRO40                                                            
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         B     COS10                                                            
*                                                                               
PRO40    LA    RE,MYWORK                                                        
         SR    R7,RE                                                            
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE COST AND TYPE                           *         
*      C,COS=P20.00                                                   *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE COST AND TYPE    *         
*      B,JAN16-6W,.....S.,01,2-4P,E,030,MOVIE,P20.00,                 *         
*      (THIS IS THE BUYLINE SO FAR)                                   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,COS='              CHANGE COST                    
         LA    R7,MYWORK+6                                                      
*                                                                               
COS10    LA    R6,SVSKD+(SSKDCOST-SSKDD)         CHECK IF COST IS IN            
         CLC   0(L'SSKDCOST,R6),SPACES            SKED?                         
         BH    *+8                                                              
         LA    R6,SVBUY+(SBUYCOST-SBUYD)         NO, GET FROM BUY HDR           
         MVC   0(1,R7),SVBUY+(SBUYCQLF-SBUYD)    COST TYPE                      
         LA    R7,1(R7)                                                         
         PACK  DUB,0(L'SBUYCOST,R6)                                             
         CVB   R1,DUB                                                           
         EDIT  (R1),(10,(R7)),2,ZERO=NOBLANK,ALIGN=LEFT                         
         AR    R7,R0                             LENGTH OF COST                 
*                                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BO    COS20                                                            
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         LA    RE,MYWORK                         DONE WITH 1ST LINE             
         SR    R7,RE                             LENGTH OF INPUT                
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         XC    MYWORK,MYWORK                                                    
         LA    R7,MYWORK                                                        
         B     DEM20                                                            
*                                                                               
COS20    L     R2,AREC                                                          
         ZICM  RE,BDCOST-BUYREC(R2),3                                           
         CLR   R1,RE                          CHECK IF COST IS THE SAME         
         BE    COS30                          YES                               
         LA    RE,MYWORK                                                        
         SR    R7,RE                                                            
         B     COS90                                                            
*                                                                               
COS30    TM    BDCIND2-BUYREC(R2),X'04'           COMPARE COST TYPE             
         BNO   COS40                                                            
         CLC   BDCIND-BUYREC(1,R2),SVBUY+(SBUYCQLF-SBUYD)                       
         BE    DEM10                          COST & COST TYPE THE SAME         
         B     COS90                                                            
*                                                                               
COS40    TM    BDCIND2,X'80'                                                    
         BNO   COS50                                                            
         CLI   SVBUY+(SBUYCQLF-SBUYD),C'C'                                      
         BE    DEM10                          COST & COST TYPE THE SAME         
         B     COS90                                                            
*                                                                               
COS50    TM    BDCIND2,X'02'                                                    
         BNO   COS60                                                            
         CLI   SVBUY+(SBUYCQLF-SBUYD),C'T'                                      
         BE    DEM10                         COST & COST TYPE THE SAME          
         B     COS90                                                            
*                                                                               
COS60    LA    R1,COSTAB           CONVERT TO BDCIND TYPE                       
COS70    CLI   0(R1),X'FF'                                                      
         BE    COS90                                                            
         CLC   0(1,R1),SVBUY+(SBUYCQLF-SBUYD)                                   
         BE    COS80                                                            
         LA    R1,2(R1)                                                         
         B     COS70                                                            
*                                                                               
COS80    CLC   1(1,R1),BDCIND-BUYREC(R2)                                        
         BE    DEM10                                                            
*                                                                               
COS90    GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE DEMOS:                                  *         
*      C,DEM=0.9/3.0                                                  *         
*                                                                     *         
* OR CREATE PART OF A NEW BUYLINE WHICH INCLUDES THE DEMOS:           *         
*      B,JAN16-6W,.....S.,01,2-4P,E,030,MOVIE,P20.00,                 *         
*      0.9/3.0,                                                       *         
*      (THIS IS THE BUYLINE SO FAR)                                   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DEM10    OC    SVDEM(4),SVDEM                    DEMO OBJECT?                   
         BZ    MAS10                             NO                             
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,DEM='              CHANGE DEMO                    
         LA    R7,MYWORK+6                                                      
*                                                                               
DEM20    OC    SVDEM(4),SVDEM                    DEMO OBJECT?                   
         BZ    MAS40                             NO                             
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NUMDEMS                        NUMBER OF EST. DEMOS           
         LA    R5,DEMODSPS                       DEMO MATCHED TABLE             
DEM30    DS    0H                                                               
         CLI   0(R5),X'FF'                       END OF DEMOS                   
         BE    DEM50                                                            
         LA    R2,SVDEM+(SDEMDEM-SDEMD)                                         
         ZIC   R1,0(R5)                          LOAD MATCHING DEMO #           
         LTR   R1,R1                             IS THERE A MATCH?              
         BZ    DEM40                             NO                             
         BCTR  R1,0                                                             
         MH    R1,=AL2(L'SDEMDEM)                INDEX INTO LIST                
         LA    R2,0(R1,R2)                                                      
*                                                                               
         PACK  DUB,0(L'SDEMDEM,R2)                                              
         CVB   R1,DUB                                                           
         EDIT  (R1),(10,0(R7)),1,ZERO=NOBLANK,ALIGN=LEFT                        
         AR    R7,R0                             # CHARS INTO R1                
*                                                                               
DEM40    MVI   0(R7),C'/'                                                       
         LA    R7,1(R7)                                                         
         LA    R5,1(R5)                          NEXT EST. DEMO                 
         BCT   R6,DEM30                                                         
*                                                                               
DEM50    BCTR  R7,0                              REMOVE TRAILING /'S            
         CLI   0(R7),C'/'                                                       
         BNE   *+12                                                             
         MVI   0(R7),C' '                                                       
         B     DEM50                                                            
         LA    R7,1(R7)                                                         
*                                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   MAS40                                                            
         LA    RE,MYWORK                                                        
         SR    R7,RE                                                            
         MVI   CALLB,C'Y'                                                       
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE A LINE TO CHANGE THE MASTER PRODUCT:                         *         
*      A,M=CL                                                         *         
*                                                                     *         
* OR ADD THE NEW BUYLINE WHICH NOW MAY INCLUDE THE PRODUCT:           *         
*      B,JAN16-6W,.....S.,01,2-4P,E,030,MOVIE,P20.00,                 *         
*      0.9/3.0,M=CL                                                   *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CHECK CHANGE IN MASTER PRODUCT                                                
MAS10    CLI   SVPOLPRD,0          TEST POL                                     
         BNE   NB251B              NO, => NO CHANGE IN PRODUCT                  
         L     R1,ASVCLIST                                                      
         L     R2,AREC                                                          
MAS20    CLI   3(R1),0                                                          
         BE    MAS30                                                            
         CLC   3(1,R1),BDMASPRD-BUYREC(R2)      PRODUCT CODE                    
         BE    MAS30                                                            
         LA    R1,4(R1)                                                         
         B     MAS20                                                            
MAS30    CLC   0(3,R1),SVBUY+(SBUYMAS-SBUYD)    SAME PRODUCT?                   
         BE    NB251B                                                           
*                                                                               
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(4),=C'A,M='               CHANGE MASTER PRODUCT           
         MVC   MYWORK+4(3),SVBUY+(SBUYMAS-SBUYD)                                
         LA    R7,7                                                             
         MVI   CALLB,C'Y'                                                       
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         B     NB251B                                                           
*                                                                               
MAS40    LA    R1,SVBUY+(SBUYMAS-SBUYD)          MASTER PRODUCT                 
         CLC   0(L'SBUYMAS,R1),SPACES            OMITTED?                       
         BE    MAS50                             YES                            
*                                                                               
         MVC   0(3,R7),=C',M='                                                  
         MVC   3(L'SBUYMAS,R7),0(R1)                                            
         LA    R7,L'SBUYMAS+3(R7)                                               
*                                                                               
MAS50    LA    RE,MYWORK                         DONE WITH SECOND LINE          
         SR    R7,RE                             LENGTH OF INPUT                
         MVI   CALLB,C'Y'                                                       
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
*                                                                               
         MVC   UPUID(8),SVBUY+(SBUYUID-SBUYD)                                   
         OI    WRKRUPSW,WRKRUPSW_NODMER                                         
NB251B   L     RE,BUYCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,BUYCOUNT                                                      
*                                                                               
*  WRITE EBY OBJECT BACK AFTER CLEARING ERROR CODES                             
         GOTO1 AERR                                                             
******   B     NB252               NOP WRTSCR                                   
         GOTO1 AWRTSCR                           DEBUG                          
NB252    DS    0H                                                               
         CLI   SVBUY+(SBUYDEL-SBUYD),C'Y'        DELETED THE BUY?               
         BE    NBXX                                                             
         TM    MYFLAGS,CHNGBUY                                                  
         BO    COM10                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SEARCH BUY TABLE TO SEE IF THIS UNIQUE ID IS ON FILE ALREADY        *         
* IF IT IS, WRITE IT BACK TO THE SAME LINE NUMBER                     *         
* IF IT ISN'T, USE THE LOWEST AVAILABLE LINE NUMBER                   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
         B     COM10                                                            
*                                                                               
NB270    GOTO1 ADDREC                  ADD RECORD WITH EXISING LIN NUM          
         OC    KEY+14(4),KEY+14        TEST RUNNING W/O FILE UPDATE             
         BZ    NB272                                                            
         GOTO1 GETREC                  IN CASE OF CHANGE                        
*                                                                               
NB272    L     R5,ABUYTABL             ADD TO BUY TABLE                         
         SR    R0,R0                                                            
         IC    R0,KEY+11               GET BUYLINE NUMBER                       
         BCTR  R0,0                                                             
         MH    R0,=AL2(L'BUYTABL)                                               
         AR    R5,R0                   SLOT FOR THIS BUYLINE                    
         MVC   0(1,R5),KEY+11          MOVE LINE NUMBER TO SLOT                 
         MVI   1(R5),C'A'              SET SLOT USED                            
         MVC   4(4,R5),KEY+14          MOVE DISK ADDRESS                        
         MVC   8(8,R5),SVBUY+(SBUYUID-SBUYD)  MOVE UNIQID                       
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* CREATE LINES TO CHANGE OR ADD UP TO 4 COMMENTS:                     *         
*      C,COM=1-COMMENT LINE 1                                         *         
*      C,COM=2-COMMENT LINE 2                                         *         
*      C,COM=3-COMMENT LINE 3                                         *         
*      C,COM=4-COMMENT LINE 4                                         *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
COM10    DS    0H                                                               
         CLI   NUMCOMS,0           ANY COMMENTS?                                
         BE    ROT10               NO                                           
*                                                                               
         MVI   UPDSW,C'Y'          NEED PUTREC                                  
         GOTO1 ACLRINP             CLEAR INPUT LINES                            
         LA    R2,1                COMMENT COUNT                                
         LA    R5,SVCOM1                                                        
         USING SCOMD,R5                                                         
         LA    R6,BUYINP1H                                                      
         ST    R6,BUYLNH                                                        
*                                                                               
COM20    DS    0H                                                               
         CH    R2,=H'5'            ALL 4 COMMENTS DONE?                         
         BE    COM40               YES                                          
         OC    SCOMLEN,SCOMLEN     END OF COMMENTS?                             
         BZ    COM40               YES                                          
*                                                                               
         LA    R7,MYWORK                                                        
         MVC   0(8,R7),=C'C,COM=1-'                                             
         STC   R2,6(R7)                                                         
         OI    6(R7),X'F0'          MAKE EBCDIC                                 
         LA    R2,1(R2)             NEXT COMMENT NUMBER                         
         LA    R7,8(R7)                                                         
*                                                                               
         LA    R1,SCOMDATA+L'SCOMDATA                                           
COM30    CLI   0(R1),C' '           FIND LAST CHAR                              
         BH    *+8                                                              
         BCT   R1,COM30                                                         
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
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         B     COM20                                                            
         DROP  R5                                                               
*                                                                               
COM40    GOTO1 VCALLBAS                          SEND TO SPBUY                  
         OC    BMGEERR,BMGEERR                                                  
         BNZ   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* A,JAN23-01$100CL,FEB13-01$200CL,FEB20-01$300CL,FEB27-01$400CL       *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
ROT10    DS    0H                                                               
         CLI   NUMROTS,0                                                        
         BE    SKED10                                                           
         XC    SVRDATE,SVRDATE                                                  
         MVI   SVRSEQ,0                                                         
         MVI   ROTFLAGS,0                                                       
         MVI   NUMROTS,0                                                        
*                                                                               
* CLEAR TABLE                                                                   
         LA    R2,ROTTAB                                                        
         XC    ROTTAB,ROTTAB                                                    
*                                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT70                                                            
*                                                                               
         USING ROTTABD,R2                                                       
         L     R6,AREC                                                          
         LA    R6,BDELEM-BUYREC(R6)                                             
         USING REGELEM,R6                                                       
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
ROT20    GOTO1 ANEXTEL                                                          
         BNE   ROT70               DONE WITH '0B' & '0C' ELEMENTS               
ROT30    ZIC   R1,NUMROTS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NUMROTS                                                       
         MVC   BMGEERR,=AL2(MROTERR)             ROT OBJ > 100                  
         CLI   NUMROTS,20          TOO MANY ROTATIONS?                          
         BH    NBERR                                                            
*                                                                               
         TM    RSTATUS,X'80'       MINUS SPOT                                   
         BO    ROT20                                                            
*                                                                               
         MVC   RTDATE,RDATE                                                     
         CLC   SVRDATE,RTDATE                                                   
         BE    *+8                                                              
         MVI   SVRSEQ,0                                                         
         ZIC   R1,SVRSEQ                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SVRSEQ                                                        
         MVC   RSEQ,SVRSEQ                                                      
         MVC   SVRDATE,RTDATE                                                   
*                                                                               
         OC    RPAY,RPAY                                                        
         BZ    *+8                                                              
         OI    RSTAT,PAID                                                       
*                                                                               
         TM    RSTATUS,X'40'       MISSED                                       
         BNO   *+8                                                              
         OI    RSTAT,MISSD                                                      
         MVC   RCOST,RPCOST                                                     
         MVC   RPRD1,RPPRD                                                      
         MVC   RLEN1,RPTIME                                                     
*                                                                               
         IC    R0,RLEN             CHECK WHAT NEXT ELEMENT IS                   
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   RCODE,0                                                          
         BE    ROT60                                                            
         CLI   RCODE,X'0B'                                                      
         BE    ROT40                                                            
         CLI   RCODE,X'0C'                                                      
         BNE   ROT50                                                            
ROT40    LA    R2,ROTTABLQ(R2)                                                  
         B     ROT30                                                            
*                                                                               
ROT50    CLI   RCODE,X'10'         10 ELEMENT => MATCHED                        
         BNE   *+8                                                              
         OI    RSTAT,MATCHD                                                     
         LA    R2,ROTTABLQ(R2)                                                  
         B     ROT20                                                            
         DROP  R6                                                               
*                                                                               
ROT60    LA    R2,ROTTABLQ(R2)                                                  
ROT70    MVI   0(R2),X'FF'                                                      
         LA    R2,ROTTAB                                                        
         CLI   0(R2),X'FF'                                                      
         BE    ROT200              NO OLD ROTATIONS                             
*                                                                               
         LA    R5,NWROTS                                                        
         USING NWROTABD,R5                                                      
         XC    SVRDATE,SVRDATE                                                  
*                                                                               
ROT80    CLI   0(R5),X'FF'                                                      
         BE    ROT170                                                           
         CLC   NWRDATE,SVRDATE                                                  
         BE    ROT100                                                           
         MVC   BMGEERR,=AL2(MROTORDR)                                           
         BL    NBERR               ROTS OUT OF ORDER                            
         MVC   SVRDATE,NWRDATE                                                  
*                                                                               
         MVI   SVRSEQ,0            CHANGE OF DATE                               
ROT90    CLI   0(R2),X'FF'         FIND FIRST ENTRY WITH SAME DATE              
         BE    ROT150                                                           
         CLC   NWRDATE,RTDATE                                                   
         BE    ROT100                                                           
         BL    ROT150              DATE NOT IN TABLE                            
         LA    R2,ROTTABLQ(R2)                                                  
         B     ROT90                                                            
*                                                                               
ROT100   LR    R1,R2               ADDRESS OF FIRST ENTRY W/ SAME DATE          
ROT110   CLI   0(R2),X'FF'                                                      
         BE    ROT130                                                           
         CLC   NWRDATE,RTDATE                                                   
         BL    ROT130                                                           
         TM    RSTAT,FOUND                                                      
         BO    ROT120                                                           
         CLC   NWRCOST(7),RCOST    COST,PRD1,PRD2,LEN1,LEN2                     
         BNE   ROT120                                                           
         MVI   NWRSEQ,0            MARK FOUND                                   
         OI    RSTAT,FOUND                                                      
         B     ROT160                                                           
ROT120   LA    R2,ROTTABLQ(R2)                                                  
         B     ROT110                                                           
*                                                                               
ROT130   DS    0H                  GET LAST SEQ USED FOR DATE                   
         CLI   SVRSEQ,0                                                         
         BNE   ROT150                                                           
         LR    R2,R1                                                            
ROT140   CLI   0(R2),X'FF'                                                      
         BE    ROT150                                                           
         CLC   NWRDATE,RTDATE                                                   
         BNE   ROT150                                                           
         MVC   SVRSEQ,RSEQ                                                      
         LA    R2,ROTTABLQ(R2)                                                  
         B     ROT140                                                           
*                                                                               
ROT150   DS    0H                                                               
         ZIC   RE,SVRSEQ                                                        
         LA    RE,1(RE)                                                         
         STC   RE,SVRSEQ                                                        
         STC   RE,NWRSEQ                                                        
ROT160   LR    R2,R1                                                            
         LA    R5,NWROTLNQ(R5)                                                  
         B     ROT80                                                            
*                                                                               
ROT170   LA    R2,ROTTAB           CHECK OLD ROTS NOT FOUND FOR ERRORS          
ROT180   CLI   0(R2),X'FF'                                                      
         BE    ROT200                                                           
         TM    RSTAT,FOUND                                                      
         BO    ROT190                                                           
         MVC   BMGEERR,=AL2(MROTDLER)                                           
         TM    RSTAT,MATCHD+PAID+MISSD                                          
         BNZ   NBERR                                                            
ROT190   LA    R2,ROTTABLQ(R2)                                                  
         B     ROT180                                                           
*                                                                               
ROT200   DS    0H                                                               
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(6),=C'C,NPW='                                             
         MVI   SVNPW,0                                                          
         LA    R7,MYWORK+8                                                      
         GOTO1 ACLRINP             CLEAR INPUT LINES                            
         MVC   0(7,R7),=C'C,SKED='                                              
         LA    R7,7(R7)                                                         
*                                                                               
         LA    R2,ROTTAB                                                        
         LA    R5,NWROTS                                                        
         OI    ROTFLAGS,FIRST                                                   
         LA    R6,SVSKD+(SSKDSDT-SSKDD)     START DATE                          
         GOTO1 VDATCON,DMCB,(0,(R6)),(2,SVRDATE)                                
*                                                                               
ROT210   DS    0H                                                               
         TM    ROTFLAGS,NWRTDONE+OLRTDONE                                       
         BO    ROT280                                                           
         LA    R0,7                                                             
         GOTO1 VADDAY,DMCB,(R6),WORK+6,(R0)        ADD 7 DAYS TO GET...         
         GOTO1 VDATCON,DMCB,(0,WORK+6),(2,SVNXTWK)  THE NEXT WEEK               
         MVI   SVRSEQ,0                                                         
*                                                                               
ROT220   CLI   0(R5),X'FF'                                                      
         BNE   *+12                                                             
         OI    ROTFLAGS,NWRTDONE                                                
         B     ROT240                                                           
*                                                                               
         TM    ROTFLAGS,FIRST                                                   
         BNO   ROT230                                                           
         MVC   BMGEERR,=AL2(MROTBFST)                                           
         CLC   NWRDATE,SVRDATE                                                  
         BL    NBERR               ROTATION BEFORE START DATE                   
         NI    ROTFLAGS,X'FF'-FIRST                                             
*                                                                               
ROT230   CLC   NWRDATE,SVNXTWK                                                  
         BNL   ROT240                                                           
         CLC   NWRSEQ,SVRSEQ                                                    
         BNH   *+10                                                             
         MVC   SVRSEQ,NWRSEQ                                                    
         LA    R5,NWROTLNQ(R5)                                                  
         B     ROT220                                                           
*                                                                               
ROT240   CLI   0(R2),X'FF'                                                      
         BNE   *+12                                                             
         OI    ROTFLAGS,OLRTDONE                                                
         B     ROT270                                                           
*                                                                               
         CLC   RTDATE,SVRDATE                                                   
         BL    ROT250              ROTATION IS BEFORE NEW START DATE            
         CLC   RTDATE,SVNXTWK                                                   
         BNL   ROT270                                                           
         CLC   RSEQ,SVRSEQ                                                      
         BNH   ROT260                                                           
         TM    RSTAT,FOUND                                                      
         BO    *+12                                                             
ROT250   OI    RSTAT,DELETED       DON'T INCLUDE IN SKED                        
         B     ROT260                                                           
         MVC   SVRSEQ,RSEQ                                                      
ROT260   LA    R2,ROTTABLQ(R2)                                                  
         B     ROT240                                                           
*                                                                               
ROT270   DS    0H                                                               
         CLC   SVRSEQ,SVNPW        SAVE HIGHEST NUMBER PER WEEK                 
         BNH   *+10                                                             
         MVC   SVNPW,SVRSEQ                                                     
         EDIT  SVRSEQ,(2,(R7)),FILL=0                                           
         MVI   2(R7),C'/'                                                       
         LA    R7,3(R7)                                                         
         MVC   SVRDATE,SVNXTWK                                                  
         LA    R6,WORK                                                          
         GOTO1 VDATCON,DMCB,(2,SVRDATE),(0,(R6))                                
         B     ROT210                                                           
*                                                                               
ROT280   DS    0H                                                               
         LA    R6,BUYINP1H                                                      
         ST    R6,BUYLNH                                                        
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT290                                                           
         EDIT  SVNPW,(2,MYWORK+6),FILL=0                                        
         LA    R0,8                                                             
         GOTO1 AINP,DMCB,(R0)                      C,NPW=                       
         BNE   NBERR                                                            
*                                                                               
ROT290   BCTR  R7,0                                REMOVE LAST '/'              
         MVI   0(R7),C' '                                                       
         LA    RE,MYWORK+8                                                      
         SR    R7,RE                               LENGTH OF INPUT              
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK(0),MYWORK+8                                               
         GOTO1 AINP,DMCB,(R7)                      C,SKED=                      
*                                                                               
*                                  BUILD STRING OF HIAT DATA                    
         L     R6,BUYLNH                                                        
         XC    MYWORK,MYWORK                                                    
         LA    R7,MYWORK                                                        
         MVC   0(2,R7),=C'A,'                                                   
         LA    R7,2(R7)                                                         
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT320                                                           
*                                                                               
         LA    R2,ROTTAB                                                        
ROT300   CLI   0(R2),X'FF'                                                      
         BE    ROT320                                                           
         TM    RSTAT,FOUND                                                      
         BO    ROT310                                                           
         TM    RSTAT,DELETED                                                    
         BO    ROT310                                                           
         GOTO1 VDATCON,DMCB,(2,RTDATE),(4,WORK)                                 
         MVC   0(5,R7),WORK                                                     
         MVI   5(R7),C'-'                                                       
         EDIT  RSEQ,(2,6(R7)),FILL=0                                            
         MVC   8(5,R7),=C'HIAT,'                                                
         LA    R7,13(R7)                                                        
         GOTO1 AROTINP                                                          
         BNE   NBERR                                                            
ROT310   LA    R2,ROTTABLQ(R2)                                                  
         B     ROT300                                                           
         DROP  R2                                                               
*                                                                               
ROT320   LA    R5,NWROTS           ADD NEW ROTS (JAN01-01$100AA-AB)             
ROT330   CLI   0(R5),X'FF'                                                      
         BE    ROT360                                                           
         CLI   NWRSEQ,0                                                         
         BE    ROT350                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(2,NWRDATE),(4,WORK)                                
         MVC   0(5,R7),WORK                                                     
         MVI   5(R7),C'-'                                                       
         EDIT  NWRSEQ,(2,6(R7)),FILL=0                                          
         LA    R7,8(R7)                                                         
         EDIT  NWRCOST,(9,0(R7)),0,FLOAT=$,ALIGN=LEFT,ZERO=NOBLANK              
         AR    R7,R0                                                            
         GOTO1 AGTEPRD,DMCB,NWRPRD1,(R7)    GET EBCDIC PRODUCT                  
         CLI   2(R7),C' '                                                       
         BNE   *+6                                                              
         BCTR  R7,0                                                             
         LA    R7,3(R7)                                                         
         CLI   NWRPRD2,0           IS THERE A SECOND PRODUCT?                   
         BE    ROT340                                                           
         MVI   0(R7),C'-'                                                       
         GOTO1 AGTEPRD,DMCB,NWRPRD2,(R7)    GET EBCDIC PRODUCT                  
         CLI   3(R7),C' '                                                       
         BNE   *+6                                                              
         BCTR  R7,0                                                             
         LA    R7,4(R7)                                                         
*                                                                               
ROT340   DS    0H                                                               
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         GOTO1 AROTINP                                                          
         BNE   NBERR                                                            
*                                                                               
ROT350   LA    R5,NWROTLNQ(R5)                                                  
         B     ROT330                                                           
*                                                                               
ROT360   LA    RE,BUYINP1H                                                      
         CR    R6,RE                                                            
         BH    ROT370                                                           
         LA    RE,MYWORK+2                                                      
         CR    RE,R7                                                            
         BE    NBX                 NO INPUT LEFT                                
ROT370   OI    ROTFLAGS,FNSHROTS                                                
         GOTO1 AROTINP                                                          
         BNE   NBERR                                                            
         B     ROT360                                                           
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  THIS BLOCK IS JUNK CODE TO DELETE  WHEN I DON'T HAVE ENOUGH ROOM   *         
*  TO LOAD SOMETHING INTO CORE.                                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*&&DO                                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
         XC    SVRDATE,SVRDATE                                                  
         TM    MYFLAGS,CHNGBUY                                                  
         BNO   ROT200                                                           
*&&                                                                             
         DROP  R5                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  CHANGE SCHEDULE USING SKD OBJECT:                                  *         
*  C,SKED=00/01/00/00/01/01                                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SKED10   TM    MYFLAGS,SKEDCHG                   SKED CHANGE?                   
         BZ    NBX                               NO                             
*                                                                               
         MVI   UPDSW,C'Y'          NEED PUTREC                                  
         GOTO1 ACLRINP                                                          
         LA    R7,MYWORK                                                        
         MVC   0(7,R7),=C'C,SKED='                                              
         LA    R7,7(R7)                                                         
         LA    R1,SVSKD+(SSKDCNTR-SSKDD)                                        
         ZIC   R2,NUMWKS                         # WEEKS IN TABLE               
*                                                                               
SKED20   MVC   0(2,R7),0(R1)                                                    
         MVI   2(R7),C'/'                                                       
         LA    R7,3(R7)                                                         
         LA    R1,2(R1)                          NEXT SKED                      
         BCT   R2,SKED20                                                        
*                                                                               
         BCTR  R7,0                              REMOVE LAST '/'                
         MVI   0(R7),C' '                                                       
         LA    RE,MYWORK                                                        
         SR    R7,RE                             LENGTH OF INPUT                
         MVI   CALLB,C'Y'                                                       
         GOTO1 AINP,DMCB,(R7)                                                   
         BNE   NBERR                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* DO PUTREC                                                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
NBX      DS    0H                                                               
         TM    MYFLAGS,CHNGBUY                                                  
         BO    NBXX                                                             
         CLI   UPDSW,C'Y'                                                       
         BNE   NBXX                                                             
         OC    KEY+14(4),KEY+14    TEST HAVE NO DISK ADDRESS                    
         BZ    NBXX                NO - MUST BE TEST SYSTEM U=N                 
         GOTO1 PUTREC                                                           
         B     NBXX                                                             
*                                                                               
NBERR    GOTO1 AERR                                                             
*                                                                               
NBXX     XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LTORG                                                                  
COSTAB   DC    C'F',X'80'                                                       
         DC    C'Q',X'40'                                                       
         DC    C' ',X'20'                                                       
         DC    C'N',X'10'                                                       
         DC    C'V',X'08'                                                       
         DC    C'S',X'04'                                                       
         DC    C'X',X'02'                                                       
         DC    C'P',X'00'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* WRITE THE TWA OUT TO TMPSTR                                         *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* WORKER EOF ENCOUNTERED CLEAN UP AND EXIT(EVENTUALLY)                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* HANDLE ERRORS                                                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
         GOTO1 AWRTSCR                   DEBUG                                  
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* PLACE HEADER DATA ON SCREEN FOR NEW ESTIMATE.                       *         
* AND BUILD THE DEMO TABLE(EVENTUALLY)                                *         
*                                                                     *         
* IF AN ERROR OCCURS SET THE ERROR IN THE FILE AND TRY THE NEXT       *         
* HEADER                                                              *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
         XC    UPDEMS,UPDEMS                                                    
         L     RF,VDEMOVAL                                                      
         LA    R1,DMCB                                                          
         GOTO1 (RF),(R1),(1,MYWORK),(14,UPDEMS),(C'S',DBLOCK),USRDEMS           
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
PROCH25  LA    R1,UPDEMS           REPLACE END-OF-LIST WITH 3X'0                
         MH    RE,=H'3'                                                         
         LA    R1,0(RE,R1)                                                      
         XC    0(3,R1),0(R1)                                                    
         GOTO1 =A(BDEMTAB),RR=RELO36                                            
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* HDR ERRORS                                                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
*                                                                               
         GOTO1 AWRTSCR              DEBUG                                       
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
MBUYNFND EQU   X'800A'             BUY TO DELETE NOT FOUND                      
MROTERR  EQU   X'800B'             ROT OBJECT > 100                             
MIVPRDRO EQU   X'800C'             INVALID PRODUCT IN ROTATION OBJECT           
MROTORDR EQU   X'800D'             ROTATION OBJECT OUT OF ORDER                 
MROTDLER EQU   X'800E'             DELETING PAID/MISSED/MATCHED ROT             
MROTBFST EQU   X'800F'             ROTATION BEFORE START DATE                   
MSKDERR  EQU   X'8010'             SKED OBJECT NOT FOUND                        
MSTDTLK  EQU   X'8011'             START DATE < LOCK DATE                       
* DON'T FORGET TO UPDATE ERRTAB IN SPREPCF02 WHEN ADDING NEW ERRORS             
* CHECK WITH SPBUY33 SO THAT CODES DON'T OVERLAP                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* READ TILL NEXT HDR* OBJECT                                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ENDBUY - PROCESS OBJECTS UNTIL EBY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SKIPSTA - READ FILE UNTIL BUY* WITH NEW STATION                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* READ BUYS FOR THIS PRODUCT/ESTIMATE/STATION                         *         
* TABLE FORMAT IS +0  LINENUM  (1)                                    *         
*                 +1  FLAG     (1)                                    *         
*                 +2  SPARE    (2)                                    *         
*                 +4  DISKADDR (4)                                    *         
*                 +8  UNIQID   (8)                                    *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0H                                                               
RDBUYS   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*RDBUYS*'                                                    
* CLEAR PREVIOUS BUY TABLE                                                      
         L     R0,ABUYTABL                                                      
         LH    R1,=AL2(BUYTABLX-BUYTABL)                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
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
         TM    BUYREC+15,X'80'         TEST DELETED                             
         BO    RDB10                                                            
         CLC   BDSTART,SVAGYLK         COMPARE START & LOCK DATE                
         BL    RDB10                                                            
         CLC   =C'AUTO - I5',BDPROGRM  IGNORE BUYS CREATED BY I5                
         BE    RDB10                                                            
         CLC   =C'ADJ',BDPROGRM        IGNORE ADJUSTMENT BUYS                   
         BE    RDB10                                                            
* CHECK FOR INVOICE DATA PRESENT                                                
         LA    R6,BDELEM                                                        
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
RDB30    L     R5,ABUYTABL                                                      
         SR    R0,R0                                                            
         IC    R0,BUYREC+10        GET BUYLINE NUMBER                           
         BCTR  R0,0                                                             
         MH    R0,=AL2(L'BUYTABL)                                               
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  RECALL BUY LINE FROM BUY TABLE                                     *         
*  INPUT:                                                             *         
*        SVBUY       ADDRESS OF LATEST BUY* OBJECT                    *         
*  OUTPUT:                                                            *         
*        MYBUYLIN    BUY LINE NUMBER IF BUY FOUND                     *         
*        MYBUYADR    ADDRESS OF THE LINE IN THE TABLE                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0H                                                               
FNDBUY   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*FNDBUY*'                                                    
*                                                                               
         XC    MYBUYADR,MYBUYADR   CLEAR OUTPUT AREA                            
*                                                                               
         L     R5,AMGWORK                                                       
         USING SVOBJD,R5                                                        
         LA    R4,SVBUY            POINT TO SAVED BUY* OBJECT                   
         USING SBUYD,R4                                                         
*                                                                               
         L     R1,ABUYTABL                                                      
         LA    R0,256                                                           
*                                                                               
FNDBUY10 CLC   SBUYUID(8),8(R1)    MATCH UNIQUE ID                              
         BE    FNDBUY30                                                         
         LA    R1,L'BUYTABL(R1)                                                 
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* AT END OF STATION DELETE ALL UNUSED BUY ENTRIES IN TABLE            *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
         L     R5,ABUYTABL                                                      
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
         MVI   MYWORK,C'D'                                                      
         GOTO1 ASETIN,DMCB,BUYINP2H,MYWORK,1                                    
*                                                                               
         GOTO1 VCALLBAS                                                         
         OC    BMGEERR,BMGEERR                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ENDSTA10 LA    R5,L'BUYTABL(R5)                                                 
         BCT   R6,ENDSTA2                                                       
*                                                                               
ENDSTAX  OI    WRKRUPSW,WRKRUPSW_NOIO   SET NOIO SWITCH                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* BUILD DEMO LOOKUP TABLE                                             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
BDTAB5   LA    R5,UPDEMS                                                        
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* BUILD THE DEMO TABLE TO CHECK FOR CHANGES                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* INITIALIZE                                                          *         
* WORKER FILE CONTAINS THE FOLLOWING                                  *         
* 2101SCRIPTSPCOKUPL   ...                                            *         
* 2102000001SIGNON INFORMATION  COKEAT                                *         
* 2102000002                    @#$%NN  WHERE NN IS FILE NUMBER       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         DS    0H                                                               
INIT     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**INIT**'                                                    
*                                                                               
         LH    RE,=AL2(BUYTABL-WORKD)                                           
         AR    RE,R9               ADD LOCAL W/S REG                            
         ST    RE,ABUYTABL                                                      
         SH    RE,=H'8'                                                         
         MVC   0(8,RE),=CL8'*BUYTABL*'                                          
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
         A     RF,RELO36                                                        
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
         DC    A(GTBPRD)                                                        
         DC    A(GTEPRD)                                                        
         DC    A(ROTINP)                                                        
         DC    A(INP)                                                           
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE MOVES DATA TO A TWA FIELD AND SETS APPROPRIATE HEADER       *         
* ON ENTRY, PARAMETER 1 = A(FIELD HEADER)                             *         
*           PARAMETER 2 = A(DATA FIELD)                               *         
*           PARAMETER 3 = L'DATA FIELD                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE FINDS BINARY PRODCUT                                        *         
* ON ENTRY, PARAMETER 1 = A(EBCDIC PRODUCT)                           *         
* ON EXIT,  PARAMETER 1 = BYTE 0= BINARY PRODUCT CODE                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         DS    0H                                                               
GTBPRD   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GTBPRD*'                                                    
         L     R6,0(R1)            R6=A(EBCDIC PRODUCT)                         
*                                                                               
         L     R2,ASVCLIST                                                      
GTBPRD10 CLI   0(R2),0                                                          
         BE    GTBPRDX                                                          
         CLC   0(3,R2),0(R6)       PRODUCT CODE                                 
         BE    GTBPRD20                                                         
         LA    R2,4(R2)                                                         
         B     GTBPRD10                                                         
GTBPRD20 MVC   DMCB(1),3(R2)       BINARY PRODUCT                               
*                                                                               
GTBPRDX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE FINDS EBCDIC PRODCUT                                        *         
* ON ENTRY, PARAMETER 1 = A(BINARY PRODUCT)                           *         
*           PARAMETER 2 = A(EBCDIC PRODUCT) TO PUT                    *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         DS    0H                                                               
GTEPRD   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GTEPRD*'                                                    
         L     R6,0(R1)            R6=A(BINARY PRODUCT)                         
         L     R5,4(R1)            R5=A(EBCDIC PRODUCT) TO PUT                  
*                                                                               
         L     R2,ASVCLIST                                                      
GTEPRD10 CLI   0(R2),0                                                          
         BE    GTEPRDX                                                          
         CLC   3(1,R2),0(R6)       PRODUCT CODE                                 
         BE    GTEPRD20                                                         
         LA    R2,4(R2)                                                         
         B     GTEPRD10                                                         
GTEPRD20 MVC   0(3,R5),0(R2)       EBCDIC PRODUCT                               
*                                                                               
GTEPRDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE SETS ROTATION INPUT                                         *         
* ON ENTRY, R7 NEXT INPUT SPACE IN MYWORK                             *         
*           R6 NEXT AVAILABLE BUY INPUT LINE                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         DS    0H                                                               
ROTINP   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*ROTINP*'                                                    
*                                                                               
         LR    R2,R7                                                            
         TM    ROTFLAGS,FNSHROTS                                                
         BO    ROTINP10                                                         
         LA    R1,MYWORK+L'BUYINP1                                              
         CR    R7,R1                                                            
         BL    ROTINPX                                                          
*                                                                               
         SH    R2,=H'2'            FIND LAST COMMA                              
ROTINP10 CLI   0(R2),C','                                                       
         BE    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         LA    R1,MYWORK           FIND LENGTH OF INPUT                         
         LR    R4,R2                                                            
         SR    R4,R1                                                            
         CH    R4,=H'2'            IF LENGTH < 2 DON'T DISPLAY LINE             
         BNH   ROTINP14                                                         
         GOTO1 ASETIN,DMCB,(R6),MYWORK,(R4)                                     
*                                                                               
         SR    R7,R2               MOVE LEFTOVER INPUT                          
         LTR   R7,R7                                                            
         BP    *+12                NO MORE INPUT                                
         LA    R7,MYWORK+2                                                      
         B     ROTINP13                                                         
         LA    R1,MYWORK                                                        
         BCTR  R7,0                                                             
         EX    R7,*+4                                                           
         MVC   1(0,R1),0(R2)                                                    
         LA    R7,2(R1,R7)                                                      
*                                                                               
ROTINP13 SH    R4,=H'2'            AMOUNT OF MYWORK TO CLEAR                    
         EX    R4,*+4                                                           
         XC    0(0,R7),0(R7)                                                    
*                                                                               
ROTINP14 ZIC   R1,0(R6)            POINT TO NEXT BUY INPUT LINE                 
         AR    R6,R1                                                            
         LA    R1,BUYOUTH                                                       
         TM    ROTFLAGS,FNSHROTS                                                
         BO    ROTINP15                                                         
         CR    R6,R1                                                            
         BL    ROTINPX                                                          
*                                                                               
ROTINP15 XC    BMGEERR,BMGEERR                                                  
         GOTO1 VCALLBAS            FILLED IN ALL INPUT LINES                    
         OC    BMGEERR,BMGEERR                                                  
         BNZ   ROTINPNX                                                         
****     B     ROTINP20            NOP WRTSCR                                   
         GOTO1 AWRTSCR             DEBUG                                        
ROTINP20 DS    0H                                                               
         GOTO1 ACLRINP             CLEAR INPUT LINES                            
         LA    R6,BUYINP1H                                                      
*                                                                               
ROTINPX  DS    0H                                                               
         CR    RB,RB                                                            
         B     *+6                                                              
ROTINPNX LTR   RB,RB                                                            
         XIT1  REGS=(R6,R7)                                                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE SETS ROTATION INPUT                                         *         
* ON ENTRY, R7 NEXT BUY INPUT LINE AVAILABLE                          *         
*           R7 LENGTH OF INPUT                                        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         DS    0H                                                               
INP      NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**INP***'                                                    
*                                                                               
         L     R7,DMCB                                                          
         L     R6,BUYLNH                                                        
         GOTO1 ASETIN,DMCB,(R6),MYWORK,(R7)                                     
         CLI   CALLB,C'Y'                                                       
         BE    INP05                                                            
         L     R7,BUYLNH                                                        
         ZIC   R1,0(R7)            POINT TO NEXT BUY INPUT LINE                 
         AR    R7,R1                                                            
         LA    R1,BUYOUTH                                                       
         CR    R7,R1                                                            
         BL    INPX                                                             
*                                                                               
INP05    MVI   CALLB,C'N'                                                       
         XC    BMGEERR,BMGEERR                                                  
         GOTO1 VCALLBAS            FILLED IN ALL INPUT LINES                    
         OC    BMGEERR,BMGEERR                                                  
         BNZ   INPNX                                                            
****     B     INP10               NOP WRTSCR                                   
         GOTO1 AWRTSCR             DEBUG                                        
INP10    DS    0H                                                               
         GOTO1 ACLRINP             CLEAR INPUT LINES                            
         LA    R7,BUYINP1H                                                      
*                                                                               
INPX     DS    0H                                                               
         ST    R7,BUYLNH                                                        
         CR    RB,RB                                                            
         B     *+6                                                              
INPNX    LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE TO CLEAR ALL UNPROTECTED FIELDS IN THE TWA                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ROUTINE TO CLEAR ALL BUY INPUT LINES                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* GENERATE A BUYING GUIDELINES REQUEST                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
REQBGLX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* WORKING STORAGE                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
RELO36   DS    A                                                                
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
AGTBPRD  DC    A(GTBPRD)                                                        
AGTEPRD  DC    A(GTEPRD)                                                        
AROTINP  DC    A(ROTINP)                                                        
AINP     DC    A(INP)                                                           
*                                                                               
SVRE     DS    A                                                                
SVPOP    DS    A                   TOP OF MY D-CHAIN                            
ABUYTABL DS    A                                                                
         EJECT                                                                  
BUYLNH   DS    A                                                                
BUYCOUNT DS    F                                                                
ERRCNT   DS    F                                                                
SVBDELEM DS    A                   SAVE THE ADDRESS OF BDELEM                   
SVBCOST  DS    F                   SAVE BUY COST                                
MYBUYLIN DS    0CL1                                                             
MYBUYADR DS    A                                                                
FILNO    DS    H                   FILE NUMBER                                  
SVUID    DS    XL2                 USER ID FROM TWA + 10                        
MYSTA    DS    CL(L'SBUYSTA)       SAVE OF STATION STRING                       
MYACN    DS    CL5                                                              
MYORDN   DS    CL4                 FIRST 4 BYTES OF UNIQUE BUY ID               
MYSRCE   DS    CL2                 SOURCE OF UPLOAD (MM=MMPLUS)                 
SVUPDTFL DS    C                   UPDATES ONLY FLAG                            
SVRDATE  DS    XL2                 SAVE ROTATION DATE                           
SVNXTWK  DS    XL2                 SAVE NEXT WEEK                               
SVRSEQ   DS    X                   ROTATION SEQUENCE NUMBER                     
SVNPW    DS    X                   SAVE LARGEST NUMBER PER WEEK                 
SVLASTDT DS    XL2                 SAVE LAST WEEK IN SKED                       
MYFLAGS  DS    X                                                                
SKEDCHG  EQU   X'80'                                                            
MBADSTA  EQU   X'40'                                                            
PERCHG   EQU   X'20'                                                            
CHNGBUY  EQU   X'10'               BUY EXISTS, JUST CHANGE IT                   
SKEDDATA EQU   X'08'               SKED DATA ADDED                              
ROTFLAGS DS    X                                                                
FNSHROTS EQU   X'80'               FINISHED PROCESSING ROTS                     
FIRST    EQU   X'40'               FIRST TIME PROCESSING ROTS                   
NWRTDONE EQU   X'20'               NEW ROTS DONE                                
OLRTDONE EQU   X'10'               OLD ROTS DONE                                
CALLB    DS    C                   Y=GOTO CALLBAS SUB                           
SVLNNUM  DS    X                   SAVE BUY LINE # FROM BUYTABL                 
SVSKDT   DS    15XL2               SAVE SKED INFO FROM RECALL                   
SVDEMT   DS    15XL6               SAVE DEMO INFO FROM RECALL                   
SVBROT   DS    CL(L'SBUYROT)       SAVE ROTATION DAYS                           
SVBRDAY  DS    C                   SAVE ROTAION START DAY                       
NUMWKS   DS    X                                                                
NUMCOMS  DS    X                   NUMBER OF COMMENTS                           
NUMROTS  DS    X                   NUMBER OF ROTATIONS                          
MYWORK   DS    XL256                                                            
USRDEMS  DS    CL35                STUPID DEMOVAL                               
*                                                                               
DEMODSPS DS    14C                 DEMO DISPLACEMENTS                           
NUMDEMS  DS    X                                                                
*                                                                               
SLINES   DS    3CL32               SCANNER                                      
*                                                                               
NWROTS   DS    XL(20*NWROTLNQ+1)                                                
ROTTAB   DS    XL(20*ROTTABLQ+1)                                                
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
BUYTABL  DS    256CL16                                                          
BUYTABLX EQU  *                                                                 
WORKLENQ EQU   *-WORKD                                                          
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SAVE OBJECTS FOR NEWBUYS (USE TIA)                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE                                                                  
NWROTABD DSECT                                                                  
NWRDATE  DS    XL2                 ROTATION DATE                                
NWRSEQ   DS    X                   SEQUENC NUMBER                               
NWRCOST  DS    XL3                 COST                                         
NWRPRD1  DS    X                                                                
NWRPRD2  DS    X                                                                
NWRLEN1  DS    X                                                                
NWRLEN2  DS    X                                                                
NWROTLNQ EQU   *-NWRDATE                                                        
*                                                                               
ROTTABD  DSECT                                                                  
RTDATE   DS    XL2                                                              
RSEQ     DS    X                                                                
RSTAT    DS    X                                                                
MATCHD   EQU   X'80'                                                            
PAID     EQU   X'40'                                                            
MISSD    EQU   X'20'                                                            
FOUND    EQU   X'10'                                                            
DELETED  EQU   X'08'                                                            
RCOST    DS    XL3                                                              
RPRD1    DS    X                                                                
RPRD2    DS    X                                                                
RLEN1    DS    X                                                                
RLEN2    DS    X                                                                
ROTTABLQ EQU   *-RTDATE                                                         
SVOBJD   DSECT                                                                  
SVBUY    DS    CL(SBUYLENQ)                                                     
SVSKD    DS    CL(SSKDLENQ)                                                     
SVDEM    DS    CL(SDEMLENQ)                                                     
SVCOM1   DS    CL(SCOMLENQ)                                                     
SVCOM2   DS    CL(SCOMLENQ)                                                     
SVCOM3   DS    CL(SCOMLENQ)                                                     
SVCOM4   DS    CL(SCOMLENQ)                                                     
SVOEND   DS    0C                                                               
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
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
**PAN#1  DC    CL21'183SPBUY36   04/10/13'                                      
         END                                                                    
