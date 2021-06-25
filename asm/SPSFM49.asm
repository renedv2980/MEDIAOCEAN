*          DATA SET SPSFM49    AT LEVEL 009 AS OF 01/29/06                      
*PHASE T21749A                                                                  
***********************************************************************         
* SEAN: 6/2000 :                                                                
*        1: FOR ALL THE RECORD TYPE OTHER THAN DAY/TIME, USER CAN ADD           
*           UP TO 5 PAGES OF DATA.  MORE PAGES CAN BE ADDED IN THE              
*           FUTURE IF NEEDED.                                                   
*        2: PF6-PF8 IS USED TO PROCEED BACK AND FORTH BETWEEN THE 5             
*           PAGES, PF12 TO RETURN TO LIST AFTER SELECT OR CHANGE.               
*        3: SCROLLING BETWEEN PAGES BECAME VERY TRICKY WHILE CODING,            
*           IF CHANGES ARISE, PLEASE DO SO WITH CARE.                           
*        4: PROGRAM NOW GAIN CONTROL FROM CONTROLLER EVERYTIME A PFKEY          
*           'S PRESSED.                                                         
*        5: THE BYTES NEEDED TO MAINTAIN THE TABLE ARE ALLOCATED IN             
*           SVSPARE IN THE TWA.                                                 
***********************************************************************         
*INCLUDE DPTRD                                                                  
         TITLE 'SPSFM49 BUYING RULES MAINT'                                     
T21749   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21749,R6,R7,RR=R2                                             
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*&&DO                                                                           
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*&&                                                                             
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         LR    R8,RA                                                            
         AHI   R8,(SVSPARE-T217FFD)                                             
         USING SVSPARE,R8                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
*                                                                               
MR       DS    0H                                                               
         MVI   NLISTS,11           11 LIST LINES                                
         MVI   ACTOUT,C'Y'         ACTOUT=Y IF LIST OR REP                      
         CLI   ACTEQU,ACTLIST                                                   
         BE    MR02                                                             
         CLI   ACTEQU,ACTREP                                                    
         BE    MR02                                                             
         MVI   ACTOUT,C'N'                                                      
*                                                                               
MR02     DS    0H                                                               
*                                                                               
         OI    CONSERVH+1,X'01'    NO "NO ENTRY" STUFF!                         
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLC   =C'SOON',CONWHEN    DONT CHECK PFKEYS IF SOON!                   
         BE    MR03                                                             
         CLI   PFKEY,6                                                          
         BE    VR                                                               
         CLI   PFKEY,8                                                          
         BE    VR                                                               
MR03     CLI   MODE,VALKEY         VALKEY                                       
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY REC                                  
         BE    DISPLAY                                                          
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         BE    DISPLAY                                                          
         CLI   MODE,XRECADD        RECORD ADDED                                 
         BE    DISPLAY                                                          
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    DISPLAY                                                          
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECS                                    
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
EXITR2   XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
VK       MVI   KEYCHG,C'N'         KEY CHANGED - NOTE, THIS FIELD               
*                                                                               
         XC    ERRDISP,ERRDISP                                                  
         CLI   ACTOUT,C'Y'         FOR LIST/REP                                 
         BNE   *+8                                                              
         MVI   MYSCR,0                                                          
*                                                                               
         LA    R2,SRUTYPEH         TYPE                                         
         CLI   5(R2),0             IF NOT INPUT                                 
         BE    VK06                SKIP PREV VAL'D TEST                         
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK06                                                             
         NI    SRUMEDH+4,X'DF'     NO, FORCE REVAL OF MEDIA                     
*                                                                               
VK06     DS    0H                                                               
         MVI   SVRUTYPE,0                                                       
         CLI   ACTEQU,ACTSEL       UNLESS IN SELECT                             
         BE    *+8                                                              
         MVI   FLTRTYP,0           ALSO CLEAR TYPE FILTER                       
         CLI   5(R2),0             TYPE OPTIONAL FOR LIST/REP                   
         BE    VK07                                                             
         CLC   8(3,R2),=C'ALL'     ALL OK FOR LIST/REP                          
         BNE   VK08                                                             
*                                                                               
VK07     DS    0H                                                               
         CLI   ACTOUT,C'Y'                                                      
         BE    VK30                                                             
         B     MISSERR                                                          
*                                                                               
VK08     DS    0H                                                               
         MVI   SVSCR,0                                                          
         LA    R3,RULETAB                                                       
         USING RULETABD,R3                                                      
         ZIC   R1,5(R2)            L'INPUT                                      
         BCTR  R1,0                                                             
*                                                                               
VK10     CLI   RUNAME,X'FF'        END OF TABLE                                 
         BE    INVERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RUNAME(0),8(R2)                                                  
         BE    VK20                                                             
         LA    R3,RUTABLNQ(R3)                                                  
         B     VK10                                                             
*                                                                               
VK20     MVC   8(L'RUNAME,R2),RUNAME                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   SVRUTYPE,RUEQU                                                   
         CLI   ACTEQU,ACTSEL       UNLESS IN SELECT                             
         BE    *+10                                                             
         MVC   FLTRTYP,RUEQU       ALSO SET TYPE FILTER                         
         MVC   SVSCR,RUSCR                                                      
*                                                                               
VK30     OI    4(R2),X'20'         MARK IT VALID                                
         LA    R2,SRUMEDH          MEDIA                                        
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK40                                                             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVI   KEYCHG,C'Y'                                                      
         GOTO1 VALIMED                                                          
         MVC   SRUMEDN,MEDNM       SET NAME                                     
         OI    SRUMEDNH+6,X'80'    TRANSMIT                                     
         NI    SRUCLTH+4,X'DF'                                                  
*                                                                               
VK40     OI    4(R2),X'20'         VALIDATED                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R2,SRUCLTH          CLIENT                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK50                                                             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVI   KEYCHG,C'Y'                                                      
         GOTO1 VALICLT                                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGENCY    READ AGENCY LEVEL                            
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TWODEC,WORK+25                                                   
*                                                                               
         MVC   SRUCLTN,CLTNM       SET NAME                                     
         OI    SRUCLTNH+6,X'80'    TRANSMIT                                     
         NI    SRUPRDH+4,X'DF'                                                  
*                                                                               
VK50     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SRUPRDH          PRODUCT/GROUP                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK70                                                             
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         MVI   KEYCHG,C'Y'                                                      
         XC    SVPGRP,SVPGRP                                                    
         MVI   BPRD,0                                                           
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         NI    SRUESTH+4,X'DF'                                                  
         CLI   5(R2),3                                                          
         BNH   VK60                                                             
         BAS   RE,VALPGR           THEN IT MUST BE PRODUCT GROUP                
         B     VK70                                                             
*                                                                               
VK60     DS    0H                                                               
         CLI   ACTOUT,C'Y'         IF LIST OR REPORT                            
         BNE   VK61                                                             
         CLI   5(R2),0             PRODUCT IS OPTIONAL                          
         BE    VK61B                                                            
*                                                                               
VK61     DS    0H                                                               
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK62                                                             
VK61B    DS    0H                                                               
         MVI   BPRD,X'FF'                                                       
         MVC   QPRD,=C'ALL'                                                     
         XC    SRUPRDN,SRUPRDN                                                  
         CLI   ACTEQU,ACTSEL       UNLESS IN SELECT                             
         BE    *+8                                                              
         MVI   FLTPRD,X'FF'        ALSO SET PRODUCT FILTER                      
         B     VK63                                                             
*                                                                               
VK62     DS    0H                                                               
         CLC   =C'POL',8(R2)        POL NOT ALLOWED                             
         BE    INVERR                                                           
         GOTO1 VALIPRD                                                          
         MVC   SRUPRDN,PRDNM       SET NAME                                     
         CLI   ACTEQU,ACTSEL       UNLESS IN SELECT                             
         BE    *+10                                                             
         MVC   FLTPRD,BPRD         ALSO SET PRODUCT FILTER                      
VK63     DS    0H                                                               
         OI    SRUPRDNH+6,X'80'    TRANSMIT                                     
*                                                                               
VK70     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SRUESTH          ESTIMATE                                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK80                                                             
*                                                                               
         CLI   ACTOUT,C'Y'         IF LIST OR REPORT                            
         BNE   VK71                                                             
         CLI   5(R2),0             ESTIMATE IS OPTIONAL                         
         BE    VK71B                                                            
*                                                                               
VK71     DS    0H                                                               
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK72                                                             
VK71B    DS    0H                                                               
         MVI   BEST,0                                                           
         MVC   QEST,=C'ALL'                                                     
         XC    SRUESTN,SRUESTN                                                  
         MVI   SVESTSD,0                                                        
         MVI   SVESTED,X'FF'                                                    
         CLI   ACTEQU,ACTSEL       UNLESS IN SELECT                             
         BE    *+8                                                              
         MVI   FLTEST,0            ALSO SET ESTIMATE FILTER                     
         B     VK73                                                             
*                                                                               
VK72     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVI   KEYCHG,C'Y'                                                      
*                                                                               
         CLI   BPRD,X'FF'          IF 'ALL' PRODUCTS                            
         BNE   VK72F                                                            
*                                  MUST VALIDATE EST MYSELF                     
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVERR                                                           
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STC   RE,BEST                                                          
         EDIT  (RE),(3,QEST),FILL=0                                             
         XC    SRUESTN,SRUESTN                                                  
         MVI   SVESTSD,0                                                        
         MVI   SVESTED,X'FF'                                                    
         B     VK73                                                             
*                                                                               
VK72F    DS    0H                                                               
         GOTO1 VALIEST                                                          
         MVC   SRUESTN,ESTNM       SET NAME                                     
         L     RF,AIO                                                           
         MVC   SVESTSD(12),ESTART-ESTHDR(RF)  SET START/END DATES               
*                                                                               
VK73     DS    0H                                                               
         CLI   ACTEQU,ACTSEL       UNLESS IN SELECT                             
         BE    *+10                                                             
         MVC   FLTEST,BEST         ALSO SET ESTIMATE FILTER                     
         OI    SRUESTNH+6,X'80'    TRANSMIT                                     
         CLI   SVRUTYPE,RULKRDSQ   IF DAYPART/STATION                           
         BE    VK74                                                             
         CLI   SVRUTYPE,RULKRRTQ   OR RATING                                    
         BNE   VK80                                                             
VK74     DS    0H                                                               
         MVI   BYTE,C'0'           DEFAULT DAYPART MENU CODE                    
         CLI   BEST,0                                                           
         BE    VK75                                                             
         CLI   BPRD,X'FF'                                                       
         BE    VK75                                                             
         L     RF,AIO                                                           
         MVC   BYTE,EDAYMENU-ESTHDR(RF)                                         
         DROP  R3                                                               
VK75     DS    0H                                                               
         CLC   BYTE,SVMENUC        UNLESS WE ALREADY HAVE MENU                  
         BE    VK80                                                             
         MVC   SVMENUC,BYTE                                                     
         BAS   RE,GETDPT           GET DPT MENU                                 
*                                                                               
VK80     OI    4(R2),X'20'         VALIDATED                                    
         BAS   RE,SETKEY                                                        
**NOOP   CLI   KEYCHG,C'Y'         **NOOP                                       
**NOOP   BNE   VKX                 **NOOP                                       
         CLI   ACTOUT,C'Y'         FOR LIST OR REPORT                           
         BE    VKX                 SKIP SET SCREEN                              
         BAS   RE,SETSCR                                                        
*                                                                               
VKX      MVC   PFKFLAG,PFKEY                                                    
         BAS   RE,CLRTAB           CLEAR THE TABLE                              
         CLC   SVKEY1,KEY          DID THE KEY CHANGE?                          
         BE    EXIT                                                             
         MVI   CHANGED,0                                                        
         MVI   ENDSCR,0                                                         
         XC    ELEMPTR,ELEMPTR                                                  
         MVC   PRVPTR,=Y(PAGE1)                                                 
         MVC   CURPTR,=Y(PAGE1)                                                 
         MVC   NXTPTR,=Y(PAGE2)                                                 
         MVC   SVKEY1,KEY                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
GETDPT   NTR1                                                                   
         LA    R1,DMCB                                                          
         MVC   0(2,R1),AGENCY                                                   
         MVC   2(1,R1),QMED                                                     
         MVC   3(1,R1),SVMENUC     MENU CODE                                    
         GOTO1 =V(DPTRD),(R1),,AIO2,DATAMGR,RR=RELO                             
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),X'08'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  SAVE VALID CODES                             
         XC    SVMENU,SVMENU                                                    
         LA    R0,L'SVMENU-1       R0=COUNTER                                   
         L     R1,AIO2                                                          
         LA    R4,SVMENU                                                        
*                                                                               
GDPT10   CLI   0(R1),0             TEST FOR EOT                                 
         BE    GDPTX                                                            
         MVC   0(1,R4),0(R1)                                                    
         LA    R4,1(R4)                                                         
         LA    R1,5(R1)                                                         
         BCT   R0,GDPT10                                                        
*                                                                               
GDPTX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              DISPLAY RECORD IN AIO                                            
*                                                                               
**********************************************************************          
*        ON DISPLAY, CURPTR POINTS TO ADDRESS IN TABLE TO START      *          
*        DISPLAYING! STARTING FROM PAGE ON, AND CHANGE BY PROCPFK    *          
*        IN THE VR LOGIC!                                            *          
**********************************************************************          
DISPLAY  DS    0H                                                               
         BAS   RE,DSPLY                                                         
         B     EXIT                                                             
*                                                                               
DSPLY    NTR1                                                                   
         LA    R4,ELEMTAB                                                       
         AH    R4,CURPTR                                                        
*                                                                               
         BAS   RE,DARKKEY          DARK THE PF12 KEY FIRST!                     
*                                                                               
         CLI   ACTNUM,ACTSEL       ON LIST/SELECT, LIGHT UP PF12                
         BNE   D05                                                              
         BAS   RE,LITEKEY                                                       
*                                                                               
D05      CLI   CHANGED,1                                                        
         BE    D09                 USE THE RECORD IN IO AREA ALREADY            
*                                                                               
         OC    SVSELDA,SVSELDA     IF HAVE DA OF SEL'D REC                      
         BZ    D08                                                              
         MVC   KEY+14(4),SVSELDA   USE IT - SEE DK                              
         GOTO1 GETREC                                                           
         XC    SVSELDA,SVSELDA                                                  
*                                                                               
D08      DS    0H                                                               
*                                                                               
D09      BAS   RE,CLRSCR           CLEAR THE SCREEN                             
         XC    ELEMPTR,ELEMPTR                                                  
         BAS   RE,STREL            STORE ALL X'05' ELEM: REC -> TAB             
         B     D10A                                                             
*                                                                               
D10      LA    R4,L'ELEMTAB(R4)    NEXT ENTRY                                   
D10A     CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    D10B                                                             
*                                                                               
         LA    RE,ELEMTAB          NO: END OF SCREEN??                          
         AH    RE,NXTPTR                                                        
         CR    R4,RE               CURPTR = NXTPTR, WE FILLED A SCREEN          
         BNE   D12                 NOT FILLED? DISPLAY ONE ELEMENT              
         MVI   ENDSCR,1                                                         
         B     DX                                                               
**********************************************************************          
D10B     CLI   ACTNUM,ACTDIS       YES: END OF TABLE                            
         BE    DX                                                               
         CLI   ACTNUM,ACTCHA       NO NEW SCREEN WILL BE ALLOCATED FOR          
         BE    D10C                DISPLAY, BECAUSE TABLE ENDED.                
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DX                                                               
         CLI   SVSEL,C'C'                                                       
         BNE   DX                                                               
*                                                                               
D10C     LA    RE,ELEMTAB          5 FULL PAGES DISPLAYED!                      
         AH    RE,NXTPTR                                                        
         CR    R4,RE                                                            
         BNE   DX                                                               
         MVI   ENDSCR,1                                                         
         B     DX                                                               
**********************************************************************          
*                                                                               
D12      TM    1(R2),X'20'         NEXT FIELD PROTECTED?                        
         BNO   D15                                                              
         MVI   ENDSCR,1                                                         
         B     DX                                                               
*                                                                               
D15      MVI   ENDSCR,0                                                         
         CLI   SVRUTYPE,RULKRPGQ   PROGRAM                                      
         BNE   D20                                                              
         BAS   RE,DISPPGM                                                       
         B     D10                                                              
*                                                                               
D20      CLI   SVRUTYPE,RULKRDSQ   DAYPART/STATION                              
         BNE   D30                                                              
         BAS   RE,DISPDPT                                                       
         B     D10                                                              
*                                                                               
D30      CLI   SVRUTYPE,RULKRDTQ   DAY/TIME                                     
         BNE   D40                                                              
         BAS   RE,DISPDTIM                                                      
         B     D10                                                              
*                                                                               
D40      CLI   SVRUTYPE,RULKRSDQ   SPOTS PER DAY                                
         BNE   D50                                                              
         BAS   RE,DISPSPTD                                                      
         B     D10                                                              
*                                                                               
D50      CLI   SVRUTYPE,RULKRSWQ   SPOTS PER WEEK                               
         BNE   D60                                                              
         BAS   RE,DISPSPTW                                                      
         B     D10                                                              
*                                                                               
D60      CLI   SVRUTYPE,RULKRHLQ   HOLIDAYS                                     
         BNE   D70                                                              
         BAS   RE,DISPHLDY                                                      
         B     D10                                                              
*                                                                               
D70      CLI   SVRUTYPE,RULKRRTQ   RATINGS                                      
         BNE   D80                                                              
         BAS   RE,DISPRTG                                                       
         B     D10                                                              
*                                                                               
D80      DC    H'0'                                                             
*                                                                               
DX       DS    0H                                                               
         MVI   CHANGED,0                                                        
         XC    PFKFLAG,PFKFLAG                                                  
         BAS   RE,SETVAL           TURN ON ALL PREVIOUSLY VALIDATE BITS         
         CLI   ACTEQU,ACTDIS                                                    
         BNE   DX4                                                              
         LA    R2,CONACTH          CURSOR TO ACTION FIELD                       
         ST    R2,ACURFORC                                                      
DX4      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              VALIDATE RECORD                                                  
*                                                                               
***********************************************************************         
*  REASON WHY PROCPFK COMES TO VR ALSO IS ON CHANGE, PF6/PF8 WILL               
*  FUNCTION JUST LIKE THE USER HAS HIT "ENTER"!                                 
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
         XC    PFKEY,PFKEY                                                      
*                                                                               
         CLC   MYSCR,SVSCR         DO WE HAVE RIGHT SCREEN?                     
         BE    VR05                                                             
         BAS   RE,SETSCR                                                        
         BAS   RE,DSPLY                                                         
         B     VRX                                                              
*                                                                               
VR05     CLI   ACTNUM,ACTDIS       DISPLAY AND SELECT DOESNT NEED               
         BE    VRX                 RECORD VALIDATION!                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   VR06                                                             
         CLI   SVSEL,C'S'                                                       
         BE    VRX                                                              
*                                                                               
VR06     DS    0H                                                               
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,STREL            BACKUP THE ELEMENTS                          
         MVI   CHANGED,0                                                        
*                                                                               
         BAS   RE,CHKVAL           DID ANYTHING CHANGE?                         
         BNE   VRX                                                              
         MVI   CHANGED,1                                                        
*                                                                               
         MVC   ELEMPTR,CURPTR      MAKE SURE POINT TO RIGHT SPOT                
         L     R4,AIO                                                           
         USING RULDATAD,R4                                                      
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         BAS   RE,SETR2            SET R2 TO FIRST FIELD                        
*                                                                               
         CLI   SVRUTYPE,RULKRPGQ   PROGRAM                                      
         BNE   VR20                                                             
         BAS   RE,VALPGM                                                        
         B     VRX                                                              
*                                                                               
VR20     CLI   SVRUTYPE,RULKRDSQ   DAYPART/STATION                              
         BNE   VR30                                                             
         BAS   RE,VALDPT                                                        
         B     VRX                                                              
*                                                                               
VR30     CLI   SVRUTYPE,RULKRDTQ   DAY/TIME                                     
         BNE   VR40                                                             
         BAS   RE,VALDTIM                                                       
         B     VRX                                                              
*                                                                               
VR40     CLI   SVRUTYPE,RULKRSDQ   SPOTS PER DAY                                
         BNE   VR50                                                             
         BAS   RE,VALSPTD                                                       
         B     VRX                                                              
*                                                                               
VR50     CLI   SVRUTYPE,RULKRSWQ   SPOTS PER WEEK                               
         BNE   VR60                                                             
         BAS   RE,VALSPTW                                                       
         B     VRX                                                              
*                                                                               
VR60     CLI   SVRUTYPE,RULKRHLQ   HOLIDAYS                                     
         BNE   VR70                                                             
         BAS   RE,VALHLDY                                                       
         B     VRX                                                              
*                                                                               
VR70     CLI   SVRUTYPE,RULKRRTQ   RATINGS                                      
         BNE   VR80                                                             
         BAS   RE,VALRTG                                                        
         B     VRX                                                              
*                                                                               
VR80     DC    H'0'                                                             
*                                                                               
VRX      CLI   CHANGED,1           IF NOTHING CHANGED, NO I/O                   
         BNE   VRX10                                                            
         MVI   ELCODE,X'05'        ON CHANGES:                                  
         GOTO1 REMELEM             REMOVE ALL OLD ELEMENTS                      
         BAS   RE,STELEM           STORE ALL  NEW ELEMENTS                      
         B     EXIT                DISPLAY THE SAME PAGE AGAIN                  
*                                                                               
VRX10    CLI   PFKFLAG,6           NO NEED TO SET POINTER IF NO PFKEY           
         BE    VRX20                                                            
         CLI   PFKFLAG,8                                                        
         BNE   EXIT                                                             
VRX20    CLC   CURPTR,=Y(PAGE5)    ALREADY AT LAST PAGE?                        
         BE    *+8                 THEN SKIP CLEAR SCREEN!                      
         BAS   RE,CLRSCR           CLEAR SCREEN BEFORE DISPLAY                  
         BAS   RE,SETPTR           PRV = CUR, CUR = NXT, NXT = CUR+LEN          
         B     DISPLAY             DISPLAY THE NEXT PAGE                        
         EJECT                                                                  
*                                                                               
*        DISPLAY A PROGRAM ELEMENT                                              
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
* RETURN R2 - A(NEXT FIELD TO DISPLAY)                                          
*                                                                               
DISPPGM  NTR1                                                                   
         MVC   8(L'SPGPGM1,R2),RUPGPGM     PROGRAM NAME                         
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LH    R1,RUPGMIN                                                       
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                           
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LH    R1,RUPGMAX                                                       
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                           
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         B     EXITR2                                                           
         EJECT                                                                  
*                                                                               
*        DISPLAY A DPT/STATION TYPE ELEMENT                                     
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
* RETURN R2 - A(NEXT FIELD TO DISPLAY)                                          
*                                                                               
DISPDPT  NTR1                                                                   
         MVC   8(3,R2),MYSPACES                                                 
         CLI   RUDSDPT,X'FF'       'ALL'                                        
         BNE   *+14                                                             
         MVC   8(3,R2),=C'ALL'                                                  
         B     *+10                                                             
         MVC   8(1,R2),RUDSDPT     DAYPART                                      
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         MVC   BYTE,RUDSSTP                                                     
         BAS   RE,DISPSTTY         DISPLAY STATION TYPE                         
         BAS   RE,BUMP                                                          
*                                                                               
         LH    R1,RUDSMIN                                                       
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                           
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LH    R1,RUDSMAX                                                       
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                           
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         B     EXITR2                                                           
         EJECT                                                                  
*                                                                               
*        DISPLAY A DAY/TIME ELEMENT                                             
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
* RETURN R2 - A(NEXT FIELD TO DISPLAY)                                          
*                                                                               
DISPDTIM NTR1                                                                   
*                                                                               
         MVC   8(3,R2),=C'ALL'                                                  
         CLI   RUDTTZ,C' '         NO TZ = 'ALL'                                
         BNH   *+10                                                             
         MVC   8(L'SDTTZON,R2),RUDTTZ   TIME ZONE(S)                            
*                                                                               
DDT10    DS    0H                                                               
         OI    6(R2),X'80'                                                      
*                                                                               
DDT30    BAS   RE,BUMP                                                          
         MVC   BYTE,RUDTSTYP                                                    
         BAS   RE,DISPSTTY         DISPLAY STATION TYPE                         
         BAS   RE,BUMP                                                          
         MVC   BYTE,RUDTDAYS                                                    
         MVC   8(L'SDTDAYS,R2),MYSPACES                                         
         BAS   RE,DISPDAYS         DISPLAY DAYS                                 
         BAS   RE,BUMP                                                          
         XC    FULL,FULL                                                        
         MVC   FULL(2),RUDTSTIM                                                 
         BAS   RE,DISPTIME         DISPLAY START TIME                           
         BAS   RE,BUMP                                                          
         XC    FULL,FULL                                                        
         MVC   FULL(2),RUDTETIM                                                 
         BAS   RE,DISPTIME         DISPLAY END TIME                             
         BAS   RE,BUMP                                                          
         B     EXITR2                                                           
         EJECT                                                                  
*                                                                               
*        DISPLAY A SPOTS PER DAY ELEMENT                                        
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
* RETURN R2 - A(NEXT FIELD TO DISPLAY)                                          
*                                                                               
DISPSPTD NTR1                                                                   
         EDIT  RUSDLEN,(3,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                        
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
*                                                                               
         MVC   BYTE,RUSDDAYS                                                    
         MVC   8(L'SDTDAYS,R2),MYSPACES                                         
         TM    RUSDCTL,X'80'       IS IT A DAY COUNT?                           
         BZ    DSPT4                                                            
         EDIT  RUSDDAYS,(3,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                       
         B     DSPT8                                                            
*                                                                               
DSPT4    DS    0H                                                               
         TM    RUSDCTL,X'40'       EXACT MATCH NEEDED? (= PREFIX)               
         BZ    DSPT6                                                            
         BAS   RE,DISPDAYS         DISPLAY DAYS                                 
         MVC   WORK+1(L'SDTDAYS-1),8(R2)                                        
         MVI   WORK,C'='                                                        
         MVC   8(L'SDTDAYS,R2),WORK                                             
         B     DSPT8                                                            
*                                                                               
DSPT6    DS    0H                                                               
         BAS   RE,DISPDAYS         DISPLAY DAYS                                 
*                                                                               
DSPT8    DS    0H                                                               
         BAS   RE,BUMP                                                          
         EDIT  RUSDSPTS,(4,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                       
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         B     EXITR2                                                           
         EJECT                                                                  
*                                                                               
*        DISPLAY A SPOTS PER WEEK ELEMENT                                       
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
* RETURN R2 - A(NEXT FIELD TO DISPLAY)                                          
*                                                                               
DISPSPTW NTR1                                                                   
         MVC   8(L'SPGPGM1,R2),RUSWPGM     PROGRAM NAME                         
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
*                                                                               
         EDIT  RUSWSPTS,(2,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                       
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         B     EXITR2                                                           
         EJECT                                                                  
*                                                                               
*        DISPLAY A HOLIDAY ELEMENT                                              
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
* RETURN R2 - A(NEXT FIELD TO DISPLAY)                                          
*                                                                               
DISPHLDY NTR1                                                                   
         MVC   8(17,R2),MYSPACES                                                
         OC    RUHLSDT,RUHLSDT                                                  
         BZ    DH40                                                             
         GOTO1 DATCON,DMCB,(2,RUHLSDT),(5,8(R2))                                
         OC    RUHLEDT,RUHLEDT                                                  
         BZ    DH40                                                             
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,RUHLEDT),(5,17(R2))                               
*                                                                               
DH40     DS    0H                                                               
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
*                                                                               
DHX      B     EXITR2                                                           
         EJECT                                                                  
*                                                                               
*        DISPLAY A RATINGS ELEMENT                                              
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
* RETURN R2 - A(NEXT FIELD TO DISPLAY)                                          
*                                                                               
DISPRTG  NTR1                                                                   
         MVC   8(3,R2),MYSPACES                                                 
         CLI   RURTDPT,X'FF'                                                    
         BNE   *+14                                                             
         MVC   8(3,R2),=C'ALL'                                                  
         B     *+10                                                             
         MVC   8(1,R2),RURTDPT     DAYPART                                      
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         MVC   BYTE,RURTSTP                                                     
         BAS   RE,DISPSTTY         DISPLAY STATION TYPE                         
         BAS   RE,BUMP                                                          
*                                                                               
         CLI   TWODEC,C'Y'         DOING 2 DECIMAL?                             
         BE    DRTG10              YES                                          
         EDIT  (B4,RURTMIN),(7,8(R2)),1,ALIGN=LEFT                              
         B     DRTG20                                                           
*                                                                               
DRTG10   MVC   FULL,RURTMIN                                                     
         TM    FULL,X'40'                     HAVE A 2 DECIMAL NUMBER?          
         BO    DRTG15                         YES                               
         L     R0,FULL                        NO - MULTIPLY BY 10               
         MHI   R0,10                                                            
         ST    R0,FULL                                                          
*                                                                               
DRTG15   NI    FULL,X'FF'-X'40'               TURN OFF X'40'                    
         EDIT  (B4,FULL),(7,8(R2)),2,ALIGN=LEFT                                 
*                                                                               
DRTG20   OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         CLI   TWODEC,C'Y'         DOING 2 DECIMAL?                             
         BE    DRTG30              YES                                          
         EDIT  (B4,RURTMAX),(7,8(R2)),1,ALIGN=LEFT                              
         B     DRTG40                                                           
*                                                                               
DRTG30   MVC   FULL,RURTMAX                                                     
         TM    FULL,X'40'                     HAVE A 2 DECIMAL NUMBER?          
         BO    DRTG35                         YES                               
         L     R0,FULL                        NO - MULTIPLY BY 10               
         MHI   R0,10                                                            
         ST    R0,FULL                                                          
*                                                                               
DRTG35   NI    FULL,X'FF'-X'40'               TURN OFF X'40'                    
         EDIT  (B4,FULL),(7,8(R2)),2,ALIGN=LEFT                                 
*                                                                               
DRTG40   OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         B     EXITR2                                                           
*                                                                               
**********************************************************************          
*                                                                               
DIV100   DS    0H                                                               
         M     R0,=F'1'                                                         
         L     RF,=F'100'                                                       
         DRNDR (R0),(RF)                                                        
         BR    RE                                                               
**********************************************************************          
         EJECT                                                                  
*                                                                               
*        VALIDATE A PROGRAM ELEMENT                                             
*        R4 - ELEMENT                                                           
*        R2 - A(FIRST FIELD)                                                    
*                                                                               
VALPGM   NTR1                                                                   
*                                                                               
VPGM10   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
VPGM10B  TM    1(R2),X'20'         END OF SCREEN                                
         BO    VPGMX                                                            
*                                                                               
         LA    R0,3                NUMBER OF FIELDS                             
         BAS   RE,CHKLIN           TEST ANYTHING THIS LINE                      
*                                                                               
         BNZ   VPGM15                                                           
         LA    R5,ELEMTAB                                                       
         AH    R5,ELEMPTR                                                       
         CLI   0(R5),X'FF'         WAS EMPTY, STILL EMPTY                       
         BE    VPGM10B             IT'S NOT GOING TO LEAVE A GAP THEN           
         MVI   0(R5),C'E'          WAS THERE, NOW DELETED                       
         LA    R5,L'ELEMTAB(R5)    MAKE SURE ELEMTAB KNOWS THAT                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VPGM10B                                                          
*                                                                               
VPGM15   MVC   RULDCD(2),=X'051A'                                               
*                                                                               
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    MISSERR                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   RUPGPGM(0),8(R2)    PROGRAM NAME                                 
         OC    RUPGPGM,MYSPACES                                                 
*                                                                               
VPGM20   BAS   RE,BUMP                                                          
*                                                                               
         BAS   RE,GETPCT           MIN %                                        
         MVC   RUPGMIN,HALF                                                     
         BAS   RE,BUMP                                                          
*                                                                               
         BAS   RE,GETPCT           MAX %                                        
         OC    RUPGMIN,RUPGMIN     IF HAD MIN, THEN                             
         BZ    VPGM22                                                           
         OC    HALF,HALF           MAX DEFAULT IS 100%                          
         BNZ   *+10                                                             
         MVC   HALF,=H'10000'                                                   
VPGM22   DS    0H                                                               
         CLC   HALF,RUPGMIN        MAX MUST NOT BE LT MIN                       
         BL    EMLMERR                                                          
         MVC   RUPGMAX,HALF                                                     
         BAS   RE,BUMP                                                          
*                                                                               
         OC    ELEM+2(24),ELEM+2                                                
         BZ    VPGM10                                                           
*                                                                               
         LA    R5,ELEMTAB          CHANGE THE ELEMENT AS IT CHANGES ON          
         AH    R5,ELEMPTR          SCREEN!                                      
         CLI   0(R5),X'FF'         ARE WE ADDING AN ELEMENT?                    
         BNE   VPGM25                                                           
         LA    RE,L'ELEMTAB(R5)                                                 
         MVI   0(RE),X'FF'                                                      
*                                                                               
VPGM25   ZIC   RE,ELEM+1                                                        
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEM                                                     
         LA    R5,L'ELEMTAB(R5)                                                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
*        GOTO1 ADDELEM                                                          
         B     VPGM10                                                           
*                                                                               
VPGMX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A DPT/STYPE ELEMENT                                           
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
*                                                                               
VALDPT   NTR1                                                                   
*                                                                               
VDPT10   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
VDPT10B  TM    1(R2),X'20'         PROTECTED? WE ARE DONE                       
         BO    VDPTX                                                            
         ST    R2,SVLINE           SAVE START OF LINE                           
*                                                                               
         LA    R0,4                NUMBER OF FIELDS                             
         BAS   RE,CHKLIN           TEST ANYTHING THIS LINE                      
*                                                                               
         BNZ   VDPT15                                                           
         LA    R5,ELEMTAB                                                       
         AH    R5,ELEMPTR                                                       
         CLI   0(R5),X'FF'         WAS EMPTY, STILL EMPTY                       
         BE    VDPT10B             IT'S NOT GOING TO LEAVE A GAP THEN           
         MVI   0(R5),C'E'          WAS THERE, NOW DELETED                       
         LA    R5,L'ELEMTAB(R5)    MAKE SURE ELEMTAB KNOWS THAT                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VDPT10B                                                          
*                                                                               
VDPT15   MVC   RULDCD(2),=X'0508'                                               
         CLI   5(R2),0                                                          
         BE    VDPT40                                                           
         MVI   RUDSDPT,X'FF'       ALL DAYPARTS                                 
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VDPT40                                                           
         CLI   5(R2),1             ELSE LENGTH MUST BE ONE                      
         BH    INVERR                                                           
         LA    R0,L'SVMENU-1       R0=MAX N'DAYPARTS                            
         LA    R1,SVMENU                                                        
*                                                                               
VDPT20   CLC   8(1,R2),0(R1)                                                    
         BE    VDPT30                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VDPT20                                                        
         B     INVERR                                                           
*                                                                               
VDPT30   MVC   RUDSDPT,8(R2)       SET DPT                                      
*                                                                               
VDPT40   BAS   RE,BUMP             POINT TO STATION TYPE                        
*                                                                               
         BAS   RE,VALSTTYP         VALIDATE STATION TYPE                        
         MVC   RUDSSTP,BYTE                                                     
         BAS   RE,BUMP             POINT TO MIN                                 
*                                                                               
         BAS   RE,GETPCT           MIN %                                        
         MVC   RUDSMIN,HALF                                                     
         BAS   RE,BUMP             POINT TO MAX                                 
*                                                                               
         BAS   RE,GETPCT           MAX %                                        
         OC    RUDSMIN,RUDSMIN     IF MIN IS GIVEN THEN                         
         BZ    VDPT42                                                           
         OC    HALF,HALF           MAX DEFAULT IS 100%                          
         BNZ   *+10                                                             
         MVC   HALF,=H'10000'                                                   
VDPT42   DS    0H                                                               
         CLC   HALF,RUDSMIN        MAX MUST NOT BE LT MIN                       
         BL    EMLMERR                                                          
         MVC   RUDSMAX,HALF                                                     
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         OC    ELEM+2(6),ELEM+2                                                 
         BZ    VDPT10                                                           
*                                                                               
         CLI   RUDSDPT,0           MUST HAVE HAD DAYPART                        
         BNE   *+12                                                             
         L     R2,SVLINE                                                        
         B     MISSERR                                                          
*                                                                               
         CLI   RUDSSTP,0           AND STATION TYPE                             
         BNE   *+16                                                             
         L     R2,SVLINE                                                        
         BAS   RE,BUMP                                                          
         B     MISSERR                                                          
*                                                                               
         LA    R5,ELEMTAB          CHANGE THE ELEMENT AS IT CHANGES ON          
         AH    R5,ELEMPTR          SCREEN!                                      
         CLI   0(R5),X'FF'         ARE WE ADDING AN ELEMENT?                    
         BNE   VDPT50                                                           
         LA    RE,L'ELEMTAB(R5)                                                 
         MVI   0(RE),X'FF'                                                      
*                                                                               
VDPT50   ZIC   RE,ELEM+1                                                        
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEM                                                     
         LA    R5,L'ELEMTAB(R5)                                                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VDPT10                                                           
*                                                                               
VDPTX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A DAY/TIME ELEMENT                                            
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
*                                                                               
VALDTIM  NTR1                                                                   
*                                                                               
VDT10    XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
VDT10B   TM    1(R2),X'20'         END OF SCREEN                                
         BO    VPGMX                                                            
*                                                                               
         LA    R0,5                NUMBER OF FIELDS                             
         BAS   RE,CHKLIN           TEST ANYTHING THIS LINE                      
*                                                                               
         BNZ   VDT15                                                            
         LA    R5,ELEMTAB                                                       
         AH    R5,ELEMPTR                                                       
         CLI   0(R5),X'FF'         WAS EMPTY, STILL EMPTY                       
         BE    VDT10B              IT'S NOT GOING TO LEAVE A GAP THEN           
         MVI   0(R5),C'E'          WAS THERE, NOW DELETED                       
         LA    R5,L'ELEMTAB(R5)    MAKE SURE ELEMTAB KNOWS THAT                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VDT10B                                                           
*                                                                               
VDT15    MVC   RULDCD(2),=X'0510'                                               
         CLI   5(R2),0             NO INPUT MEANS 'ALL'                         
         BE    VDT40                                                            
         CLC   =C'ALL',8(R2)                                                    
         BE    VDT40                                                            
*                                                                               
         LA    R3,8(R2)                                                         
         LA    R1,RUDTTZ                                                        
         ZIC   R0,5(R2)                                                         
*                                                                               
VDT26    DS    0H                                                               
         CLI   0(R3),C' '                                                       
         BNH   *+14                                                             
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,VDT26                                                         
*                                                                               
         MVC   X(L'RUDTTZ),RUDTTZ    CHECK FOR DUPES                            
         LA    R0,L'RUDTTZ-1         OUTER LOOP                                 
         LA    R5,RUDTTZ                                                        
*                                                                               
VDT27    DS    0H                                                               
         CLI   0(R5),C' '          SKIP IF NO ENTRY IN THIS POSITION            
         BNH   VDT30                                                            
*                                                                               
         LA    R3,1(R5)            INNER LOOP                                   
         LR    RF,R0                                                            
*                                                                               
VDT28    DS    0H                                                               
         CLC   0(1,R5),0(R3)                                                    
         BE    INVERR                                                           
         LA    R3,1(R3)            INNER LOOP BUMP                              
         BCT   RF,VDT28                                                         
*                                                                               
VDT30    DS    0H                                                               
         LA    R5,1(R5)            OUTER LOOP BUMP                              
         BCT   R0,VDT27                                                         
*                                                                               
VDT40    DS    0H                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,VALSTTYP         VALIDATE STATION TYPE                        
         MVC   RUDTSTYP,BYTE                                                    
*                                                                               
         BAS   RE,BUMP                                                          
         CLI   8(R2),C'='          EXACT MATCH PREFIX                           
         BE    INVERR              NOT ALLOWED HERE                             
         BAS   RE,VALDAYS          VALIDATE DAYS                                
         MVC   RUDTDAYS,HALF                                                    
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,VALTIME          VALIDATE START TIME                          
         MVC   RUDTSTIM,WORK                                                    
         BAS   RE,BUMP                                                          
         BAS   RE,VALTIME          VALIDATE END TIME                            
         MVC   RUDTETIM,WORK                                                    
*                                                                               
         BAS   RE,BUMP             POINT TO NEXT FIELD                          
*                                                                               
         OC    ELEM+2(14),ELEM+2                                                
         BZ    VDT10                                                            
*                                                                               
         LA    R5,ELEMTAB          CHANGE THE ELEMENT AS IT CHANGES ON          
         AH    R5,ELEMPTR          SCREEN!                                      
         CLI   0(R5),X'FF'         ARE WE ADDING AN ELEMENT?                    
         BNE   VDT50                                                            
         LA    RE,L'ELEMTAB(R5)                                                 
         MVI   0(RE),X'FF'                                                      
*                                                                               
VDT50    ZIC   RE,ELEM+1                                                        
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEM                                                     
         LA    R5,L'ELEMTAB(R5)                                                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VDT10                                                            
*                                                                               
VDTX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A SPOTS PER DAY ELEMENT                                       
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
*                                                                               
VALSPTD NTR1                                                                    
*                                                                               
VSPD10   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
VSPD10B  TM    1(R2),X'20'         END OF SCREEN                                
         BO    VSPDX                                                            
*                                                                               
         LA    R0,3                NUMBER OF FIELDS                             
         BAS   RE,CHKLIN           TEST ANYTHING THIS LINE                      
*                                                                               
         BNZ   VSPD15                                                           
         LA    R5,ELEMTAB                                                       
         AH    R5,ELEMPTR                                                       
         CLI   0(R5),X'FF'         WAS EMPTY, STILL EMPTY                       
         BE    VSPD10B             IT'S NOT GOING TO LEAVE A GAP THEN           
         MVI   0(R5),C'E'          WAS THERE, NOW DELETED                       
         LA    R5,L'ELEMTAB(R5)    MAKE SURE ELEMTAB KNOWS THAT                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VSPD10B                                                          
*                                                                               
VSPD15   MVC   RULDCD(2),=X'0508'                                               
*                                                                               
         LA    R3,L'SSDPROG                                                     
         BAS   RE,VALNUM           PROGRAM LENGTH                               
         BL    INVERR                                                           
         BE    MISSERR                                                          
         MVC   RUSDLEN,HALF                                                     
         BAS   RE,BUMP                                                          
*                                  DAYS                                         
         BAS   RE,VALNUM           IF FIND A NUMERIC VALUE                      
         BNH   VSPD12                                                           
         MVC   RUSDDAYS,HALF+1     IT IS DAY COUNT                              
         OI    RUSDCTL,X'80'       DAY COUNT INDICATOR                          
         B     VSPD16                                                           
*                                                                               
VSPD12   DS    0H                                                               
         CLI   8(R2),C'='          EXACT MATCH PREFIX                           
         BNE   *+8                                                              
         OI    RUSDCTL,X'40'       SET INDICATOR                                
*                                                                               
         BAS   RE,VALDAYS          VALIDATE DAYS                                
         MVC   RUSDDAYS,HALF                                                    
*                                                                               
VSPD16   DS    0H                                                               
         BAS   RE,BUMP                                                          
*                                                                               
         LA    R3,L'SSDSDAY                                                     
         BAS   RE,VALNUM                                                        
         BL    INVERR                                                           
         BE    MISSERR                                                          
         MVC   RUSDSPTS,HALF                                                    
         BAS   RE,BUMP                                                          
*                                                                               
         OC    ELEM+2(6),ELEM+2                                                 
         BZ    VSPD10                                                           
*                                                                               
         LA    R5,ELEMTAB          CHANGE THE ELEMENT AS IT CHANGES ON          
         AH    R5,ELEMPTR          SCREEN!                                      
         CLI   0(R5),X'FF'         ARE WE ADDING AN ELEMENT?                    
         BNE   VSPD50                                                           
         LA    RE,L'ELEMTAB(R5)                                                 
         MVI   0(RE),X'FF'                                                      
*                                                                               
VSPD50   ZIC   RE,ELEM+1                                                        
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEM                                                     
         LA    R5,L'ELEMTAB(R5)                                                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VSPD10                                                           
*                                                                               
VSPDX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A SPOTS PER WEEK ELEMENT                                      
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
*                                                                               
VALSPTW NTR1                                                                    
*                                                                               
VSPW10   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
VSPW10B  TM    1(R2),X'20'         END OF SCREEN                                
         BO    VSPWX                                                            
*                                                                               
         LA    R0,2                NUMBER OF FIELDS                             
         BAS   RE,CHKLIN           TEST ANYTHING THIS LINE                      
*                                                                               
         BNZ   VSPW15                                                           
         LA    R5,ELEMTAB                                                       
         AH    R5,ELEMPTR                                                       
         CLI   0(R5),X'FF'         WAS EMPTY, STILL EMPTY                       
         BE    VSPW10B             IT'S NOT GOING TO LEAVE A GAP THEN           
         MVI   0(R5),C'E'          WAS THERE, NOW DELETED                       
         LA    R5,L'ELEMTAB(R5)    MAKE SURE ELEMTAB KNOWS THAT                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VSPW10B                                                          
*                                                                               
VSPW15   MVC   RULDCD(2),=X'0518'                                               
*                                                                               
         CLI   5(R2),0                                                          
         BE    VSPW20                                                           
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   RUSWPGM(0),8(R2)    PROGRAM NAME                                 
         OC    RUSWPGM,MYSPACES                                                 
*                                                                               
VSPW20   BAS   RE,BUMP                                                          
*                                                                               
         LA    R3,L'SSWSPWK                                                     
         BAS   RE,VALNUM                                                        
         BE    MISSERR                                                          
         BNH   INVERR                                                           
         MVC   RUSWSPTS,HALF                                                    
         BAS   RE,BUMP                                                          
*                                                                               
         OC    ELEM+2(22),ELEM+2                                                
         BZ    VSPW10                                                           
*                                                                               
         LA    R5,ELEMTAB          CHANGE THE ELEMENT AS IT CHANGES ON          
         AH    R5,ELEMPTR          SCREEN!                                      
         CLI   0(R5),X'FF'         ARE WE ADDING AN ELEMENT?                    
         BNE   VSPW50                                                           
         LA    RE,L'ELEMTAB(R5)                                                 
         MVI   0(RE),X'FF'                                                      
*                                                                               
VSPW50   ZIC   RE,ELEM+1                                                        
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEM                                                     
         LA    R5,L'ELEMTAB(R5)                                                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VSPW10                                                           
*                                                                               
VSPWX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A HOLIDAY ELEMENT                                             
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
*                                                                               
VALHLDY NTR1                                                                    
*                                                                               
VHL10    TM    1(R2),X'20'         END OF SCREEN                                
         BO    VHLX                                                             
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVC   RULDCD(2),=X'0506'                                               
*                                                                               
         CLI   5(R2),0                                                          
         BE    VHL20                                                            
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         CLC   WORK(6),SVESTSD     BEFORE EST START?                            
         BL    EDTERR              ERROR                                        
         CLC   WORK(6),SVESTED     AFTER EST END?                               
         BH    EDTERR              ERROR                                        
         GOTO1 DATCON,DMCB,(0,WORK),(2,RUHLSDT)                                 
         LA    R3,16(R2)                                                        
         CLI   16(R2),C'A'                                                      
         BNL   VHL12                                                            
         LA    R3,17(R2)                                                        
         CLI   17(R2),C'A'                                                      
         BL    VHL20                                                            
*                                                                               
VHL12    DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,0(R3)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         CLC   WORK(6),SVESTSD     BEFORE EST START?                            
         BL    EDTERR              ERROR                                        
         CLC   WORK(6),SVESTED     AFTER EST END?                               
         BH    EDTERR              ERROR                                        
         GOTO1 DATCON,DMCB,(0,WORK),(2,RUHLEDT)                                 
*                                                                               
         CLC   RUHLEDT,RUHLSDT     END NOT BEFORE START                         
         BL    EBFRSERR                                                         
*                                                                               
VHL20    DS    0H                                                               
         BAS   RE,BUMP                                                          
*                                                                               
         OC    ELEM+2(4),ELEM+2                                                 
*                                                                               
         BNZ   VHL30                                                            
         LA    R5,ELEMTAB                                                       
         AH    R5,ELEMPTR                                                       
         CLI   0(R5),X'FF'         WAS EMPTY, STILL EMPTY                       
         BE    VHL10               IT'S NOT GOING TO LEAVE A GAP THEN           
         MVI   0(R5),C'E'          WAS THERE, NOW DELETED                       
         LA    R5,L'ELEMTAB(R5)    MAKE SURE ELEMTAB KNOWS THAT                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VHL10                                                            
*                                                                               
VHL30    LA    R5,ELEMTAB          CHANGE THE ELEMENT AS IT CHANGES ON          
         AH    R5,ELEMPTR          SCREEN!                                      
         CLI   0(R5),X'FF'         ARE WE ADDING AN ELEMENT?                    
         BNE   VHL40                                                            
         LA    RE,L'ELEMTAB(R5)                                                 
         MVI   0(RE),X'FF'                                                      
*                                                                               
VHL40    ZIC   RE,ELEM+1                                                        
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEM                                                     
         LA    R5,L'ELEMTAB(R5)                                                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VHL10                                                            
*                                                                               
VHLX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A RATINGS ELEMENT                                             
*        R4 - ELEMENT                                                           
*        R2 - A(CURRENT FIELD)                                                  
*                                                                               
VALRTG   NTR1                                                                   
*                                                                               
VRTG10   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
VRTG10B  TM    1(R2),X'20'         END OF SCREEN                                
         BO    VHLX                                                             
         ST    R2,SVLINE           SAVE START OF LINE                           
*                                                                               
         LA    R0,4                NUMBER OF FIELDS                             
         BAS   RE,CHKLIN           TEST ANYTHING THIS LINE                      
*                                                                               
         BNZ   VRTG15                                                           
         LA    R5,ELEMTAB                                                       
         AH    R5,ELEMPTR                                                       
         CLI   0(R5),X'FF'         WAS EMPTY, STILL EMPTY                       
         BE    VRTG10B             IT'S NOT GOING TO LEAVE A GAP THEN           
         MVI   0(R5),C'E'          WAS THERE, NOW DELETED                       
         LA    R5,L'ELEMTAB(R5)    MAKE SURE ELEMTAB KNOWS THAT                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VRTG10B                                                          
*                                                                               
VRTG15   MVC   RULDCD(2),=X'050C'                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VRTG40                                                           
         MVI   RURTDPT,X'FF'       ALL DAYPARTS                                 
         CLC   =C'ALL',8(R2)                                                    
         BE    VRTG40                                                           
         CLI   5(R2),1             ELSE LENGTH MUST BE ONE                      
         BNE   INVERR                                                           
         LA    R0,L'SVMENU-1       R0=MAX N'DAYPARTS                            
         LA    R1,SVMENU                                                        
*                                                                               
VRTG20   CLC   8(1,R2),0(R1)                                                    
         BE    VRTG30                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VRTG20                                                        
         B     INVERR                                                           
*                                                                               
VRTG30   MVC   RURTDPT,8(R2)       SET DPT                                      
*                                                                               
VRTG40   BAS   RE,BUMP             POINT TO STATION TYPE                        
*                                                                               
         BAS   RE,VALSTTYP         VALIDATE STATION TYPE                        
         MVC   RURTSTP,BYTE                                                     
         BAS   RE,BUMP             POINT TO MIN                                 
*                                                                               
         BAS   RE,GETRTG           MIN RTG                                      
         MVC   RURTMIN,FULL                                                     
         BAS   RE,BUMP             POINT TO MAX                                 
*                                                                               
         BAS   RE,GETRTG           MAX RTG                                      
         OC    FULL,FULL           IF MAX IS GIVEN                              
         BZ    VRTG42                                                           
         CLC   FULL,RURTMIN        MAX MUST NOT BE LT MIN                       
         BL    EMLMERR                                                          
VRTG42   DS    0H                                                               
         MVC   RURTMAX,FULL                                                     
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         OC    ELEM+2(6),ELEM+2    TEST ANY DATA IN ELEM                        
         BZ    VRTG10                                                           
*                                                                               
         CLI   RURTDPT,0           YES, MUST HAVE HAD DAYPART                   
         BNE   *+12                                                             
         L     R2,SVLINE                                                        
         B     MISSERR                                                          
*                                                                               
         CLI   RURTSTP,0           AND STATION TYPE                             
         BNE   *+16                                                             
         L     R2,SVLINE                                                        
         BAS   RE,BUMP                                                          
         B     MISSERR                                                          
*                                                                               
         LA    R5,ELEMTAB          CHANGE THE ELEMENT AS IT CHANGES ON          
         AH    R5,ELEMPTR          SCREEN!                                      
         CLI   0(R5),X'FF'         ARE WE ADDING AN ELEMENT?                    
         BNE   VRTG50                                                           
         LA    RE,L'ELEMTAB(R5)                                                 
         MVI   0(RE),X'FF'                                                      
*                                                                               
VRTG50   ZIC   RE,ELEM+1                                                        
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEM                                                     
         LA    R5,L'ELEMTAB(R5)                                                 
         LA    RE,ELEMTAB                                                       
         SR    R5,RE                                                            
         STH   R5,ELEMPTR          UPDATE THE POINTER                           
         B     VRTG10                                                           
*                                                                               
VRTGX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        BYTE = STATION TYPE                                                    
*        DISPLAY A STATION TYPE AT 8(R2)                                        
*                                                                               
DISPSTTY NTR1                                                                   
         OI    6(R2),X'80'                                                      
         MVC   8(L'STNAME,R2),MYSPACES                                          
         LA    R3,STATYPE                                                       
         USING STATYPED,R3                                                      
         CLI   BYTE,0                                                           
         BE    DSTX                                                             
*                                                                               
DST10    CLC   STEQU,BYTE                                                       
         BE    DST20                                                            
         CLI   STEQU,0                                                          
         BE    DST20                                                            
         LA    R3,STATYPEQ(R3)                                                  
         B     DST10                                                            
*                                                                               
DST20    MVC   8(L'STNAME,R2),STNAME                                            
*                                                                               
DSTX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        INPUT - BYTE                                                           
*        DISPLAY A DAY EXPRESSION AT 8(R2)                                      
*                                                                               
DISPDAYS NTR1                                                                   
         OI    6(R2),X'80'                                                      
         CLI   BYTE,0                                                           
         BE    DDYX                                                             
         GOTO1 CALLOV,DMCB,0,X'D9000A0F'  GET ADDRESS OF DAYUNPK                
         L     RF,0(R1)                                                         
         ZIC   R5,5(R2)            L'EXPRESSION                                 
         GOTO1 (RF),(R1),((R5),BYTE),8(R2)                                      
*                                                                               
DDYX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DISPLAY A TIME AT 8(R2)                                                
*        INPUT - FULL                                                           
*                                                                               
DISPTIME NTR1                                                                   
         MVC   8(L'SDTSTIM,R2),MYSPACES                                         
         OI    6(R2),X'80'                                                      
         OC    FULL,FULL                                                        
         BZ    VTX                                                              
         GOTO1 UNTIME,DMCB,(0,FULL),8(R2)                                       
*                                                                               
DTX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A STATION TYPE AT 8(R2)                                       
*        RETURN IN BYTE                                                         
*                                                                               
VALSTTYP NTR1                                                                   
         MVI   BYTE,0                                                           
         CLI   5(R2),0                                                          
         BE    VSTX                                                             
         LA    R3,STATYPE                                                       
         USING STATYPED,R3                                                      
         ZIC   R1,5(R2)            L'INPUT                                      
         BCTR  R1,0                                                             
*                                                                               
VST10    CLI   STNAME,C'*'       END OF TABLE                                   
         BE    INVERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STNAME(0),8(R2)                                                  
         BE    VST20                                                            
         LA    R3,STATYPEQ(R3)                                                  
         B     VST10                                                            
*                                                                               
VST20    MVC   BYTE,STEQU                                                       
*                                                                               
VSTX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A DAY EXPRESSION AT 8(R2)                                     
*        RETURN IN HALF                                                         
*                                                                               
VALDAYS  NTR1                                                                   
         XC    HALF,HALF                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 CALLOV,DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                 
         L     RF,0(R1)                                                         
         ZIC   R5,5(R2)            L'EXPRESSION                                 
         LA    R3,8(R2)                                                         
         CLI   8(R2),C'='          EXACT MATCH PREFIX                           
         BNE   *+10                                                             
         BCTR  R5,0                LENGTH OF DAYS IS 1 LESS                     
         LA    R3,9(R2)                                                         
*                                                                               
         GOTO1 (RF),(R1),((R5),0(R3)),HALF,HALF+1                               
         CLI   HALF,0                                                           
         BE    INVERR                                                           
*                                                                               
VDYX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A TIME AT 8(R2)                                               
*        RETURN IN WORK                                                         
*                                                                               
VALTIME  NTR1                                                                   
         MVI   ERROR,INVTIME                                                    
         XC    WORK,WORK                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         ZIC   R0,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R0),8(R2)),WORK                                    
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
*                                                                               
VTX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE A NUMBER AT 8(R2)                                             
*        R3 - L'FIELD                                                           
*        RETURN IN HALF                                                         
*                                                                               
VALNUM   NTR1                                                                   
         XC    HALF,HALF                                                        
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    VNX                                                              
         MVC   HALF,=X'FFFF'                                                    
         LA    R4,8(R2)                                                         
*                                                                               
VN10     CLI   0(R4),C' '          HAVE WE REACHED A NUMBER                     
         BH    VN20                                                             
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
         BCT   R3,VN10                                                          
         B     VNX                                                              
*                                                                               
VN20     BCTR  R1,0                                                             
         XC    DUB,DUB                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),0(R4)                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),ZEROS                                                     
         BNE   VNX                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   R1,DUB                                                           
         STH   R1,HALF                                                          
*                                                                               
VNX      SR    R0,R0                                                            
         LH    RF,HALF    SET CC (LOW=ERR,EQU=NO VAL,HIGH=OK)                   
         CR    RF,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        GET AN AMOUNT AT 8(R2)                                                 
*        RETURN IN HALF                                                         
*                                                                               
GETPCT   NTR1                                                                   
         LA    R3,0                                                             
         CLI   5(R2),0                                                          
         BZ    GPCTX                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R3)    MIN/MAX % NNN.NN                  
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         L     R3,4(R1)                                                         
         C     R3,=F'10000'                                                     
         BH    INVERR                                                           
*                                                                               
GPCTX    STH   R3,HALF                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        GET AN AMOUNT AT 8(R2)                                                 
*        RETURN IN HALF                                                         
*                                                                               
GETRTG   NTR1                                                                   
         LA    R3,0                                                             
         CLI   5(R2),0                                                          
         BZ    GRTGX                                                            
         ZIC   R3,5(R2)                                                         
         CLI   TWODEC,C'Y'          TWO DECIMAL?                                
         BE    GRTG10               YES                                         
         GOTO1 CASHVAL,DMCB,(1,8(R2)),(R3)    MIN/MAX RTG NNNN.N                
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         L     R3,4(R1)                                                         
         B     GRTGX                                                            
*                                                                               
GRTG10   GOTO1 CASHVAL,DMCB,(2,8(R2)),(R3)    MIN/MAX RTG NNNN.N                
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         OI    4(R1),X'40'          TURN ON 2 DECIMAL FLAG                      
         L     R3,4(R1)                                                         
*                                                                               
GRTGX    ST    R3,FULL                                                          
         B     EXIT                                                             
*                                                                               
*        ANY ACTIVE FIELDS ON LINE?                                             
*        R0 HAS NUMBER OF FIELDS                                                
*                                                                               
CHKLIN   NTR1                                                                   
         LR    R3,R2               SAVE START OF LINE                           
CKL2     DS    0H                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BNE   CKLYES              YES                                          
         BAS   RE,BUMP             NO, NEXT FIELD                               
         BCT   R0,CKL2                                                          
CKLNO    DS    0H                                                               
         CR    RE,RE               NO DATA, SET CC =                            
         B     CKLX                AND LEAVE R2 AT NEXT LINE                    
CKLYES   DS    0H                                                               
         LR    R2,R3               RESTORE R2 TO START                          
         LTR   RE,RE               SET CC NOT EQU                               
CKLX     XIT1  REGS=(R2)                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*        SET R2 TO FIRST FIELD OF SVSCR                                         
*                                                                               
SETR2    NTR1                                                                   
         LA    R2,SPGPGM1H         SET R2 TO FIRST FIELD ON SCREEN              
         CLI   SVRUTYPE,RULKRPGQ   PROGRAM                                      
         BE    SR2X                                                             
         LA    R2,SDSDPTH                                                       
         CLI   SVRUTYPE,RULKRDSQ   DAYPART/STATION                              
         BE    SR2X                                                             
         LA    R2,SDTTZONH                                                      
         CLI   SVRUTYPE,RULKRDTQ   DAY/TIME                                     
         BE    SR2X                                                             
         LA    R2,SSDPROGH                                                      
         CLI   SVRUTYPE,RULKRSDQ   SPOTS PER DAY                                
         BE    SR2X                                                             
         LA    R2,SSWPROGH                                                      
         CLI   SVRUTYPE,RULKRSWQ   SPOTS PER WEEK                               
         BE    SR2X                                                             
         LA    R2,SHLDATEH                                                      
         CLI   SVRUTYPE,RULKRHLQ   HOLIDAYS                                     
         BE    SR2X                                                             
         LA    R2,SRTDPTH                                                       
         CLI   SVRUTYPE,RULKRRTQ   RATINGS                                      
         BE    SR2X                                                             
         DC    H'0'                                                             
*                                                                               
SR2X     ST    R2,AFRSTREC                                                      
         B     EXITR2                                                           
         EJECT                                                                  
*                                                                               
*        CLEAR FROM R2 TO THE BOTTOM OF SCREEN                                  
*                                                                               
CLRSCR   NTR1                                                                   
*                                                                               
         BAS   RE,SETR2                                                         
*                                                                               
CSCR10   ZIC   R1,0(R2)                                                         
         LTR   R1,R1               END OF SCREEN                                
         BZ    CSCRX                                                            
         TM    1(R2),X'20'         LET'S NOT CLEAR PROTECTED FIELDS             
         BO    CSCRX                                                            
         CH    R1,=H'9'                                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         SH    R1,=H'9'            SUB 1 FOR EX & 8 FOR HDR                     
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'            EXTENTED HEADER                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR THE REST OF THE SCREEN                 
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   5(R2),0                                                          
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         B     CSCR10                                                           
*                                                                               
CSCRX    BAS   RE,SETR2                                                         
         B     EXITR2                                                           
*                                                                               
*        BUMP TO NEXT FIELD                                                     
*                                                                               
BUMP     DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE VALIDATES PRODUCT GROUP                                  
*                                                                               
VALPGR   NTR1                                                                   
         CLC   =C'PGR=',8(R2)                                                   
         BNE   INVERR                                                           
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         MVC   PRGKTYP,=X'0D01'    RECORD TYPE                                  
         MVC   PRGKAGMD,BAGYMD     AGY/MEDIA                                    
         MVC   PRGKCLT,BCLT        CLIENT                                       
         MVC   PRGKID,12(R2)       PRODUCT GROUP ID                             
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'5'                                                         
         LTR   R5,R5                                                            
         BZ    VP20                                                             
         CH    R5,=H'3'            MAXIMUM OF 3 DIGITS                          
         BH    INVERR                                                           
*                                                                               
         LA    R4,13(R2)           POINT R4 TO START OF DIGITS                  
* MAY NOT ENTER ALL 9'S                                                         
         CLC   =C'999',0(R4)                                                    
         BE    INVERR                                                           
         STM   R4,R5,WORK          SAVE R4/R5                                   
*                                                                               
VP10     CLI   0(R4),C'0'          ENSURE ALL DIGITS                            
         BL    INVERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    INVERR                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,VP10                                                          
*                                                                               
         LM    R4,R5,WORK          RESTORE R4/R5                                
         XC    WORK,WORK                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4) *EXECUTED*                                         
         PACK  HALF+2(2),WORK(3)   GET DIGITS LEFT ALIGNED                      
         NI    HALF+3,X'F0'                                                     
         MVC   PRGKGRP,HALF+2                                                   
*                                                                               
VP20     GOTO1 HIGH                                                             
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   INVERR                                                           
         MVC   SVPGRP,PRGKID       SAVE ID/GRP NUMBER                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        DK - DISPLAY KEY OF SELECTED RECORD                                    
*                                                                               
***********************************************************************         
         SPACE 1                                                                
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING RULKEY,R4                                                        
*                                  RULE TYPE                                    
         LA    RF,RULETAB          TABLE OF RULES                               
         USING RULETABD,RF                                                      
*                                                                               
DK04     DS    0H                                                               
         CLI   0(RF),X'FF'         EOL                                          
         BNE   *+6                                                              
         DC    H'0'                BAD RULE TYPE                                
         CLC   RULKRUL,RUEQU                                                    
         BE    DK04B                                                            
         LA    RF,RUTABLNQ(RF)     NEXT ENTRY                                   
         B     DK04                                                             
*                                                                               
DK04B    DS    0H                                                               
         MVC   SRUTYPE(L'RUNAME),RUNAME                                         
         OI    SRUTYPEH+6,X'80'     XMIT                                        
         NI    SRUTYPEH+4,X'DF'     UNVAL                                       
         MVI   SRUTYPEH+5,L'RUNAME  LENGTH                                      
*                                                                               
         MVC   SRUMED,QMED         MEDIA                                        
         OI    SRUMEDH+6,X'80'     XMIT                                         
         MVI   SRUMEDH+5,1         LENGTH                                       
*                                                                               
         MVC   SRUCLT,QCLT         CLIENT                                       
         OI    SRUCLTH+6,X'80'     XMIT                                         
         NI    SRUCLTH+4,X'DF'     UNVAL                                        
         MVI   SRUCLTH+5,3         LENGTH                                       
*                                                                               
         MVC   SRUPRD(3),=C'ALL'      PRODUCT                                   
         OI    SRUPRDH+6,X'80'     XMIT                                         
         NI    SRUPRDH+4,X'DF'     UNVAL                                        
         MVI   SRUPRDH+5,3         LENGTH                                       
         CLI   RULKPRD,X'FF'                                                    
         BE    DK08                                                             
*                                                                               
         LA    RF,SVCLIST          PRODUCT CODE LIST                            
*                                                                               
DK06     DS    0H                                                               
         CLI   0(RF),0             EOL                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RULKPRD,3(RF)                                                    
         BE    DK06B                                                            
         LA    RF,4(RF)                                                         
         B     DK06                                                             
*                                                                               
DK06B    DS    0H                                                               
         MVC   SRUPRD(3),0(RF)                                                  
*                                                                               
DK08     DS    0H                                                               
         MVC   SRUEST,=C'ALL'      ESTIMATE                                     
         OI    SRUESTH+6,X'80'     XMIT                                         
         NI    SRUESTH+4,X'DF'     UNVAL                                        
         MVI   SRUESTH+5,3         LENGTH                                       
         CLI   RULKEST,0                                                        
         BE    DK10                                                             
         EDIT  (B1,RULKEST),(3,SRUEST),FILL=0                                   
         OI    SRUESTH+4,X'08'     SET NUMERIC                                  
*                                                                               
DK10     DS    0H                                                               
DKX      DS    0H                                                               
*                                                                               
*        *** THIS DOESN'T SEEM RIGHT, BUT GENCON                                
*        *** DOESN'T SEEM TO BE OF MUCH HELP                                    
*                                                                               
         MVC   SVSELDA,KEY+14     SAVE DA OF SELECTED REC - SEE DISPLAY         
         B     VK                 NOW VALIDATE KEY                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        LR - LIST RECORDS                                                      
*                                                                               
***********************************************************************         
         SPACE 1                                                                
LR       DS    0H                                                               
*                                                                               
         MVI   CHANGED,0           RESET ALL KEY VALUES.                        
         MVI   ENDSCR,0                                                         
         XC    ELEMPTR,ELEMPTR                                                  
         MVC   PRVPTR,=Y(PAGE1)                                                 
         MVC   CURPTR,=Y(PAGE1)                                                 
         MVC   NXTPTR,=Y(PAGE2)                                                 
*                                                                               
         LA    R4,KEY                                                           
         USING RULKEY,R4                                                        
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                NO                                           
         MVC   RULKTYP,=X'0D7C'                                                 
         MVC   RULKAGMD,BAGYMD                                                  
         MVC   RULKCLT,BCLT                                                     
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   RULKTYP,=X'0D7C'    TEST SAME TYPE                               
         BNE   LRX                                                              
         CLC   RULKAGMD,BAGYMD     AND A/M                                      
         BNE   LRX                                                              
         CLC   RULKCLT,BCLT        AND CLIENT                                   
         BNE   LRX                                                              
*                                                                               
         CLI   FLTRTYP,0           TYPE FILTER GIVEN?                           
         BE    *+14                                                             
         CLC   RULKRUL,FLTRTYP     YES, MUST MATCH                              
         BNE   LR20                                                             
*                                                                               
         CLI   FLTPRD,X'FF'        ALL PRODUCTS?                                
         BE    *+14                                                             
         CLC   RULKPRD,FLTPRD                                                   
         BNE   LR20                                                             
*                                                                               
         CLI   FLTEST,0            ALL ESTS?                                    
         BE    *+14                                                             
         CLC   RULKEST,FLTEST                                                   
         BNE   LR20                                                             
*                                                                               
LR40     DS    0H                                                               
*                                  RULE TYPE                                    
         LA    RF,RULETAB          TABLE OF RULES                               
         USING RULETABD,RF                                                      
*                                                                               
LR42     DS    0H                                                               
         CLI   0(RF),X'FF'         EOL                                          
         BNE   *+6                                                              
         DC    H'0'                BAD RULE TYPE                                
         CLC   RULKRUL,RUEQU                                                    
         BE    LR42B                                                            
         LA    RF,RUTABLNQ(RF)     NEXT ENTRY                                   
         B     LR42                                                             
*                                                                               
LR42B    DS    0H                                                               
         MVC   LSTTYPE,RUNAME                                                   
*                                                                               
         MVC   LSTPRD,=C'ALL'      PRODUCT                                      
         CLI   RULKPRD,X'FF'                                                    
         BE    LR46                                                             
*                                                                               
         LA    RF,SVCLIST          PRODUCT CODE LIST                            
*                                                                               
LR44     DS    0H                                                               
         CLI   0(RF),0             EOL                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RULKPRD,3(RF)                                                    
         BE    LR44B                                                            
         LA    RF,4(RF)                                                         
         B     LR44                                                             
*                                                                               
LR44B    DS    0H                                                               
         MVC   LSTPRD,0(RF)                                                     
*                                                                               
LR46     DS    0H                                                               
         MVC   LSTEST,=C'ALL'      ESTIMATE                                     
         CLI   RULKEST,0                                                        
         BE    LR48                                                             
         EDIT  (B1,RULKEST),(3,LSTEST),FILL=0                                   
*                                                                               
LR48     DS    0H                                                               
         GOTO1 GETREC              MUST READ RECORD FOR LISTMON                 
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        PR - PRINT REPORT                                                      
*                                                                               
***********************************************************************         
         SPACE 1                                                                
PR       DS    0H                                                               
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         OC    SPOOLDM,SPOOLDM     TRY TO PREVENT DUMPS IF PRINT                
         BNZ   PR08                FIELDS ARE NOT FILLED IN.                    
         LA    R2,CONWHENH         GENCON SHOULD SET ERROR ITSELF               
         B     MISSERR             BUT DOESN'T                                  
*                                                                               
PR08     DS    0H                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   LASTTYP,0                                                        
         USING PRTLD,R2                                                         
*                                                                               
         LA    R4,KEY                                                           
         USING RULKEY,R4                                                        
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   PR10                NO                                           
         MVC   RULKTYP,=X'0D7C'                                                 
         MVC   RULKAGMD,BAGYMD                                                  
         MVC   RULKCLT,BCLT                                                     
*                                                                               
PR10     GOTO1 HIGH                FIRST RECORD                                 
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   RULKTYP,=X'0D7C'    TEST SAME TYPE                               
         BNE   PRX                                                              
         CLC   RULKAGMD,BAGYMD     AND A/M                                      
         BNE   PRX                                                              
         CLC   RULKCLT,BCLT        AND CLIENT                                   
         BNE   PRX                                                              
*                                                                               
         CLI   FLTRTYP,0           TYPE FILTER GIVEN?                           
         BE    *+14                                                             
         CLC   RULKRUL,FLTRTYP     YES, MUST MATCH                              
         BNE   PR20                                                             
*                                                                               
         CLI   FLTPRD,X'FF'        ALL PRODUCTS?                                
         BE    *+14                                                             
         CLC   RULKPRD,FLTPRD                                                   
         BNE   PR20                                                             
*                                                                               
         CLI   FLTEST,0            ALL ESTS?                                    
         BE    *+14                                                             
         CLC   RULKEST,FLTEST                                                   
         BNE   PR20                                                             
*                                                                               
PR40     DS    0H                                                               
         LA    R2,P                                                             
         MVI   HAVMID,C'N'                                                      
         CLC   RULKRUL,LASTTYP     CHANGE OF RULE TYPE?                         
         BE    PR43                                                             
         MVC   LASTTYP,RULKRUL     SAVE RULE TYPE                               
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A  LINE                                 
         MVI   HAVMID,C'Y'                                                      
*                                  RULE TYPE                                    
         LA    RF,RULETAB          TABLE OF RULES                               
         USING RULETABD,RF                                                      
*                                                                               
PR42     DS    0H                                                               
         CLI   0(RF),X'FF'         EOL                                          
         BNE   *+6                                                              
         DC    H'0'                BAD RULE TYPE                                
         CLC   RULKRUL,RUEQU                                                    
         BE    PR42B                                                            
         LA    RF,RUTABLNQ(RF)     NEXT ENTRY                                   
         B     PR42                                                             
*                                                                               
PR42B    DS    0H                                                               
         MVC   PRTRTYP(15),RUNAME      RULE NAME                                
         LA    RF,PRTRTYP+L'PRTRTYP-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(5,RF),=C'RULES'                                                
         DROP  RF                                                               
*                                                                               
         ZIC   RF,RULKRUL          GET REST OF 'MID HEAD'                       
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         L     RF,RIXTAB(RF)       RULE INDEX TABLE                             
         A     RF,RELO                                                          
         ZIC   RE,0(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRTRULE(0),1(RF)                                                 
         LA    RF,2(RF,RE)         NOW DO UNDERLINES                            
         ZIC   RE,0(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRTRULE+132(0),1(RF)                                             
         LA    R2,P3               P1 AND P2 USED FOR 'MID HEADS'               
         MVC   SVMH1,P1            SAVE 'MID HEADS'                             
         MVC   SVMH2,P2                                                         
                                                                                
PR43     DS    0H                                                               
         LA    R3,PRTRTYP+2        PRD/EST INDENTED UNDER RULE TYPE             
         MVC   0(15,R3),=C'PRD=XXX,EST=XXX'                                     
         MVC   4(3,R3),=C'ALL'      PRODUCT                                     
         CLI   RULKPRD,X'FF'                                                    
         BE    PR46                                                             
*                                                                               
         LA    RF,SVCLIST          PRODUCT CODE LIST                            
*                                                                               
PR44     DS    0H                                                               
         CLI   0(RF),0             EOL                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RULKPRD,3(RF)                                                    
         BE    PR44B                                                            
         LA    RF,4(RF)                                                         
         B     PR44                                                             
*                                                                               
PR44B    DS    0H                                                               
         MVC   4(3,R3),0(RF)       PRD CODE                                     
*                                                                               
PR46     DS    0H                                                               
         MVC   12(3,R3),=C'ALL'      ESTIMATE                                   
         CLI   RULKEST,0                                                        
         BE    PR47                                                             
         EDIT  (B1,RULKEST),(3,12(R3)),FILL=0                                   
*                                                                               
PR47     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         LA    R3,RULEL-RULKEY(R3) FIRST ELEM                                   
*                                                                               
PR50     DS    0H                                                               
         CLI   0(R3),0             EOR                                          
         BE    PR90                                                             
         CLI   0(R3),X'05'                                                      
         BE    PR51                                                             
PR50B    DS    0H                                                               
         ZIC   R0,1(R3)            NEXT ELEM                                    
         AR    R3,R0                                                            
         B     PR50                                                             
*                                                                               
PR51     DS    0H                                                               
         LA    R2,P1                                                            
         CLI   HAVMID,C'Y'         IF PRINTING MID-HEADS                        
         BNE   PR52                                                             
         LA    R2,P3               PUT RULE IN P3                               
         MVI   HAVMID,C'N'                                                      
*                                                                               
PR52     DS    0H                                                               
         ZIC   RF,RULKRUL          RULE TYPE                                    
         BCTR  RF,0                GET A(PROC)                                  
         SLL   RF,3                                                             
         L     RF,RIXTAB+4(RF)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
PR88     DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     PR50B                                                            
*                                                                               
PR90     DS    0H                                                               
         B     PR20                                                             
*                                                                               
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
HDHK     NTR1                                                                   
*                                                                               
         MVC   H1+9(1),QMED        MEDIA                                        
         MVC   H2+9(3),QCLT        CLIENT                                       
         MVC   H3+9(3),QPRD        PRODUCT                                      
         MVC   H4+9(3),QEST        ESTIMATE                                     
*                                                                               
         MVC   H1+14(10),MEDNM                                                  
         MVC   H2+14(20),CLTNM                                                  
         CLC   QPRD,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   H3+14(20),PRDNM                                                  
         CLC   QEST,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   H4+14(20),ESTNM                                                  
*                                  MUST PRINT SAVED 'MID HEADS'                 
         CLC   SVMH1,P1            BUT DON'T PRINT TWICE                        
         BE    HDHKX                                                            
         MVC   P3,P1                                                            
         MVC   P1,SVMH1                                                         
         MVC   P2,SVMH2                                                         
*                                                                               
HDHKX    B     EXIT                                                             
         SPACE 5                                                                
HEADING  SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H1,38,C'SPOTPAK BUYING GUIDELINES'                               
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,1,C'CLIENT'                                                   
         SSPEC H2,38,C'-------------------------'                               
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,1,C'PRODUCT'                                                  
         SSPEC H3,73,REPORT                                                     
         SSPEC H3,85,RUN                                                        
         SSPEC H4,1,C'ESTIMATE'                                                 
         SSPEC H4,73,REQUESTOR                                                  
         SSPEC H4,99,PAGE                                                       
         DC    X'00'                                                            
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*        LOAD IN CORRECT SCREEN                                                 
*        SVSCR - SCREEN NUUMBER                                                 
*                                                                               
SETSCR   NTR1                                                                   
         CLC   MYSCR,SVSCR                                                      
         BE    SSCRX                                                            
         MVC   MYSCR,SVSCR                                                      
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(3),=X'D90217'                                             
         MVC   DMCB+7(1),SVSCR                                                  
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB,SRUTAGH                                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XR    R1,R1                                                            
         LA    R3,SRUTAGH                                                       
*                                                                               
SSCR10   OI    6(R3),X'80'         TRANSMIT                                     
         ICM   R1,1,0(R3)                                                       
         BZ    SSCR20                                                           
         AR    R3,R1                                                            
         B     SSCR10                                                           
*                                                                               
SSCR20   MVC   1(2,R3),=X'0101'    INSURE WE TRANSMIT ALL                       
*                                                                               
SSCRX    BAS   RE,SETR2            SET R2 TO FIRST FIELD                        
         ST    R2,AFRSTREC                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SETVAL   NTR1                                                                   
         BAS   RE,SETR2                                                         
SV10     OI    4(R2),X'20'         MARK FIELD PREVIOUS VALIDATED                
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,BUMP             NEXT FIELD                                   
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    SVX                                                              
         B     SV10                                                             
SVX      B     EXIT                                                             
*                                                                               
CHKVAL   NTR1                                                                   
         BAS   RE,SETR2                                                         
CV10     TM    4(R2),X'20'         IS FIELD PREVIOUSLY VALIDATED                
         BNO   YES                 YES: FIELDS CHANGED                          
         BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BO    NO                  : NOTHING CHANGED                            
         B     CV10                                                             
CVX      B     EXIT                                                             
*                                                                               
STELEM   NTR1                      STORING BACK THE ELEMENTS                    
         LA    R4,ELEMTAB                                                       
SL10     CLI   0(R4),X'FF'                                                      
         BE    SLX                                                              
         CLI   0(R4),C'E'          EMPTY SLOT, SKIP OVER                        
         BE    SL05                                                             
         MVC   ELEM,0(R4)                                                       
         GOTO1 ADDELEM                                                          
SL05     LA    R4,L'ELEMTAB(R4)                                                 
         B     SL10                                                             
SLX      B     EXIT                                                             
*                                                                               
CLRTAB   NTR1                                                                   
         LA    R1,ELEMTAB          HARD CODED TO CLEAR 3600 BYTES               
         LA    RE,15               240 A TIME FOR 15 TIMES                      
CT10     XC    0(240,R1),0(R1)                                                  
         LA    R1,240(R1)                                                       
         BCT   RE,CT10                                                          
         MVI   ELEMTAB,X'FF'                                                    
CTX      B     EXIT                                                             
*                                                                               
SETPTR   NTR1                                                                   
         LA    R4,ELEMTAB                                                       
*                                                                               
         CLI   PFKFLAG,6         PREVIOUS                                       
         BNE   SP20                                                             
         MVI   ENDSCR,0          PREVIOUS CALLS, WONT NEED NEW SCREEN           
         CLC   CURPTR,=Y(PAGE1)                                                 
         BE    SPX                                                              
         LH    RE,PRVPTR                                                        
         LA    R4,0(RE,R4)                                                      
         MVC   NXTPTR,CURPTR     PREVIOUS  >  CURRENT  >  NEXT                  
         MVC   CURPTR,PRVPTR                                                    
         CLC   CURPTR,=Y(PAGE1)                                                 
         BE    SPX                                                              
         LA    RE,ELEMTAB                                                       
         SR    R4,RE                                                            
         SH    R4,=H'720'        24*30                                          
         STH   R4,PRVPTR         NEW PREVIOUS                                   
         B     SPX                                                              
*                                                                               
SP20     CLI   PFKFLAG,8         NEXT                                           
         BNE   SPX                                                              
         LH    RE,NXTPTR                                                        
         LA    R4,0(RE,R4)                                                      
         CLI   0(R4),X'05'       X'05' ELEMENT?                                 
         BE    SP30                                                             
         CLI   ENDSCR,1                                                         
         BNE   SPX                                                              
SP30     CLC   CURPTR,=Y(PAGE5)  NO CHANGES                                     
         BE    SPX                                                              
         MVC   PRVPTR,CURPTR                                                    
         MVC   CURPTR,NXTPTR                                                    
         LA    RE,ELEMTAB                                                       
         SR    R4,RE                                                            
         AH    R4,=H'720'         24*30                                         
         STH   R4,NXTPTR          NEW NEXT                                      
SPX      MVI   PFKFLAG,0                                                        
         B     EXIT                                                             
*                                                                               
STREL    NTR1                     BACK UP THE X'05' ELEM TO A TABLE             
*                                                                               
         L     R4,AIO                                                           
         LA    R5,ELEMTAB                                                       
*                                                                               
         MVI   ELCODE,5            SEE IF ANY ELEMENTS TO LIST                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
SE10     BAS   RE,NEXTEL                                                        
         BNE   SEX                 NO PUN INTENDED                              
*                                                                               
         ZIC   RE,1(R4)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
*                                                                               
         LA    R5,L'ELEMTAB(R5)                                                 
         XC    0(L'ELEMTAB,R5),0(R5)                                            
         MVI   0(R5),X'FF'         MARK END OF TABLE                            
         B     SE10                                                             
*                                                                               
SEX      B     EXIT                                                             
*                                                                               
*        BUILD KEY                                                              
*                                                                               
SETKEY   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RULRECD,R4                                                       
         MVC   RULKTYP,=X'0D7C'                                                 
         MVC   RULKAGMD,BAGYMD     AGY/MEDIA                                    
         MVC   RULKCLT,BCLT        CLIENT                                       
         MVC   RULKRUL,SVRUTYPE    RULE                                         
         OC    SVPGRP,SVPGRP       IS THERE A PRODUCT GROUP                     
         BZ    SK10                                                             
         MVC   RULKPGCD(3),SVPGRP  PRODUCT GROUP                                
         B     *+10                                                             
*                                                                               
SK10     MVC   RULKPRD,BPRD        PRODUCT                                      
         MVC   RULKEST,BEST        ESTIMATE                                     
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        RULE DISPLAY PROCS                                                     
*                                                                               
*        ALL   ARE CALLED WITH R2 AT P, AND R3 AT RULE ELEM                     
*                                                                               
***********************************************************************         
*                                                                               
         USING PRTLD,R2                                                         
         USING RULDATAD,R3                                                      
*                                                                               
***********************************************************************         
*        PGMPRC - PROC FOR PROGRAM RULE                                         
***********************************************************************         
*                                                                               
PGMPRC   NTR1                                                                   
         MVC   PRTPGPG,RUPGPGM                PROGRAM                           
         SR    R1,R1                                                            
         ICM   R1,3,RUPGMIN                                                     
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,PRTPGMN),ZERO=NOBLANK  MIN                               
         SR    R1,R1                                                            
         ICM   R1,3,RUPGMAX                                                     
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,PRTPGMX),ZERO=NOBLANK  MAX                               
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        DPTPRC - PROC FOR DAYPART RULE                                         
***********************************************************************         
*                                                                               
DPTPRC   NTR1                                                                   
         MVC   PRTDPDP,RUDSDPT               DAYPART                            
         MVC   BYTE,RUDSSTP                  STATION TYPE                       
         BAS   RE,PRCSTP                                                        
         MVC   PRTDPST,WORK                                                     
         SR    R1,R1                                                            
         ICM   R1,3,RUDSMIN                                                     
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,PRTDPMN),ZERO=NOBLANK  MIN                               
         SR    R1,R1                                                            
         ICM   R1,3,RUDSMAX                                                     
         BAS   RE,DIV100                                                        
         EDIT  (R1),(3,PRTDPMX),ZERO=NOBLANK  MAX                               
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        DTMPRC - PROC FOR DAY/TIME RULE                                        
***********************************************************************         
*                                                                               
DTMPRC   NTR1                                                                   
         MVC   PRTDTTZ,RUDTTZ                         TIME ZONE                 
         CLI   RUDTTZ,C' '                                                      
         BH    *+10                                                             
         MVC   PRTDTTZ(3),=C'ALL'                                               
         MVC   BYTE,RUDTSTYP                          STATION TYPE              
         BAS   RE,PRCSTP                                                        
         MVC   PRTDTST,WORK                                                     
         MVC   BYTE,RUDTDAYS                          DAYS                      
         BAS   RE,PRCDAY                                                        
*                                                                               
         MVC   PRTDTDY,WORK                                                     
         XC    FULL,FULL                                                        
         OC    FULL(2),RUDTSTIM                                                 
         BZ    DTMP04                                                           
         GOTO1 UNTIME,DMCB,(0,FULL),PRTDTSTM                                    
DTMP04   DS    0H                                                               
         MVC   FULL(2),RUDTETIM                                                 
         OC    FULL(2),FULL                                                     
         BZ    DTMP06                                                           
         GOTO1 UNTIME,DMCB,(0,FULL),PRTDTETM                                    
*                                                                               
DTMP06   DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        SPDPRC - PROC FOR SPOTS/DAY RULE                                       
***********************************************************************         
*                                                                               
SPDPRC   NTR1                                                                   
         EDIT  (B2,RUSDLEN),(4,PRTSDPLN),ZERO=NOBLANK  PGM LENGTH               
         MVC   BYTE,RUSDDAYS                           DAYS                     
         BAS   RE,PRCDAY                                                        
         MVC   PRTSDDY,WORK                                                     
         EDIT  (B2,RUSDSPTS),(3,PRTSDMX),ZERO=NOBLANK  MAX                      
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        SPWPRC - PROC FOR SPOTS/WEEK RULE                                      
***********************************************************************         
*                                                                               
SPWPRC   NTR1                                                                   
         MVC   PRTSWPG,RUSWPGM                          PROGRAM                 
         EDIT  (B2,RUSWSPTS),(3,PRTSWMX),ZERO=NOBLANK   MAX                     
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        HOLPRC - PROC FOR HOLIDAYS (DATES) RULE                                
***********************************************************************         
*                                                                               
HOLPRC   NTR1                                                                   
         OC    RUHLSDT,RUHLSDT                                                  
         BZ    HOLP04                                                           
         GOTO1 DATCON,DMCB,(2,RUHLSDT),(5,PRTHLEX)                              
         OC    RUHLEDT,RUHLEDT                                                  
         BZ    HOLP04                                                           
         MVI   PRTHLEX+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(2,RUHLEDT),(5,PRTHLEX+9)                            
*                                                                               
HOLP04   DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        RTGPRC - PROC FOR RATINGS RULE                                         
***********************************************************************         
*                                                                               
RTGPRC   NTR1                                                                   
         MVC   PRTRTDP,RURTDPT                DAYPART                           
         MVC   BYTE,RURTSTP                   STATION TYPE                      
         BAS   RE,PRCSTP                                                        
         MVC   PRTRTST,WORK                                                     
         CLI   TWODEC,C'Y'                    TWO DECIMAL?                      
         BE    RTG10                          YES                               
         EDIT  (B4,RURTMIN),(7,PRTRTMN),1     MIN                               
         EDIT  (B4,RURTMAX),(7,PRTRTMX),1     MAX                               
         B     RTGX                                                             
*                                                                               
RTG10    MVC   FULL,RURTMIN                                                     
         TM    FULL,X'40'                     HAVE A 2 DECIMAL NUMBER?          
         BO    RTG20                          YES                               
         L     R0,FULL                        NO - MULTIPLY BY 10               
         MHI   R0,10                                                            
         ST    R0,FULL                                                          
*                                                                               
RTG20    NI    FULL,X'FF'-X'40'               TURN OFF X'40'                    
         EDIT  (B4,FULL),(7,PRTRTMN),2        MIN                               
*                                                                               
         MVC   FULL,RURTMAX                                                     
         TM    FULL,X'40'                     HAVE A 2 DECIMAL NUMBER?          
         BO    RTG30                          YES                               
         L     R0,FULL                        NO - MULTIPLY BY 10               
         MHI   R0,10                                                            
         ST    R0,FULL                                                          
*                                                                               
RTG30    NI    FULL,X'FF'-X'40'               TURN OFF X'40'                    
         EDIT  (B4,FULL),(7,PRTRTMX),2        MAX                               
*                                                                               
RTGX     B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        PRCSTP - SHOW STATION TYPE    BYTE=>WORK                               
***********************************************************************         
*                                                                               
PRCSTP   NTR1                                                                   
         LA    RF,STATYPE                                                       
         USING STATYPED,RF                                                      
*                                                                               
PRCSTP2  DS    0H                                                               
         CLI   STEQU,0             EOL                                          
         BE    PRCSTP6                                                          
         CLC   STEQU,BYTE                                                       
         BE    PRCSTP6                                                          
         LA    RF,STATYPEQ(RF)                                                  
         B     PRCSTP2                                                          
*                                                                               
PRCSTP6  MVC   WORK(3),STNAME                                                   
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        PRCDAY - SHOW DAY(S)   BYTE=>WORK                                      
***********************************************************************         
*                                                                               
PRCDAY   NTR1                                                                   
         MVC   WORK(8),MYSPACES                                                 
         CLI   BYTE,0                                                           
         BE    DDYX                                                             
         GOTO1 CALLOV,DMCB,0,X'D9000A0F'  GET ADDRESS OF DAYUNPK                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(8,BYTE),WORK                                          
*                                                                               
PRCDAYX  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
EBFRSERR MVI   ERROR,INVEBFRS      END BEFORE START ERROR                       
         B     TRAPERR                                                          
*                                                                               
EMLMERR  MVI   ERROR,37            MAX LESS THAN MIN                            
         B     TRAPERR                                                          
*                                                                               
EDTERR   MVI   ERROR,38            OUTSIDE OF EST PERIOD                        
         B     TRAPERR                                                          
*                                                                               
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  GOTO1 ERREX               NEVER TO RETURN                              
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*                                                                               
*****************************************                                       
*        SETUP                          *                                       
*****************************************                                       
SETUP    NTR1                                                                   
         MVC   PFKFLAG,PFKEY                                                    
         CLI   ACTNUM,ACTSEL                                                    
         BNE   EXIT                                                             
*                                                                               
         CLI   THISLSEL,C'S'                                                    
         BE    *+12                                                             
         CLI   THISLSEL,C'C'                                                    
         BNE   *+10                                                             
         MVC   SVSEL,THISLSEL                                                   
*                                                                               
         CLI   PFKEY,12                                                         
         BE    SETUPX                                                           
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
         B     EXIT                                                             
SETUPX   OI    GENSTAT2,NEXTSEL    GOTO NEXT SELECTION NOW                      
         XC    ELEMPTR,ELEMPTR                                                  
         MVC   PRVPTR,=Y(PAGE1)                                                 
         MVC   CURPTR,=Y(PAGE1)                                                 
         MVC   NXTPTR,=Y(PAGE2)                                                 
         B     EXIT                                                             
*                                                                               
LITEKEY  NTR1                                                                   
         LA    R2,SRUTAGH                                                       
LK10     LR    R3,R2                                                            
         ZIC   R1,0(R2)                                                         
         AR    R3,R1                                                            
         SHI   R3,8                                                             
         CLI   0(R3),99                                                         
         BE    LK20                                                             
         BAS   RE,BUMP                                                          
         B     LK10                                                             
*                                                                               
LK20     NI    1(R2),X'FF'-X'04'   LIGHT UP PF12=RETURN FIELD                   
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
*                                                                               
DARKKEY  NTR1                                                                   
         LA    R2,SRUTAGH                                                       
DARK10   LR    R3,R2                                                            
         ZIC   R1,0(R2)                                                         
         AR    R3,R1                                                            
         SHI   R3,8                                                             
         CLI   0(R3),99                                                         
         BE    DARK20                                                           
         BAS   RE,BUMP                                                          
         B     DARK10                                                           
*                                                                               
DARK20   OI    1(R2),X'0C'         DARKEN THE PF12=RETURN FIELD                 
         OI    6(R2),X'80'                                                      
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
RELO     DS    F                                                                
LINSW    DS    XL1                                                              
*                                                                               
ZEROS    DC    CL16'0000000000000000'                                           
MYSPACES DC    CL(L'SPACES)' '                                                  
*                                                                               
         DS    0D                                                               
STATYPE  DC    CL3'AFF',AL1(RUDSQAFF)                                           
         DC    CL3'FOX',AL1(RUDSQFOX)                                           
         DC    CL3'IND',AL1(RUDSQIND)                                           
         DC    CL3'CBL',AL1(RUDSQCBL)                                           
         DC    CL3'UPN',AL1(RUDSQUPN)                                           
         DC    CL3'WB ',AL1(RUDSQWB)                                            
         DC    CL3'PAX',AL1(RUDSQPAX)                                           
         DC    CL3'ALL',X'FF'                                                   
         DC    CL3'***',X'00'                                                   
*                                                                               
         DS    0D                                                               
RULETAB  DC    CL15'PROGRAM        ',X'88',AL1(RULKRPGQ)                        
         DC    CL15'DAYPART        ',X'8A',AL1(RULKRDSQ)                        
         DC    CL15'DPT            ',X'8A',AL1(RULKRDSQ)                        
         DC    CL15'DAY/TIME       ',X'8B',AL1(RULKRDTQ)                        
         DC    CL15'DT             ',X'8B',AL1(RULKRDTQ)                        
         DC    CL15'SPOTS/DAY      ',X'8C',AL1(RULKRSDQ)                        
         DC    CL15'SD             ',X'8C',AL1(RULKRSDQ)                        
         DC    CL15'SPOTS/WEEK     ',X'8D',AL1(RULKRSWQ)                        
         DC    CL15'SW             ',X'8D',AL1(RULKRSWQ)                        
         DC    CL15'DATES          ',X'8E',AL1(RULKRHLQ)                        
         DC    CL15'HOLIDAYS       ',X'8E',AL1(RULKRHLQ)                        
         DC    CL15'RATING         ',X'8F',AL1(RULKRRTQ)                        
         DC    CL15'RTG            ',X'8F',AL1(RULKRRTQ)                        
         DC    X'FF'                                                            
*                                                                               
*        MIDLINE COLUMN HEADERS                                                 
*                                                                               
PGMID    DC    AL1(30),C' PROGRAM            MIN%  MAX%'                        
PGMIDU   DC    AL1(30),C' ----------------   ----  ----'                        
DPMID    DC    AL1(34),C' DAYPART  STATION TYPE  MIN%  MAX%'                    
DPMIDU   DC    AL1(34),C' -------  ------------  ----  ----'                    
DTMID    DC    AL1(48),C' TIME ZONES  STATION TYPE  DAY(S)   START  ENDX        
                 '                                                              
DTMIDU   DC    AL1(48),C' ----------  ------------  -------- -----  ---X        
               --'                                                              
SDMID    DC    AL1(31),C' PGM LENGTH  DAY(S)   MAX SPOTS'                       
SDMIDU   DC    AL1(31),C' ----------  ------   ---------'                       
SWMID    DC    AL1(31),C' PROGRAM              MAX SPOTS'                       
SWMIDU   DC    AL1(31),C' ----------------     ---------'                       
HLMID    DC    AL1(18),C' EXCLUDED DATES   '                                    
HLMIDU   DC    AL1(18),C' -----------------'                                    
RTMID    DC    AL1(42),C' DAYPART  STATION TYPE  MIN/SPOT  MAX/SPOT'            
RTMIDU   DC    AL1(42),C' -------  ------------  --------  --------'            
*                                                                               
*        TABLE OF MIDLINE ADDRS AND PROC ADDRS FOR EACH RULE TYPE               
*        - RULKRUL CODE IS INDEX -                                              
*                                                                               
RIXTAB   DS    0D                                                               
         DC    A(PGMID),A(PGMPRC)  PROGRAM                                      
         DC    A(DPMID),A(DPTPRC)  DAYPART                                      
         DC    A(DTMID),A(DTMPRC)  DAY/TIME                                     
         DC    A(SDMID),A(SPDPRC)  SPOTS/DAY                                    
         DC    A(SWMID),A(SPWPRC)  SPOTS/WEEK                                   
         DC    A(HLMID),A(HOLPRC)  HOLIDAYS                                     
         DC    A(RTMID),A(RTGPRC)  RATINGS                                      
*                                                                               
*********************************************************************           
         DROP  R8                                                               
         EJECT                                                                  
STATYPED DSECT                                                                  
STNAME   DS    CL3                                                              
STEQU    DS    XL1                                                              
STATYPEQ EQU   *-STATYPED                                                       
*                                                                               
RULETABD DSECT                                                                  
RUNAME   DS    CL15                                                             
RUSCR    DS    XL1                                                              
RUEQU    DS    XL1                                                              
RUTABLNQ EQU   *-RULETABD                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         PRINT   OFF                                                            
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT   ON                                                             
       ++INCLUDE SPSFMSAVED                                                     
         EJECT                                                                  
*                                                                               
*++INCLUDE SCSFM89D                                                             
*++INCLUDE SCSFM88D                                                             
*++INCLUDE SCSFM8AD                                                             
*++INCLUDE SCSFM8BD                                                             
*++INCLUDE SCSFM8CD                                                             
*++INCLUDE SCSFM8DD                                                             
*++INCLUDE SCSFM8ED                                                             
*++INCLUDE SCSFM8FD                                                             
*                                                                               
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM89D          MAIN                                         
         ORG   SRUTAGH                                                          
       ++INCLUDE SCSFM88D          PROGRAM                                      
         ORG   SRUTAGH                                                          
       ++INCLUDE SCSFM8AD          DPT/STATION TYPE                             
         ORG   SRUTAGH                                                          
       ++INCLUDE SCSFM8BD          DAY/TIME                                     
         ORG   SRUTAGH                                                          
       ++INCLUDE SCSFM8CD          SPOTS/DAY                                    
         ORG   SRUTAGH                                                          
       ++INCLUDE SCSFM8DD          SPOTS/WEEK                                   
         ORG   SRUTAGH                                                          
       ++INCLUDE SCSFM8ED          DATES                                        
         ORG   SRUTAGH                                                          
       ++INCLUDE SCSFM8FD          RATING                                       
*                                                                               
         PRINT ON                                                               
*&&DO                                                                           
*        ORG   CONHEAD+3900                                                     
*&&                                                                             
         ORG   SVSPARE                                                          
MYSVSPAR DS    0X                                                               
*                                                                               
SVSEL    DS    CL1                                                              
SVKEY1   DS    XL13                                                             
NXTPTR   DS    H                   DISPLACEMENT IN TABLE TO NEXT SCREEN         
CURPTR   DS    H                                                                
PRVPTR   DS    H                   ""                    "" PREV SCREEN         
CHANGED  DS    XL1                                                              
ENDSCR   DS    XL1                 FLAG LAST LINE WAS END OF SCREEN             
PFKFLAG  DS    XL1                 PFKEY BACKUP                                 
*                                                                               
ELEMPTR  DS    H                   POINTER FOR THE TABLE                        
ELEMSIZ  DS    0XL3600             ABLE TO STORE 120 ELEMENTS                   
ELEMTAB  DS    0XL30               HAVE TO HARD CODE THIS, TROUBLE!!!           
PAGE1    EQU   *-ELEMTAB                                                        
         DS    24XL30              1ST   PAGE                                   
PAGE2    EQU   *-ELEMTAB                                                        
         DS    24XL30              2ND   PAGE                                   
PAGE3    EQU   *-ELEMTAB                                                        
         DS    24XL30              3RD   PAGE                                   
PAGE4    EQU   *-ELEMTAB                                                        
         DS    24XL30              4TH   PAGE                                   
PAGE5    EQU   *-ELEMTAB                                                        
         DS    24XL30              5TH   PAGE                                   
TABEND   DS    XL1                                                              
TABENDQ  EQU   *                                                                
*                                                                               
MYSVSPRL EQU   (*-MYSVSPAR)                                                     
*                                                                               
         DS    0XL((L'SVSPARE-MYSVSPRL)+1)                                      
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
*                                                                               
         SPACE 2                                                                
       ++INCLUDE SPGENRULE                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         SPACE 2                                                                
*DDSPLWORKD                                                                     
*DDSPOOLD                                                                       
*FATIOB                                                                         
*ESTHDRD                                                                        
*FAGETTXTD                                                                      
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTTYPE  DS    CL15                                                             
         DS    CL1                                                              
LSTPRD   DS    CL3                                                              
         DS    CL1                                                              
LSTEST   DS    CL3                                                              
*                                                                               
PRTLD    DSECT                                                                  
         DS    CL2                                                              
PRTRTYP  DS    CL20                                                             
         DS    CL1                                                              
PRTRULE  DS    CL(132-(*-PRTLD))                                                
         ORG   PRTRULE                                                          
         DS    CL1                                                              
PRTPGPG  DS    CL18                PROGRAM RULE                                 
         DS    CL2                                                              
PRTPGMN  DS    CL3                                                              
         DS    CL3                                                              
PRTPGMX  DS    CL3                                                              
         ORG   PRTRULE                                                          
         DS    CL4                                                              
PRTDPDP  DS    CL1                 DAYPART RULE                                 
         DS    CL9                                                              
PRTDPST  DS    CL3                                                              
         DS    CL8                                                              
PRTDPMN  DS    CL3                                                              
         DS    CL3                                                              
PRTDPMX  DS    CL3                                                              
         ORG   PRTRULE                                                          
         DS    CL1                                                              
PRTDTTZ  DS    CL8                 DAY/TIME RULE                                
         DS    CL8                                                              
PRTDTST  DS    CL3                                                              
         DS    CL7                                                              
PRTDTDY  DS    CL8                                                              
         DS    CL1                                                              
PRTDTSTM DS    CL5                                                              
         DS    CL2                                                              
PRTDTETM DS    CL5                                                              
         ORG   PRTRULE                                                          
         DS    CL3                                                              
PRTSDPLN DS    CL3                 SPOTS/DAY RULE                               
         DS    CL7                                                              
PRTSDDY  DS    CL8                                                              
         DS    CL4                                                              
PRTSDMX  DS    CL4                                                              
         ORG   PRTRULE                                                          
         DS    CL1                                                              
PRTSWPG  DS    CL18                SPOTS/WEEK RULE                              
         DS    CL6                                                              
PRTSWMX  DS    CL5                                                              
         ORG   PRTRULE                                                          
         DS    CL1                                                              
PRTHLEX  DS    CL17                DATE EXCLUSIONS (HOLIDAYS)                   
         ORG   PRTRULE                                                          
         DS    CL4                                                              
PRTRTDP  DS    CL1                 RATINGS RULE                                 
         DS    CL9                                                              
PRTRTST  DS    CL3                                                              
         DS    CL6                                                              
PRTRTMN  DS    CL7                                                              
         DS    CL3                                                              
PRTRTMX  DS    CL7                                                              
*                                                                               
SYSD     DSECT                     RETURN TO SYSD                               
         ORG   SYSSPARE                                                         
SVMH1    DS    CL132                                                            
SVMH2    DS    CL132                                                            
*                                                                               
ERRDISP  DS    H                                                                
SVSCR    DS    CL1                 SCREEN                                       
MYSCR    DS    CL1                 SCREEN                                       
SVRUTYPE DS    CL1                 RULE TYPE                                    
SVPGRP   DS    CL3                 PRODUCT GROUP                                
KEYCHG   DS    CL1                                                              
ACTOUT   DS    CL1                 Y IF ACTLIST OR ACTREP                       
LASTTYP  DS    XL1                                                              
SVLINE   DS    A                                                                
SVSELDA  DS    XL4                                                              
SVMENUC  DS    CL1                                                              
SVMENU   DS    XL37                NEW DPT MENU X'00' = EOT                     
SVESTSD  DS    CL6                 ESTIMATE START                               
SVESTED  DS    CL6                 ESTIMATE END                                 
HAVMID   DS    CL1                                                              
*                                  FILTERS FOR LIST/REPORT                      
FLTRTYP  DS    XL1                                                              
FLTPRD   DS    XL1                                                              
FLTEST   DS    XL1                                                              
*                                                                               
X        DS    XL256                                                            
TWODEC   DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPSFM49   01/29/06'                                      
         END                                                                    
