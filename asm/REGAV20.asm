*          DATA SET REGAV20    AT LEVEL 003 AS OF 10/04/12                      
*PHASE T81320B                                                                  
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
         TITLE 'T81320 - RSTATION MAINT - AVAIL CDE CHA/DIS'                    
T81320   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81320,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81320+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
*                                                                               
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     RF,0(R1)            POINT TO FAFACTS AREA                        
         MVC   WORK2,0(RF)         SAVE FACTS                                   
         DROP  R7                                                               
*                                                                               
         OI    CONSERVH+1,X'01'    ALWAYS MODIFIED                              
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         LA    RF,WORK2                                                         
         USING FACTSD,RF                                                        
         MVC   USERLUID,FASYM      GET LUID                                     
         DROP  RF                                                               
*                                                                               
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         OI    GENSTAT5,NODLST     CAN'T DELETE FROM LIST                       
*                                                                               
         CLI   ACTNUM,ACTDEL       DELETE                                       
         BE    MAIN05                                                           
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    MAIN05                                                           
         CLI   ACTNUM,ACTREST      AND RESTORE ARE INVALID                      
         BE    MAIN05                                                           
         B     MAIN10                                                           
*                                                                               
MAIN05   DS    0H                                                               
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
*                                                                               
MAIN10   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
VKEY     DS    0H                                                               
         XC    CODEHLD,CODEHLD                                                  
         XC    OPTFLAG,OPTFLAG                                                  
*                                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,RSTCODEH                                                      
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         TM    4(R2),X'80'         INPUT AT THIS TIME?                          
         BZ    *+10                                                             
         XC    ASVELEM,ASVELEM                                                  
*                                                                               
         CLI   5(R2),2             REQUIRED                                     
         BL    ERREND                                                           
         MVC   CODEHLD,8(R2)                                                    
         OC    CODEHLD,=8X'40'                                                  
*                                                                               
         LA    R2,RSTOPTH          CHECK OPTIONS                                
         CLI   5(R2),0                                                          
         BE    VK50                                                             
*                                                                               
         GOTO1 SCANNER,DMCB,(0,(R2)),AIO3,C',=,='                               
         CLI   DMCB+4,0            ANY INPUT                                    
         BE    ERREND                                                           
*                                                                               
         ZIC   R3,DMCB+4           # OF OPTIONS                                 
         L     R4,AIO3                                                          
*                                                                               
VK20     DS    0H                                                               
         CLC   =C'ACT',12(R4)      DISPLAY ACTIVE STATIONS?                     
         BNE   *+12                                                             
         OI    OPTFLAG,DRACT                                                    
         B     VK40                                                             
*                                                                               
         CLC   =C'INA',12(R4)      DISPLAY INACTIVE STATIONS?                   
         BNE   *+12                                                             
         OI    OPTFLAG,DRINACT                                                  
         B     VK40                                                             
*                                                                               
         CLC   =C'PUR',12(R4)      DISPLAY PURGED STATIONS?                     
         BNE   ERREND                                                           
         OI    OPTFLAG,DRPURGE                                                  
*                                                                               
VK40     DS    0H                                                               
         LA    R4,32(R4)           BUMP TO NEXT SCANNER BLOCK                   
         BCT   R3,VK20                                                          
*                                                                               
VK50     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
*                                                                               
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODEHLD                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,TODAYJUL)                                  
*                                                                               
VKXIT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
DKEY     DS    0H                                                               
         MVC   RSTCODE,CODEHLD                                                  
         OI    RSTCODEH+6,X'80'                                                 
DKXIT    B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
DREC     DS    0H                                                               
         BAS   RE,CLRSCRN                                                       
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'03'                                                     
         LA    R2,RSTSTATH         STATUS                                       
*                                                                               
         OC    ASVELEM,ASVELEM                                                  
         BZ    DR10                                                             
         L     R2,ASVELEM                                                       
         MVC   ELCODE,SVELEMCD                                                  
         B     DR17                                                             
*                                                                               
DR10     DS    0H                                                               
         BAS   RE,GETEL                                                         
         BNE   DR100                                                            
         B     *+8                                                              
*                                                                               
DR15     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR100                                                            
*                                                                               
DR17     DS    0H                                                               
         LA    RF,RSTTAGH                                                       
         CR    R2,RF                                                            
         BNL   DR50                                                             
*                                                                               
         CLI   OPTFLAG,0           ANY DISPLAY OPTIONS                          
         BE    DR18                                                             
*                                                                               
         CLI   ELCODE,X'03'                                                     
         BNE   *+12                                                             
         TM    OPTFLAG,DRACT                                                    
         BZ    DR15                                                             
*                                                                               
         CLI   ELCODE,X'04'                                                     
         BNE   *+12                                                             
         TM    OPTFLAG,DRINACT                                                  
         BZ    DR15                                                             
*                                                                               
         CLI   ELCODE,X'05'                                                     
         BNE   *+12                                                             
         TM    OPTFLAG,DRPURGE                                                  
         BZ    DR15                                                             
*                                                                               
DR18     DS    0H                                                               
         OI    6(R2),X'80'                                                      
         CLI   ELCODE,X'03'        ACTIVE ELEMENT                               
         BE    DR19                                                             
*                                                                               
         MVI   8(R2),C'I'          INACTIVE ELEMENT                             
         CLI   ELCODE,X'04'                                                     
         BE    DR19                                                             
*                                                                               
         TM    18(R6),X'01'        HAS THIS STATION BEEN RESTORED?              
         BZ    *+12                                                             
         MVI   8(R2),C' '                                                       
         B     DR15                                                             
*                                                                               
         MVI   8(R2),C'P'          PURGED ELEMENT                               
*                                                                               
DR19     DS    0H                                                               
         BAS   RE,NEXTFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   8(4,R2),2(R6)                                                    
         CLI   6(R6),C'T'          SATELLITE?                                   
         BE    DR20                                                             
         MVI   12(R2),C'-'                                                      
         MVC   13(1,R2),6(R6)                                                   
*                                                                               
DR20     DS    0H                  ACT DATE                                     
         BAS   RE,NEXTFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(8,7(R6)),(8,8(R2))                                  
         MVI   5(R2),X'08'         FIXED LEN OF 8 CHAR FOR DATE                 
*                                                                               
DR30     DS    0H                  LUID                                         
         BAS   RE,NEXTFLD                                                       
*                                                                               
         CLI   ELCODE,X'03'        DISPLAYING ACTIVE STATIONS?                  
         BE    DR40                                                             
*                                                                               
         MVC   8(8,R2),10(R6)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
DR40     DS    0H                  NEXT STATION                                 
         BAS   RE,NEXTFLD                                                       
         B     DR15                                                             
*                                                                               
DR50     DS    0H                                                               
         ST    R2,ASVELEM          SAVE POINTER TO LAST ELEM                    
         MVC   SVELEMCD,ELCODE     SAVE AWAY ELEM CODE                          
         B     DRXIT                                                            
*                                                                               
DR100    DS    0H                                                               
         L     R6,AIO                                                           
         CLI   ELCODE,X'03'                                                     
         BNE   *+12                                                             
         MVI   ELCODE,X'04'                                                     
         B     DR10                                                             
*                                                                               
         CLI   ELCODE,X'04'                                                     
         BNE   *+12                                                             
         MVI   ELCODE,X'05'                                                     
         B     DR10                                                             
*                                                                               
DRXIT    B     EXIT                                                             
*                                                                               
MVESPACE MVC   8(0,R2),SPACES                                                   
*                                                                               
* POINT TO NEXT SCREEN FIELD                                                    
*                                                                               
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
****************************************************************                
*              CLEAR SCREEN                                    *                
****************************************************************                
CLRSCRN  NTR1                                                                   
         LA    R2,RSTSTATH                                                      
*                                                                               
CLR10    DS    0H                                                               
         LA    RF,RSTTAGH                                                       
         CR    R2,RF                                                            
         BNL   CLRSCRNX                                                         
*                                                                               
         ZIC   R3,0(R2)                                                         
         SHI   R3,8                                                             
*                                                                               
         SHI   R3,1                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTFLD                                                       
         B     CLR10                                                            
*                                                                               
CLRSCRNX DS    0H                                                               
         B     EXIT                                                             
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
VREC     DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         XC    OPTFLAG,OPTFLAG                                                  
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,RSTOPTH          CHECK OPTIONS                                
         CLI   5(R2),0                                                          
         BE    VR09                                                             
*                                                                               
         GOTO1 SCANNER,DMCB,(0,(R2)),AIO3,C',=,='                               
         CLI   DMCB+4,0            ANY INPUT                                    
         BE    ERREND                                                           
*                                                                               
         ZIC   R3,DMCB+4           # OF OPTIONS                                 
         L     R4,AIO3                                                          
*                                                                               
VR05     DS    0H                                                               
         CLC   =C'ACT',12(R4)      DISPLAY ACTIVE STATIONS?                     
         BNE   *+12                                                             
         OI    OPTFLAG,DRACT                                                    
         B     VR08                                                             
*                                                                               
         CLC   =C'INA',12(R4)      DISPLAY INACTIVE STATIONS?                   
         BNE   *+12                                                             
         OI    OPTFLAG,DRINACT                                                  
         B     VR08                                                             
*                                                                               
         CLC   =C'PUR',12(R4)      DISPLAY PURGED STATIONS?                     
         BNE   ERREND                                                           
         OI    OPTFLAG,DRPURGE                                                  
*                                                                               
VR08     DS    0H                                                               
         LA    R4,32(R4)           BUMP TO NEXT SCANNER BLOCK                   
         BCT   R3,VR05                                                          
*                                                                               
VR09     DS    0H                                                               
         LA    R2,RSTSTATH         STATUS                                       
*                                                                               
VR10     DS    0H                                                               
         LA    RF,RSTTAGH                                                       
         CR    R2,RF                                                            
         BNL   VREX                                                             
*                                                                               
         NI    MYFLAG,X'FF'-(ACTIVE+INACTIVE)                                   
*                                                                               
         TM    4(R2),X'80'         CHANGED STATUS?                              
         BO    *+12                                                             
         BAS   RE,NEXTFLD                                                       
         B     VR200               CHECK NEXT STATION                           
*                                                                               
         ST    R2,AERRFLD                                                       
*                                                                               
         CLI   5(R2),0             MAKE IT ACTIVE                               
         BNE   *+12                                                             
         OI    MYFLAG,ACTIVE                                                    
         B     VR20                                                             
*                                                                               
         CLI   8(R2),C'A'          MAKE IT ACTIVE                               
         BNE   *+12                                                             
         OI    MYFLAG,ACTIVE                                                    
         B     VR20                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'I'          MAKE STATION INACTIVE                        
         BNE   ERREND                                                           
         OI    MYFLAG,INACTIVE                                                  
*                                                                               
VR20     DS    0H                                                               
         BAS   RE,NEXTFLD          POINT TO STATION FIELD                       
         CLI   8(R2),0             ANY MORE STATIONS?                           
         BE    VREX                                                             
*                                                                               
* CHECK IF THIS STATION WAS ORIGINALLY ACTIVE/INACTIVE/PURGED                   
*                                                                               
VR30     DS    0H                                                               
         XC    WORK2,WORK2                                                      
         MVI   ELCODE,X'03'        CHECK IF STATION WAS ACTIVE                  
*                                                                               
VR35     DS    0H                                                               
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VR40     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   VR100                                                            
*                                                                               
         MVC   STAHLD(4),8(R2)                                                  
         MVI   STAHLD+4,C'T'                                                    
         CLI   12(R2),C'-'          ANY SATELLITE                               
         BNE   *+10                                                             
         MVC   STAHLD+4(1),13(R2)                                               
*                                                                               
         CLC   STAHLD,2(R6)        FOUND STATION?                               
         BNE   VR40                                                             
*                                                                               
         XC    ASVELEM,ASVELEM                                                  
         XC    SVELEMCD,SVELEMCD                                                
*                                                                               
* CHANGE STATUS OF STATION                                                      
*                                                                               
         CLI   ELCODE,X'03'        ORIGINALLY ACTIVE?                           
         BE    VR50                                                             
         CLI   ELCODE,X'04'        ORIGINALLY INACTIVE?                         
         BE    VR60                                                             
         CLI   ELCODE,X'05'        ORIGINALLY PURGED?                           
         BE    VR70                                                             
         DC    H'00'                                                            
*                                                                               
VR50     DS    0H                  STATION WAS ORIGINALLY ACTIVE                
         TM    MYFLAG,ACTIVE       MAKE IT ACTIVE AGAIN?                        
         BO    VR200                                                            
         TM    MYFLAG,INACTIVE     MAKE IT INACTIVE?                            
         BZ    VRERRINV                                                         
*                                                                               
         LA    R4,WORK2            BUILD NEW INACTIVE ELEMENT                   
         USING RAIACODE,R4                                                      
         USING RASTCODE,R6                                                      
*                                                                               
         MVI   RAIACODE,X'04'                                                   
         MVC   RAIASTA,RASTSTA                                                  
         MVC   RAIACDTE,TODAYJUL                                                
         MVC   RAIALUID,USERLUID                                                
*                                                                               
         ZIC   RF,RASTLEN                                                       
         AHI   RF,8                                                             
         STC   RF,RAIALEN                                                       
*                                                                               
         ZIC   R5,RASTLEN                                                       
         SHI   R5,RASTLENQ                                                      
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RAIAYR(0),RASTLENQ(R6)                                           
         DROP  R4,R6                                                            
*                                                                               
         MVI   0(R6),X'FF'         REMOVE ACTIVE ELEMENT                        
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'03'                                                     
*                                                                               
* ADD NEW INACTIVE ELEMENT                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),AIO,WORK2                              
         B     VR200                                                            
*                                                                               
VR60     DS    0H                  STATION WAS ORIGINALLY INACTIVE              
         TM    MYFLAG,INACTIVE       MAKE IT INACTIVE AGAIN?                    
         BO    VR200                                                            
         TM    MYFLAG,ACTIVE       MAKE IT ACTIVE?                              
         BZ    VRERRINV                                                         
*                                                                               
         LA    R4,WORK2            BUILD NEW ACTIVE ELEMENT                     
         USING RASTCODE,R4                                                      
         USING RAIACODE,R6                                                      
*                                                                               
         MVI   RASTCODE,X'03'                                                   
         MVC   RASTSTA,RAIASTA                                                  
         MVC   RASTCDTE,TODAYJUL                                                
*                                                                               
         ZIC   RF,RAIALEN                                                       
         SHI   RF,8                                                             
         STC   RF,RASTLEN                                                       
*                                                                               
         ZIC   R5,RAIALEN                                                       
         SHI   R5,RAIALENQ                                                      
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RASTLENQ(0,R4),RAIALENQ(R6)                                      
         DROP  R4,R6                                                            
*                                                                               
         MVI   0(R6),X'FF'         REMOVE INACTIVE ELEMENT                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'04'                                                     
*                                                                               
* ADD NEW ACTIVE ELEMENT                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),AIO,WORK2                              
         B     VR200                                                            
*                                                                               
VR70     DS    0H                  STATION WAS ORIGINALLY PURGED                
         TM    MYFLAG,ACTIVE       MAKE IT ACTIVE AGAIN?                        
         BZ    VRERRINV                                                         
*                                                                               
         LA    R4,WORK2            BUILD NEW ACTIVE ELEMENT                     
         USING RASTCODE,R4                                                      
         USING RAPUCODE,R6                                                      
*                                                                               
         OI    RAPUFLG,RAPUREST    THIS STATION IS NOW RESTORED                 
*                                                                               
         MVI   RASTCODE,X'03'                                                   
         MVC   RASTSTA,RAPUSTA                                                  
         MVC   RASTCDTE,TODAYJUL                                                
*                                                                               
         LA    RF,RASTLENQ                                                      
         AHI   RF,1                ADD 1 FOR RASTYR                             
         STC   RF,RASTLEN                                                       
*                                                                               
* ADD NEW ACTIVE ELEMENT                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),AIO,WORK2                              
*                                                                               
         B     VR200               GO TO NEXT STATION                           
*                                                                               
VR100    DS    0H                                                               
         CLI   ELCODE,X'03'        WAS IT ACTIVE?                               
         BNE   *+12                                                             
         MVI   ELCODE,X'04'        CHECK INACTIVES                              
         B     VR35                                                             
*                                                                               
         CLI   ELCODE,X'04'        WAS IT INACTIVE?                             
         BE    *+6                 IF NOT ACT/INACT/PURG THEN ERROR             
         DC    H'00'                                                            
*                                                                               
         MVI   ELCODE,X'05'        CHECK PURGED                                 
         B     VR35                                                             
*                                                                               
VR200    DS    0H                  GO TO NEXT STATION                           
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         B     VR10                                                             
*                                                                               
VREX     DS    0H                                                               
         B     DREC                                                             
*                                                                               
VRERRINV DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         L     R2,AERRFLD                                                       
         B     ERREND                                                           
********************************************************************            
*                                                                               
         GETEL  R6,DATADISP,ELCODE                                              
*                                                                               
********************************************************************            
ERREND   GOTO1 ERREX                                                            
*                                                                               
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* REGAVFFD                                                                      
* DDGENTWA                                                                      
* REGAVWTWA                                                                     
* REGAVD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* REGAVWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGAVFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVE6D                                                       
         EJECT                                                                  
       ++INCLUDE REGAVWTWA                                                      
         EJECT                                                                  
RARTRECD DSECT                                                                  
       ++INCLUDE REGENARTE                                                      
         EJECT                                                                  
       ++INCLUDE REGAVWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*                                                                               
INVLIST  DS    F                   POINTER TO INVENTORY INFO                    
INVDYTIM DS    CL60                EXTENDED DAY TIME DEMO TABLE                 
*                                                                               
INVMED   DS    CL1                 MEDIA                                        
INVSTAT  DS    CL5                 STATION                                      
INVMKT   DS    CL2                 MARKET                                       
INVSRC   DS    CL1                 SOURCE                                       
INVFBK   DS    CL2                 FROM BOOK                                    
INVTYP   DS    CL1                 I OR P                                       
INVEFDT  DS    CL2                 EFFECTIVE DATE - COMPRESSED                  
INVNO    DS    CL1                 NUMBER IN INVENTORY LIST                     
INVBAD   DS    CL1                 0=NO ERROR, N=NUMBER OF BAD ITEM             
TOTWGHT  DS    CL1                 TOTAL NUMBER QTR HOURS                       
INVTOBK  DS    CL15                TO BOOK CODES                                
*                                                                               
INVIND   DS    CL1                 INVENTORY TYPE INDICATOR                     
INVDAYS  DS    CL1                 1=MON, 7=SUN                                 
INVTIM   DS    CL4                 MILITARY TIME                                
INVCODE  DS    CL2                 PROGRAM CODE                                 
INVCDCTL DS    B                   CONTROL BITS FOR PROGRAM CODE                
INVBTYPE DS    C                   BOOK TYPE (USER INPUT, APPLIES TO            
*                                  DEMO FILE TRANSFERS)                         
INVFRBT  DS    C                   BOOK TYPE (ON INV TO INV TRANSFER            
*                                                                               
TRBKLIST DS    CL60                BOOK ENTRIES BUILT BY REBKLST                
         SPACE                                                                  
TRBKCNT  DS    X                   COUNT OF BOOK ENTRIES                        
TRMODE   DS    C                   COMMUNICATION TO BUFFER ROUTINE              
TRWTOV   DS    C                   USER WEIGHTING OVERRIDE (Y/N)                
TRHOOKSW DS    C                   HOOK ENTERED FOR DEMAND CALL (Y/N)           
TRSVKEY  DS    CL27                                                             
TRFNOVER DS    C                   Y=SUPPRESS TIME PERIOD FOOTNOTING            
TRAPAGE  DS    A                   A(2304 BYTE PAGE)                            
TRPAGE   DS    X                   PAGES WRITTEN TO TWA                         
TRRECS   DS    X                   RECORDS GENERATED DURING LINE EDIT           
         SPACE 1                                                                
DEMEDIA  DS    CL1                 FROM MEDIA                                   
DEMSTA   DS    CL5                      STATION                                 
DEMRKT   DS    CL2                      MARKET FOR DEMOS                        
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    CL1                                                              
BYTE3    DS    CL1                                                              
BYTE4    DS    CL1                                                              
*****************************************************                           
CODEHLD  DS    CL8                 AVAIL CODE HOLD AREA                         
YEARHLD  DS    CL1                 YEAR HOLD AREA                               
INPSW    DS    CL1                 DATA INPUTTED SWITCH                         
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
WORK2    DS    CL200               EXTRA WORK AREA                              
SAVEKEY  DS    CL27                                                             
CHNGLEN  DS    CL1                                                              
*                                                                               
TMPYEAR  DS    CL1                                                              
TMPLEN   DS    CL1                                                              
*                                                                               
DUPYRLEN EQU   840                 DUPLICATE YEAR/LEN COMBO                     
*                                                                               
FIRSTSW  DS    CL1                                                              
DAYINP   DS    CL1                                                              
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
         EJECT                                                                  
*                                                                               
ASVELEM  DS    F                   POINTER TO LAST ELEM                         
SVELEMCD DS    XL1                 SAVED ELEM CODE                              
TMPELECD DS    XL1                 TEMPORARY ELEM CODE                          
USERLUID DS    CL8                 USER LUID                                    
TODAYJUL DS    XL3                 TODAY'S DATE IN JULIAN                       
*                                                                               
AERRFLD  DS    F                   A(FIELD FOR ERROR)                           
*                                                                               
STAHLD   DS    CL5                 STATION                                      
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
ACTIVE   EQU   X'01'               MAKE THIS STATION ACTIVE                     
INACTIVE EQU   X'02'               MAKE THIS STATION INACTIVE                   
PURGED   EQU   X'04'               PURGE THIS STATIONS                          
*                                                                               
OPTFLAG  DS    XL1                 OPTION FLAGS                                 
DRACT    EQU   X'01'               DISPLAY ACTIVE STATIONS ONLY                 
DRINACT  EQU   X'02'               DISPLAY INACTIVE STATIONS ONLY               
DRPURGE  EQU   X'04'               DISPLAY PURGED STATIONS ONLY                 
*                                                                               
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL3                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T813FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T813FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REGAV20   10/04/12'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
