*          DATA SET SRQDQ00    AT LEVEL 007 AS OF 03/09/09                      
*PHASE T15300A                                                                  
         TITLE '$QDQ - DISPLAY ENQUEUE RECORD INFO'                             
         PRINT NOGEN                                                            
QDQ      CSECT                                                                  
         NMOD1 WRKX-WRKD,*$QDQ**,CLEAR=YES                                      
         USING WRKD,RC                                                          
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         LR    R2,R1                                                            
*                                                                               
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
         L     RA,SRPARM1                                                       
         MVC   ASELIST,VSELIST                                                  
*                                                                               
         USING SRQDQFFD,R3         R3=A(TWA)                                    
         L     R3,SRPARM6                                                       
*                                                                               
         USING COMFACSD,R4         R4=A(COM FAC LIST)                           
         L     R4,SRPARM4                                                       
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VDATCON,CDATCON                                                  
         USING FLDHDRD,R4                                                       
         NI    SRVIDH+6,X'BF'                                                   
         XC    MSG,MSG                                                          
         EJECT                                                                  
VALP1    LA    R4,SRVP1H           P1=ACTION                                    
         MVI   ACTN,1                                                           
         CLI   FLDILEN,0                                                        
         BE    VP1X                DEFAULT ACTION IS COUNT                      
         CLI   FLDILEN,7                                                        
         BH    ERR3                                                             
         SR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         BCTR  R1,0                                                             
         LA    R5,ACTNTBL                                                       
*                                                                               
VP1A     CLI   0(R5),0             SEARCH ACTION TABLE                          
         BE    ERR3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),FLDDATA                                                  
         BE    *+12                                                             
         LA    R5,8(R5)                                                         
         B     VP1A                                                             
         MVC   ACTN,7(R5)          SAVE ACTION VALUE                            
*                                                                               
VP1X     EQU   *                                                                
         SPACE 2                                                                
VALP2    LA    R4,SRVP2H           P2=SUB ACTION                                
         MVI   SACTN,0             DEFAULT SUBACTION IS ZERO                    
         XC    REPEAT,REPEAT                                                    
         MVC   RSRC,=CL8' '                                                     
         CLI   FLDILEN,7                                                        
         BH    ERR2                                                             
         CLI   FLDILEN,0                                                        
         BNE   VP2A                                                             
         CLI   ACTN,3              TEST IF ENQ TYPE ACTION                      
         BNL   ERR1                YES MUST HAVE RESOURCE NAME                  
         B     VP2X                                                             
*                                                                               
VP2A     CLI   ACTN,1              SUBACTION FOR COUNT                          
         BNE   VP2B                                                             
         CLI   FLDILEN,5                                                        
         BNE   ERR2                                                             
         CLC   FLDDATA(5),=C'RESET'                                             
         BNE   ERR2                                                             
         MVI   SACTN,1             SET RESET SUBACTION                          
         B     VP2X                                                             
*                                                                               
VP2B     CLI   ACTN,2              SUBACTION FOR DISPLAY                        
         BNE   VP2C                                                             
         CLI   FLDILEN,0                                                        
         BE    VP2X                                                             
         ZIC   R1,FLDILEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLDDATA(0),=C'CONTROL'                                           
         BNE   VP2B1                                                            
         MVI   SACTN,2             SET DISPLAY CONTROL SYSTEM INFO              
         B     VP2X                                                             
                                                                                
VP2B1    TM    FLDIIND,FINPNUM     MUST BE NUMERIC REPEAT                       
         BZ    ERR2                                                             
         ZIC   R1,FLDILEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)                                                   
         CVB   R1,DUB                                                           
         CH    R1,=H'1000'         TEST MAX VALUE                               
         BH    ERR2                                                             
         ST    R1,REPEAT                                                        
         MVI   SACTN,3             SET REPEAT COUNT INPUT                       
         B     VP2X                                                             
*                                                                               
VP2C     CLI   FLDILEN,0           VALIDATE RESOURCE NAME                       
         BE    ERR1                                                             
         CLI   FLDILEN,3                                                        
         BL    ERR2                                                             
         SR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSRC(0),FLDDATA                                                  
         LA    R5,RSRCTBL                                                       
*                                                                               
VP2C1    CLI   0(R5),0             SEARCH RESOURCE TABLE                        
         BE    ERR2                                                             
         CLC   0(8,R5),RSRC        MUST HAVE EXACT MATCH ON RESOURCE            
         BE    *+12                                                             
         LA    R5,8(R5)                                                         
         B     VP2C1                                                            
         CLI   ACTN,3              TEST IF ENQ/DEQ ACTION                       
         BNH   VP2X                                                             
         LA    R4,SRVP3H                                                        
         CLC   SRVP3(6),=C'PLEASE' MAKE SURE WE KNOW WHAT WE MEAN               
         BNE   ERR1                                                             
*                                                                               
VP2X     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* Initialization                                                                
***********************************************************************         
INIT     GOTO1 VDATAMGR,DMCBH,=C'ENQDEQ',(C'X',0)                               
         L     RE,DMCB+4                                                        
         MVC   RETADDRS,0(RE)      SAVE ADCONS RETURNED BY ENQDEQ               
         L     RE,RMYKEY                                                        
         MVC   CPU,0(RE)           SAVE MY CPU ID                               
         MVC   ADR,4(RE)           SAVE MY ADR ID                               
         MVC   FULL,CPU                                                         
         BAS   R9,CPUOUT           FORMAT MY CPU ID                             
         MVC   FULL,ADR                                                         
         BAS   R9,ADROUT           FORMAT MY ADR ID                             
*                                                                               
         GOTO1 VDATAMGR,DMCBH,=C'ENQCTL',(C'X',0)                               
         L     RE,DMCB+4                                                        
         MVC   RETADDRC,0(RE)      SAVE ADCONS RETURNED BY ENQCTL               
*                                                                               
         LA    RE,ECOUNTS          COPY ENQDEQ COUNTS TO WORK                   
         LA    RF,1024                                                          
         L     R0,RCOUNTS                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         MVC   ECOUNTSX,=4X'FF'                                                 
         CLC   RCOUNTS,CCOUNTS     TEST IF ENQDEQ SAME AS ENQCTL                
         BE    INIT3                                                            
         ICM   RE,15,CCOUNTS                                                    
         BZ    INIT3                                                            
         LA    RF,ECOUNTS                                                       
INIT2    CLI   0(RE),X'FF'         ADD ENQCTL COUNTS TO ENQDEQ COUNTS           
         BE    INIT3                                                            
         CLI   0(RF),X'FF'                                                      
         BE    INIT3                                                            
         AP    0(4,RF),0(4,RE)                                                  
         LA    RF,24(RF)                                                        
         LA    RE,24(RE)                                                        
         B     INIT2                                                            
*                                                                               
INIT3    MVC   MSG(24),=C'XXX/XXXX/NNNNNN/HH.MM.SS'                             
         MVC   MSG+00(3),MSGCPU                                                 
         MVC   MSG+04(4),MSGADR                                                 
         L     RE,VSSB                                                          
         L     RE,SSBSIN-SSBD(RE)                                               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSG+09(6),DUB                                                    
         TBIN  SECS                GET TIME NOW                                 
         ST    R1,TIM                                                           
         MVC   FULL,TIM                                                         
         BAS   R9,TIMOUT                                                        
         MVC   MSG+16(2),MSGTIM+0                                               
         MVC   MSG+19(2),MSGTIM+2                                               
         MVC   MSG+22(2),MSGTIM+4                                               
*                                                                               
INIT4    CLI   ACTN,1              COUNTERS FROM DMENQDEQ                       
         BE    COUNT                                                            
         CLI   ACTN,2              DISPLAY ACTION                               
         BNE   INIT5                                                            
         CLI   SACTN,2             TEST TO DISPLAY CONTROL SYSTEM               
         BNE   DISP                                                             
         L     R5,ASELIST          Find control system                          
         LH    RE,0(,R5)                                                        
         L     RF,2(,R5)                                                        
         LA    R5,6(,R5)                                                        
                                                                                
         USING SELISTD,R5                                                       
         CLI   SESYS,X'0A'         Control system - 10                          
         BE    INIT4F                                                           
         BXLE  R5,RE,*-8                                                        
         DC    H'00'               That is bad                                  
*                                                                               
INIT4F   L     R5,SEFILES          A(Start of CSECT to DTFs)                    
         L     R5,0(,R5)           A(Header of files list for system)           
                                                                                
         USING SYSFLSTD,R5                                                      
         CLI   SYSFSYS#,X'0A'      Should match header info                     
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LH    R0,SYSF#FLS         Number of files                              
         STC   R0,#FILES                                                        
         LA    R5,SYSFLIST         Start of list of files                       
                                                                                
         USING FILED,RE                                                         
         LA    RE,FILETAB          File info                                    
INIT4N   MVC   FILE#,SYSFILE#                                                   
                                                                                
         USING DTFPHD,RF                                                        
         SR    RF,RF                                                            
         ICM   RF,7,SYSFADTF       DTF of file                                  
         MVC   FILEDD,DTFDD        DDNAME                                       
         LA    R5,SYSFLNQ(,R5)     Next file                                    
         LA    RE,FILELNQ(,RE)                                                  
         BRCT  R0,INIT4N                                                        
         DROP  RF                                                               
                                                                                
         MVI   FILE#,0             Mark end                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         SR    R5,R5                                                            
         B     DISC                                                             
         DROP  RE                                                               
                                                                                
INIT5    IC    R0,SRVP1            TEST/ENQ/DEQ RESOURCE                        
         GOTO1 VDATAMGR,DMCBH,=C'ENQDEQ',((R0),RSRC)                            
         B     DISP                                                             
*                                                                               
INITX    MVC   SRVMSG,MSG          MOVE OUTPUT MESSAGE TO SCREEN                
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR ON REQUIRED FIELD                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Exit and Errors                                                               
***********************************************************************         
ERR1     LA    R0,1                MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R0,2                INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR3     LA    R0,21               INVALID ACTION                               
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         GOTO1 VGETTXT,DMCB,(R0)                                                
         OI    6(R4),X'40'         POSN CURSOR ON INVALID FIELD                 
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* Display counters                                                              
***********************************************************************         
COUNT    LA    R4,SRVHLH           R4=A(FIRST DISPLAY LINE)                     
         MVI   FLAG,0                                                           
         LA    R5,ECOUNTS          R5=A(ENQDEQ COUNTERS)                        
         CLI   0(R5),X'FF'                                                      
         BE    COUNTR                                                           
         MVC   REPEAT,4(R5)        SAVE NAME OF FIRST COUNTER GROUP             
*                                                                               
COUNT1   LA    R6,8(R4)            POSN TO LEFT/RIGHT                           
         CLI   FLAG,0                                                           
         BE    COUNT1A                                                          
         LA    R6,39(R6)                                                        
         B     COUNT1B                                                          
COUNT1A  MVC   8(78,R4),SPACES     CLEAR SCREEN LINE                            
         OI    6(R4),X'80'                                                      
COUNT1B  MVC   0(20,R6),4(R5)      SET COUNTER NAME                             
         NC    5(15,R6),LOWER                                                   
         LA    RE,20(R6)                                                        
         MVC   FULL,0(R5)          GET COUNTER VALUE PACKED                     
         CP    FULL,=P'0'                                                       
         BNE   COUNT2                                                           
         MVC   0(7,RE),=8C'.'                                                   
         B     COUNT3                                                           
COUNT2   OI    FULL+3,X'0F'                                                     
         UNPK  DUB,FULL                                                         
         MVC   0(7,RE),DUB+1                                                    
*                                                                               
COUNT3   LA    R4,86(R4)           BUMP TO NEXT SCREEN LINE                     
         LA    R6,86(R6)                                                        
         CLI   0(R4),10            TEST END OF SCREEN                           
         BL    COUNT4                                                           
COUNT3A  LA    R5,24(R5)           BUMP TO NEXT COUNTER                         
         CLI   0(R5),X'FF'         TEST END OF RETURNED COUNTERS                
         BE    COUNTR                                                           
         CLC   REPEAT,4(R5)        TEST IF SAME GROUP OF COUNTERS               
         BE    COUNT1              YES                                          
         MVC   REPEAT,4(R5)        NO SAVE NEW GROUP AND SKIP A LINE            
         LA    R0,SRVHLH                                                        
         CR    R0,R4                                                            
         BE    COUNT1              DONT SKIP IF ON FIRST LINE                   
         MVC   0(39,R6),SPACES                                                  
         LA    R4,86(R4)                                                        
         LA    R6,86(R6)                                                        
         CLI   0(R4),10            TEST END OF SCREEN                           
         BNL   COUNT1                                                           
*                                                                               
COUNT4   CLI   FLAG,1              TEST IF LEFT/RIGHT                           
         BE    COUNTR                                                           
         MVI   FLAG,1              SET RIGHT HAND OF SCREEN                     
         LA    R4,SRVHLH           AND GO BACK TO FIRST LINE                    
         LA    R6,8+39(R4)                                                      
         B     COUNT3A                                                          
*                                                                               
COUNTR   MVC   MSG+25(23),=C'ENQDEQ COUNTS DISPLAYED'                           
         NC    MSG+26(22),LOWER                                                 
         LA    R4,SRVP2H                                                        
         CLI   SACTN,1                                                          
         BNE   INITX                                                            
         MVC   MSG+48(10),=C' AND RESET'                                        
         NC    MSG+48(10),LOWER                                                 
         ICM   R5,15,RCOUNTS       R5=A(ENQDEQ COUNTERS)                        
         BZ    COUNTR2                                                          
COUNTR1  CLI   0(R5),X'FF'                                                      
         BE    COUNTR2                                                          
         ZAP   0(4,R5),=P'0'                                                    
         LA    R5,24(R5)                                                        
         B     COUNTR1                                                          
COUNTR2  ICM   R5,15,CCOUNTS       R5=A(ENQCTL COUNTERS)                        
         BZ    INITX                                                            
         CLC   RCOUNTS,CCOUNTS                                                  
         BE    INITX                                                            
COUNTR3  CLI   0(R5),X'FF'                                                      
         BE    INITX                                                            
         ZAP   0(4,R5),=P'0'                                                    
         LA    R5,24(R5)                                                        
         B     COUNTR3                                                          
         EJECT                                                                  
***********************************************************************         
* Display DDSQDQ record data                                                    
***********************************************************************         
DISP     MVC   SRVHL,DISPHDR       SET HEADLINE FOR ENQREC DISPLAY              
         MVC   SRVUL,DISPUND                                                    
         LA    R4,SRVL1H           R4=A(NEXT DISPLAY LINE)                      
         USING LINED,R4                                                         
         MVI   FLAG,0                                                           
*                                                                               
DISP0    TBIN  SECS                GET TIME NOW AND READ ENQ REC                
         ST    R1,TIM                                                           
         GOTO1 VDATAMGR,DMCBH,=C'ENQDEQ',(C'R',0)                               
*                                                                               
DISP1    L     R5,RIORKEY          R5=A(ENQREC KEY)                             
         MVC   ENT,0(R5)                                                        
         LA    R5,30(R5)           R5=A(ENQREC DUMMY FIRST ENTRY)               
         XC    LINEDATA,LINEDATA                                                
         OC    ENT(8),ENT          TEST IF ENQREC IS OWNED                      
         BZ    DISP2               NO LEAVE FIRST LINE EMPTY                    
         MVC   ENTR1,=CL8'*ENQREC*'                                             
         B     DISP3               DISPLAY ENQREC OWNER FIRST                   
*                                                                               
DISP2    LA    R5,30(R5)           BUMP TO NEXT ENTRY                           
         OC    0(8,R5),0(R5)                                                    
         BZ    DISP4               END OF ENTRYS                                
         MVC   ENT,0(R5)                                                        
         LA    R4,86(R4)           BUMP TO NEXT DISPLAY LINE                    
         CLI   0(R4),10                                                         
         BL    DISP4               END OF SCREEN                                
*                                                                               
DISP3    XC    LINEDATA,LINEDATA   DISPLAY ENQDEQ ENTRY IN ENT                  
         MVC   FULL,ENTCPU                                                      
         BAS   R9,CPUOUT                                                        
         MVC   LINECPU,MSGCPU      CPU ID                                       
         MVC   FULL,ENTADR                                                      
         BAS   R9,ADROUT                                                        
         MVC   LINEADR,MSGADR      ADR ID                                       
         MVC   LINERSC,ENTR1       RESOURCE ID                                  
         MVC   LINEJOB,ENTR2       JOB NMAE                                     
         MVC   LINETYP,=8C'.'      ENTRY TYPE                                   
         TM    ENTTYP,X'80'                                                     
         BO    DISP3A                                                           
         MVC   LINETYP,=C'OFF'                                                  
         CLC   ENTR1,=CL8'*ENQREC*'                                             
         BNE   *+10                                                             
         MVC   LINETYP,=8C'.'                                                   
DISP3A   MVC   LINECNT,=8C'.'      ENTRY COUNT                                  
         ZIC   RF,ENTCNT                                                        
         LTR   RF,RF                                                            
         BZ    DISP3B                                                           
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINECNT,DUB                                                      
DISP3B   L     RF,ENTTIM           TIME OF ENQ IN SECS                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINETIM,DUB                                                      
         L     R0,TIM              DELTA TIME IN SECONDS                        
         SR    R0,RF                                                            
         BP    *+6                                                              
         SR    R0,R0               SET NEGATIVES TO ZERO                        
         MVC   LINEDLT,=8C'.'                                                   
         LTR   R0,R0                                                            
         BZ    DISP3C                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINEDLT,DUB                                                      
DISP3C   MVI   FLAG,1              SET DATA DISPLAYED FLAG                      
         B     DISP2                                                            
*                                                                               
DISP4    CLI   FLAG,0              WAS DATA DISPLAYED                           
         BNE   DISPR               YES EXIT                                     
         L     R1,REPEAT           NO DECR REPEAT COUNT AND TRY AGAIN           
         SH    R1,=H'1'                                                         
         BNP   DISPR                                                            
         ST    R1,REPEAT                                                        
         BAS   R9,IOWAIT           DO I/0 WAIT AND TRY AGAIN                    
         B     DISP0                                                            
*                                                                               
DISPR    MVC   MSG+25(34),=C'ENQDEQ RECORD DISPLAYED SECS=NNNNN'                
         NC    MSG+26(27),LOWER                                                 
         L     RF,TIM                                                           
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSG+54(5),DUB                                                    
         LA    R4,SRVP2H           POSN CURSOR TO FIELD AFTER LAST              
         CLI   SACTN,0                                                          
         BE    *+8                                                              
         LA    R4,SRVP3H                                                        
         B     INITX                                                            
         EJECT                                                                  
***********************************************************************         
* Display DDSQDC record data                                                    
***********************************************************************         
         USING LINCD,R4                                                         
DISC     MVC   SRVHL,DISCHIS       Set headline for control display             
         MVC   SRVUL,DISCUND                                                    
         MVC   SRVL7,DISCHDA                                                    
         MVC   SRVL8,DISCUND                                                    
         LA    R4,SRVULH           R4=A(Display line for IS less one)           
         ST    R4,ISLINE                                                        
         LA    R4,SRVL8H           R4=A(Display line for DA less one)           
         ST    R4,DALINE                                                        
         MVI   FLAG,0                                                           
*                                                                               
DISC0    TBIN  SECS                GET TIME NOW AND READ CONTROL REC            
         ST    R1,TIM                                                           
         GOTO1 VDATAMGR,DMCBH,=C'ENQCTL',(C'1',0)                               
*                                                                               
DISC1    L     R5,CCRDATA          R5=A(CONTROL RECORD)                         
         MVC   CTRHDR,0(R5)                                                     
*                                                                               
DISC2    LA    R5,32(R5)           BUMP TO NEXT ENTRY                           
         OC    0(8,R5),0(R5)                                                    
         BZ    DISC4               END OF ENTRYS                                
         MVC   CTRFIL,0(R5)                                                     
                                                                                
         L     R4,ISLINE           Screen line for IS display                   
         TM    CTRFL1,SFISF        IS file or DA file                           
         BO    *+8                 Yes IS                                       
         L     R4,DALINE           Screen line for DA display                   
         LA    R4,LINCLNQ(,R4)     Bump to next display line                    
         XC    LINCDATA,LINCDATA   Clear the line down                          
         LA    RE,ISLINE                                                        
         TM    CTRFL1,SFISF        IS file?                                     
         BO    *+8                 Yes so RE is okay                            
         LA    RE,DALINE           No, DA file then,                            
         ST    R4,0(,RE)           Save off new location                        
*                                                                               
         CLI   0(R4),10            Won't fully work, but protection             
         BL    DISC4               End of screen                                
*                                                                               
                                                                                
         USING FILED,RE                                                         
DISC3    LA    RE,FILETAB                                                       
         LLC   R0,#FILES                                                        
DISC3A   CLC   FILE#,CTRFNO        Match on file                                
         BE    DISC3B                                                           
         LA    RE,FILELNQ(,RE)                                                  
         BRCT  R0,DISC3A                                                        
         DC    H'00'                                                            
                                                                                
DISC3B   MVC   LINCDDN,FILEDD                                                   
         DROP  RE                                                               
                                                                                
         TM    CTRFSTAT,X'80'      TEST IF ENTRY UPDATED LAST TIME              
         BO    DISC3C              YES                                          
         MVI   LINCSTA,C'*'                                                     
         MVC   LINCCPU,DOTS        NO DONT KNOW CPU/ASID/JOB                    
         MVC   LINCADR,DOTS                                                     
         MVC   LINCJOB,DOTS                                                     
         B     DISC3E                                                           
                                                                                
DISC3C   MVC   FULL,CTRCPU         SET LAST KNOWN CPU/ASID/JOB                  
         BAS   R9,CPUOUT                                                        
         MVC   LINCCPU,MSGCPU                                                   
         MVC   FULL,CTRADR                                                      
         BAS   R9,ADROUT                                                        
         MVC   LINCADR,MSGADR                                                   
         MVC   LINCJOB,CTRJOB                                                   
                                                                                
DISC3E   GOTO1 VDATCON,DMCB,(X'06',CTRFDATE),(X'00',LINCDATE)                   
         MVC   FULL,CTRFTIME                                                    
         BAS   R9,TIMOUT                                                        
         MVC   LINCTIME,MSGTIM                                                  
         GOTO1 VHEXOUT,DMCB,CTRFNO,LINCFNO,1,=C'MIX'                            
         GOTO1 VHEXOUT,DMCB,CTRFL1,LINCFL1,1,=C'MIX'                            
         GOTO1 VHEXOUT,DMCB,CTRFL2,LINCFL2,1,=C'MIX'                            
         GOTO1 VHEXOUT,DMCB,CTRFE1,LINCFE1,4,=C'MIX'                            
         TM    CTRFL1,SFISF        IS file                                      
         BO    DISC3I                                                           
         GOTO1 VHEXOUT,DMCB,CTRFE1X,LINCFE2,4,=C'MIX'                           
         B     DISC3L                                                           
                                                                                
DISC3I   GOTO1 VHEXOUT,DMCB,CTRFE2,LINCFE2,4,=C'MIX'                            
                                                                                
DISC3L   MVI   FLAG,1              SET DATA DISPLAYED FLAG                      
         B     DISC2                                                            
*                                                                               
DISC4    CLI   FLAG,0              WAS DATA DISPLAYED                           
         BNE   DISCR               YES EXIT                                     
         L     R1,REPEAT           NO DECR REPEAT COUNT AND TRY AGAIN           
         SHI   R1,1                                                             
         BNP   DISCR                                                            
         ST    R1,REPEAT                                                        
         BAS   R9,IOWAIT           DO I/O WAIT AND TRY AGAIN                    
         B     DISC0                                                            
*                                                                               
DISCR    MVC   MSG+25(24),=C'Control record displayed'                          
         LA    R4,SRVP2H           POSN CURSOR TO FIELD AFTER LAST              
         CLI   SACTN,0                                                          
         BE    *+8                                                              
         LA    R4,SRVP3H                                                        
         B     INITX                                                            
         EJECT                                                                  
***********************************************************************         
* Routines                                                                      
***********************************************************************         
CPUOUT   MVC   MSGCPU,=CL4' '      MOVE CPU-ID IN FULL TO MSG CPU               
         TM    FULL,X'C0'                                                       
         BO    CPUOUTO                                                          
CPUOUTD  UNPK  DUB,FULL+2(3)       DOS FORMAT IS X'00000NNN' PWOS               
         MVC   MSGCPU(3),DUB+4                                                  
         BR    R9                                                               
                                                                                
CPUOUTO  MVC   MSGCPU,FULL         MVS FORMAT IS C'XXXX'                        
         BR    R9                                                               
                                                                                
         SPACE 2                                                                
ADROUT   MVC   MSGADR,=CL4' '      MOVE ADR-ID IN FULL TO MSG ADR               
         CLI   FULL,C'*'                                                        
         BNE   *+14                                                             
         MVC   MSGADR(3),FULL+1    FACPAK ID FORMAT                             
         B     ADROUTX                                                          
         CLI   FULL,C'A'                                                        
         BE    ADROUTO                                                          
ADROUTD  CLI   FULL,C'P'           DOS FORMAT                                   
         BE    *+12                                                             
         MVC   MSGADR(2),FULL      NEW FORMAT IS C'BG'/X'00P0'                  
         BR    R9                                                               
         ZIC   R0,FULL+3           OLD FORMAT IS C'PTN'/X'P0'                   
         SRL   R0,4                                                             
         LA    R1,12               ASSUME 12 PARTNS                             
         MVC   MSGADR(2),=C'BG'                                                 
         LA    RF,1(R1)                                                         
         SR    RF,R0               RF=NUMPARTNS+1-PIKNUM                        
         CR    RF,R1                                                            
         BE    ADROUTX             PARTN IS BG                                  
         MVI   MSGADR,C'F'         PARTN IS FN                                  
         LA    RF,ADROUTT(RF)                                                   
         MVC   MSGADR+1(1),0(RF)                                                
         B     ADROUTX                                                          
ADROUTO  GOTO1 VHEXOUT,DUB,FULL+2,MSGADR,2,=C'MIX'                              
ADROUTX  BR    R9                                                               
ADROUTT  DC    C'0123456789AB'                                                  
         SPACE 2                                                                
TIMOUT   SR    R0,R0               CONVERT TIME IN SECS TO HHMMSS               
         L     R1,FULL                                                          
         D     R0,=F'60'                                                        
         STC   R0,SECS                                                          
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         STC   R0,MINS                                                          
         STC   R1,HOURS                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSGTIM+0(2),DUB                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSGTIM+2(2),DUB                                                  
         IC    R0,SECS                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSGTIM+4(2),DUB                                                  
TIMOUTX  BR    R9                                                               
         SPACE 2                                                                
IOWAIT   GOTO1 VDADDS,DMCB,VDACPUID,0,0,VWKFILE                                 
         BR    R9                                                               
         EJECT                                                                  
NOTREL   DC    8C'.'                                                            
         SPACE 2                                                                
ACTNTBL  DS    0CL8                                                             
         DC    C'COUNT  ',X'01'                                                 
         DC    C'DISPLAY',X'02'                                                 
         DC    C'TEST   ',X'03'                                                 
         DC    C'ENQUEUE',X'04'                                                 
         DC    C'DEQUEUE',X'05'                                                 
ACTNTBLX DC    X'0000'                                                          
         SPACE 2                                                                
RSRCTBL  DS    0CL8                                                             
         DC    CL8'PRTQ1   '                                                    
         DC    CL8'PRTQ2   '                                                    
         DC    CL8'PRTQ3   '                                                    
         DC    CL8'PRTQ4   '                                                    
         DC    CL8'PRTQ5   '                                                    
         DC    CL8'PRTQ6   '                                                    
         DC    CL8'PRTQ7   '                                                    
         DC    CL8'PRTQ8   '                                                    
         DC    CL8'WRKR    '                                                    
         DC    CL8'FACW    '                                                    
         DC    CL8'CTRL    '                                                    
RSRCCBLX DC    X'0000'                                                          
         SPACE 2                                                                
DISPHDR  DC    CL78'CPU  ASID JOBNAME  RESOURCE TYP CNT TSECS DELTA'            
DISPUND  DC    CL78'---- ---- -------- -------- --- --- ----- -----'            
*                                                                               
DISCHDA  DC    CL78' CPU  ASID  DATE   TIME  JOB NAME F# FS FD  DNEXT  X        
                PRIOR     DDNAME'                                               
DISCHIS  DC    CL78' CPU  ASID  DATE   TIME  JOB NAME F# FS FD  OVLAST X        
                PDLAST    DDNAME'                                               
DISCUND  DC    CL78' ---- ---- ------ ------ -------- -- -- -- --------X        
                -------- --------'                                              
*                                                                               
DOTS     DC    16C'.'                                                           
SPACES   DC    78C' '                                                           
LOWER    DC    78X'BF'                                                          
         LTORG                                                                  
         EJECT                                                                  
LINED    DSECT                     DISPLAY LINE FORMAT                          
LINEHDR  DS    CL8                                                              
LINEDATA DS    0CL78                                                            
LINECPU  DS    CL4                                                              
         DS    CL1                                                              
LINEADR  DS    CL4                                                              
         DS    CL1                                                              
LINEJOB  DS    CL8                                                              
         DS    CL1                                                              
LINERSC  DS    CL8                                                              
         DS    CL1                                                              
LINETYP  DS    CL3                                                              
         DS    CL1                                                              
LINECNT  DS    CL3                                                              
         DS    CL1                                                              
LINETIM  DS    CL5                                                              
         DS    CL1                                                              
LINEDLT  DS    CL5                                                              
         SPACE 2                                                                
LINCD    DSECT                     CONTROL RECORD DISPLAY LINE                  
LINCHDR  DS    CL8                 Header                                       
LINCDATA DS    CL78                                                             
         ORG   LINCDATA                                                         
LINCSTA  DS    CL1                 Data                                         
LINCCPU  DS    CL4                                                              
         DS    CL1                                                              
LINCADR  DS    CL4                                                              
         DS    CL1                                                              
LINCDATE DS    CL6                                                              
         DS    CL1                                                              
LINCTIME DS    CL6                                                              
         DS    CL1                                                              
LINCJOB  DS    CL8                                                              
         DS    CL1                                                              
LINCFNO  DS    CL2                                                              
         DS    CL1                                                              
LINCFL1  DS    CL2                                                              
         DS    CL1                                                              
LINCFL2  DS    CL2                                                              
         DS    CL1                                                              
LINCFE1  DS    CL8                                                              
         DS    CL1                                                              
LINCFE2  DS    CL8                                                              
         DS    CL1                                                              
LINCDDN  DS    CL8                                                              
         ORG                                                                    
LINCLNQ  EQU   *-LINCD                                                          
         SPACE 2                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
DMCBH    DS    F                                                                
DMCB     DS    6F                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
ASELIST  DS    A                   A(SE list)                                   
*                                  Control Enqueu only                          
ISLINE   DS    A                   Last display line for IS files               
DALINE   DS    A                   Last display line for DA files               
*                                                                               
RETADDRS DS    0CL24               RETURN ADDRS FROM DMENQDEQ                   
RDDSQDQ  DS    A                   A(DDSQDQ DTF)                                
RMYKEY   DS    A                   A(KEY WHEN I OWN RECORD)                     
RIOKEY   DS    A                   A(KEY WHEN HE OWNS RECORD)                   
RIORKEY  DS    A                   A(KEY AND DATA ACTUAL)                       
RCOUNTS  DS    A                   A(ENQUEUE COUNTERS)                          
RCRDATA  DS    A                   A(CONTROL RECORD DATA)                       
*                                                                               
RETADDRC DS    0CL24               RETURN ADDRS FROM DMENQCTL                   
CDDSQDQ  DS    A                   A(DDSQDQ DTF)                                
CMYKEY   DS    A                   A(KEY WHEN I OWN RECORD)                     
CIOKEY   DS    A                   A(KEY WHEN HE OWNS RECORD)                   
CIORKEY  DS    A                   A(KEY AND DATA ACTUAL)                       
CCOUNTS  DS    A                   A(ENQUEUE COUNTERS)                          
CCRDATA  DS    A                   A(CONTROL RECORD DATA)                       
*                                                                               
ENT      DS    0CL30                                                            
ENTCPU   DS    XL4                                                              
ENTADR   DS    XL4                                                              
ENTTIM   DS    XL4                                                              
ENTTYP   DS    XL1                                                              
ENTCNT   DS    XL1                                                              
ENTR1    DS    CL8                                                              
ENTR2    DS    CL8                                                              
*                                                                               
CTRHDR   DS    0XL32               UNIQUE KEY FOR THIS TASK                     
CTRCPU   DS    XL4                 CPU ID                                       
CTRADR   DS    XL4                 ADR ID                                       
CTRIDATE DS    XL4                 DATE THAT INITIALISE OCCURRED                
CTRITIME DS    XL4                 TIME THAT INITIALISE OCCURRED                
CTRJOB   DS    CL8                 JOB NAME                                     
CTRDATE  DS    XL4                 DATE RECORD LAST WRITTEN                     
CTRTIME  DS    XL4                 TIME RECORD LAST WRITTEN                     
*                                                                               
CTRFIL   DS    0XL32               INFO FOR EACH FILE IN SYSTEM                 
CTRFDATE DS    XL4                 ENTRY DATE                                   
CTRFTIME DS    XL4                 ENTRY TIME                                   
CTRFSTAT DS    XL1                 ENTRY STATUS X'80' UPDATED                   
CTRFNO   DS    XL1                 FILE NUMBER                                  
CTRFL1   DS    XL1                 FILE FLAGS FROM SYSFLES LIST                 
CTRFL2   DS    XL1                 FILE FLAGS FROM DTF+36                       
CTRFE1   DS    XL4                 FILE EOF ADDR 1 ISOVLAST/DNEXT               
CTRFE2   DS    XL4                 FILE EOF ADDR 2 ISPDLAST/.....               
CTRFE1X  DS    XL4                 FILE EOF PREV 1                              
CTRFE2X  DS    XL4                 FILE EOF PREV 2              .               
         DS    XL4                 N/D                                          
*                                                                               
#FILES   DS    X                                                                
FILETAB  DS    20XL(FILELNQ)                                                    
         DS    X                   End of table                                 
*                                                                               
HALF     DS    H                                                                
ACTN     DS    C                                                                
SACTN    DS    C                                                                
FLAG     DS    X                                                                
FLAG1    DS    X                                                                
REPEAT   DS    F                                                                
CPU      DS    F                                                                
ADR      DS    F                                                                
TIM      DS    F                                                                
RSRC     DS    CL8                                                              
MSGCPU   DS    CL4                                                              
MSGADR   DS    CL4                                                              
MSGTIM   DS    CL6                                                              
         DS    X                                                                
HOURS    DS    XL1                                                              
MINS     DS    XL1                                                              
SECS     DS    XL1                                                              
MSG      DS    CL60                                                             
*                                                                               
ECOUNTS  DS    1024C                                                            
ECOUNTSX DS    XL4                                                              
WRKX     DS    0C                                                               
         EJECT                                                                  
FILED    DSECT                                                                  
FILE#    DS    XL1                 File number                                  
FILEDD   DS    CL8                 File DDNAME                                  
FILELNQ  EQU   *-FILED                                                          
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         SPACE 2                                                                
*DMSYSFD                                                                        
       ++INCLUDE DMSYSFD                                                        
         SPACE 2                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRQDQFFD DSECT                                                                  
         DS    CL64                                                             
*SRQDQFFD                                                                       
       ++INCLUDE SRQDQFFD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SRQDQ00   03/09/09'                                      
         END                                                                    
