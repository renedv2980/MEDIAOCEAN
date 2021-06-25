*          DATA SET RECNT1F    AT LEVEL 061 AS OF 10/08/15                      
*PHASE T8021FA                                                                  
*INCLUDE RETIMVAL                                                               
*INCLUDE REDAYVAL                                                               
*INCLUDE REGENPLN                                                               
*INCLUDE RETMPBUC                                                               
         TITLE 'T8021E - REPPAK BUY TEMPLATE CONTRACT MAINTENANCE'              
*                                                                               
***********************************************************************         
*                                                                     *         
*     RECNT1F (T8021F) --- BUY TEMPLATE CONTRACT MAINTENANCE          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
* 08Oct15 KWA Byapss AM validation                                              
* 07AUG01 RHV VOILA                                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
T8021F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8021F                                                         
         LR    R9,RB                                                            
         A     R9,=A(COMMON-T8021F)                                             
         USING COMMON,R9                                                        
*                                                                               
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
         MVI   STAT2,0             CLEAR OUT STATUS BYTE                        
*                                                                               
         L     R2,4(R1)                                                         
*                                                                               
         L     R1,AFACILS                                                       
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         MVC   DUB(2),TIOBCURD                                                  
         ZIC   R0,TIOBAID                                                       
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY                                                         
*                                                                               
         GOTO1 (RFGETTAB,VREPFACS),DMCB,('RTTMPFLD',0)                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   AFLDTAB,0(R1)       SAVE A(FIELD TABLE)                          
*                                                                               
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT                                                             
         DC    H'0'                                                             
*                                                                               
DISP     DS    0H                                                               
         GOTO1 =A(DISPCON),RR=Y                                                 
         LA    R2,TMPTMPH                                                       
         OI    6(R2),X'40'                                                      
         B     XIT                                                              
*                                                                               
EDIT     DS    0H                                                               
         LA    R2,TMPTMPH          TEMPLATE NAME                                
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   ED030                                                            
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         L     RE,4(RE)                                                         
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),0,          +        
               (0,C' TMP'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
ED030    LA    R3,2                                                             
         CLI   5(R2),0                                                          
         BE    ERROR          MUST HAVE A TEMPLATE NAME                         
*                                                                               
         OI    CONCACTH+1,X'01'    MAKE MODIFIED                                
*                                                                               
         XC    KEY,KEY             GET TEMPLATE HEADER REC                      
         LA    R6,KEY                                                           
         USING RTMPREC,R6                                                       
         MVC   RTMPKTYP,=X'1507'                                                
         MVC   RTMPKREP,REPALPHA                                                
         MVC   RTMPKTMP,8(R2)                                                   
         OC    RTMPKTMP,MYSPACES                                                
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFGETREC,VREPFACS),DMCB,KEY,IOAREA,0,DUB                        
         BNE   ERROR               REC NOT FOUND                                
*                                                                               
         LA    R6,IOAREA                                                        
         LA    R3,899                                                           
         TM    RTMPHFLG,X'80'      TEMPLATE INACTIVE?                           
         BO    ERROR                                                            
         MVC   TMPDESC,RTMPHDSC                                                 
         OI    TMPDESCH+6,X'80'                                                 
*                                                                               
* LOOP THRU FIELD TABLE, OPTIMIZE FIELD ARRANGEMENT, BUILD DEFINITION           
*                                                                               
         L     R4,AFLDTAB                                                       
         MVC   HALF,0(R4)          TABLE ENTRY LENGTH                           
         LA    R4,4(R4)            START OF TABLE                               
*                                                                               
         LA    R2,LINDEF           DEFINITION TABLE                             
         MVI   0(R2),X'FF'         INITIALIZE                                   
*                                                                               
         MVI   LINSIZE,1           INIT SIZE OF ENTRY                           
*                                                                               
         LA    R3,LINELNQ          REMAINING ON LINE                            
*                                                                               
ED050    DS    0H                  LOOP START                                   
         CLI   0(R4),X'FF'         END OF FIELD TABLE?                          
         BE    ED100                                                            
*                                                                               
         BAS   RE,ADDDEF           ADD (R4)DEFINITION (UPDATE R3)               
         BE    ED080               SUCESSFUL ADD                                
         LA    R3,LINELNQ          NEW LINE                                     
         ZIC   RE,LINSIZE                                                       
         LA    RE,1(RE)            INCREMENT SIZE OF ENTRY                      
         STC   RE,LINSIZE                                                       
         BAS   RE,ADDDEF           TRY AGAIN                                    
         BE    *+6                                                              
         DC    H'0'                CAN'T FAIL WITH FRESH LINE                   
*                                                                               
ED080    DS    0H                                                               
         AH    R4,HALF             NEXT FIELD                                   
         B     ED050               LOOP                                         
         DROP  R6                                                               
*                                                                               
ED100    DS    0H                  CHECK DISPLAY VS. UPDATE                     
         TM    TMPTMPH+4,X'20'     CHANGED TMEPLATE NAME?                       
         BO    ED110                                                            
         OI    TMPTMPH+4,X'20'                                                  
         MVI   LINSTART,0                                                       
         B     ED180                                                            
*                                                                               
ED110    DS    0H                                                               
         CLI   PFKEY,6             PG DOWN?                                     
         BNE   ED120                                                            
         MVC   LINSTART,LINNEXT                                                 
         B     ED200                                                            
*                                                                               
ED120    DS    0H                                                               
         TM    TMPLINH+4,X'20'     INPUT LINE?                                  
         BO    ED400               UPDATE CALL                                  
*                                                                               
ED180    DS    0H                  VALIDATE START LINE FIELD                    
         LA    R2,TMPLINH                                                       
         OI    4(R2),X'20'         VALIDATED NOW                                
         LA    R3,2                                                             
         CLI   5(R2),0                                                          
         BE    ED200                                                            
         TM    4(R2),X'08'         NUMERIC                                      
         BZ    ERROR               NO                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,254                                                           
         BH    ERROR                                                            
         STC   R1,LINSTART                                                      
         XC    TMPLIN,TMPLIN       CLEAR                                        
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
* NOW BUILD SCREEN FIELDS                                                       
*                                                                               
ED200    DS    0H                                                               
         MVC   TMPLAST(3),=X'000101' INITIALIZE SCREEN                          
         MVC   LINLEN,=H'1'        INITIALIZE                                   
         MVI   LINNEXT,0                                                        
         XC    SCRSTART,SCRSTART   INITIALIZE                                   
         XC    SCRLAST,SCRLAST    INITIALIZE                                    
*                                                                               
         SR    R2,R2                                                            
         LA    R3,LINENUMQ         LINES/SCREEN                                 
         LA    R3,1(R3)            SPACE BETWEEN ENTRIES FENCEPOST              
         ZIC   R1,LINSIZE          LINES/ENTRY                                  
         LA    R1,1(R1)            SPACE BETWEEN ENTRIES                        
         DR    R2,R1                                                            
         STC   R3,LINNUM           ENTRIES/SCREEN                               
*                                                                               
         MVI   MYROW,7             INIT STARTING DISPLAY ROW                    
*                                                                               
         MVC   KEY+26(1),LINSTART                                               
         CLI   KEY+26,0                                                         
         BNE   *+8                                                              
         MVI   KEY+26,1                                                         
         GOTO1 VHIGH                                                            
         B     ED212                                                            
*                                                                               
ED210    DS    0H                                                               
         GOTO1 VSEQ                                                             
ED212    DS    0H                                                               
         CLC   KEY(26),KEYSAVE                                                  
         BNE   ED300                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
* BUILD THE SELECT FIELD                                                        
*                                                                               
         MVI   MYCOL,2                                                          
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(1),MYROW                                                  
         MVC   DMCB+7(1),MYCOL                                                  
         GOTO1 =A(ADDFLD),DMCB,3,,RR=Y                                          
*                                                                               
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         ST    RF,SCRLAST                                                       
         OC    SCRSTART,SCRSTART   1ST TIME ROUND?                              
         BNZ   ED215               NO                                           
         ST    RF,SCRSTART         YES - SAVE DISPL OF 1ST FIELD                
         B     ED218                                                            
ED215    DS    0H                                                               
         CLC   LINLEN,=H'1'        2ND TIME ROUND?                              
         BNE   ED218               NO                                           
         LR    RF,R2               YES - SAVE LEN OF LINE                       
         SR    RF,RA                                                            
         L     RE,SCRSTART                                                      
         SR    RF,RE                                                            
         STH   RF,LINLEN                                                        
*                                                                               
ED218    DS    0H                                                               
         LA    R5,LINDEF           LINE DEFINITION TABLE                        
         MVI   MYCOL,6                                                          
ED220    DS    0H                  BUILD SINGLE DISPLAY ENTRY                   
         CLI   0(R5),X'FF'         DONE?                                        
         BE    ED280               YES                                          
*                                                                               
         L     R4,AFLDTAB          FIND FIELD DEFINITION ENTRY                  
         MVC   HALF,0(R4)                                                       
         LA    R4,4(R4)                                                         
         CLC   0(1,R5),0(R4)                                                    
         BE    *+18                                                             
         AH    R4,HALF                                                          
         CLI   0(R4),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                BAD FIELD EQU                                
*                                                                               
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R2,9(R1)            MAX LABEL LEN                                
         BCTR  R2,0                                                             
         CLI   0(R2),C' '                                                       
         BE    *-6                                                              
         SR    R2,R1               LABEL LEN                                    
*                                                                               
         LA    R2,1(R2)            +SPACE                                       
*                                                                               
         ZIC   R1,17(R4)           FIELD LEN                                    
         AR    R2,R1                                                            
*                                                                               
         LA    RF,80                                                            
         ZIC   RE,MYCOL                                                         
         SR    RF,RE               ROOM LEFT ON LINE                            
*                                                                               
         CR    R2,RF                                                            
         BNH   ED250               FITS                                         
*                                                                               
         ZIC   R1,MYROW                                                         
         LA    R1,1(R1)            NEXT ROW                                     
         STC   R1,MYROW                                                         
         MVI   MYCOL,6                                                          
*                                                                               
* WRITE FIELDS TO TWA                                                           
*                                                                               
ED250    DS    0H                  ADD LABEL FIELD                              
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R6,9(R1)            MAX LABEL LEN                                
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         BE    *-6                                                              
         SR    R6,R1               LABEL LEN                                    
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(1),MYROW                                                  
         MVC   DMCB+7(1),MYCOL                                                  
         GOTO1 =A(ADDFLD),DMCB,(R6),,RR=Y                                       
         OI    1(R2),X'20'         PROTECTED                                    
         ZIC   RE,MYCOL                                                         
         LA    RE,1(R6,RE)                                                      
         STC   RE,MYCOL            INCR COLUMN LABEL LEN + 1 SPACE              
         BCTR  R6,0                                                             
         EX    R6,*+4                                                           
         MVC   8(0,R2),1(R4)       LABEL TEXT                                   
*                                                                               
*                                  ADD DATA FIELD                               
         ZIC   R6,17(R4)           DATA LEN                                     
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(1),MYROW                                                  
         MVC   DMCB+7(1),MYCOL                                                  
         GOTO1 =A(ADDFLD),DMCB,(R6),,RR=Y                                       
         ZIC   RE,MYCOL                                                         
         AR    RE,R6                                                            
         CHI   RE,80                                                            
         BNL   *+8                                                              
         LA    RE,1(RE)            ALLOW 1 SPACE BEFORE NEXT FIELD              
         STC   RE,MYCOL            INCR COLUMN DATA LEN                         
*                                                                               
         CLI   0(R5),0             NUM FIELD?                                   
         BNE   ED260                                                            
         EDIT  (1,KEY+26),(3,8(R2)),ALIGN=LEFT                                  
         STC   R0,5(R2)                                                         
         OI    1(R2),X'20'         PROTECT NUM FLD                              
         B     ED270                                                            
*                                                                               
ED260    DS    0H                  DISPLAY THIS FIELD                           
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',IOAREA),(1,0(R5)),0          
         TM    12(R1),X'06'        NOT FOUND?                                   
         BO    ED270                                                            
         L     RE,12(R1)           A(ELEM)                                      
         USING RTMPFLD,RE                                                       
         ZIC   R1,1(RE)            LEN                                          
         AHI   R1,-(RTMPFOLQ)                                                   
         ZIC   RF,0(R2)                                                         
         AHI   RF,-8                                                            
         CR    R1,RF                                                            
         BNH   *+6                                                              
         LR    R1,RF               MAKE SURE WE DON'T OVERLOAD SCREEN           
         STC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R2),RTMPFVAL                                                 
         DROP  RE                                                               
*                                                                               
ED270    DS    0H                                                               
         LA    R5,1(R5)            NEXT FIELD                                   
         B     ED220                                                            
*                                                                               
ED280    DS    0H                  DONE INDIVIDUAL ENTRY                        
         ZIC   R1,MYROW                                                         
         LA    R1,2(R1)            NEXT ROW + 1 BLANK                           
         STC   R1,MYROW                                                         
         BCT   R3,ED210            NEXT ENTRY DISPLAY                           
*                                                                               
         GOTO1 VSEQ                NO MORE ROOM, CHECK FOR MORE RECS            
         CLC   KEY(26),KEYSAVE                                                  
         BNE   ED300                                                            
         MVC   LINNEXT,KEY+26      START HERE NEXT TIME                         
*                                                                               
ED300    DS    0H                  BUILD PFKEY LINE                             
         GOTO1 =A(ADDFLD),DMCB,60,(24,3),RR=Y                                   
         OI    1(R2),X'20'         PROTECT FIELD                                
         LA    R3,8(R2)                                                         
*                                                                               
         OC    SCRSTART,SCRSTART                                                
         BZ    ED310               NO SELECT FIELDS ONSCREEN                    
         MVC   0(14,R3),=C'PF2=Buy Screen'                                      
         LA    R3,16(R3)                                                        
ED310    DS    0H                                                               
***>     CLI   LINNEXT,0                                                        
***>     BE    ED320               NO CONTINUATION                              
         MVC   0(10,R3),=C'PF6=PgDown'                                          
         LA    R3,12(R3)                                                        
ED320    DS    0H                                                               
         B     EDX                                                              
*                                                                               
ED400    DS    0H                  UDATE CALL, VALIDATE LINES                   
         MVI   ADDFIRST,0          INITIALIZE                                   
         MVI   ADDLAST,0           INITIALIZE                                   
*                                                                               
         LA    R2,TMPFILLH                                                      
ED410    DS    0H                  MAKE ALL UNPROT FIELDS MODIFIED              
         CLI   0(R2),0                                                          
         BE    ED420                                                            
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    ED415                                                            
         OI    1(R2),X'01'                                                      
         OI    6(R2),X'80'                                                      
ED415    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     ED410                                                            
*                                                                               
ED420    DS    0H                                                               
         ICM   R2,15,SCRSTART                                                   
         BZ    EDX                 NO LINES ON SCREEN                           
         AR    R2,RA               A(1ST SELECT FIELD)                          
*                                                                               
ED450    DS    0H                  VALIDATE SELECT FIELD                        
         MVI   SELACT,0                                                         
         CLI   5(R2),0                                                          
         BE    ED500               NO ACTION ON THIS SELECTION                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),8(R2)                                                    
         OC    WORK,MYSPACES                                                    
         CLC   WORK(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE INPUT                              
         LA    R1,WORK                                                          
         CLI   0(R1),C' '          STRIP PRECEEDING SPACES                      
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
*                                                                               
         LA    R3,2                                                             
         SR    R5,R5                                                            
         LA    R4,3(R1)                                                         
ED460    BCTR  R4,0                                                             
         CR    R4,R1                                                            
         BL    ED470                                                            
         LTR   R5,R5                                                            
         BNZ   *+12                                                             
         CLI   0(R4),C' '                                                       
         BE    ED460                                                            
         CLI   0(R4),X'F0'                                                      
         BL    ED470                                                            
         LR    R5,R4                                                            
         B     ED460                                                            
ED470    LA    R4,1(R4)                                                         
         SR    R4,R1                                                            
         BZ    UNWIND                                                           
*                                                                               
         MVI   SELNUM,1            DEFAULT                                      
         LTR   R5,R5                                                            
         BZ    ED480               NO NUMERIC ENTRY                             
         ZIC   RF,5(R2)                                                         
         SR    RF,R4               LEN OF NUMERIC PORTION                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R5)                                                      
         CVB   RF,DUB                                                           
         STC   RF,SELNUM                                                        
*                                                                               
ED480    DS    0H                  VALIDATE ALPHA                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'BUY'                                                  
         BNE   ED490                                                            
         MVI   SELACT,C'B'                                                      
         CLI   SELNUM,1                                                         
         BNE   UNWIND                                                           
         B     ED600                                                            
*                                                                               
ED490    DS    0H                                                               
         CLC   =C'X',0(R1)                                                      
         BNE   UNWIND                                                           
         MVI   SELACT,C'X'                                                      
         CLI   SELNUM,1                                                         
         BNE   UNWIND                                                           
         B     ED650                                                            
*                                                                               
ED500    DS    0H                                                               
         CLI   PFKEY,2             BUY ALL?                                     
         BNE   ED650                                                            
*                                                                               
ED600    DS    0H                                                               
         ST    R2,SELFLD                                                        
         OI    1(R2),X'01'                                                      
         OI    6(R2),X'80'                                                      
         GOTO1 =A(BUYLINE),DMCB,(RC),RR=Y                                       
*                                                                               
ED650    DS    0H                  NEXT LINE                                    
         AH    R2,LINLEN                                                        
         L     RF,SCRLAST                                                       
         AR    RF,RA                                                            
         CR    R2,RF                                                            
         BNH   ED450                                                            
*                                                                               
ED700    DS    0H                                                               
         CLI   ADDFIRST,0          ANY LINES ADDED?                             
         BZ    EDX                                                              
         LA    R3,WORK2                                                         
         EDIT  ADDFIRST,(3,WORK2),ALIGN=LEFT                                    
         AR    R3,R0                                                            
         CLC   ADDFIRST,ADDLAST                                                 
         BE    ED710                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         EDIT  ADDLAST,(3,0(R3)),ALIGN=LEFT                                     
         AR    R3,R0                                                            
ED710    LA    R0,WORK2                                                         
         SR    R3,R0                                                            
         GOTO1 VDISMSG,DMCB,(0,47),0,0,((R3),WORK2)                             
*                                                                               
*                                  NEED TO CLEAR ALL SEL FIELDS NOW             
         ICM   R2,15,SCRSTART                                                   
         BZ    EDX                 NO LINES ON SCREEN                           
         AR    R2,RA               A(1ST SELECT FIELD)                          
ED750    DS    0H                                                               
         XC    8(3,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         AH    R2,LINLEN                                                        
         L     RF,SCRLAST                                                       
         AR    RF,RA                                                            
         CR    R2,RF                                                            
         BNH   ED750                                                            
         B     EDX                                                              
*                                                                               
EDX      XIT1                                                                   
         LTORG                                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
COMMON   DS    0H                                                               
*                                                                               
UNWIND   DS    0H            CALL ERROR MSG & UNWIND TRANSACTION I/O            
         XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',0),0,0,0                                
         OI    6(R2),X'40'+X'80'   CURSOR & XMIT                                
         DC    H'0',C'$ABEND'      UNWIND THIS TRANSACTION                      
*                                                                               
BUYERROR DS    0H                                                               
         LTR   R2,R2                                                            
         BZ    BUYERRO5                                                         
         L     RF,AIO3                                                          
         CR    R2,RF                                                            
         BL    UNWIND                                                           
         LA    RF,500(RF)                                                       
         CR    R2,RF                                                            
         BH    UNWIND                                                           
BUYERRO5 LA    R2,TMPFILLH                                                      
         B     UNWIND                                                           
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     XIT                                                              
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     XIT                                                              
EXITOK   CR    RB,RB               SET CC EQUAL                                 
XIT      XIT1                                                                   
*                                                                               
ADDDEF   NTR1   ON RETURN: CC NOT= NO ROOM ON LINE                              
*                                                                               
* CHECK IF FIELD ALREADY IN DEFINITION                                          
*                                                                               
         LA    R2,LINDEF                                                        
ADF010   CLI   0(R2),X'FF'                                                      
         BE    ADF020                                                           
         CLC   0(1,R4),0(R2)       IN DEFINITION ALREADY?                       
         BE    ADFXOK              YES                                          
         LA    R2,1(R2)                                                         
         B     ADF010                                                           
*                                                                               
* CHECK HEADER RECORD IF FIELD BELONGS IN DEFINITION                            
*                                                                               
ADF020   DS    0H                                                               
         CLI   0(R4),0             NUM FIELD ALWAYS IN DEFINITION               
         BE    ADF050                                                           
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',IOAREA),(1,0(R4)),0          
         CLI   12(R1),0                                                         
         BE    ADF040                                                           
         TM    12(R1),X'06'        NOT FOUND?                                   
         BO    ADFXOK                                                           
         DC    H'0'                                                             
*                                                                               
ADF040   DS    0H                                                               
         L     R6,12(R1)                                                        
         USING RTMPFLD,R6                                                       
***      TM    RTMPFFLG,X'80'      FIELD IN CONTRACT ONLY                       
***      BO    ADFXOK              YES                                          
*                                                                               
* NOW DECIDE IF WE HAVE ROOM ON LINE FOR THIS FIELD                             
*                                                                               
ADF050   DS    0H                                                               
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R2,9(R1)            MAX LABEL LEN                                
         BCTR  R2,0                                                             
         CLI   0(R2),C' '                                                       
         BE    *-6                                                              
         SR    R2,R1               LABEL LEN                                    
*                                                                               
         LA    R2,1(R2)            +SPACE                                       
*                                                                               
         ZIC   R1,17(R4)           FIELD LEN                                    
         AR    R2,R1                                                            
*                                                                               
         CR    R2,R3                                                            
         BH    ADF150              DOESN'T FIT                                  
*                                                                               
* FIELD FITS ON LINE - UPDATE LEN & ADD TO DEFINITION TABLE                     
*                                                                               
         SR    R3,R2               UPDATE RUNNING LINE LEN                      
         BZ    *+6                                                              
         BCTR  R3,0                ALLOW 1 SPACE BEFORE NEXT FIELD              
*                                                                               
         LA    R2,LINDEF           FITS - ADD TO DEFINITION                     
         CLI   0(R2),X'FF'         FIND END OF DEFINITION TABLE                 
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         MVC   0(1,R2),0(R4)       ADD TO DEFINITION                            
         MVI   1(R2),X'FF'                                                      
         B     ADFXOK                                                           
*                                                                               
* FIELD DOESN'T FIT, TRY FOR SMALLER FIELD & RECURSIVELY CALL THIS ROUT         
*                                                                               
ADF150   DS    0H                                                               
         AH    R4,HALF             NEXT FIELD IN TABLE                          
         CLI   0(R4),X'FF'         NO MORE                                      
         BE    ADFXL                                                            
*                                                                               
         BAS   RE,ADDDEF                                                        
         BNE   ADFXL               NO FIELDS FIT, GET OUT                       
         B     ADF150              FIT A SMALLER ONE, KEEP TRYING               
*                                                                               
ADFXH    CLI   *,0                 SET CC HIGH                                  
         B     ADFX                                                             
ADFXL    CLI   *,X'FF'             SET CC LOW                                   
         B     ADFX                                                             
ADFXOK   CR    RB,RB               SET CC EQUAL                                 
ADFX     XIT1  REGS=(R3)                                                        
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTDDD                                                       
*                                                                               
         ORG   LOCALTWA                                                         
AFLDTAB  DS    A                                                                
SCRSTART DS    F                   DISPL TO 1ST SEL FLD ON SCREEN               
SCRLAST  DS    F                   DISPL TO LAST SEL FLD ON SCREEN              
PFKEY    DS    X                   PFKEY HIT                                    
ADDFIRST DS    X                   LIN NUM OF 1ST BUY REC ADDED                 
ADDLAST  DS    X                   LIN NUM OF LAST BUY REC ADDED                
SELACT   DS    C                   SELECT FIELD ACTION                          
SELNUM   DS    X                   SELECT FIELD NUMBER                          
SELFLD   DS    F                   A(SELECT FIELD FOR LINE IN PROCESS)          
MYFLD    DS    X                   FIELD EQUATE IN PROCESS                      
LINSTART DS    X                   DISPLAY LINE START #                         
LINNEXT  DS    X                   START REC FOR NEXT PAGE                      
LINELNQ  EQU   74                  DISPLAY LINE WIDTH                           
LINENUMQ EQU   17                  # OF DISPLAY LINES                           
LINSIZE  DS    X                   # OF SCREEN LINES OF EACH ENTRY              
LINNUM   DS    X                   # OF ENTRIES ON FORMATTED SCREEN             
LINDEF   DS    24XL1               TEMPLATE LINE DISPLAY DEFINITION             
*                                  MAX 24 1-BYTE FIELD CODES                    
LINLEN   DS    H                   DISPL LIN LEN (SEL TO SEL FIELD)             
MYCOL    DS    X                                                                
MYROW    DS    X                                                                
*                                                                               
ADAYFLD  DS    F                                                                
ATIMFLD  DS    F                                                                
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE REGENTMP                                                       
       ++INCLUDE DDFH                                                           
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
         CSECT                                                                  
***********************************************************************         
* ADDFLD - DYNAMICALLY ADD SCREEN FIELD TO TWA (DEFAULT ATTRIBUTES)             
***********************************************************************         
*                                                                               
*  P1 : LENGTH                                                                  
*                                                                               
*  P2 : BYTE 0 : ROW                                                            
*       BYTE 3 : COL                                                            
*                                                                               
*  ON RETURN R2=FLD HEADER                                                      
*                                                                               
ADDFLD   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TMPLAST  FIND LAST FIELD ON SCREEN                            
ADDFLD1  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    ADDFLD2                                                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     ADDFLD1                                                          
ADDFLD2  DS    0H                                                               
         USING FHD,R2              GENERATE NEW FIELD                           
         XC    0(8,R2),0(R2)                                                    
         ZIC   R3,4(R1)            ROW                                          
         ZIC   R4,7(R1)            COL                                          
         BCTR  R3,0                                                             
         BCTR  R4,0                                                             
         MH    R3,=H'80'                                                        
         AR    R3,R4                                                            
         STCM  R3,3,FHAD           SCREEN ADDRESS                               
         OI    FHOI,X'80'          XMIT                                         
         ZIC   R3,3(R1)            FIELD LEN                                    
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   FHDA(0),MYSPACES    BLANK FIELD                                  
         LA    R3,9(R3)            +HDR                                         
         STC   R3,FHLN                                                          
         AR    R3,R2                                                            
         MVC   0(3,R3),=X'000101'                                               
         XIT1  REGS=(R2)                                                        
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
* DISPLAY ABBREVIATED CONTRACT HEADER INFO                                      
*                                                                               
         CSECT                                                                  
DISPCON  NTR1  BASE=*,LABEL=*                                                   
         MVC   TMPTYPE(L'RCONTYPE),RCONTYPE                                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISCON05                                                         
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED, SHOW MOD NUM.                     
         BO    DISCON30                                                         
*                                                                               
         ZIC   R3,1(R6)                                                         
         AR    R6,R3               GET NEXT ELEMENT                             
*                                                                               
DISCON05 DS    0H                                                               
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BNE   DISCON35                                                         
         USING RCONSEND,R6                                                      
                                                                                
DISCON10 DS    0H                                                               
         OI    CONMODH+6,X'80'     XMIT FIELD                                   
*                                                                               
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    DISCON15                                                         
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     DISCON20                                                         
DISCON15 EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
                                                                                
DISCON20 DS    0H                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET?                                    
         BZ    DISCON25                                                         
         MVC   CONMOD(7),=C'ESL VER'                                            
         B     DISCON35                                                         
                                                                                
DISCON25 DS    0H                  DISPLAY WORK IN PROGRESS                     
         MVC   CONMOD+4(3),=C'VER'                                              
         TM    RCONSENF,X'10'+X'20' SHOW IF WIP                                 
         BO    DISCON35                                                         
         MVC   CONMOD(3),=C'WIP'                                                
         B     DISCON35                                                         
         DROP  R6                                                               
                                                                                
DISCON30 CLI   RCONMOD,0           K MOD NUM                                    
         BE    DISCON35                                                         
         MVC   CONMOD(7),=C'MOD NUM'                                            
         EDIT  (1,RCONMOD),(3,CONMOD+8),ALIGN=LEFT                              
*                                                                               
DISCON35 DS    0H                                                               
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,TMPSTA                         
*                                                                               
         MVC   TMPSAL,RCONSAL                                                   
*                                                                               
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),REPALPHA                                               
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),TMPHDTE,0,WORK            
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,TMPAGY,WORK2,WORK              
         MVC   TMPAGYN,WORK2                                                    
*                                                                               
         MVC   TMPADV,RCONKADV                                                  
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKADV,WORK2,0,WORK                   
         MVC   TMPADVN,WORK2                                                    
*                                                                               
DISCONX  DS    0H                                                               
         B     XIT                                                              
         LTORG                                                                  
*                                                                               
         DROP  R8                                                               
*                                                                               
* BUYLINE - WRITE NEW BUY FROM TEMPLATE LINE                                    
*                                                                               
BUYLINE  NMOD1 0,*BLIN*,R8                                                      
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     RF,AIO3                                                          
         MVI   0(RF),0             INITIALIZE                                   
*                                                                               
         XC    RBUYREC(255),RBUYREC                                             
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         L     R2,SELFLD                                                        
         DROP  R7                                                               
         LA    R3,BACERR                                                        
         TM    RCONCNTL,X'01'      COMPRESSED CONTRACT                          
         BO    BUYERROR                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'08'       TRADE CONTRACT?                              
         BNO   BUED0010            NO                                           
         OI    BUYFLAGS,X'80'      YES - TURN ON INDICATOR                      
         B     BUED0015                                                         
         DROP  R6                                                               
BUED0010 EQU   *                                                                
*                                  CASH ORDERS:                                 
*                                                                               
         CLI   RCONKSTA+4,C'A'     AM MEDIA/RADIO ORDER?                        
         BE    BUED0015            YES - CAN BE CASH+TRADE                      
         CLI   RCONKSTA+4,C'F'     FM MEDIA/RADIO ORDER?                        
         BE    BUED0015            YES - CAN BE CASH+TRADE                      
         TM    RCONMODR,X'20'      NO  - TV: FIRST BUYLINE ENTERED?             
         BNO   BUED0015            NO  - CAN BE EITHER CASH OR TRADE            
*                                     NOT SET YET                               
         OI    BUYFLAGS,X'40'      CASH ORDER FOR TV FOUND                      
BUED0015 EQU   *                                                                
         TM    RCONMODR+1,X'80'    IF 'ACE' THERE ARE SOME                      
         BZ    BUED0040            SPECIAL TESTS TO BE DONE                     
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING RCONSEND,R6                                                      
*                                                                               
* CAN'T MAKE CHANGES IF OTHER SIDE IS IN PROCESS OF MAKING CHANGES              
*                                                                               
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         TM    RCONSENF,X'10'      X'10'=STA. VERS. NOT ADVANCED                
         BZ    BUYERROR                                                         
         DROP  R6                                                               
*                                                                               
BUED0040 DS    0H                                                               
         CLC   TMPADV(3),=C'GEN'   DON'T ALLOW BUYS WHEN ADV=GEN &              
         BNE   BUED0060                                                         
         CLC   CONCAT,=C'ZZ'       CATEGORY=ZZ (I.E. GENERAL AVAILS)            
         BNE   BUED0060                                                         
         LA    R3,192                                                           
         B     BUYERROR                                                         
         SPACE 1                                                                
*              MUST BE NEW BUY                                                  
         SPACE 1                                                                
BUED0060 EQU   *                                                                
         MVI   ECFORMAT,C' '       CLEAR EC FORMAT                              
         CLI   RCONKSTA+4,C' '     TV STATION?                                  
         BE    BUED0160            YES                                          
         CLI   RCONKSTA+4,C'L'     TV STATION?                                  
         BE    BUED0160            YES                                          
         SPACE                                                                  
BUED0080 EQU   *                   FORCE BOP BEFORE BUYS ON NEW CONT.           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    BUED0140                                                         
         GOTO1 =A(GETSTAT),RR=Y                                                 
         B     BUED0120                                                         
         EJECT                                                                  
*                                                                               
BUED0120 EQU   *                                                                
         TM    RSTASTAT,X'80'      BOP CHECK OVERRIDE                           
         BO    BUED0140            YES                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    BUED0140            FOUND BOP                                    
         GOTO1 =A(ISTAKOV),RR=Y                                                 
         BZ    BUED0140                                                         
         LA    R3,BOPERR                                                        
         B     BUYERROR                                                         
         SPACE                                                                  
BOPERR   EQU   173                 MISSING BOP DATA                             
         SPACE 2                                                                
BUED0140 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0200            NO EPL ELEMENT                               
         USING RCONSPEL,R6                                                      
         CLC   RCONSPAM(3),=C'CAN' NO BUYS ALLOWED ON CANCELLED                 
         BNE   BUED0200            CONTRACTS                                    
         LA    R3,190                                                           
         B     BUYERROR                                                         
         DROP  R6                                                               
         SPACE 1                                                                
BUED0160 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUED0180            NO SAR ELEMENT                               
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         OC    RSARRDT,RSARRDT     CONTRACT RESOLVED?                           
         BNZ   BUED0180            YES - FIELD CONTAINS DATE                    
         SPACE 1                                                                
         MVC   RSARRDT,TODAY       NO  - RESOLVE CONTRACT                       
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
***>>    GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     BUED0180                                                         
         DROP  R6                                                               
         SPACE 1                                                                
BUED0180 DC    0H'0'                                                            
         GOTO1 =A(GETSTAT),RR=Y                                                 
         MVC   ECFORMAT,RSTATRAF   SAVE EC FORMAT                               
***      CLI   TWAACCS,C'$'        IF STATION, GO DIRECTLY TO GET               
***      BE    CHGBUY              BUY RECORD, THEN TO T80216                   
         SPACE 1                                                                
BUED0200 DS    0H                                                               
*        CLC   =C'CHA',BUYACT     CHANGE?                                       
*        BE    CHGBUY                                                           
*        CLC   BUYACT(3),=C'CHX'   SPECIAL NO BUCKET ACTION                     
*        BE    CHGBUY                                                           
         EJECT                                                                  
         MVI   RBUYLEN+1,77        ELEM LENGTH (34 + 43)                        
         MVC   RBUYCODE(2),=X'012B'     BUY DESC ELEM CODE + LEN                
*                                                                               
*              EDIT DAYS AND TIMES                                              
DAYED    DS    0H                                                               
         GOTO1 =A(GETFLD),DMCB,1,RR=Y   DAY FIELD                               
         LA    R3,1                                                             
         ICM   R2,15,0(R1)                                                      
         BZ    BUYERROR                                                         
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         ST    R2,ADAYFLD                                                       
*              DELETE ALL DAY-TIME ELEMENTS                                     
         GOTO1 VDELELEM,DMCB,(2,RBUYREC)                                        
*                                                                               
         MVC   WORK2(2),=X'0209'   ELEM CODE + LENGTH                           
         MVI   WORK2+8,1           WEIGHTING FACTOR                             
*                                                                               
*              PREPARE TO EDIT STRING OF DAY-TIME FIELDS IN PARALLEL            
         LA    R4,7(R2)                                                         
         LR    R5,R2                                                            
         GOTO1 =A(GETFLD),DMCB,2,RR=Y  TIME FIELD                               
         LA    R3,1                                                             
         ICM   R2,15,0(R1)                                                      
         BZ    BUYERROR                                                         
         ST    R2,ATIMFLD                                                       
         DROP  R7                                                               
         LR    R7,R2                                                            
         LA    R6,7(R2)                                                         
*                                                                               
         STM   R4,R7,DMCB+8        PARAMETERS FOR SCAN                          
         SR    R6,R6                                                            
         SR    R3,R3               START DAY FOR ALL 02 ELEMENTS                
         SR    R7,R7               END DAY                                      
DAYTIMED DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         L     R2,ADAYFLD                                                       
         DROP  RF                                                               
         GOTO1 SCAN,DMCB+8         SCAN DAY FIELD TO GET LENGTH                 
         XC    WORK2+2(6),WORK2+2                                               
         CLI   DMCB+8,0            NO DAY ENTRY?                                
         BNE   DAYTIM50                                                         
*              NO DAY LENGTH                                                    
         LTR   R6,R6               ANY DAYS?                                    
         BNZ   *+12                                                             
         LA    R3,DAYERR                                                        
         B     BUYERROR                                                         
         CLI   DMCB+20,C'*'        ANOTHER TIME ENTRY?                          
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     BUYERROR                                                         
* GET START AND END DAYS FOR ALL 02 ELEMENTS                                    
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         STC   R3,RBUYSTED                                                      
*                                                                               
         B     LENEDIT             EDIT NEXT FIELD                              
DAYTIM50 MVC   DMCB2(4),DMCB+8      DAY FIELD ADDR + LEN                        
*                                                                               
*              EDIT DAY FIELD                                                   
         GOTO1 =V(REDAYVAL),DMCB2,,WORK2+3,WORK2+2,RR=YES                       
*                                                                               
         CLI   WORK2+3,0           VALID DAY?                                   
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     BUYERROR                                                         
*                                                                               
         LA    R6,1(R6)            COUNTER                                      
*                                                                               
*              GET FIRST START DAY AND LAST END DAY                             
         SR    R4,R4               START                                        
         SR    R5,R5               END                                          
         IC    R4,WORK2+2          START-END                                    
         SRDL  R4,4                                                             
         SRL   R5,28               END                                          
         LTR   R3,R3               FIRST 02 ELEMENT?                            
         BNZ   *+6                                                              
         LR    R3,R4               FIRST START DAY IS KEY                       
         CR    R4,R5               START V END                                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CR    R3,R4               CHECK AGAINST 1ST START DAY                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CH    R5,=H'8'            END DAY                                      
         BL    DAY100                                                           
* MAKE SURE NO MORE THAN 7 DAYS COVERED                                         
         LA    R1,7(R3)                                                         
         CR    R1,R5                                                            
         BH    *+12                                                             
         LA    R3,DAYERR                                                        
         B     BUYERROR            MORE THAN 7 DAYS                             
DAY100   CR    R5,R7               END                                          
         BNH   *+6                                                              
         LR    R7,R5               NEW HIGH END                                 
*                                                                               
*              EDIT TIME FIELD                                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         L     R2,ATIMFLD                                                       
         DROP  RF                                                               
*                                                                               
         GOTO1 SCAN,DMCB+16        SCAN NEXT TIME FIELD FOR LENGTH              
*                                                                               
         CLI   DMCB+16,0           NO TIME ENTRY?                               
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     BUYERROR                                                         
*                                                                               
         ZIC   R4,DMCB+16          ALLOW INPUT OF 'VARIOUS'                     
         L     R5,DMCB+16                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'VARIOUS'                                              
         BNE   DAY140                                                           
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     BUYERROR                                                         
         MVC   WORK2+4(4),=C'VARY'                                              
         B     DAY200                                                           
*                                                                               
DAY140   EX    R4,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   0(0,R5),=C'NONE'                                                 
         BNE   DAY150              OR GO TO RETIMVAL                            
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     BUYERROR                                                         
         MVC   WORK2+4(4),=C'NONE'                                              
         B     DAY200                                                           
*                                                                               
DAY150   MVC   DMCB(4),DMCB+16     TIME LEN + ADDR                              
         LA    RF,WORK2+4                                                       
         ST    RF,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY155              6AM - 559AM                                  
         DROP  RF                                                               
         MVI   DMCB+4,X'80'                                                     
*                                                                               
DAY155   DS    0H                                                               
         GOTO1 =V(RETIMVAL),DMCB,,,RR=YES  EDIT TIME                            
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     BUYERROR                                                         
*                                                                               
         OC    WORK2+6(2),WORK2+6  IS THERE AN END TIME?                        
         BZ    DAY200              NO                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         B     DAY200              Bypass AM validation                         
*                                                                               
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAY160              6AM - 559AM                                  
         DROP  RF                                                               
*                                                                               
         CLC   WORK2+4(2),=H'0600' START TIME LT 6AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0600' END TIME GT = 6AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     BUYERROR                                                         
*                                                                               
DAY160   DS    0H                                                               
         CLC   WORK2+4(2),=H'0500' START TIME LT 5AM?                           
         BNL   DAY200              NO                                           
         CLC   =C'CC',WORK2+6      TO CONCLUSION END TIME?                      
         BE    DAY200              YES                                          
         CLC   WORK2+6(2),=H'0500' END TIME GT = 5AM?                           
         BL    DAY200              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     BUYERROR                                                         
*              ADD DAY-TIME ELEMENT TO BUYREC                                   
DAY200   DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        CHECK IF ELECTRONIC CONTRACT                 
         BNE   DAY230                                                           
         DROP  RF                                                               
DAY210   DS    0H                                                               
         BAS   RE,VCROSS           VALIDATE CROSS DAY                           
                                                                                
DAY230   DS    0H                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         B     DAYTIMED            EDIT NEXT DAY TIME FIELD COMBO               
         EJECT                                                                  
********************************************************************            
* FOR BIAS ELECTRONIC CONTRACT, VALIDATE CROSS DAY.  CROSS DAY MUST             
*   HAVE AT LEAST ONE DAY OPEN                                                  
********************************************************************            
VCROSS   NTR1                                                                   
         ZIC   RF,WORK2+2          START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,WORK2+3                                                       
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
                                                                                
         LA    R6,RCONREC          NOW CHECK IF BIAS ELEMENT EXISTS             
         MVI   ELCODE,X'13'        IN CONTRACT                                  
         BAS   RE,GETEL                                                         
         BNE   VCROSS30            ELEMENT HASN'T BEEN ADDED YET                
         USING RCONCCEL,R6                                                      
                                                                                
         LA    R3,399              CANNOT CROSS CROSS DAY DEFAULT               
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    VCROSS10                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         L     R2,ADAYFLD                                                       
         B     BUYERROR            CANNOT CROSS CROSS DAY DEFAULT               
         DROP  RF                                                               
VCROSS10 DS    0H                                                               
         OC    WORK(1),RCONCCCD    COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK,X'7F'          ERROR IF ALL ON                              
         BNO   VCROSS20                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         L     R2,ADAYFLD                                                       
         B     BUYERROR                                                         
         DROP  RF                                                               
VCROSS20 DS    0H                                                               
         MVC   RCONCCCD,WORK       UPDATE NEW CROSS DAY                         
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     VCROSSX                                                          
         DROP  R6                                                               
                                                                                
VCROSS30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
         ZIC   RF,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL2                                                
         STC   RF,RCONCCCD         DAYS IN CROSS DAY FOR THIS BUY               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
VCROSSX  DS    0H                                                               
         B     EXXMOD                                                           
         TITLE 'T80115 - REPPAK BUY LENGTH EDIT'                                
*              EDIT LENGTH                                                      
LENEDIT  DS    0H                                                               
         LA    R3,LENERR                                                        
         GOTO1 =A(GETFLD),DMCB,3,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    BUYERROR                                                         
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VPACK               LENGTH                                       
         LTR   R0,R0                                                            
         BZ    LENE0060                                                         
* VALID SECONDS                                                                 
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0020            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0030            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0020 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0040                                                         
LENE0030 EQU   *                                                                
         CH    R0,=H'120'                                                       
         BNH   LENE0040                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     BUYERROR                                                         
*                                                                               
LENE0040 STH   R0,HALF                                                          
         MVC   RBUYDUR,HALF                                                     
         B     BUYCDEDT                                                         
* TEST FOR MINUTES                                                              
LENE0060 LA    R4,4                                                             
         LA    R5,8(R2)                                                         
*                                                                               
LENE0080 CLI   0(R5),C'M'          MINUTES?                                     
         BE    LENE0100                                                         
         CLI   0(R5),X'F0'                                                      
         BL    BUYERROR                                                         
         CLI   0(R5),X'F9'                                                      
         BH    BUYERROR                                                         
         LA    R5,1(R5)                                                         
         BCT   R4,LENE0080                                                      
         B     BUYERROR                                                         
* PACK MINUTES (MINUTES NOT ALLOWED FOR SPOTPAK XFER)                           
LENE0100 DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    LENE0110            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    LENE0115            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
LENE0110 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0120                                                         
LENE0115 EQU   *                                                                
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     BUYERROR                                                         
*                                                                               
LENE0120 DS    0H                                                               
         LA    R6,4                                                             
         SR    R6,R4                                                            
         BNP   BUYERROR                                                         
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   RBUYDUR,HALF                                                     
         OI    RBUYDUR,X'80'       MINUTES IND                                  
*                                                                               
BUYCDEDT EQU   *                                                                
         GOTO1 =A(BUYCDX),DMCB,(RC),RR=Y                                        
         BZ    BUYC0020            NO ERROR FOUND                               
         L     R2,DUB              SET A(FIELD IN ERROR)                        
         LA    R3,909              INVALID BUYCODE ERROR MESSAGE                
         B     BUYERROR                                                         
BUYC0020 EQU   *                                                                
                                                                                
         TITLE 'T80115 - REPPAK FLIGHT CODE EDIT'                               
*              EDIT FLIGHT CODE                                                 
FLTEDIT  TM    TWASTREO,X'80'      IS STEREO IN USE?                            
         BZ    DATEDIT             NO, SKIP AROUND                              
*&&DO                                                                           
         LA    R3,1                                                             
         GOTO1 =A(GETFLD),DMCB,4,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    BUYERROR                                                         
***      LA    R2,BUYFLTH                                                       
         LA    R3,INVINP                                                        
*                                                                               
         ZIC   R6,5(R2)            NUM OF INPUT CHARS                           
         LTR   R6,R6               IS IT 0?                                     
         BZ    FLT100              YES                                          
*                                                                               
         TM    4(R2),X'08'     IS IT VALID NUMERIC?                             
         BZ    BUYERROR            NO                                           
*                                                                               
         BCTR  R6,0                GET BINARY OF NUMBER                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R6,DUB                                                           
*                                                                               
FLT100   STC   R6,RBUYFLT          PUT NUMBER INTO RECORD                       
*&&                                                                             
         TITLE 'T80115 - REPPAK EFFECTIVE DATES EDIT'                           
*              VALIDATE DATES                                                   
DATEDIT  DS    0H                                                               
         LA    R3,1                                                             
         GOTO1 =A(GETFLD),DMCB,4,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    BUYERROR                                                         
*                                                                               
* SET UP 2 DATE INPUT FIELDS IN WORK3(PRETEND 1 FIELD)                          
         XC    WORK3,WORK3                                                      
         ZIC   RE,5(R2)            LEN OF FIELD 1                               
         LA    RE,7(RE)                                                         
         EX    RE,*+4                                                           
         MVC   WORK3(0),0(R2)                                                   
         LA    R4,WORK3+8(RE)      END OF FIELD 1                               
*                                                                               
*              DELETE ALL PREVIOUS DATE ELEMENTS (CHANGE)                       
DATE30   DS    0H                                                               
**       CLC   =C'CR=',WORK3+8     LOOK FOR SPECIAL CHARS                       
**       BNE   DATE40                                                           
**       GOTO1 =A(CREDIT),DMCB,(RC),RR=Y                                        
**       B     BUYED2                                                           
*                                                                               
* +/- WEEK(S) TO CURRENT DATES?                                                 
DATE40   DS    0H                                                               
***      GOTO1 =A(DTADDSUB),DMCB,(RC),RR=Y                                      
*                                                                               
***      GOTO1 =A(SAVEMG03),DMCB,(RC),RR=Y                                      
*                                                                               
         GOTO1 VDELELEM,DMCB,(3,RBUYREC)                                        
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
*                                                                               
         MVC   WORK2(2),=X'030B'   DATE ELEM CODE + LEN                         
*              GET LOWEST START DAY AND HIGHEST END DAY IN DAY ELEMENTS         
         SR    R5,R5                                                            
         ZIC   R4,RBUYSTED                                                      
         SRDL  R4,4                                                             
         SRL   R5,28                                                            
         STM   R4,R5,DMCB+16                                                    
*                                                                               
         XC    WORK+24(6),WORK+24  FOR CONSECUTIVE TEST                         
         LA    R7,WORK3+7        FOR SCAN                                       
         ST    R7,DMCB+12                                                       
*              EDIT START DATE                                                  
STARTED  MVC   DMCB(4),DMCB+12                                                  
         LA    R3,SDTERR                                                        
         GOTO1 SCAN,DMCB,,WORK3     SCAN FOR NEXT DATE FIELD                    
*                                                                               
         CLI   DMCB,0              NONE?                                        
         BNE   DATE50                                                           
*              NO DATE                                                          
         OC    WORK+24(6),WORK+24  NO DATES GIVEN?                              
         BZ    BUYERROR                                                         
         B     DATE240                                                          
DATE50   L     R5,DMCB             FIELD ADDR                                   
         MVC   DMCB+12(4),DMCB     SAVE LEN + ADDR FOR SCAN                     
         CLC   0(2,R5),=C'S-'      K START INDICATOR                            
         BNE   DATE75                                                           
*                                                                               
*              DETERMINE DATE IN FIRST WEEK OF CONTRACT                         
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
*                                                                               
         CLC   FULL(3),MYSPACES    VALID K START DATE?                          
         BNE   *+6                                                              
         DC    H'0'                K ERROR                                      
*                                                                               
         ZIC   RE,DMCB             DAY OF WEEK                                  
         L     R4,DMCB+16          BUY START DAY                                
         SR    R4,RE                                                            
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            NEXT WEEK                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4) GET 4ST DATE                        
*                                                                               
         LA    R5,2(R5)            NEXT FIELD                                   
*                                                                               
         B     DATE150                                                          
DATE75   EQU   *                                                                
*                                                                               
*              EDIT START DATE                                                  
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+12                                     
*                                                                               
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    BUYERROR                                                         
*                                                                               
         LA    R5,1(R7,R5)         NEXT FIELD                                   
         MVC   WORK+12(2),WORK     K START YEAR                                 
         CLC   WORK+14(4),WORK+2   BUY MMDD V K MMDD                            
         BNL   DATE100                                                          
*              BUY MMDD LOW                                                     
DATE90   CLC   WORK(2),WORK+6      K START AND END YEARS SAME?                  
         BE    BUYERROR                                                         
         MVC   WORK+12(2),WORK+6   USE K END YEAR                               
DATE100  GOTO1 GETDAY,DMCB,WORK+12,FULL VALIDATE START DATE                     
*                                                                               
         CLC   FULL(3),MYSPACES                                                 
         BE    BUYERROR                                                         
         LA    R3,SDYERR                                                        
         ZIC   R4,DMCB             START DAY                                    
         C     R4,DMCB+16          SAME AS 1ST DAY?                             
         BE    DATE150                                                          
* TEST FOR FLIGHT BUY WHERE WRONG START DAY OK TO EASE INPUT                    
***      CLC   BUYBACT(4),=C'BUYF'                                              
***      BE    DATE110                                                          
         SPACE 1                                                                
         CLC   WORK+12(2),WORK+6   BUY YEAR SAME AS END YEAR                    
         BE    BUYERROR                                                         
         B     DATE90              FOR CONTRACTS MORE THAN 1 CALENDER           
* GET CORRECT START DAY                                                         
DATE110  L     R7,DMCB+16          DAY FIELD DAY NO.                            
*                                  YEAR - BUY DATE COULD BE SECOND YEAR         
* GET K START DAY - DEPENDS ON WHETHER K HAS OUT-OF-WEEK DATES                  
         GOTO1 GETDAY,(R1),WORK,FULL                                            
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB             K START DAY                                  
*                                                                               
*              DETERMINE END DAY FROM K END DATE                                
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES    ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB,5              IF END DAY IS A FRIDAY                       
         BE    *+12                                                             
         CLI   DMCB,7              OR IF IT IS A SUNDAY                         
         BNE   *+8                                                              
         LA    R6,1                MAKE WEEK NORMAL FLIGHT WEEK (HAVE           
*                                     IT START ON MONDAY)                       
         LR    R3,R7                                                            
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
         CR    R3,R6               DAY V K START DAY                            
         BL    DATE125                                                          
         CR    R4,R6               DATE DAY V K START DAY                       
         BNL   DATE140                                                          
         SH    R7,=H'7'            PREVIOUS WEEK                                
         B     DATE140                                                          
*                                                                               
DATE125  CR    R4,R6               DATE DAY V K START DAY                       
         BL    DATE140                                                          
         AH    R7,=H'7'            NEXT WEEK                                    
* GET PROPER DATE IN WEEK                                                       
DATE140  GOTO1 ADDAY,(R1),WORK+12,DUB,(R7)                                      
         MVC   WORK+12(6),DUB                                                   
         CLC   WORK+12(6),WORK     DATE V K START DATE                          
         BNL   *+12                                                             
         LA    R3,SDYERR                                                        
         B     BUYERROR            JUST IN CASE                                 
DATE150  XC    WORK2+8(2),WORK2+8                                               
         CLC   WORK+12(6),WORK+6   BUY START V K END                            
         BNH   *+12                                                             
         LA    R3,SDTERR                                                        
         B     BUYERROR                                                         
         CLC   WORK+12(6),WORK+24  BUY START DATE V LAST ELEM END DATE          
         BH    *+12                                                             
         LA    R3,SDTERR                                                        
         B     BUYERROR                                                         
*              EDIT END DATE                                                    
*              END DATE                                                         
         CLI   0(R5),C'E'          -E INDICATOR?                                
         BNE   DATE175                                                          
*              DETERMINE END DATE FROM K END DATE                               
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES    ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,DMCB             K END DAY                                    
*                                                                               
         S     R4,DMCB+20          BUY END DAY                                  
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            PREVIOUS WEEK                                
         LNR   R4,R4                                                            
*              BACK UP K END DATE TO LAST BUY DATE                              
         GOTO1 ADDAY,DMCB,WORK+6,WORK+18,(R4)                                   
*                                                                               
         LA    R5,1(R5)                                                         
         B     DATE200                                                          
DATE175  EQU   *                   EDIT END DATE                                
         CLI   0(R5),C'*'          NO END DATE?                                 
         BE    DATE177                                                          
         LR    R6,R5                                                            
         BCTR  R6,R0                                                            
         CLI   0(R6),C'*'                                                       
         BE    DATE176                                                          
         CLI   0(R6),C'('                                                       
         BE    DATE176                                                          
         CLI   0(R6),0                                                          
         BE    DATE176                                                          
         CLI   0(R6),C' '                                                       
         BNE   DATE180                                                          
* NO END DATE GIVEN                                                             
DATE176  LR    R5,R6                                                            
DATE177  LM    R6,R7,DMCB+16       START AND END DAYS                           
         CR    R6,R7               END IN NEXT WEEK?                            
         BNH   *+8                                                              
         LA    R7,7(R7)                                                         
         SR    R7,R6                                                            
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(R7)                                  
         B     DATE210                                                          
*                                                                               
DATE180  GOTO1 DATVAL,DMCB,(1,(R5)),WORK+18 END DATE                            
*                                                                               
         L     RE,DMCB             LENGTH                                       
         LTR   RE,RE                                                            
         BNZ   DATE199A                                                         
*                                                                               
* CHECK FOR END WEEKS OPTION                                                    
         LA    R4,1                                                             
         CLI   1(R5),C'W'          WEEKS IND?                                   
         BE    DATE193                                                          
         LA    R4,2                                                             
         CLI   2(R5),C'W'                                                       
         BE    *+12                                                             
         LA    R3,EDTERR                                                        
         B     BUYERROR                                                         
* W HAS BEEN ENTERED - PACK WEEKS                                               
DATE193  LR    R7,R5                                                            
         LR    R3,R5                                                            
         LA    R5,1(R4,R5)         END OF FIELD                                 
         LR    R6,R4                                                            
*                                                                               
DATE195  CLI   0(R7),X'F0'         NUMERIC?                                     
         BNL   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BUYERROR                                                         
         CLI   0(R7),X'F9'                                                      
         BNH   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BUYERROR                                                         
         LA    R7,1(R7)                                                         
         BCT   R4,DATE195                                                       
* NUMERIC WEEKS ENTERED                                                         
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R3)                                                      
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BUYERROR                                                         
         MVC   WORK+30(6),WORK+12   START DATE                                  
         MVC   WORK+18(6),WORK+12  START TO END                                 
         OI    WORK2+8,X'80'       EVERY WEEK                                   
         LA    R3,7                                                             
         BCTR  R4,R0               NUMBER OF WEEKS                              
         LTR   R4,R4                                                            
         BZ    DATE199                                                          
*                                                                               
* TEST FOR ALTERNATE WEEKS                                                      
         CLI   0(R5),C'A'                                                       
         BNE   DATE198                                                          
         OI    WORK2+8,X'40'                                                    
         NI    WORK2+8,X'7F'                                                    
         LA    R5,1(R5)                                                         
         LA    R3,14                                                            
* GET NEXT WEEK                                                                 
DATE198  GOTO1 ADDAY,DMCB,WORK+30,WORK+18,(R3)                                  
         MVC   WORK+30(6),WORK+18                                               
         BCT   R4,DATE198          GET NUMBER OF WEEKS-1                        
*                                                                               
* GET DAY SPAN FOR WEEK                                                         
DATE199  L     R6,DMCB+20          END DAY OF WEEK                              
         C     R6,DMCB+16          END V START DAY                              
         BNL   *+8                                                              
         LA    R6,7(R6)            OUT OF WEEK ROTATOR                          
         S     R6,DMCB+16          GET DAY SPAN                                 
         GOTO1 ADDAY,(R1),WORK+30,WORK+18,(R6)                                  
         B     DATE215                                                          
*                                                                               
* END DATE IS VALID MONTH-DAY                                                   
DATE199A MVC   WORK+18(2),WORK+6   K END YEAR                                   
         CLC   WORK+20(4),WORK+8   BUY END MMDD V K END MMDD                    
         BNH   *+10                                                             
         MVC   WORK+18(2),WORK     MOVE K START YEAR                            
*                                                                               
         LA    R5,0(RE,R5)         FIELD END                                    
*                                                                               
*              VALIDATE END DATE                                                
         GOTO1 GETDAY,DMCB,WORK+18,FULL                                         
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+12                                                             
         LA    R3,EDTERR                                                        
         B     BUYERROR                                                         
*                                                                               
         ZIC   R4,DMCB             DAY OF WEEK                                  
*                                                                               
         C     R4,DMCB+20          SAME DAY AS END DAY?                         
         BE    DATE200                                                          
***      CLC   BUYBACT(4),=C'BUYF'                                              
***      BE    *+12                                                             
         LA    R3,EDYERR                                                        
         B     BUYERROR                                                         
* FLIGHT BUY NEED NOT HAVE PROPER END DATE - FIND WEEK AND ADJUST DATE          
         L     R7,DMCB+20          END DAY                                      
* GET K END DAY                                                                 
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB                                                          
         LR    R3,R7                                                            
*                                                                               
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
DATE199G GOTO1 ADDAY,(R1),WORK+18,DUB,(R7)                                      
         MVC   WORK+18(6),DUB                                                   
*                                                                               
         CLC   WORK+18(6),WORK+6   DATE V K END                                 
         BNH   *+12                                                             
         LA    R3,EDYERR                                                        
         B     BUYERROR            JUST IN CASE                                 
*                                                                               
DATE200  CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DATE210                                                          
         LA    R5,1(R5)                                                         
         OI    WORK2+8,X'40'                                                    
         B     *+8                                                              
DATE210  OI    WORK2+8,X'80'       EVERY WEEK                                   
DATE215  LA    R3,EDTERR                                                        
         CLC   WORK+18(6),WORK+6   BUY END V K END DATE                         
         BH    BUYERROR                                                         
*                                                                               
         CLC   WORK+18(6),WORK+12  BY END V BUY START                           
         BL    BUYERROR                                                         
*                                                                               
         MVC   WORK+24(6),WORK+18  SAVE BUY END DATE FOR CONSECUTIVE            
*                                  TEST                                         
* SEE IF NO. PER WEEK GIVEN                                                     
         CLI   0(R5),C'('                                                       
         BNE   DATE230                                                          
* GET NPW OVERRIDE - FORMAT IS JAN15(4) JAN15-JAN26(5) JAN5-E(6)                
*                           OR JAN15-8W(3)                                      
         LA    R3,NPWERR                                                        
         SR    R1,R1                                                            
         LA    R5,1(R5)                                                         
         LR    RF,R5                                                            
* CHECK NUMBER PER WEEK                                                         
DATE220  CLI   0(R5),X'F0'                                                      
         BL    BUYERROR                                                         
         CLI   0(R5),X'F9'                                                      
         BH    BUYERROR                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=H'3'                                                         
         BH    BUYERROR                                                         
         CLI   0(R5),C')'                                                       
         BNE   DATE220                                                          
*                                                                               
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'                                                       
         BH    BUYERROR                                                         
*                                                                               
         OI    WORK2+8,X'01'       NPW OVERRIDE INDICATOR                       
         STC   R1,WORK2+9          NPW                                          
* NOW GET TOTAL WEEKS                                                           
DATE230  SR    R7,R7               CTR                                          
         MVC   WORK+30(6),WORK+12  START                                        
*                                                                               
         LA    R3,7                                                             
         TM    WORK2+8,X'40'       ALT?                                         
         BZ    *+8                                                              
         LA    R3,14                                                            
DATE235  LA    R7,1(R7)                                                         
         GOTO1 ADDAY,DMCB,WORK+30,WORK+36,(R3)                                  
         MVC   WORK+30(6),WORK+36                                               
*                                                                               
         CLC   WORK+30(6),WORK+18  PAST END?                                    
         BNH   DATE235                                                          
*                                                                               
         STC   R7,WORK2+10                                                      
*              CONVERT DATES FOR BUYREC                                         
         GOTO1 DATCON,DMCB,WORK+12,(3,WORK2+2)    START DATE                    
*                                                                               
         GOTO1 (RF),(R1),WORK+18,(3,WORK2+5)      END DATE                      
*                                                                               
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2  ADD DATE ELEM TO BUYREC             
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         B     STARTED             SEE IF ANOTHER DATE ENTRY                    
DATE240  EQU   *                                                                
***      GOTO1 =A(CHKMGDS),DMCB,(RC),RR=Y                                       
***      BZ    BUYED2              ALL MAKEGOODS OKAY                           
***      LA    R3,MG1ERR           MAKEGOOD ERROR: SEND MESSAGE                 
***      B     BUYERROR                                                         
         EJECT                                                                  
BUYED2   DS    0H                  THIS WAS CNT16                               
         MVI   BSTATUS,0           CLEAR OUT STATUS BYTE                        
*                                                                               
*  FOR STATION, DEAL WITH ORDER COMMMENT                                        
*                                                                               
*&&DO                                                                           
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   ADDSPOT                                                          
         SPACE 1                                                                
         LA    R4,2                BCT THROUGH 2 COMMENT LINES                  
         LA    R2,BUYORDCH                                                      
         TM    4(R2),X'20'         HAS 1ST LINE CHANGED                         
         BZ    STA5                                                             
         LA    R2,BUYORD2H                                                      
         TM    4(R2),X'20'         HAS 2ND LINE CHANGED                         
         BO    EXXMOD              NEITHER CHANGED, GET OUT                     
         LA    R2,BUYORDCH         OTHERWISE, REDO BOTH LINES                   
         SPACE 1                                                                
STA5     OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VDELELEM,DMCB,(X'84',RBUYREC)  YES, DELETE OLD                   
         SPACE 1                                                                
         CLI   5(R2),0            IF EITHER COMMENT LINE IS                     
         BNE   STA7                BLANK, DON'T ADD BLANK ELEMENT               
         LA    R2,BUYORD2H                                                      
         LA    R4,1                ONLY BCT ONCE                                
         CLI   5(R2),0                                                          
         BE    STA10                                                            
         SPACE 1                                                                
STA7     CLC   8(3,R2),=C'#DS'     STATION NOT ALLOWED TO USE                   
         BNE   *+12                'DON'T SEND' FEATURE                         
         LA    R3,2                INVALID INPUT FLD                            
         B     BUYERROR                                                         
         XC    WORK2(100),WORK2    AND REBUILD NEW ELEMENT                      
         MVI   WORK2,X'84'                                                      
*   WORK2+2 IS ZERO TO DESIGNATE STATION BUY ORDER COMMENT                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+3(0),8(R2)                                                 
         LA    RE,4(RE)            TOTAL LENGTH                                 
         STC   RE,WORK2+1                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         SPACE 1                                                                
         LA    R2,BUYORD2H         LOOK AT 2ND COMMENT LINE                     
         CLI   5(R2),0                                                          
         BE    STA10                                                            
         BCT   R4,STA7                                                          
         SPACE 1                                                                
* UNCONFIRM CONTRACT AND UPDATE VERSION NUMBER IF NECESSARY                     
*                                                                               
STA10    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THERE SHOULD ALREADY BE A '1F'               
                                                                                
         USING RCONXEL,R6                                                       
         OI    RCONCONF,X'80'      TURN ON NOT CONFIRMED                        
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    STA11                                                            
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'       TURN ON CONFIRMED PREVIOUSLY                
         DROP  R6                                                               
                                                                                
STA11    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    STA12                                                            
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
                                                                                
STA12    DS    0H                                                               
         TM    RCONSENF,X'10'      ON MEANS STA VERSION NOT ADVANCED            
         BZ    STA20                                                            
*                                                                               
* ADVANCE STATION VERSION AND UPDATE VERSION DATES                              
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    STA15                                                            
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
STA15    DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'S',RCONREC)                                     
         BNZ   BUYERROR                                                         
                                                                                
STA20    DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSSV          STORE VERSION NUMBER IN BUY                  
         DROP  R6                                                               
         STC   R1,RBUYVER                                                       
         OI    TAREQ,X'01'         T/A REQ INDICATOR- TO PUT CONTRACT           
         MVI   BYTE,C'O'           UPDATE BUY CHANGE INDICATOR                  
         BAS   RE,ADDCODE                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
*&&                                                                             
ADDSPOT  DS    0H                                                               
*                                                                               
         LA    R3,265                                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    ADDSP000            NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    ADDSP010            YES - TREAT AS RTS BUY                       
         DROP  RF                                                               
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
ADDSP000 EQU   *                                                                
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   TYPEDIT                                                          
*                                                                               
ADDSP010 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'        X'08' ELEMENT MUST BE UNIQUE                 
         BAS   RE,GETEL                                                         
         BE    TYPEDIT                                                          
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWASPES,0                                                        
         BNE   ADDSP10                                                          
         DROP  RF                                                               
                                                                                
* PROFILE TO ALLOW SPOTPAK INTERFACE DATA                                       
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    TYPEDIT             IF OFF, SKIP SPOTPAK INTERFACE ADD           
         B     BUYERROR                                                         
                                                                                
ADDSP10  DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
WK2      USING RBUYSPEL,WORK2                                                   
         MVC   WK2.RBUYSPCD(2),=X'0830'                                         
         MVC   WK2.RBUYSPAG,TWASPAG    SPOTPAK AGENCY POWER CODE                
         MVC   WK2.RBUYSPMD,TWASPMD    SPOTPAK MEDIA CODE                       
         MVC   WK2.RBUYSPCL,TWASPCL    SPOTPAK CLIENT CODE                      
         MVC   WK2.RBUYSPPD,TWASPPD    SPOTPAK PRODUCT CODE                     
         MVC   WK2.RBUYSPES,TWASPES    SPOTPAK ESTIMATE NUMBER                  
         MVC   WK2.RBUYSPPP,TWASPPP    SPOTPAK PIGGY PRODUCT CODE               
         MVC   WK2.RBUYSPP1,TWASPP1    SPOTPAK PRODUCT 1 SPLIT                  
         MVC   WK2.RBUYSPP2,TWASPP2    SPOTPAK PRODUCT 2 SPLIT                  
         MVC   WK2.RBUYSPST,RCONKSTA   STATION CALL LETTERS                     
         MVC   WK2.RBUYSADV,RCONKADV   REPPAK ADVERTISER CODE                   
         MVC   WK2.RBUYSPRD,RCONPRD    REPPAK PRODUCT CODE                      
         DROP  WK2                                                              
         DROP  RF                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2   ADD SPOTPAK INTERFACE ELEM         
         B     TYPEDIT                                                          
*                                                                               
TYPEDIT  DS    0H                  PROGRAM TYPE                                 
*        LA    R2,BUYTYPH          PROGRAM TYPE                                 
         GOTO1 =A(GETFLD),DMCB,7,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    DPTEDIT                                                          
         MVI   RBUYTYP,0                                                        
         CLI   5(R2),0             TYPE FIELD IS OPTIONAL                       
         BE    DPTEDIT                                                          
         SPACE 2                                                                
         LA    R3,58               TYPE MUST BE 1 CHARACTER                     
         CLI   5(R2),1                                                          
         BH    BUYERROR                                                         
         SPACE 1                                                                
         XC    KEY,KEY             VERIFY PROGRAM TYPE EXISTS                   
         MVI   KEY,X'25'                                                        
         MVC   KEY+24(2),REPALPHA                                               
         MVC   KEY+26(1),8(R2)                                                  
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+12                                                             
         LA    R3,2                INVALID INPUT                                
         B     BUYERROR                                                         
         SPACE 1                                                                
         GOTO1 VMOVE                                                            
         MVC   RBUYTYP,WORK                                                     
*              VALIDATE DAYPART                                                 
DPTEDIT  DS    0H                  DAYPART                                      
*        LA    R2,BUYDPTH          DAYPART                                      
         XC    RBUYDPT,RBUYDPT                                                  
         GOTO1 =A(GETFLD),DMCB,8,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    DPTEDIT6                                                         
         CLI   5(R2),0             FIELD IS OPTIONAL                            
         BE    DPTEDIT5                                                         
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    RE,TWADAYPT                                                      
         LA    R3,L'TWADAYPT       MAX 36 CODES TO CHECK                        
         CLI   0(RE),0                                                          
         BNE   DPTEDIT3                                                         
         LA    RE,DPTTAB                                                        
         LA    R3,L'DPTTAB                                                      
         B     DPTEDIT3                                                         
         DROP  RF                                                               
DPTTAB   DC    CL19'MDERATLWKFNPVSJOXYZ'                                        
*                                                                               
DPTEDIT3 CLC   8(1,R2),0(RE)                                                    
         BE    DPTEDIT5                                                         
         LA    RE,1(RE)                                                         
         BCT   R3,DPTEDIT3                                                      
         LA    R3,2                INVALID INPUT                                
         B     BUYERROR                                                         
*                                                                               
DPTEDIT5 DS    0H                                                               
         GOTO1 VMOVE                                                            
         MVC   RBUYDPT,WORK                                                     
DPTEDIT6 DS    0H                                                               
*                                                                               
         TM    TWAFLAGS,TWAFLPTQ      PROFILE FOR PATTERN?                      
         BO    CLSEDIT2               YES                                       
*                                                                               
*              VALIDATE CLASS                                                   
CLSEDIT  DS    0H                                                               
***      LA    R2,BUYCLSH          CLASS                                        
         XC    RBUYCLS,RBUYCLS                                                  
         GOTO1 =A(GETFLD),DMCB,9,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    PLNEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         GOTO1 VMOVE                                                            
         MVC   RBUYCLS,WORK                                                     
         B     PLNEDIT                                                          
*                                                                               
CLSEDIT2 DS    0H                  EDIT PATTERN/NOTATION FIELDS                 
         GOTO1 =A(PTNEDIT),DMCB,(RC),RR=Y                                       
*                                  VALIDATE PATTERN/NOTATION                    
         GOTO1 =A(UPTEDIT),DMCB,(RC),RR=Y                                       
*                                  VALIDATE 'USE PATTERN TIMES'                 
PLNEDIT  DS    0H                                                               
         GOTO1 =A(SECEDIT),DMCB,(RC),RR=Y   VALIDATE SECTION                    
*                                                                               
***      LA    R2,BUYPLNH          PLAN                                         
         GOTO1 =A(GETFLD),DMCB,12,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    PLNEDIT2                                                         
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R3,PLNERR           PLAN ERROR                                   
         TM    4(R2),X'0C'         MUST BE ALL ALPHA OR ALL NUMERIC             
         BC    0,BUYERROR                                                       
         CLI   5(R2),3             LEN                                          
         BH    BUYERROR                                                         
         GOTO1 VMOVE                                                            
         MVC   RBUYKPLN,WORK                                                    
         CLC   RBUYKPLN,=3C' '     ANY PACKAGE?                                 
         BNE   *+10                                                             
PLNEDIT2 MVC   RBUYKPLN,=3X'FF'    FOR SORT                                     
         SPACE 3                                                                
NPWEDIT  DS    0H                  NUMBER PER WEEK                              
***      LA    R2,BUYNPWH          NUMBER PER WEEK                              
         LA    R3,1                                                             
         GOTO1 =A(GETFLD),DMCB,5,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    BUYERROR                                                         
*        CLC   BUYCOM1(3),=C'CR='                                               
*        BNE   *+8                                                              
*        NI    4(R2),X'DF'                                                      
         SPACE 1                                                                
*                                                                               
*        TM    4(R2),X'20'                                                      
*        BO    RATEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R3,NPWERR                                                        
*                                                                               
         GOTO1 VPACK                                                            
         BNZ   *+12                                                             
         TM    4(R2),X'08'                                                      
         BZ    BUYERROR                                                         
         CH    R0,=H'255'                                                       
         BH    BUYERROR                                                         
         STC   R0,RBUYNW                                                        
         EJECT                                                                  
RATEDIT  DS    0H                  RATE EDIT                                    
***      LA    R2,BUYRATEH         RATE EDIT                                    
         LA    R3,1                                                             
         GOTO1 =A(GETFLD),DMCB,6,RR=Y                                           
         ICM   R2,15,0(R1)                                                      
         BZ    BUYERROR                                                         
*                                                                               
         LR    R5,R2               SAVE ADDR OF CURRENT COMBO RATE FLD          
*        CLC   BUYCOM1(3),=C'CR='                                               
*        BNE   *+8                                                              
*        NI    4(R2),X'FF'-X'20'                                                
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    COMEDIT                                                          
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         LA    R3,RATERR                                                        
*                                                                               
         XC    RBUYCOMB,RBUYCOMB   DEFAULT BUY TO NON-COMBO                     
         XC    RBUYCOS,RBUYCOS                                                  
         CLI   5(R2),0                                                          
         BE    ADD10                                                            
*                                                                               
*              EDIT RATE                                                        
         XC    DMCB+4(3),DMCB+4                                                 
         ZIC   RF,5(R2)            CHECK FOR 'TRADE' FLAG                       
         LA    RE,8(R2)            SET A(CASH VALUE FIELD)                      
         AR    RE,RF               ADD L(CASH VALUE INPUT)                      
         BCTR  RE,0                BACK UP 1 CHARACTER                          
*        CLC   =C'BUY',BUYACT      BUY ACTION?                                  
*        BNE   REDI0160            NO  - DON'T RESET TRADE FLAG                 
         NI    RBUYFLG2,X'FF'-X'02'                                             
REDI0160 EQU   *                                                                
*                                  TURN OFF POSSIBLE TRADE FLAG                 
         CLI   0(RE),C'T'          'TRADE' INDICATOR?                           
         BE    REDI0165            YES                                          
         TM    BUYFLAGS,X'80'      TRADE CONTRACT?                              
         BO    REDI0175            YES                                          
         B     REDI0260            NO                                           
REDI0165 EQU   *                                                                
         TM    BUYFLAGS,X'40'      CASH ORDER IN PROCESS?                       
         BNO   REDI0170            NO  - OKAY TO BOOK TRADE                     
         LA    R3,TRDINCSH         YES - NO TRADE IN CASH                       
         B     BUYERROR                                                         
REDI0170 EQU   *                                                                
         BCTR  RF,0                YES - DECREMENT LENGTH BY 1                  
REDI0175 EQU   *                                                                
         STC   RF,DMCB+7           INSERT INTO PARA LIST                        
*        CLC   =C'BUY',BUYACT      BUY ACTION?                                  
*        BNE   REDI0180            NO  - DON'T RESET TRADE FLAG                 
         OI    RBUYFLG2,X'02'      YES - SET TRADE BUY FLAG                     
         CLI   RCONKSTA+4,C'A'     AM MEDIA/RADIO ORDER?                        
         BE    REDI0220            YES - CAN BE CASH+TRADE                      
         CLI   RCONKSTA+4,C'F'     FM MEDIA/RADIO ORDER?                        
         BE    REDI0220            YES - CAN BE CASH+TRADE                      
*                                  ONLY SET FOR TV ORDERS                       
         GOTO1 =A(SETRADE),DMCB,(RC),RR=Y                                       
*                                                                               
*                                                                               
REDI0180 EQU   *                                                                
*&&DO                                                                           
         LA    RE,BUYCOMUH         SET COMMENT # 2 TITLE                        
         MVC   BUYCOMU(07),=C'       '                                          
         NI    1(RE),X'FF'-X'08'   TURN OFF HIGH INTENSITY                      
         LR    RF,RA               SPECIAL IF COMBO K                           
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          COMBO BUY?                                   
         BE    REDI0200            NO  - SKIP NEXT TEST                         
         TM    TWACMTRD,X'80'      ANY 'TRADE' COMBO BUYS?                      
*                                                                               
         DROP  RF                                                               
*                                                                               
         BNO   REDI0240            NO                                           
         B     REDI0220                                                         
REDI0200 EQU   *                                                                
         TM    RBUYFLG2,X'02'      TRADE ORDER?                                 
         BNO   REDI0240            NO                                           
*&&                                                                             
REDI0220 EQU   *                                                                
*        MVC   BUYCOMU(07),=C'*TRADE*'                                          
*        OI    1(RE),X'08'         TURN ON HIGH INTENSITY                       
REDI0240 EQU   *                                                                
*        OI    BUYCOMUH+6,X'80'    SET FIELD TO TRANSMIT                        
*        B     REDI0280                                                         
REDI0260 EQU   *                                                                
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
REDI0280 EQU   *                                                                
*                                                                               
         GOTO1 CASHVAL,DMCB,8(R2)                                               
*                                                                               
         CLI   DMCB,X'FF'          ERROR?                                       
         BE    BUYERROR                                                         
*                                                                               
         MVC   RBUYCOS,DMCB+4                                                   
*                                                                               
ADD10    DS    0H                                                               
* GET LINE NUMBER FOR NEW BUY                                                   
ADD20    MVI   RBUYKTYP,X'0B'      BUY KEY TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  RF                                                               
         OI    DMINBTS,X'08'       GET DELETED RECORDS                          
*              FIND NEXT LINE NUMBER                                            
         XC    HALF,HALF           LINE NUMBER                                  
         MVC   KEY(22),RBUYREC                                                  
         XC    KEY+22(10),KEY+22                                                
         GOTO1 VHIGH                                                            
*                                                                               
ADD50    CLC   KEY(22),KEYSAVE                                                  
         BNE   ADD100                                                           
         CLI   KEY+26,255          PLANREC?                                     
         BE    ADD75                                                            
         CLC   HALF+1(1),KEY+26                                                 
         BNL   *+10                                                             
         MVC   HALF+1(1),KEY+26    HIGHEST LINE NUMBER SO FAR                   
         SPACE 1                                                                
ADD75    OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         B     ADD50                                                            
*                                                                               
ADD100   LH    RE,HALF             LAST LINE NUMBER                             
         LA    RE,1(RE)                                                         
         CH    RE,=H'254'                                                       
         BNH   ADD105                                                           
***      LA    R2,BUYBACTH                                                      
         LA    R3,MAXERR                                                        
         B     BUYERROR                                                         
*                                                                               
ADD105   STC   RE,RBUYKLIN         BUY LINE NUMBER                              
         STC   RE,RBUYKMLN         IN CASE NO MAKE-GOOD                         
         NI    DMINBTS,X'F7'       TURN OFF DELETE PASS                         
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         STC   RE,ADDLAST          KEEP TRACK OF LINES ADDED THIS TRNS          
         CLI   ADDFIRST,0                                                       
         BNE   *+8                                                              
         STC   RE,ADDFIRST                                                      
         DROP  RF                                                               
*                                                                               
*              VALIDATE BUY COMMENTS                                            
COMEDIT  DS    0H                                                               
**       LA    R2,BUYCOM1H                                                      
         GOTO1 =A(GETFLD),DMCB,13,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    ORDCOM                                                           
         CLI   5(R2),0                                                          
         BE    ORDCOM                                                           
*                                                                               
         LA    R3,881              'P=' FORMAT NO LONGER ALLOWED                
         CLC   =C'P=',8(R2)                                                     
         BE    BUYERROR                                                         
*                                                                               
COED0040 DS    0H                                                               
*        CLC   8(3,R2),=C'CR='                                                  
*        BNE   *+8                                                              
*        NI    4(R2),X'DF'                                                      
*                                                                               
         XC    IOAREA(32),IOAREA   NO OLD MG LINE                               
*                                  ELIMINATE OLD MG REFERENCE,IF ANY            
*                                  GET OLD MG ELEM                              
COED0340 DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(4,RBUYREC)                                        
*                                                                               
COED0350 DS    0H                                                               
         MVC   RBUYKMLN,RBUYKLIN   FOR CHANGE IF MG REF ELIMINATED              
         MVI   WORK2,4             ELEM CODE                                    
         CLI   5(R2),0             ENTRY?                                       
         BE    ORDCOM                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK2+2(0),8(R2)                                                 
         LA    R1,3(R1)                                                         
         STC   R1,WORK2+1          LEN                                          
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
*                                  ADD COMMENT 1                                
*                                                                               
*  NOTE: A NEW BUY ORDER COMMENT IS REQUIRED UNLESS THE CONTRACT HAS            
*        NEVER BEEN SENT (VERSION 1) OR UNLESS THE ONLY CHANGE MADE             
*        WAS A CHANGE TO PROGRAM TYPE (STAT2 NOT X'80')                         
*                                                                               
*  ****  SKIP BUY ORDER COMMENT VALIDATION IF ACTION IS MAKEGOOD APPLY          
*        SINCE NO ORDER COMMENTS WILL BE PASSED                                 
*                                                                               
*        A BUY LINE CAN BE DESIGNATED 'DON'T SEND' IF                           
*         A. IT'S AN ACE OR GRAPHNET CONTRACT AND                               
*         B. IT'S NOT VERSION 1 AND                                             
*         C. THE CHARACTERS IN QUOTES '#DS' ARE THE 1ST 3 CHARACTERS            
*            ON THE 1ST REP BUY ORDER COMMENT LINE AND                          
*         D. THE STATION IS DESIGNATED OK FOR 'DON'T SEND' BUYLINES             
*                                                                               
ORDCOM   DS    0H                                                               
         GOTO1 =A(GETFLD),DMCB,14,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    ENDCOM                                                           
         CLI   5(R2),0                                                          
         BE    ENDCOM                                                           
*                                                                               
ORDCOM11 XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         MVI   WORK2,X'84'                                                      
         OI    WORK2+2,X'80'       REP COMMENT                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+3(0),8(R2)                                                 
         LA    RE,4(RE)            GET TOTAL LENGTH (1 FROM BCTR + 2)           
         STC   RE,WORK2+1                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
ENDCOM   DS    0H                                                               
*                                                                               
* EDIT PGM (PROGRAM) FIELD                                                      
*                                                                               
*        LA    R2,BUYPGMH                                                       
         GOTO1 =A(GETFLD),DMCB,17,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    ENDPGM                                                           
         CLI   5(R2),0                                                          
         BE    ENDPGM                                                           
         XC    WORK2(255),WORK2                                                 
         MVI   WORK2,X'21'         ELEM CODE                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK2+2(0),8(R2)                                                 
         AHI   R1,3                                                             
         STC   R1,WORK2+1                                                       
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
ENDPGM   DS    0H                                                               
*                                                                               
         TITLE 'T80216 REPPAK EDIT END'                                         
ENDEDIT  DS    0H                                                               
* PUT RBUYNW IN 03 ELEMENTS (EFF DATES) IF NO OVERRIDE                          
*     AND NOT 'DARE' ORDER (EFF DATES CAN'T BE CHANGED FOR THEM)                
         MVI   ELCODE,X'1D'        DARE ELEMENT?                                
         BAS   RE,GETEL                                                         
         BNE   END080              NO  - PROCEED                                
         LA    R6,RCONREC          YES - CHECK FOR 'CONFIRMED'                  
         MVI   ELCODE,X'1F'        EXTENDED DESCRIPTION ELEMENT?                
         BAS   RE,GETEL                                                         
         BNE   END200              NOT FOUND - NEW DARE ORDER.                  
         TM    RCONCONF-RCONXEL(R6),X'E0'                                       
*                                  ANY 'CONFIRMED' FLAG ON?                     
         BZ    END200              NO  - TREAT AS DARE ORDER                    
*                                  YES - UPDATE # SPOTS/WEEK                    
*                                                                               
*   NOTE - THIS TEST MAY HAVE TO BE CHANGED FOR ORDERS WHICH ARE                
*        CONFIRMED/UNCONFIRMED FOR DARE.                                        
*                                                                               
END080   EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   END200                                                           
END100   DS    0H                                                               
         TM    8(R6),X'01'                                                      
         BO    *+10                                                             
         MVC   9(1,R6),RBUYNW                                                   
         BAS   RE,NEXTEL                                                        
         BE    END100                                                           
END200   DS    0H                                                               
*                                  ADD BUY                                      
*                                  UPDATE CHANGE INDICATORS                     
ADDBUY   MVC   RBUYCREA,TODAY      CREATION DATE                                
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET CONTRACT                        
         BNZ   *+12                                                             
*                                                                               
         CLI   RCONMOD,X'FF'       -1?                                          
         BE    ADD150                                                           
         CLI   RBUYKLIN,1          LINE 1?                                      
         BNE   ADD150                                                           
         CLC   RCONCREA,TODAY                                                   
         BE    *+14                                                             
         OI    TAREQ,1             T/A REQ IND                                  
         MVC   RCONCREA,TODAY      LINE 1 SHOULD NOT BUMP K MOD                 
*                                                                               
         OI    RCONMODR,X'20'+X'10' X'20'=LINE 1 ADDED                          
*                                   X'10'=NOT PENDING/BUYLINE(S) WERE           
*                                         ADDED AT ONE POINT.  THIS BIT         
*                                         DOES NOT GET RESET.                   
         B     *+8                                                              
ADD150   BAS   RE,BUMPNUM          BUMP MODIFICATION NUMBER IN K                
         MVC   RBUYKMOD,RCONMOD                                                 
         TM    RCONMODR+1,X'C0'    IF ACE/GRAPHNET SKIP THIS CODE               
         BNZ   ADD160              (X'FF' IS VALID MOD #)                       
         SPACE 1                                                                
         CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RBUYKMOD,0                                                       
ADD160   MVC   RBUYCHGI,=C'A '     ADD IND                                      
*                                                                               
*                                                                               
ADD170   DS    0H                                                               
*                                  ADD EXTRA DESCRIPTION ELEMENT X'10'          
         GOTO1 =A(ADDEXTRA),DMCB,(RC),RR=Y                                      
*                                                                               
         CLI   RCONKSTA+4,C'F'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
         CLI   RCONKSTA+4,C'A'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
         CLI   RCONKSTA+4,C'C'     RADIO COMMENT - MUST KEEP                    
         BE    END300                                                           
*                                                                               
         GOTO1 =A(SARCOMS),RR=Y                                                 
         B     END300                                                           
         SPACE 4                                                                
* ROUTINE TO BUMP CONTRACT MODIFICATION MUMBER                                  
         SPACE 1                                                                
BUMPNUM  OI    RCONMODR,X'40'      BUY CHANGE IND                               
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BCR   8,RE                                                             
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BNZ   BUMP5                                                            
*                                                                               
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BCR   8,RE                                                             
* UPDATE MODIFICATION NUMBER                                                    
BUMP5    OI    TAREQ,1             T/A REQ IND                                  
         NI    RCONMODR,X'5F'      NO K HEADLINE CHANGE                         
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET- DON'T BUMP MOD NUMBER          
         BNZ   BUMP10                                                           
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,RCONMOD          MOD NUMBER                                   
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
BUMP10   MVC   RCONMODD,TODAY                                                   
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*  UPDATE REP VERSION NUMBER                                                    
*                                                                               
END300   DS    0H                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    END500                                                           
*        TM    STAT2,X'80'         BUY CHANGED-UP VER/MOD                       
*        BO    END325                                                           
*        OI    BYTE4,X'02'         NO, BUT STILL DO PUTREC                      
*        B     END500                                                           
         SPACE 1                                                                
END325   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    END330                                                           
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
END330   DS    0H                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    END350                                                           
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATE                                   
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWADARE,X'08'                                                    
         BZ    END340                                                           
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
         DROP  RF                                                               
*                                                                               
END340   DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC)                                     
         BNZ   BUYERROR                                                         
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
                                                                                
END350   DS    0H                                                               
*                                                                               
*   FOLLOWING CODE USES R1 AS INTERMEDIATE RECIPIENT FIELD BECAUSE              
*        EARLIER 'USING R6' MESSES UP ADDRESSABILITY OF RBUYVER                 
*        FIELD, COVERING IT WITH R6 RATHER THAN R12.                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         ZIC   R1,RCONSRV         STORE REP VERSION NO. IN BUY                  
         DROP  R6                                                               
         STC   R1,RBUYVER          INSERT REP VERSION #                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    END400                                                           
         SPACE 1                                                                
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
END400   OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
         OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         SPACE 1                                                                
*                                                                               
* PROCESS MAKEGOODS                                                             
*                                                                               
END500   DS    0H                                                               
                                                                                
*        CLC   =C'CR=',BUYCOM1                                                  
*        BE    EXXMOD                                                           
*                                                                               
*        GOTO1 =A(MGPROC),DMCB,(RC),RR=Y                                        
*                                                                               
         GOTO1 =V(RETMPBUC),DMCB,(RC),RR=Y  WRITE BUY, UPDATE CONTRACT          
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD CODE TO RBUYCHGI  (CODE IN BYTE)                               
***********************************************************************         
ADDCODE  NTR1                                                                   
         CLI   BYTE,0                                                           
         BE    ADDCXIT                                                          
         BAS   RE,BUMPNUM          BUMP K NUMBER                                
         MVC   RBUYKMOD,RCONMOD    K MODIFICATION NUMBER                        
         SPACE 1                                                                
*                                                                               
* FOR ACE/GRAPHNET -  THE VERSION, NOT THE DAY, IS WHAT MATTERS                 
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ADDC50                                                           
         CLI   TWAACCS,C'$'        STATION ONLY HAS 1 CHG CODE, SO IT           
         BE    ADDC75              DOESN'T MATTER IF UPDATED ALREADY            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    ADDC10                                                           
         DC    H'0'                                                             
ADDC10   TM    4(R6),X'20'         REP VERSION NOT ADVANCED                     
         BNO   ADDC30                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
         SPACE 1                                                                
ADDC30   CLC   5(1,R6),RBUYVER     COMPARE CONTRACT TO BUY VERSION              
         BE    ADDC35                                                           
         TM    BSTATUS,X'40'       WAS THERE ALREADY A CHANGE CODE              
         BO    ADDC100                                                          
         OI    BSTATUS,X'40'       NOW THERE'S BEEN A CHANGE CODE               
         B     ADDC75                                                           
ADDC35   CLI   RBUYCHGI,C'A'       IF ADDED THIS VERSION,                       
         BE    ADDCXIT             DON'T CARE ABOUT OTHER CHANGES               
         B     ADDC100                                                          
         SPACE 1                                                                
*  DO SOME TESTS FOR NON ACE/GRAPHNET CONTRACTS                                 
         SPACE 1                                                                
ADDC50   CLI   RCONMOD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   RBUYKMOD,0                                                       
         CLC   TODAY,RBUYCHGD                                                   
         BE    ADDC100                                                          
ADDC75   MVC   RBUYCHGD,TODAY                                                   
         MVC   RBUYCHGI(1),BYTE    CHANGE CODE                                  
         MVI   RBUYCHGI+1,C' '                                                  
*                                                                               
ADDCXIT  XIT1                                                                   
* BUY ALREADY CHANGED TODAY                                                     
ADDC100  CLI   RBUYCHGI,C' '                                                    
         BNE   *+14                                                             
         MVC   RBUYCHGI(1),BYTE    FIRST                                        
         B     ADDCXIT                                                          
         CLI   RBUYCHGI,C'*'                                                    
         BE    ADDCXIT                                                          
         CLC   RBUYCHGI(1),BYTE                                                 
         BE    ADDCXIT                                                          
         CLI   RBUYCHGI+1,C' '                                                  
         BNE   *+14                                                             
         MVC   RBUYCHGI+1(1),BYTE  2D                                           
         B     ADDCXIT                                                          
         CLC   RBUYCHGI+1(1),BYTE                                               
         BE    ADDCXIT                                                          
         MVC   RBUYCHGI,=C'* '     MORE THAN 2                                  
         B     ADDCXIT                                                          
         EJECT                                                                  
         TITLE 'SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS'                
*        P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                        
*        P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR. FOUND            
*        AN ASTERISK DELIMITS SUB-FIELDS                                        
SCAN     NTR1                                                                   
*                                                                               
         L     R2,4(R1)       FIELD HEADER                                      
         L     R3,0(R1)       LAST FIELD                                        
*                                                                               
         ZIC   R4,0(R1)       LENGTH OF PREVIOUS FIELD                          
         LA    R3,1(R4,R3)    NEW SCAN PLUS DELIMITER                           
         ST    R3,0(R1)       LAST FIELD                                        
         SR    R5,R5          LENGTH COUNTER                                    
         IC    R4,5(R2)       TOTAL LENGTH OF INPUT                             
         LA    R2,8(R4,R2)    TOTAL FIELD END                                   
         MVI   4(R1),0        ELIM. STOP INDICATOR                              
FIELD25  CR    R3,R2          END OF INPUT?                                     
         BL    FIELD100                                                         
FIELD50  STC   R5,0(R1)       NEW FIELD LENGTH                                  
         XIT1                                                                   
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   *+12                                                             
         MVI   4(R1),C'*'                                                       
         B     FIELD50                                                          
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
***********************************************************************         
* GETFLD - RESOLVE ADDRESS OF BUY FIELD                                         
***********************************************************************         
* INPUT: P1 = FIELD EQUATE #                                                    
*        R2 = A(SELECT FLD HEADER FOR CURRENT LINE)                             
* OUTPUT:P1 = A(DATA FIELD),OR NULL IF NO INPUT FOR FIELD TYPE                  
*                                                                               
*   NOTE: IF ROUTINE DETECTS THAT 'FILL' FIELD IS IN USE, THE FIELD             
*         ADDRESS RETURNED WILL BE A DUMMY FIELD IN LOCAL STORAGE               
*         THE ERROR ROUTINE WILL LATER DETECT THIS AND SET THE CURSOR           
*         AS NEEDED TO THE FILL FIELD                                           
*                                                                               
*         USES IO3 FOR DUMMY FIELD AREA & IO3+500 FOR SCANNER AREA              
*                                                                               
***********************************************************************         
GETFLD   NTR1  BASE=*,LABEL=*                                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         MVC   MYFLD,3(R1)                                                      
*                                                                               
* FIND FIELD IN DISPLAY LINE                                                    
*                                                                               
         L     R2,SELFLD           CURRENT LINE SELECT FIELD                    
         LA    R3,LINDEF                                                        
         ZIC   RF,0(R2)                                                         
         LA    R4,0(RF,R2)         1ST FIELD LABEL                              
         SR    R2,R2               CLEAR AS INDICATOR                           
GFLD050  DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    GFLD100                                                          
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               DATA FIELD                                   
         CLC   0(1,R3),MYFLD       MATCH FIELD?                                 
         BNE   GFLD060                                                          
         LR    R2,R4                                                            
         OI    1(R2),X'01'                                                      
         OI    6(R2),X'81'                                                      
         B     GFLD100                                                          
GFLD060  DS    0H                                                               
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD LABEL                             
         LA    R3,1(R3)                                                         
         B     GFLD050                                                          
*                                                                               
* FIND FIELD IN 'FILL'                                                          
*                                                                               
GFLD100  DS    0H                                                               
         CLI   TMPFILLH+5,0        ANY FILL?                                    
         BE    GFLDX                                                            
         L     R6,AIO3                                                          
         LA    R6,500(R6)          SCANNER AREA = IO3+500                       
*        MVI   DMWORK,C' '                                                      
*        MVC   DMWORK+1(79),DMWORK CLEAR W/SPACES                               
*        ZIC   R1,TMPFILLH+5                                                    
*        BCTR  R1,0                                                             
*        EX    R1,*+4                                                           
*        MVC   DMWORK(0),TMPFILL                                                
*        LA    RF,DMWORK                                                        
*        ST    RF,DMCB                                                          
*        MVI   DMCB,C'C'                                                        
*        MVI   DMCB,24                                                          
*        OI    DMCB,X'80'                                                       
*        GOTO1 SCANNER,DMCB,,(R6),0                                             
         GOTO1 SCANNER,DMCB,(24,TMPFILLH),(R6),0                                
         ZICM  R5,4(R1),1                                                       
         BZ    FILLERR                                                          
GFLD110  DS    0H                                                               
         ZICM  R1,0(R6),1          LEN OF 1ST FIELD HALF                        
         BZ    FILLERR                                                          
         BCTR  R1,0                                                             
         L     R4,AFLDTAB                                                       
         MVC   HALF,0(R4)          TABLE ENTRY LENGTH                           
         LA    R4,4(R4)            START OF TABLE                               
GFLD120  DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    FILLERR                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   9(0,R4),12(R6)      MATCH FILL KEYWORD TO TYPE?                  
         BE    GFLD130             YES                                          
         AH    R4,HALF                                                          
         B     GFLD120                                                          
GFLD130  CLC   0(1,R4),MYFLD       MATCH THIS FIELD?                            
         BE    GFLD140             YES                                          
         LA    R6,46(R6)           NEXT SCANNER LINE                            
         BCT   R5,GFLD110                                                       
         B     GFLDX               NO FILL FOUND                                
*                                                                               
GFLD140  DS    0H                  BUILD DUMMY FIELD                            
         L     R2,AIO3             FIND NEXT DUMMY LOCATION                     
GFLD145  CLI   0(R2),0                                                          
         BE    GFLD150                                                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     GFLD145                                                          
*                                                                               
GFLD150  DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         ZIC   R1,17(R4)           MAX LEN                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R2),MYSPACES                                                 
         LA    R1,9(R1)                                                         
         STC   R1,0(R2)                                                         
         ZICM  RF,1(R6),1          LEN OF 2ND FIELD HALF                        
         BZ    FILLERR                                                          
         STC   RF,5(R2)                                                         
         CLC   1(1,R6),17(R4)      INPUT LEN VS MAX LEN                         
         BH    FILLERR                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   8(0,R2),22(R6)                                                   
         TM    3(R6),X'80'         NUMERIC?                                     
         BZ    *+8                                                              
         OI    4(R2),X'08'         VALIDITY BITS                                
         AR    R1,R2                                                            
         MVI   0(R1),0             NEW END MARK                                 
                                                                                
GFLDX    DS    0H                                                               
         ST    R2,DMCB                                                          
         XIT1                                                                   
*                                                                               
FILLERR  DS    0H                                                               
         LA    R2,TMPFILLH                                                      
         LA    R3,2                                                             
         B     UNWIND                                                           
         DROP  R7                                                               
         LTORG                                                                  
*********************************************************************           
* SARCOMS - CHANGES PENDING COMMENTS TO SPL COMMENTS                *           
*********************************************************************           
SARCOMS  NTR1  BASE=*,LABEL=*                                                   
         B     *+12                                                             
         DC    CL8'*SARCOMS'                                                    
*                                                                               
SARCM2   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   SARCMX                                                           
*                                                                               
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R6)                                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),(R6)                                  
*                                                                               
         LA    R6,RCONELEM                                                      
SARCM4   CLI   0(R6),0                                                          
         BE    SARCM6                                                           
         CLI   0(R6),X'07'                                                      
         BH    SARCM6                                                           
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     SARCM4                                                           
*                                                                               
SARCM6   MVI   WORK2,X'07'                                                      
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),WORK2,(R6)                            
         B     SARCM2                                                           
*                                                                               
SARCMX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         CSECT                                                                  
***********************************************************************         
* SET TRADE ORDER FLAG                                                          
***********************************************************************         
SETRADE  NMOD1 0,*SETR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RETRIEVE RANDOM FLAG ELEMENT                 
         BAS   RE,GETEL                                                         
         USING RCONRFEL,R6                                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OI    RCONRF1,X'08'       SET TRADE CONTRACT                           
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SECTION                                                              
***********************************************************************         
SECEDIT  NMOD1 0,*SECD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         XC    RBUYSEC,RBUYSEC                                                  
         GOTO1 =A(GETFLD),DMCB,11,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    SECE0010                                                         
         CLI   5(R2),0                                                          
         BNE   SECE0015                                                         
SECE0010 TM    TWASTAOB,X'80'      STATION PROF #18?                            
         BZ    SECX                NO - OK                                      
         LA    R3,850                                                           
         B     BUYERROR                                                         
*                                                                               
SECE0015 DS    0H                                                               
         CLI   ECFORMAT,C'B'       BIAS EC FORMAT?                              
         BE    SECE0020            YES - TEST WITHIN BIAS LIMITS                
         LA    R3,SECERR           SET POSSIBLE ERROR                           
         CLI   5(R2),3                                                          
         BH    BUYERROR                                                         
         GOTO1 VMOVE                                                            
         MVC   RBUYSEC,WORK                                                     
         B     SECX                                                             
*                                                                               
SECE0020 EQU   *                                                                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    SECE0040            NO                                           
         LA    R3,BIASSEC          SET POSSIBLE ERROR: BIAS FORMAT              
         CLI   5(R2),2             BIAS ONLY PERMITS 2 CHARS                    
         BH    BUYERROR                                                         
         CLI   8(R2),C'0'          FIRST CHAR:  BETWEEN 0-9?                    
         BL    BUYERROR               NO                                        
         CLI   8(R2),C'9'                                                       
         BH    BUYERROR                                                         
         CLI   5(R2),2             TWO CHARACTERS ENTERED?                      
         BL    SECE0040            NO                                           
         CLI   9(R2),C'0'          YES - SECOND CHAR:  BETWEEN 0-9?             
         BL    BUYERROR               NO                                        
         CLI   9(R2),C'9'                                                       
         BH    BUYERROR                                                         
*                                                                               
SECE0040 EQU   *                                                                
         GOTO1 VMOVE                                                            
         MVC   RBUYSEC,WORK                                                     
*                                                                               
SECX     DS    0H                                                               
         XIT1                                                                   
         DROP  R7                                                               
***********************************************************************         
* VALIDATE PATTERN                                                              
***********************************************************************         
PTNEDIT  NMOD1 0,*PTNE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    NOTX                SKIP PATTERN/NOTATION FIELDS                 
***      LA    R2,BUYCLSH          POINT TO PATTERN INPUT                       
         GOTO1 =A(GETFLD),DMCB,10,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    PTNX                                                             
         MVI   ELCODE,X'20'                                                     
         LA    R6,RBUYREC                                                       
         BAS   RE,GETEL                                                         
         BE    PTN010                                                           
*                                                                               
         XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         LA    R6,WORK2                                                         
         USING RBUYPTEL,R6                                                      
         MVI   RBUYPTCD,X'20'                                                   
         MVI   RBUYPTLN,RBUYPTLQ                                                
         MVC   RBUYPTPT,8(R2)                                                   
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         B     PTNX                                                             
         DROP  R6                                                               
*                                                                               
PTN010   DS    0H                                                               
         USING RBUYPTEL,R6                                                      
         CLI   5(R2),0             ANY INPUT?     .                             
         BNE   PTN020              YES                                          
         XC    RBUYPTPT,RBUYPTPT   NO - CLEAR PATTERN                           
         OC    RBUYPTNT,RBUYPTNT   ANY NOTATION IN ELEM?                        
         BNZ   PTNX                YES - KEEP IT                                
         GOTO1 VDELELEM,DMCB,(X'20',RBUYREC)  YES, DELETE OLD                   
         B     PTNX                                                             
PTN020   DS    0H                                                               
         MVC   RBUYPTPT,8(R2)                                                   
PTNX     DS    0H                                                               
         DROP  R6                                                               
*              VALIDATE PATTERN NOTATION                                        
***      LA    R2,BUYNOTH          POINT TO PATTERN INPUT                       
         GOTO1 =A(GETFLD),DMCB,15,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    NOTX                                                             
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    NOT010                                                           
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    NOTX                NO - LEAVE                                   
         XC    WORK2(100),WORK2    BUILD NEW ELEMENT                            
         LA    R6,WORK2                                                         
         USING RBUYPTEL,R6                                                      
         MVI   RBUYPTCD,X'20'                                                   
         MVI   RBUYPTLN,RBUYPTLQ                                                
         MVC   RBUYPTNT,8(R2)                                                   
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK2                                      
         B     NOTX                                                             
         DROP  R6                                                               
*                                                                               
NOT010   DS    0H                                                               
         USING RBUYPTEL,R6                                                      
         CLI   5(R2),0             ANY INPUT?     .                             
         BNE   NOT020              YES                                          
         XC    RBUYPTNT,RBUYPTNT   NO - CLEAR NOTATION                          
         OC    RBUYPTPT,RBUYPTPT   ANY PATTERN IN ELEM?                         
         BNZ   NOTX                YES - KEEP IT                                
         GOTO1 VDELELEM,DMCB,(X'20',RBUYREC)  YES, DELETE OLD                   
         B     NOTX                                                             
NOT020   DS    0H                                                               
         MVC   RBUYPTNT,8(R2)                                                   
NOTX     DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         DROP  R7                                                               
***********************************************************************         
* VALIDATE 'USE PATTERN TIMES'                                                  
***********************************************************************         
UPTEDIT  NMOD1 0,*UPTE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         TM    TWAFLAGS,TWAFLPTQ   PROFILE FOR PATTERN?                         
         BZ    UPTX                         NO - SKIP                           
***      LA    R2,BUYUPTH          POINT TO PATTERN INPUT                       
         GOTO1 =A(GETFLD),DMCB,16,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    UPTX                                                             
*                                                                               
UPT010   DS    0H                                                               
         LA    R3,INVINP                                                        
         OI    STAT2,X'80'         BUY CHANGED-UPDATE VER/MOD                   
         MVI   ELCODE,X'20'                                                     
         LA    R6,RBUYREC                                                       
         BAS   RE,GETEL                                                         
         BE    UPT020                                                           
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         LA    R3,846              NO PATTERN INPUT                             
         B     BUYERROR                                                         
         CLI   8(R2),0                                                          
         BE    UPTX                                                             
         CLI   8(R2),C'N'                                                       
         BE    UPTX                                                             
         B     BUYERROR                                                         
*                                                                               
UPT020   DS    0H                                                               
         USING RBUYPTEL,R6                                                      
         CLI   8(R2),0             NO INPUT                                     
         BNE   UPT030                                                           
         MVI   8(R2),C'N'          DEFAULT 'N'                                  
*                                                                               
UPT030   DS    0H                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   UPT040                                                           
         OC    RBUYPTPT,RBUYPTPT   HAVE A PATTERN?                              
         BNZ   *+12                NO                                           
         LA    R3,846                                                           
         B     BUYERROR                                                         
         OI    RBUYPTFL,X'80'                                                   
         B     UPTX                                                             
UPT040   DS    0H                                                               
         CLI   8(R2),C'N'                                                       
         BNE   BUYERROR                                                         
         NI    RBUYPTFL,X'FF'-X'80'                                             
         DROP  R6                                                               
UPTX     DS    0H                                                               
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
* EDIT BUY CODE ROUTINE                                                         
*                                                                               
BUYCDX   NTR1  BASE=*,LABEL=*                                                   
         XC    ORIGBYCD,ORIGBYCD   CLEAR SAVE AREA                              
         TM    PROFILES+CNTFOXSB,CNTFOXSA                                       
*                                  BUYLINE CODE FIELD TURNED ON?                
         BZ    BYCD0800            NO  - EXIT CC ZERO                           
*                                                                               
         LA    R6,RBUYREC          YES                                          
         MVI   ELCODE,X'5F'        RETRIEVE ANY BUYLINE CODE ELEMENT            
         BAS   RE,GETEL                                                         
         BNE   BYCD0005            NOT FOUND                                    
         MVC   ORIGBYCD,RBYSCDBC-RBYSCDEL(R6)                                   
*                                  SAVE BUYLINE CODE FROM ORIG ELEMENT          
BYCD0005 EQU   *                                                                
***      LA    R2,BUYSCODH         SET A(BUYLINE CODE INPUT)                    
         GOTO1 =A(GETFLD),DMCB,18,RR=Y                                          
         ICM   R2,15,0(R1)                                                      
         BZ    BYCD0800            NO  - FINISHED EXIT CC ZERO                  
         CLI   5(R2),0             ANY INPUT IN FIELD?                          
         BE    BYCD0800            NO  - FINISHED EXIT CC ZERO                  
*                                  SAVE BUYLINE CODE FROM ORIG ELEMENT          
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   BYCD0010                                                         
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R2),0,        +        
               (0,C' BCD'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
BYCD0010 EQU   *                   SAVE BUYLINE CODE FROM ORIG ELEMENT          
         XC    KEY,KEY             VERIFY PROGRAM TYPE EXISTS                   
         MVI   KEY,X'4B'                                                        
         MVC   KEY+21(2),REPALPHA                                               
         MVI   KEY+23,1            SET CODE TYPE TO 'CODE'                      
         MVC   KEY+24(3),8(R2)     INSERT CODE FROM SCREEN                      
***>>    OC    KEY+24(3),=C'   '   SET TRAILING SPACES                          
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   BYCD0780            NO  - ERROR: RETURN CC NOT ZERO              
         ZIC   RF,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RBYSCDEL,R6                                                      
         MVI   RBYSCDCD,X'5F'      SET BUYCODE ELT TYPE                         
         MVI   RBYSCDLN,RBYSCDLQ   INSERT LENGTH                                
         MVC   RBYSCDBC,KEY+24     INSERT BUYLINE CODE FROM KEY                 
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VDELELEM,DMCB,(X'5F',RBUYREC)                                    
*                                  DROP OLD X'5F' ELEMENT                       
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK                                       
*                                  ADD  NEW X'5F' ELEMENT                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
         B     BYCD0800            EXIT CC ZERO                                 
         DROP  R6                                                               
BYCD0780 EQU   *                                                                
         ST    R2,DUB                                                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     BYCD0900                                                         
BYCD0800 EQU   *                                                                
         SR    R0,R0                                                            
BYCD0900 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD EXTRA DESCRIPTION ELEMENT X'80'                                           
***********************************************************************         
ADDEXTRA NMOD1 0,*ADEX*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         XC    WORK,WORK                                                        
WKD      USING RBUYXXEL,WORK                                                    
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         MVC   WKD.RBUYXXMD,RCONMOD    CONTRACT MOD# AT BUY CREATION            
*                                                                               
         OI    WKD.RBUYXXFG,X'80'  CREATED BY TEMPLATE                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDEXT10                                                         
         USING RCONSEND,R6                                                      
         MVC   WKD.RBUYXXVR,RCONSRV    CONTRACT VER# AT BUY CREATION            
         TM    RCONSENF,X'20'                                                   
         BZ    ADDEXT10                                                         
         ZIC   R1,WKD.RBUYXXVR         REP VERSION NOT ADVANCED                 
         LA    R1,2(R1)                ADVANCE VERSION MANUALLY                 
         STC   R1,WKD.RBUYXXVR                                                  
         DROP  R6,WKD                                                           
*                                                                               
ADDEXT10 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RBUYREC,WORK                                       
*                                                                               
ADDEXTX  DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
*  ALTWKCHK:  TESTS NUMBER OF WEEKS IN ALTERNATING WEEK FLIGHT.  THE            
*        03 ELEMENT IS SET UP DIFFERENTLY WHEN CONSTRUCTED BY ENTRY             
*        OF DATES AS 'S-EA' OR 'JAN9-3WA'.  THE FORMER PRODUCES AN              
*        ODD NUMBER OF WEEKS, WHILE THE LATTER PRODUCES AN EVEN                 
*        NUMBER OF WEEKS.  THIS WILL DETERMINE THE DATE INCREMENT FOR           
*        THE MAKEGOOD BREAKOUT ROUTINE.                                         
*        RESULTS RETURNED BY PERVERT MAY BE:                                    
*          ODD # OF WEEKS, DAYS REMAINING    --->  EVEN # WEEKS                 
*          ODD # OF WEEKS, NO DAYS REMAINING --->  ODD # OF WEEKS               
*          EVEN # OF WEEKS, DAYS REMAINING   --->  ODD # OF WEEKS               
*          EVEN # OF WEEKS, NO DAYS REMAINING--->  EVEN # OF WEEKS              
*                                                                               
ALTWKCHK NTR1                                                                   
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,SDATSPLT)                            
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,EDATSPLT)                            
*                                  CONVERT END   DATE TO EBCDIC                 
         GOTO1 PERVERT,DMCB,SDATSPLT,EDATSPLT,0,0                               
         TM    DMCB+13,X'01'       NUMBER OF WEEKS ODD?                         
         BNO   ALTW0040            NO  - CHECK # WEEKS EVEN                     
         OC    DMCB+10(2),DMCB+10  ANY DAYS REMAINING?                          
         BNZ   ALTW0080            YES - EVEN NUMBER OF WEEKS                   
         OI    ALTWKFLG,X'02'      NO  - SET ODD WEEK INDICATOR                 
         B     ALTW0120                                                         
ALTW0040 EQU   *                                                                
*                                  # OF WEEKS EVEN - CHECK DAYS                 
         OC    DMCB+10(2),DMCB+10  ANY DAYS REMAINING?                          
         BZ    ALTW0080            NO  - EVEN NUMBER OF WEEKS                   
         OI    ALTWKFLG,X'02'      YES - SET ODD WEEK INDICATOR                 
         B     ALTW0120                                                         
ALTW0080 EQU   *                                                                
         MVI   FRSTWEEK,1          TURN OFF ONE-TIME SWITCH                     
ALTW0120 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DATETEST:  COMPARE FOR CONTINUOUS DATES (SINGLE OR ALTERNATING              
*        WEEKS), SAME NUMBER OF SPOTS.  0(R1) = 0 INDICATES PREVIOUS            
*        X'03' BEING CONSIDERED, 0(R1) = 1 INDICATES FOLLOWING.                 
*        R2 ---> MAKEGOOD X'03' ELEMENT                                         
*        R6 ---> ELEMENT WHICH MAY BE MERGED                                    
*                                                                               
DATETEST NTR1                                                                   
         L     R3,0(R1)            PREVIOUS/FOLLOWING INDICATOR                 
         LA    R4,RBUYDTST-RBUYDTEL(R6)                                         
*                                  A(START DATE IN ELT TO BE MERGED)            
         GOTO1 DATCON,DMCB,(3,(R4)),(0,SDATSPLT)                                
*                                  CONVERT IT TO EBCDIC                         
         GOTO1 DATCON,DMCB,(3,SAVCOMDT),(0,EDATSPLT)                            
*                                  CONVERT MG 03 STARTDATE TO EBCDIC            
         LA    R5,7                SET WEEK INTERVAL TO 7 DAYS                  
         LTR   R3,R3               PREVIOUS WEEK/FOLLOWING WEEK?                
         BNZ   DTES0010            NOT ZERO = FOLLOWING WEEK, WHICH             
*                                     WILL ALWAYS BE 1 OR 2 WEEKS               
*                                     AFTER MG X'03' ELEMENT                    
*                                                                               
*                                  PRIOR ELEMENT START DATE MUST BE             
*                                     CALC'ED USING NUMBER OF WEEKS             
         ZIC   RF,RBUYDTWK-RBUYDTEL(R6)                                         
         STH   RF,HALF                                                          
         MH    R5,HALF             CALCULATE # DAYS DIFFERENCE                  
*                                                                               
DTES0010 EQU   *                                                                
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK?                            
         BNO   DTES0020            NO                                           
         SLL   R5,1                YES - DOUBLE THE NUMBER OF DAYS              
DTES0020 EQU   *                                                                
         LTR   R3,R3               PREVIOUS WEEK/FOLLOWING WEEK?                
         BNZ   DTES0040            NOT ZERO = FOLLOWING WEEK                    
         LNR   R5,R5               PREVIOUS WEEK - BACK IT UP                   
DTES0040 EQU   *                                                                
         GOTO1 ADDAY,DMCB,EDATSPLT,EDATSPLT+8,(R5)                              
*                                  CALCULATE PREV/FOLLOWING DATE FROM           
*                                     MAKEGOOD X'03' ELT DATE                   
         CLC   EDATSPLT+8(6),SDATSPLT                                           
*                                  SAME DATES?                                  
         BNE   DTES0200            NO  - DON'T COMPRESS                         
         CLC   RBUYDTNW,RBUYDTNW-RBUYDTEL(R6)                                   
*                                  YES - SAME NUMBER OF SPOTS?                  
         BNE   DTES0200            NO  - DON'T COMPRESS                         
*                                  YES - COMPRESS THE ELEMENTS                  
         LTR   R3,R3               PREVIOUS WEEK/FOLLOWING WEEK?                
         BNZ   DTES0060            NOT ZERO = FOLLOWING WEEK                    
*                                  PREVIOUS WEEK: RESET START WEEK              
         MVC   RBUYDTST,RBUYDTST-RBUYDTEL(R6)                                   
*                                  MOVE START WEEK FROM PREVIOUS                
         B     DTES0080                                                         
DTES0060 EQU   *                                                                
         MVC   RBUYDTED,RBUYDTED-RBUYDTEL(R6)                                   
DTES0080 EQU   *                                                                
         ZIC   RF,RBUYDTWK         NUMBER OF WEEKS                              
         ZIC   RE,RBUYDTWK-RBUYDTEL(R6)                                         
         AR    RF,RE               INCREASE NUMBER OF WEEKS BACK                
         STC   RF,RBUYDTWK         PUT IT BACK                                  
         MVI   0(R6),X'FF'         SET ELEMENT FOR DELETION                     
DTES0200 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MY ADDELEM                                                                    
***********************************************************************         
MYADELM  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
                                                                                
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
*                                  ADD DATE ELEM TO BUYREC                      
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),(R3),0                        
         TM    DMCB+12,X'05'       REC TOO LONG                                 
         BZ    MYADELMX                                                         
         LA    R3,339              REC FULL                                     
         B     BUYERROR                                                         
                                                                                
MYADELMX DS    0H                                                               
         MVC   DMCB(24),DMCB2      RESTORE                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* CHECK IF CONTRACT IS A TAKEOVER CONTRACT                                      
********************************************************************            
ISTAKOV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         USING RCONCMEL,R6                                                      
         ZIC   R1,RCONCMLN                                                      
         SH    R1,=H'3'                                                         
         BM    TKOVNO                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RCONCMNT                                                
         DROP  R6                                                               
*                                                                               
         CLC   =C'C=TO',WORK2      COMMET MUST BE C=TOXXXX-X                    
         BNE   TKOVNO              WHERE XXXX-X IS THE STATION                  
         MVC   WORK(6),WORK2+4                                                  
                                                                                
         CLC   =C'-TV',TMPSTA+4                                                 
         BE    TKOV20                                                           
         CLC   =C'-TV',TMPSTA+3    CHECK INCASE OF 3 LETTER CALLS               
         BE    TKOV30                                                           
         CLC   =C'-C',WORK2+7      CHECK IF COMBO                               
         BE    TKOV50                                                           
         CLC   =C'-C',WORK2+8                                                   
         BE    TKOV50                                                           
                                                                                
* COMPARE FOR RADIO                                                             
         CLI   WORK+5,C' '         CHECK INCASE OF 3 LETTER CALLS               
         BNE   TKOV10                                                           
         MVI   WORK+5,C'M'                                                      
                                                                                
TKOV10   DS    0H                                                               
         CLC   WORK(6),TMPSTA                                                   
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* COMPARE FOR TV                                                                
TKOV20   DS    0H                  TV CAN BE SPECIFIED AS                       
         CLC   WORK(4),TMPSTA      XXXX OR XXXX-T                               
         BNE   TKOVNO                                                           
         CLC   WORK+4(2),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+4                                                    
         BNE   TKOVNO                                                           
         B     TKOVYES                                                          
                                                                                
* AND 3 LETTER CALLS                                                            
TKOV30   DS    0H                                                               
         CLC   WORK(3),TMPSTA                                                   
         BNE   TKOVNO                                                           
         CLC   WORK+3(3),MYSPACES                                               
         BE    TKOVYES                                                          
         CLC   =C'-T',WORK+3                                                    
         BE    TKOVYES                                                          
         B     TKOVNO                                                           
                                                                                
* CHECK FOR COMBO TAKEOVER                                                      
TKOV50   DS    0H                                                               
         XC    RSTAREC(32),RSTAREC                                              
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
                                                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TKOVNO                                                           
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   TKOVNO                                                           
         CLI   WORK+3,C'-'         TAKEOVER COMMENT STATION MUST MATCH          
         BE    TKOV60              PARENT STATION                               
         CLC   RSTACS(4),WORK                                                   
         BNE   TKOVNO                                                           
                                                                                
TKOV60   DS    0H                  INCASE PARENT'S CALL IS 3 LETTERS            
         CLC   RSTACS(3),WORK                                                   
         BE    TKOVYES                                                          
         DROP  R6                                                               
                                                                                
TKOVNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
TKOVYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GETSTAT:  RETRIEVE STATION RECORD                                           
*                                                                               
GETSTAT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           BUILD STATION KEY AND GET RECORD             
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         SPACE                                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GEST0100                                                         
         DC    H'0'                                                             
GEST0100 GOTO1 VGETREC,DMCB,IOAREA                                              
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* UPDATE CONTRACT VERSION                                                       
*                                                                               
UPDTVER  NTR1  BASE=*,LABEL=*                                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                                                               
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    UV20                                                             
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATE                                   
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
*                                                                               
         TM    TWADARE,X'08'                                                    
         BZ    UV10                                                             
         MVI   DMCB+4,X'40'        DON'T SET MANUAL CHANGES INITIATED           
*                                                                               
UV10     DS    0H                                                               
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC)                                     
         BNZ   BUYERROR                                                         
                                                                                
UV20     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    UV30                                                             
*                                                                               
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
UV30     OI    RCONCONF,X'80'      NOT CONFIRMED                                
         B     EXXMOD                                                           
         DROP  R6                                                               
         DROP  R7                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* UPDATE CONTRACT RECORD                                                        
*                                                                               
UPDTCON  NTR1  BASE=*,LABEL=*                                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
*   ESTIMATE BUCKETS HAVE BEEN CHANGED.  IT IS THEREFORE NECESSARY              
*     TO UPDATE THE TRUE ACTIVITY DATE IN THE CONTRACT RECORD.                  
*                                                                               
         BAS   RE,TRUDATE          UPDATE TRUE ACTIVITY DATE                    
*                                                                               
* MOVE K REC TO IOAREA                                                          
         MVC   KEY+28(4),TWAKADDR                                               
*                                                                               
         LA    R2,CONCNUMH         CHECK THE CONTRACT NUMBER ON                 
         OI    CONCNUMH+4,X'08'    SET NUMERIC                                  
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(4,CONCNUMH),(1,FULL)                   
         CLC   RCONKCON,FULL       BEFORE WRITING BACK THE CONTRACT             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         GOTO1 VGETREC,DMCB,AIO4                                                
         BAS   RE,CHECK                                                         
         MVI   DMOUTBTS,0                                                       
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         BAS   RE,CHECK                                                         
         CLI   RCONKGRP,C'T'       IS CONTRACT FOR TV?                          
         BNE   UPCONX              NO  -                                        
         CLI   TRUFLAG,C'Y'        YES - NEED 'EC/SAR' KEY?                     
         BNE   UPCONX              NO  -                                        
         BAS   RE,GENECKEY                                                      
*                                  YES - GEN SAR KEY IF NOT PRESENT             
UPCONX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R7                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*---------------------------------------------------------------------*         
TRUDATE  NTR1                                                                   
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,RCONDATE),(3,TDATELT+5)                           
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 VADDELEM,DMCB,RCONREC,TDATELT                                    
TDAT0040 EQU   *                                                                
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         B     XIT                                                              
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
GENECKEY NTR1                                                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'EC'           ESTABLISH SAR KEY                            
*                                                                               
         MVC   KEY+23(4),TWACNUM                                                
         MVC   KEY+21(2),REPALPHA                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               READ KEY                                     
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    XIT                 ALREADY THERE - DON'T READD                  
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWAKADDR                                               
*                                                                               
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
         EJECT                                                                  
*                                                                               
CHECK    TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                FOR RECOVERY                                 
*                                  -TO MAKE SURE TRANSACTION IS                 
*                                  UNWOUND                                      
         LTORG                                                                  
         DROP  R7                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061RECNT1F   10/08/15'                                      
         END                                                                    
