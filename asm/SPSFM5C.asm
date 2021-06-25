*          DATA SET SPSFM5C    AT LEVEL 135 AS OF 09/14/17                      
*PHASE T2175CA                                                                  
T2175C  TITLE 'SPSFM5C - STATION MASTER TWO'                                    
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* SEP14/17 132 AKAT - GRANT CARAT ACCESS TO DELETE NETWORKS           *         
* OCT05/16 132 AKAT - MAKE SURE THERE ARE NO BUYS BEFORE DELETING NTWK*         
* MAR24/05 125 AKAT - SUPPORT DEMO OVERRIDE SYSCODE                   *         
* JUN 2/98 021 BPOO - CLT SPECIFIC DONT HAVE NETWORKS, DISPLAY MESSAGE*         
* MAY20/98 001 BPOO - XSORT NETWORK OUTPUT LIST AND CHANGE NETWORK    *         
*                     FROM REFERENCE OF CABLE TABLE TO USING MSUNPK   *         
*                     CERTAIN ROUTINES WERE CHANGED ALSO              *         
*                     SUCH AS VALIDATION OF DELETING NETWORKS         *         
* ====== REFORMMATED LEVEL TO 1 ==================================    *         
* MAR26/98 090 BPOO - SCBL64,SCBLSEQ WERE ADDED FOR NEW STAPACK       *         
*                                                                     *         
* OCT28/97 086 MHER   DO NOT ALLOW -L FOR CANADIAN STATIONS           *         
*                                                                     *         
* SEP24/97 085 GLEE - CLEAR  ERROPT  AT  TRAPEND  ERROR EXIT          *         
*                                                                     *         
* MAR13/97 080 EJOR - NO SF X-POINTERS FOR AGY XD                     *         
*                                                                     *         
* SEP18/96 076 SPRI - DISPLAY NTI STATION                             *         
*                                                                     *         
* JUL17/96 075 GLEE - MAKE ROOM FOR A 3RD "CABLE NETWORK LIST" LINE   *         
*              GLEE - MOVED DISPNET ROUTINE TO ITS OWN NMOD           *         
*                                                                     *         
* APR20/95 073 SPRI - RESTRICTIONS FOR ACTIONS ADD/DEL/CHA            *         
*                                                                     *         
* FEB21/95 066 EJOR - ADD NON-SEQUENTIAL STATION NUMBERS (UGH)        *         
*                                                                     *         
* FEB02/95 059 EJOR - ALLOW FORCE OF CANADIAN STATION NUMBER          *         
*                                                                     *         
* JAN30/94 056 GLEE - CHECK CLT DFAULT EXIST BEFORE ADDING CLT EXCPTN *         
*                                                                     *         
* NOV30/94 047 GLEE - SUPPORT ALPHAMKT ON STATION LEVEL FOR RADIO     *         
*                                                                     *         
*  ??????   ?   ??  - HISTORY UNKNOWN                                 *         
***********************************************************************         
T2175C   CSECT                                                                  
         NMOD1 0,T2175C,R7,RR=R8                                                
         PRINT NOGEN                                                            
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         B     MAIN05                                                           
RELO     DS    A                                                                
*                                                                               
MAIN05   XC    ERRDISP,ERRDISP                                                  
         XC    MYMKT,MYMKT                                                      
         XC    MYFORM,MYFORM                                                    
         XC    MYEFFDTE,MYEFFDTE                                                
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    MAIN15                                                           
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    MAIN15                                                           
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN15                                                           
*                                                                               
NOTAUTHD MVI   GETMSYS,2           CHANGE TO X'02' ERROR SYSTEM                 
         LA    R2,CONACTH          ACTION ERROR                                 
         B     TRAPERR                                                          
*                                                                               
MAIN15   DS    0H                                                               
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,SETFILE        SET FILE NAME                                
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BE    DR                                                               
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   CR    RB,RC                                                            
         J     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
SF       DS    0H                                                               
         BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         B     EXIT                                                             
*                                                                               
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       XC    PASSKEY,PASSKEY                                                  
         MVI   CABLE,C'N'                                                       
         LA    R2,SSTMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   VK10                                                             
         CLI   QMED,C'N'           DON'T ALLOW MEDIA N OR C                     
         BE    INVERR                                                           
         CLI   QMED,C'C'                                                        
         BE    INVERR                                                           
*                                                                               
VK10     MVC   QCLT,ZEROES                                                      
         LA    R2,SSTCLIH          CLIENT FIELD                                 
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 VALICLT             CHECK FIRST IF THERE'S A CLIENT              
*                                                                               
VK20     LA    R2,SSTSTAH          STATION FIELD                                
         GOTO1 ANY                                                              
         MVI   ERROR,INVSTAT       ASSUME INVALID STATION INPUT                 
         CLI   8(R2),C'0'          IF IT IS A CABLE INPUT, THEN                 
         BL    VK25                                                             
         CLI   8(R2),C'9'                                                       
         BH    VK25                                                             
         CLI   5(R2),4             L'INPUT S/B <=4!                             
         BH    TRAPERR                                                          
         CLC   =C'0000',8(R2)      DON'T ALLOW STATION 0000                     
         BE    TRAPERR                                                          
*                                                                               
VK25     LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         XC    0(STBLNQ,R4),0(R4)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,QMED         SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,ACOMFACS                                                 
         GOTO1 STAVAL,DMCB,(R4)                                                 
         CLI   STBERR,0                                                         
         BNE   TRAPERR                                                          
         MVC   QSTANEW,STBSTA      SET OUTPUT STATION                           
         CLI   QSTANEW,C'0'        IF THE FIRST CHAR IS A DIGIT                 
         BNL   VK30                                                             
* NOT CABLE - SHOULD BE ALL ALPHA                                               
         LA    R1,QSTANEW+3                                                     
         LA    R0,4                                                             
         CLI   0(R1),C' '          LAST CHAR MAY BE A SPACE                     
         BE    VK28                                                             
VK26     CLI   0(R1),C'A'                                                       
         BL    TRAPERR                                                          
         CLI   0(R1),C'Z'                                                       
         BH    TRAPERR                                                          
VK28     BCTR  R1,0                                                             
         BCT   R0,VK26                                                          
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+12                                                             
         CLI   QSTANEW+4,C'L'      CANADA HAS NO LOW POWER STATIONS             
         BE    TRAPERR                                                          
         B     VK35                                                             
* CABLE                                                                         
VK30     CLI   QSTANEW,C'9'        THEN THIS IS CABLE                           
         BH    VK35                                                             
         MVI   CABLE,C'Y'                                                       
         MVI   QSTANEW+4,C'T'                                                   
         MVC   8(8,R2),QSTANEW                                                  
         MVI   12(R2),C' '                                                      
         CLC   STBNET,SPACES                                                    
         BNH   *+8                                                              
         MVI   12(R2),C'/'                                                      
         OI    6(R2),X'80'                                                      
         CLI   ACTNUM,ACTADD       IF ADDING A CABLE STATION                    
         BNE   VK34                                                             
         CLI   SSTCLIH+5,0         (GLOBAL)                                     
         BNE   VK35                                                             
         CLC   QSTANEW(4),=C'7000' IT CAN ONLY BE BETWEEN 7000 - 7500           
         BL    ADDERR                                                           
         CLC   QSTANEW(4),=C'7500' (THE REST ARE RESERVED FOR NCA)              
         BH    ADDERR                                                           
***                                                                             
* DISPLAY "DEMO OVERRIDE SYSCODE FIELD" IF BETWEEN 7000-7500                    
***                                                                             
VK34     CLC   QSTANEW(4),=C'7000' IT CAN ONLY BE BETWEEN 7000 - 7500           
         BL    VK35                                                             
         CLC   QSTANEW(4),=C'7500' (THE REST ARE RESERVED FOR NCA)              
         BH    VK35                                                             
         DROP  R4                                                               
*                                                                               
         MVC   SSTCMSG,=C'Demo Override Syscode'                                
         OI    SSTCMSGH+6,X'80'                                                 
*                                                                               
         NI    SSTCDEMH+1,X'FF'-X'20'                                           
         OI    SSTCDEMH+6,X'80'                                                 
         B     VK36                                                             
*                                                                               
VK35     XC    SSTCMSG,SSTCMSG     CLEAR THIS FIELD DESCRIPTION                 
         OI    SSTCMSGH+6,X'80'    TRANSMIT                                     
*                                                                               
         OI    SSTCDEMH+1,X'20'    PROTECT                                      
         XC    SSTCDEM,SSTCDEM     CLEAR                                        
         MVI   SSTCDEMH+5,0        NO INPUT                                     
         OI    SSTCDEMH+6,X'80'    TRANSMIT                                     
*                                                                               
VK36     LA    R2,SSTOPTH          OPTIONS                                      
         XC    CANSTA,CANSTA                                                    
         MVI   REACTIVE,C'N'                                                    
         MVI   OLDMKTS,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         CLC   =C'STUPID',8(R2)    CHECK PASSWORD TO REACTIVATE                 
         BNE   *+12                                                             
         MVI   REACTIVE,C'Y'                                                    
         B     VK40                                                             
*                                                                               
         CLC   =C'OLD',8(R2)       DISPLAYS OLD MKT NUMBERS IN NAME FLD         
         BNE   *+12                                                             
         MVI   OLDMKTS,C'Y'                                                     
         B     VK40                                                             
*                                                                               
         CLC   =C'NEXT',8(R2)      FORCE NEXT CANADIAN STATION #                
         BNE   *+14                                                             
         MVC   CANSTA,=X'FFFF'                                                  
         B     VK40                                                             
*                                                                               
         CLC   =C'XNUM=',8(R2)     FORCE SPECIFIC CANADIAN STATION #            
         BNE   INVERR                                                           
         GOTO1 SCANNER,DMCB,0(R2),(1,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         CLI   BLOCK+1,4                                                        
         BNE   INVERR                                                           
         GOTO1 HEXIN,DMCB,BLOCK+22,DUB,4                                        
         OC    DMCB+12(4),DMCB+12                                               
         BZ    INVERR                                                           
         CLC   DUB(2),=X'F000'     COMPARE INPUT TO MAX                         
         BNL   INVERR                                                           
         MVC   CANSTA,DUB                                                       
*                                                                               
VK40     DS    0H                                                               
         USING STARECD,R6                                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   STAKEY(STAKEYLN),ZEROES                                          
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKCALL,QSTANEW    CALL LETTERS                                 
         MVC   STAKAGY,AGENCY      AGENCY                                       
         MVC   STAKCLT,QCLT        CLIENT EXCEPTION                             
*                                                                               
         BAS   RE,SETDEF           SET FILENAME & OTHER DEFAULTS                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
                                                                                
*************************                                                       
VR       DS    0H                                                               
         CLI   CABLE,C'Y'          IS THIS CABLE?                               
         BNE   VRXIT               IF NOT - EXIT                                
*                                                                               
         BRAS  RE,SVNETS           SAVE NETWORK LISTS                           
*                                                                               
         LA    R2,SSTCSNH          GET SYSTEM NAME                              
         OI    4(R2),X'80'                                                      
         GOTO1 ANY                 IT IS REQUIRED                               
         MVI   4(R2),X'80'                                                      
*                                                                               
         BRAS  RE,CHK24            CHECK FOR T24 PROBLEM                        
*                                                                               
         L     R2,AIO                                                           
         USING STARECD,R2                                                       
*                                                                               
* IF RECORD HAS SHORT LENGTH, CLEAR SAVED NETWORK LISTS TO AVOID                
* 'CAN'T DELETE NETWORKS' MESSAGE                                               
         SR    R0,R0                                                            
         ICM   R0,3,STAKLEN                                                     
         CHI   R0,SCBLSQNQ                                                      
         BNL   VR100                                                            
         XC    SVSCBLSQ,SVSCBLSQ                                                
         XC    SVSCBL24,SVSCBL24                                                
VR100    DS    0H                                                               
*                                                                               
         MVC   SSYSNAME,WORK                                                    
*                                                                               
         MVC   SGRPCD,=CL15'AMS'                                                
*                                                                               
         LHI   R0,SCBLSQNQ         CABLE STATION RECORD LENGTH                  
         STCM  R0,3,STAKLEN        ADJUSTED TO INCLUDE NEW FIELDS               
*                                                                               
         CLC   STAKCLT,=C'000'     CLIENT SPECIFIC?                             
         BNE   VR600                                                            
         B     VR287                                                            
*        BE    VR287               IF NO, CONTINUE                              
* CLIENT-SPECIFIC RECORDS CAN'T HAVE ANY NETWORKS IN THE LIST                   
         LA    R1,SSTCNL1H         ADDRESS OF FIRST INPUT LINE                  
         LHI   R5,NETLNUM                                                       
VR200    DS    0H                                                               
         CLI   5(R1),0             ANY INPUT?                                   
         BE    INVERR              SHOULDN'T BE                                 
         ZIC   R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT INPUT LINE                      
         BCT   R5,VR200            MAKE SURE IT LOOPS ONLY 5 TIMES              
*                                                                               
VR287    DS    0H                                                               
*                                                                               
         XC    SCBLSEQ,SCBLSEQ    CLEAR FIELDS IN THE RECORD,                   
         XC    SCBL24,SCBL24     THEY WILL BE BUILT FROM SCREEN LATER           
         XC    NUMPOUND,NUMPOUND                                                
         XC    NUMNON24,NUMNON24                                                
         XC    NUMSCAN,NUMSCAN                                                  
         MVI   CBLINDEX,0          INDEX INTO MYCABTAB                          
*                                                                               
         LHI   R6,NETLNUM          NO OF INPUT LINES FOR NETWORK LIST           
         LA    R4,SSTCNL1H         A(NETWORK LIST LINE 1)                       
* VR290 IS OUTER LOOP, ONE ITERATION FOR EACH NETWORK LIST LINE                 
VR290    DS    0H                                                               
         CLI   5(R4),0             ANY INPUT ON THIS LINE?                      
         BE    VR500               IF NO INPUT - PROCESS NEXT LINE              
         LA    R5,SSTWORK                                                       
         AHI   R5,6000             R5 = 6000 BYTES OFF SSTWORK                  
         GOTO1 SCANNER,DMCB,0(R4),(30,(R5))                                     
         CLI   4(R1),0             ANY INVALID DATA?                            
         BE    INVERR                                                           
*                                                                               
         ZIC   R3,4(R1)            NUMBER OF LINES RETURNED BY SCANNER          
         ZIC   R0,NUMSCAN                                                       
         AR    R0,R3                                                            
         STC   R0,NUMSCAN          SAVE FOR LATER                               
*                                                                               
* VR300 IS INNER LOOP, ONE ITERATION PER EACH NETWORK                           
*                                                                               
VR300    DS    0H                                                               
*                                                                               
* SEE IF WE'RE DELETING THIS NETWORK                                            
*                                                                               
         ZIC   RF,0(R5)            L'DATA                                       
         BCTR  RF,0                                                             
         EX    RF,VRNUMBS                                                       
         BNE   VR310               NO                                           
         SR    R0,R0                                                            
         IC    R0,NUMPOUND                                                      
         AHI   R0,1                                                             
         STC   R0,NUMPOUND         COUNT NUMBER OF "###"S                       
*                                                                               
         LLC   RE,CBLINDEX         CURRENT CABLE INDEX WE'RE PROCESSING         
         MHI   RE,3                INDEX INTO WHAT THIS ### USED TO BE          
         LA    RF,MYCABTAB         THE ORIGINAL NETWORK LIST                    
         LA    RF,0(RE,RF)         POINT TO NETWORK BEING DELETED               
*                                                                               
         LA    RE,DELNLIST         DELETED NETWORK LIST                         
         BCTR  R0,0                CURRENT NUM OF DELETED NETWORKS - 1          
         MHI   R0,3                INDEX INTO DELNLIST                          
         AR    RE,R0               PUT NETWORK BEING DELETED HERE               
         MVC   0(3,RE),0(RF)       SAVE OFF NETWORK BEING DELETED               
*                                                                               
         B     VR330               SKIP VALIDATION AND INSERTION                
*                                                                               
VRNUMBS  CLC   =C'###'(0),12(R5)   ALL '#'S?                                    
*                                                                               
VR310    DS    0H                                                               
         BAS   RE,CHKNET                                                        
*                                                                               
         L     R2,AIO                                                           
         BAS   RE,INSNET                                                        
*                                                                               
VR330    DS    0H                                                               
         LA    R5,32(R5)           BUMP TO NEXT SCANNER ENTRY                   
         LLC   RE,CBLINDEX         CURRENT CABLE INDEX WE'RE PROCESSING         
         AHI   RE,1                BUMP BY 1                                    
         STC   RE,CBLINDEX         NEXT INDEX                                   
         BCT   R3,VR300                                                         
*                                                                               
VR500    DS    0H                  BUMP TO NEXT LINE IN NETWORK LIST            
         ZIC   R0,0(R4)            ZIC LENGTH OF INPUT LINE                     
         AR    R4,R0               INCREASE LINE ADDRESS BY LENGTH              
         BCT   R6,VR290                                                         
*                                                                               
         L     R2,AIO                                                           
         BRAS  RE,SETNLIST         SET THE NEW NETWORK LIST                     
*                                                                               
*&&DO                                                                           
         OC    SVSCBLSQ,SVSCBLSQ   WERE THERE ANY NON-24 NETWORKS ?             
         BZ    VR550               IF NO- DON'T CHECK FOR DELETION              
*                                                                               
***************                                                                 
* PLACEHOLDERS IN SVSCBLSEQ ARE X'FFFF'                                         
* PLACEHOLDERS IN SCBLSEQ ARE X'0000' AS IT WAS REBUILT FROM SCRATCH            
***************                                                                 
         ZIC   RE,SVSEQLEN         ORIGINAL NUMBER OF NON-24 NETWORKS           
         LA    RF,SCBLSEQ          CHANGE NULLS TO FF'S WITHIN ORIG #           
VR510    OC    0(2,RF),0(RF)          TO RETAIN THE PLACEHOLDERS                
         BNZ   *+10                                                             
         MVC   0(2,RF),=X'FFFF'                                                 
         LA    RF,2(RF)                                                         
         BCT   RE,VR510                                                         
*                                                                               
         ZIC   RE,SVSEQLEN         ORIGINAL NUMBER OF NON-24 NETWORKS           
         MHI   RE,2                LENGTH OF EACH FIELD IS 2 NOW...             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SCBLSEQ(0),SVSCBLSQ   ORIGINAL NETWORKS MUST ALL BE IN           
         BNE   CHGERR                                                           
*&&                                                                             
*                                                                               
*                                                                               
VR550    DS    0H                                                               
*        OC    SVSCBL24,SVSCBL24     WERE THERE ANY TOP24 NETWORKS?             
*        BZ    VR600                 IF NO, DON'T CHECK FOR DELETION            
*                                                                               
         L     R2,AIO                                                           
         BRAS  RE,CHKDEL24         CHECK TOP24 LIST FOR DELETES/ADDS            
*                                                                               
VR600    DS    0H                                                               
*                                                                               
         CLI   NUMDEL,0            DO WE HAVE DELETIONS?                        
         BE    VR620               NO - SKIP CHECKS                             
*                                                                               
* YES, WE HAVE DELETIONS!  SEE IF THERE ARE ADDS                                
* FIRST, SEE IF THIS IS DDS TERMINAL                                            
***      CLI   1(RA),C'*'          DDS TERMINAL?                                
***      BNE   CHGERR              NO DELETING FROM NON-DDS TERMINALS           
*                                                                               
         CLI   NUMADD,0            ANY NETWORKS ADDED?                          
         BNE   ADDELERR            CAN'T ADD AND DELETE SIMULTANEOUSLY          
*                                                                               
         CLC   NUMDEL,NUMPOUND     MUST HAVE ### FOR EVERY DELETION             
         BNE   CHGERR                                                           
*                                                                               
         CLC   NUMSCAN,COUNT       TOT NUM OF NETS MUST REMAIN SAME             
         BNE   CHGERR                                                           
*                                                                               
         BRAS  RE,READBUYS         BUY FOUND W/ SYSCD/NTWKS IN DENLIST?         
         BE    CHGERR2             YES                                          
*                                                                               
VR620    DS    0H                                                               
         XC    SCBLLKUP,SCBLLKUP                                                
         CLC   STAKCALL(4),=C'7000'                                             
         BL    VRXIT                                                            
         CLC   STAKCALL(4),=C'7500'                                             
         BH    VRXIT                                                            
*                                                                               
         CLI   SSTCDEMH+5,0          ANY INPUT?                                 
         BE    VRXIT                 NO                                         
*                                                                               
         CLC   STAKCLT,=C'000'       CLIENT SPECIFIC?                           
         BE    *+12                  NO                                         
         LA    R2,SSTCDEMH           DEMO OVERRIDE SYSCODE                      
         B     ERRCLT                YES, ERROR                                 
         DROP  R2                                                               
*                                                                               
         LA    R2,SSTCDEMH           DEMO OVERRIDE SYSCODE                      
         TM    4(R2),X'08'           NUMERIC?                                   
         BZ    NUMERR                NO, ERROR                                  
*                                                                               
         ZIC   R5,5(R2)              CONVERT SYSCODE TO BINARY                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R5,DUB                                                           
         CHI   R5,7000               LESS THAN 7000?                            
         BL    *+12                  YES                                        
         CHI   R5,7500               BETWEEN 7000-7500?                         
         BNH   NUMERR2               YES, ERROR                                 
*                                                                               
         L     R3,AIO                                                           
         USING STARECD,R3                                                       
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'STAKEY),STAKEY                                             
*                                                                               
         LA    R3,KEY                                                           
         MVC   STAKCLT,ZEROES                                                   
         EDIT  (R5),(4,STAKCALL),FILL=0                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'STAKEY),KEYSAVE      HAVE THE SYSCODE?                     
         BNE   ERRNOSYS                   NO, ERROR                             
*                                                                               
         L     R3,AIO                                                           
         CLI   SSYSDACT,X'FF'             DEACTIVATED?                          
         BE    ERRDEACT                   YES, ERROR                            
*                                                                               
         ICM   R4,15,AIO1                                                       
         CLC   SMKT,SMKT-STAKEY(R4)       MARKETS MATCH?                        
         BNE   ERRMKT                     NO, ERROR                             
*                                                                               
*                                  USES AIO1 !!!!!!!                            
         L     R0,AIO2             SET 'TO' ADDRESS                             
         LA    R1,4000             SET 'TO' LENGTH                              
         L     RE,AIO1             SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   KEY(L'STAKEY),STAKEY                                             
         GOTO1 HIGH                                                             
*                                  USES AIO1 !!!!!!!                            
         L     R0,AIO1             SET 'TO' ADDRESS                             
         LA    R1,4000             SET 'TO' LENGTH                              
         L     RE,AIO2             SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
*                                                                               
         STCM  R5,3,SCBLLKUP                                                    
*                                                                               
VRXIT    J     EQXIT                                                            
         DROP  R3                                                               
******************                                                              
* DISPLAY RECORD *                                                              
******************                                                              
DR       DS    0H                                                               
*                                                                               
* CLEAR NETWORK LIST FIELDS                                                     
         LHI   R0,NETLNUM          NUMBER OF LINES IN NETWORK LIST              
         LA    R1,SSTCNL1H                                                      
DR10     DS    0H                                                               
         OI    4(R1),X'A0'                                                      
         OI    6(R1),X'80'                                                      
         ZIC   RE,0(R1)            LENGTH OF RECORD                             
         TM    1(R1),X'02'         EXTENTION?                                   
         BZ    *+8                                                              
         AHI   RE,-8               LENGTH-EXTENTION                             
         AHI   RE,-8               MINUS HEADER                                 
         AHI   R1,8                ADVANCE TO THE FIELD ITSELF                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR THE FIELD                              
         AHI   R1,-8               BACK UP TO HEADING                           
         ZIC   RE,0(R1)                                                         
         AR    R1,RE               ADVANCE TO NEXT LINE                         
         BCT   R0,DR10                                                          
*                                                                               
         XC    SSTCSN,SSTCSN       CLEAR SYSTEM NAME                            
         OI    SSTCSNH+6,X'80'                                                  
         XC    SSTGRPC,SSTGRPC     AND GROUP CODE                               
         OI    SSTGRPCH+6,X'80'                                                 
         XC    SSTCDEM,SSTCDEM     CLEAR DEMO OVERRIDE SYSCODE                  
         OI    SSTCDEMH+6,X'80'                                                 
*                                                                               
         CLI   CABLE,C'Y'                                                       
         BNE   DRX                                                              
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         CLC   KEYSAVE(STAKEYLN),0(R6)   DID WE GET REQUESTED RECORD            
         BNE   RECNFERR                                                         
*                                                                               
         CLC   STAKLEN,=Y(STACRLNQ)                                             
         BL    DRX                                                              
         MVC   SSTCSN,SSYSNAME     SET CABLE SYSTEM NAME                        
         CLC   STAKLEN,=Y(STANCLNQ)                                             
         BL    DRX                                                              
         MVC   SSTGRPC,SGRPCD      GROUP CODE                                   
*                                                                               
         CLC   STAKCLT,=C'000'                                                  
         BE    DR20                                                             
         MVC   SSTCNL1(32),=C'CHECK AGENCY LEVEL MASTER RECORD'                 
         B     DRX                                                              
*                                                                               
         CLC   STAKLEN,=Y(SCBLSQNQ)                                             
         BL    DRX                                                              
*                                                                               
DR20     DS    0H                                                               
         MVC   SVSCBL24,SCBL24     SAVE TOP24                                   
         MVC   SVSCBLSQ,SCBLSEQ    SAVE NON-TOP24 STATIONS                      
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'0000'                                                 
         MVC   WORK+10(5),STAKCALL       STATION CALL LETTERS                   
*  GET THE PACKED MKT/STA SO I MAY USE IT FOR MSUNPK LATER                      
         GOTO1 MSPACK,DMCB,WORK,WORK+10,WORK+15                                 
         MVC   MYMKTSTA,WORK+15                                                 
*                                                                               
* COUNT INITIAL NUMBER OF NON-24 NETWORKS                                       
         LA    R1,SCBLSEQ                                                       
         LHI   R0,0                COUNTER                                      
DR42     DS    0H                                                               
         CHI   R0,103                                                           
         BE    DR43                                                             
         CLC   0(2,R1),=XL2'0'     END OF NON24 NETWORK LIST?                   
         BE    DR43                                                             
         AHI   R0,1                INCREMENT COUNTER                            
         AHI   R1,2                ADVANCE TO NEXT NON24 STATION                
         B     DR42                                                             
DR43     DS    0H                                                               
         STC   R0,SVSEQLEN         SAVE ORIGINAL NO. OF NON-24'S                
*                                                                               
         OC    SCBL24(209),SCBL24    ANYTHING IN NETWORK LIST?                  
         BZ    DR45                                                             
         BAS   RE,DISPNET2                                                      
*                                                                               
DR45     CLC   STAKCALL(4),=C'7000'                                             
         BL    DRX                                                              
         CLC   STAKCALL(4),=C'7500'                                             
         BH    DRX                                                              
         OC    SCBLLKUP,SCBLLKUP                                                
         BZ    DRX                                                              
         EDIT  SCBLLKUP,SSTCDEM,FILL=0                                          
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
*                                                                               
*        MISC & ERROR ROUTINES                                                  
*                                                                               
SETDEF   MVC   SYSDIR,=C'STATION '      SET TO READ STATION FILE                
         MVC   SYSFIL,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'            NO ACTIVITY ELEMENTS                    
         MVC   LKEY,=H'15'              SET LENGTH OF STATION KEY               
         BR    RE                                                               
*                                                                               
RECNFERR MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
CHGERR   DS    0H                                                               
         MVC   SVSCBL24,SV24                                                    
         MVC   SVSCBLSQ,SVSEQ                                                   
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'CANNOT DELETE NETWORK FROM LIST'                  
         B     MSGERR                                                           
*                                                                               
CHGERR2  BRAS  RE,CANTDEL                MOVE ERR MSG TO CONHEAD                
         B     MSGERR                    DISPLAY ERROR MESSAGE                  
*                                                                               
ADDELERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'PLEASE, DELETE FIRST, THEN ADD'                   
         B     MSGERR                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
*        B     TRAPERR                                                          
         GOTO1 ERREX                                                            
*                                                                               
ADDERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  DS    0H                                                               
*&&DO                                                                           
***                                                                             
* ERRDISP IS ALWAYS NULL SO THE FOLLOWING CODE IS COMMENTED OUT                 
***                                                                             
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
*&&                                                                             
TRAPEND  DS    0H                                                               
         MVI   ERROPT,0            NEVER TO RETURN                              
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
ERRMXNET MVC   ERRNUM,=AL2(MAXNET)                                              
         B     SPERREX                                                          
ERRMKT   MVC   ERRNUM,=AL2(1262)   MARKET NUMBERS MUST MATCH                    
         B     SPERREX                                                          
ERRDEACT MVC   ERRNUM,=AL2(1263)   SYSCODE IS DEACTIVATED                       
         B     SPERREX                                                          
ERRNOSYS MVC   ERRNUM,=AL2(1264)   SYSCODE DOES NOT EXIST                       
         B     SPERREX                                                          
NUMERR   MVC   ERRNUM,=AL2(1265)   SYSCODE MUST BE NUMERIC                      
         B     SPERREX                                                          
NUMERR2  MVC   ERRNUM,=AL2(1266)   SYSCODE CANNOT BE IN 7000-7500 RANGE         
         B     SPERREX                                                          
ERRCLT   MVC   ERRNUM,=AL2(1268)   CANNOT HAVE SYSCODE FOR CLT SPECIFIC         
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
         DROP  R7                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
STNKLNQ  EQU   20                  PASSIVE RECORD LENGTH                        
STXKLNQ  EQU   20                  PASSIVE RECORD LENGTH                        
*                                                                               
CHKNET   NTR1                                                                   
* R5 IS EXPECTED TO POINT TO SCANNER TABLE ENTRY                                
* UPON COMPLETION SVSYSNT CONTAINS SEQUENCE NUMBER AND                          
* SVSYSFL CONTAINS NETWORK FLAG.                                                
*                                                                               
*        L     RC,0(R1)            RESTORE RC                                   
*                                                                               
         LR    R2,R4               POINT R2 TO CURRENT INPUT LINE               
*                                                                               
         CLI   0(R5),0             ANY NETWORKS ENTERED AT ALL?                 
         JE    INVERR              IF NONE - GENERATE ERROR                     
         CLI   0(R5),3             NETWORK NAME LONGER THAN 3 CHARS?            
         BH    NETERR              IF LONGER - INVALID NAME                     
         MVC   SVNETWK,12(R5)      SAVE NETWORK NAME TEMPORARILY                
***                                                                             
         CLC   SVNETWK,SPACES      WHO WOULD HAVE THOUGHT USER WOULD            
         BNH   NETERR              TRY ADDING "  ,FXM"                          
***                                                                             
* BUILD THE STAPACK BLOCK                                                       
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPMED,QMED                                                     
         MVC   STAPCTRY,SVAPROF+7                                               
         MVI   STAPACT,C'C'        CHECK CBLNET EXISTS                          
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,=C'0000/'                                               
         MVC   STAPQNET,SVNETWK                                                 
         GOTO1 VSTAPACK,(R4)                                                    
         TM    STAPERR,QSTP_INVCBL IS IT A VALID NETWORK NAME?                  
         BO    NETERR              INVALID CABLE INPUT FROM SCREEN              
*                                                                               
         MVC   SVSYSNT,STAPNSEQ    MOVE SEQUENCE NUMBER INTO SVSYSNT            
         MVC   SVSYSFL,STAPNFLG    MOVE NETWORK FLAG INTOSVSYSFL                
         J     EQXIT               EXIT                                         
*                                                                               
NETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MSG11),MSG11                                           
         MVC   CONHEAD+12(4),12(R5)   MOVE NETWORK TO MESSAGE                   
         MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
MSG11    DC    C'** ERROR ** XXXX IS NOT A VALID CABLE NETWORK'                 
         J     NEQXIT                                                           
         LTORG                                                                  
*        DROP  RB                                                               
*                                                                               
                                                                                
INSNET   NTR1                                                                   
* ACCEPTS NETWORK NUMBER AND FLAG.  CHECKS FOR AVAILABLE SPACE,                 
* DUPLICATE ENTRIES AND INSERTS NETWORK NUMBER INTO RECORD                      
* SVSYSNT IS EXPECTED TO CONTAIN NETWORK NUMBER                                 
* SVSYSFL IS EXPECTED TO CONTAIN NETWORK FLAG                                   
* SVNETWK IS EXPECTED TO HAVE NAME OF CABLE NETWORK                             
* R2 IS EXPECTED TO ADDRESS THE RECORD                                          
*                                                                               
*                                                                               
         USING STARECD,R2                                                       
         LA    R1,SCBLSEQ                                                       
         LA    R3,SCBL24                                                        
*                                                                               
         TM    SVSYSFL,X'40'       ONE OF THE TOP24?                            
         BNO   INS100              NO, INSERT AS A REGULAR NETWORK              
*                                                                               
         NI    SVSYSFL,X'3F'       CLEAR ALL BUT LOWEST 6 BITS                  
         SR    R5,R5                                                            
         IC    R5,SVSYSFL          R5 HAS BIT POSITION FOR THAT NETWORK         
         LA    R4,1                SET LOWEST BIT OF R4(BIT MASK)               
         SLL   R4,24                                                            
*                                                                               
* FOLLOWING LOOP OBTAINS BIT MASK FOR THE TOP24 STATION                         
INS10    DS    0H                                                               
         SRL   R4,1                                                             
         BCT   R5,INS10                                                         
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,7,0(R3)          INSERT TOP 24 INTO R6                        
         OR    R6,R4               SET CORRESPONDING BIT IN TOP24               
         STCM  R6,7,0(R3)          PUT TOP 24 BACK                              
         J     EQXIT                                                            
                                                                                
* FOLLOWING LOOP SEARCHES FOR NEXT EMPTY SPACE TO INSERT NETWORK NO,            
* AND AT THE SAME TIME CHECKS IF NETWORK IS ALREADY IN THE RECORD               
INS100   DS    0H                  INSERTING REGULAR NETWORK                    
         LA    R0,103              MAX NO OF NETWORKS IN LIST                   
         LA    R3,SVSCBLSQ         A(OLD NETWORK LIST)                          
         XC    GAPADDR,GAPADDR                                                  
*                                                                               
INS110   DS    0H                                                               
         CLC   0(2,R3),=X'FFFF'    PLACEHOLDER IN SAVED LIST?                   
         BNE   INS115                                                           
         OC    GAPADDR,GAPADDR     DID WE ALREADY FIND A PLACEHOLDER?           
         BNZ   INS125                                                           
         ST    R3,GAPADDR                                                       
         B     INS125                                                           
*                                                                               
INS115   CLC   0(2,R3),=XL2'0'     PAST LAST ENTRY IN SAVED LIST?               
         BNE   INS120                                                           
         OC    GAPADDR,GAPADDR     YES, WE FIND A PLACEHOLDER?                  
         BZ    INS130                   NO USE NEW LIST                         
         B     INS133                   YES, USE PLACEHOLDER                    
*                                                                               
INS120   CLC   SVSYSNT,0(R3)       IS IT IN SAVED LIST?                         
         BE    INS130                                                           
*                                                                               
INS125   AHI   R1,2                BUMP TO NEXT NETWORK IN SCBLSEQ              
         AHI   R3,2                BUMP TO NEXT NWTWORK IN SVSCBLSQ             
         BCT   R0,INS110                                                        
* ALL REGULAR NETWORKS WERE SEARCHED, NOWHERE TO INSERT NETWORK                 
         OC    GAPADDR,GAPADDR     DID WE ALREADY FIND A PLACEHOLDER?           
         BZ    ERRMXNET                                                         
         B     INS133              YES, WE'RE GOING TO USE IT UP                
*                                                                               
INS130   DS    0H                  WON'T HAVE PLACEHOLDERS IN NEW LIST          
         CLC   0(2,R1),=XL2'0'     WE'LL HAVE TO LOOK THRU MORE IN              
         BE    INS136                THE NEW LIST                               
         CLC   SVSYSNT,0(R1)                                                    
         BE    EQXIT                                                            
         AHI   R1,2                ADVANCE TO NEXT SPOT IN NEW LIST             
         BCT   R0,INS130                                                        
         OC    GAPADDR,GAPADDR     DID WE ALREADY FIND A PLACEHOLDER?           
         BZ    ERRMXNET                                                         
INS133   L     R3,GAPADDR                                                       
         MVC   0(2,R3),SVSYSNT     PUT NEW NTWK IN SAVED LIST                   
         LA    R0,SVSCBLSQ                                                      
         SR    R3,R0                                                            
         LA    R1,SCBLSEQ(R3)      AND IN NEW LIST                              
*                                                                               
INS136   DS    0H                                                               
         CLC   STAKMED(STAKAGY-STAKMED),=C'T8191T'                              
         BNE   INS138                                                           
*                                                                               
         LA    RF,SCBLSEQ+L'SCBLSEQ-4                                           
         CR    R1,RF                                                            
         BNL   ERRMXNET                                                         
*                                                                               
INS138   DS    0H                                                               
         MVC   0(2,R1),SVSYSNT     PUT CODE INTO NETWORK LIST                   
*                                                                               
         ZIC   R0,NUMNON24                                                      
         AHI   R0,1                                                             
         STC   R0,NUMNON24                                                      
*                                                                               
         B     EQXIT               DONE, EXIT SUBROUTINE                        
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
********************                                                            
* DISPLAY NETWORKS *                                                            
********************                                                            
DISPNET2 NTR1                                                                   
*                                                                               
         MVC   SV24,SVSCBL24                                                    
         MVC   SVSEQ,SVSCBLSQ                                                   
*                                                                               
         MVI   LINENUM,0           NETWORK LIST LINE COUNTER                    
         L     R2,AIO                                                           
         USING STARECD,R2                                                       
*                                                                               
         LA    R3,SCBL24                                                        
         DROP  R2                                                               
         LA    R7,1                SET NETWORK POSITION FOR TOP24               
         LA    R5,MYCABTAB                                                      
         XCEF  (R5),382                                                         
*                                                                               
DN2LOOP  TM    0(R3),X'80'         BIT ON AT THIS POSITION?                     
         BZ    DNEXT24             IF NO, GET TO NEXT ONE                       
*                                                                               
         XC    WORK,WORK           IF YES, CALL UNPACK                          
         STC   R7,WORK+4                                                        
         MVI   WORK+2,X'F0'                                                     
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                         
         MVC   0(3,R5),WORK+20     COPY NETWORK NAME INTO WORK                  
         LA    R5,3(R5)            ADVANCE POINTER IN WORK                      
*                                                                               
DNEXT24  ZICM  RE,SVSCBL24,3                                                    
         SLL   RE,1                SHIFT THE BIT LEFT                           
         STCM  RE,7,SVSCBL24                                                    
         LA    R3,SVSCBL24                                                      
         LA    R7,1(R7)                                                         
         OC    SVSCBL24,SVSCBL24   ANY NETWORKS LEFT IN TOP24?                  
         BNZ   DN2LOOP                                                          
*                                                                               
         L     R2,AIO                                                           
         USING STARECD,R2                                                       
         LA    R6,SCBLSEQ                                                       
         DROP  R2                                                               
*                                                                               
         LA    R7,206(R6)           END OF THE LIST                             
         MVI   COUNT,25            NETWORK POSITION COUNTER                     
D2LOOP   CLC   =XL2'0',0(R6)                                                    
         BE    DIDN24                                                           
         CR    R6,R7                                                            
*        BH    DIDN24                                                           
         BE    DIDN24              BUG FIX (SCBLLKUP MAY BE SET)                
*                                                                               
         CLC   =X'FFFF',0(R6)      DON'T DISPLAY THESE PLACEHOLDERS             
         BE    DIDN22                                                           
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(5),MYMKTSTA                                                 
         OC    WORK+4(1),COUNT                                                  
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                         
         MVC   0(3,R5),WORK+20                                                  
*                                                                               
         LA    R5,3(R5)            ADVANCE IN MYCABTAB                          
*                                                                               
DIDN22   LA    R6,2(R6)            ADVANCE TO NEXT NETWORK                      
*                                                                               
         ZIC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         B     D2LOOP                                                           
*                                                                               
DIDN24   DS    0X                                                               
         LA    R5,MYCABTAB                                                      
         XC    COUNT,COUNT                                                      
*   COUNT TOTAL NUMBER OF NETWORKS IN TABLE TO SORT                             
SETCOUNT OC    0(3,R5),0(R5)                                                    
         BZ    SETSORT                                                          
         ZIC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         LA    R5,3(R5)                                                         
         B     SETCOUNT                                                         
*                                                                               
SETSORT  DS    0H                                                               
         LA    R5,MYCABTAB                                                      
         ZIC   R7,COUNT                                                         
         GOTO1 XSORT,DMCB,(0,MYCABTAB),(R7),3,3,0                               
         LA    R6,SVSCBLSQ                                                      
         LA    R5,MYCABTAB                                                      
*                                                                               
         LA    R2,SSTCNL1H         A(NETWORK LIST FIELD HEADER)                 
         ST    R2,CURRLINE         SAVE A(CURRENT NETWORK LIST FIELD)           
         LA    R2,SSTCNL1          A(NETWORK LIST FIELD)                        
         LA    R8,L'SSTCNL1(R2)    A(END NETWORK LIST FIELD)                    
*                                                                               
DNON24   OC    0(3,R5),0(R5)       IF WE SEE ZERO THEN STOP                     
         BZ    DN2X                                                             
*                                                                               
         LR    R0,R2                                                            
         LA    RE,4                                                             
         AR    R0,RE                                                            
         CR    R0,R8                                                            
         BH    DNEWLINE                                                         
         B     DN230                                                            
*                                                                               
DNEWLINE DS    0H                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
*                                                                               
         CLI   LINENUM,NETLNUM     DID WE HIT MAX. NUM OF LINES?                
         BNL   DN2X                IF WE DID - JUST EXIT                        
*                                                                               
         L     R2,CURRLINE         A(HEADER OF CURRENT LINE)                    
         ZIC   R0,0(R2)            L(FIELD+HEADER+EXT)                          
         AR    R2,R0               ADVANCE TO HEADER OF NEXT LINE               
         ST    R2,CURRLINE         NEXT LINE BECOMES CURRENT                    
         ZIC   R0,0(R2)            LENGTH OF NEXT LINE                          
         TM    1(R2),X'02'         IS THERE EXTENTION?                          
         BZ    *+8                                                              
         AHI   R0,-8               MINUS LENGTH OF EXTENTION                    
         AHI   R0,-8               MINUS LENGTH OF HEADER                       
         AHI   R2,8                ADVANCE PAST HEADER                          
         LR    R8,R2                                                            
         AR    R8,R0               A(END OF NETWORK LIST LINE)                  
*                                                                               
         ZIC   R0,LINENUM                                                       
         AHI   R0,1                                                             
         STC   R0,LINENUM          INCREMENT LINE COUNTER                       
*                                                                               
DN230    DS    0H                                                               
         MVC   0(3,R2),0(R5)                                                    
         CLI   2(R5),C' '                                                       
         BNE   DN240                                                            
         MVI   2(R2),C','                                                       
         LA    R2,3(R2)                                                         
         B     *+12                                                             
*                                                                               
DN240    MVI   3(R2),C','                                                       
         LA    R2,4(R2)                                                         
         LA    R5,3(R5)                                                         
         B     DNON24                                                           
*                                                                               
DN2X     DS    0H                                                               
         MVC   SVSCBL24,SV24                                                    
         MVC   SVSCBLSQ,SVSEQ                                                   
*                                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
*                                                                               
         B     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*********                                                                       
* SETUP *                                                                       
*********                                                                       
SETUP    NTR1                                                                   
*                                                                               
         CLC   TWASCR,CALLSTCK                                                  
         BNE   *+12                                                             
         MVI   CALLSTCK,0                                                       
         MVI   CALLSP,0                                                         
*                                                                               
         XC    SSTPFKY,SSTPFKY     CLEAR PFKEY DISPLAY FIELD ON SCREEN          
*                                                                               
         OI    CONSERVH+6,X'80'                                                 
         OI    CONSERVH+1,X'01'                                                 
*                                                                               
         CLI   CALLSP,0        ANYTHING ON STACK?  ANYWHERE TO RETURN?          
         BE    SETUP50                                                          
* YES, CALLSP IS NOT ZERO                                                       
         CLI   PFKEY,04                                                         
         BNE   *+8                 PFKEY=04?                                    
         MVI   PFKEY,X'FF'         DISABLE IT, ONLY PF12 VALID HERE             
         MVC   SSTPFKY+68(11),=C'PF12=RETURN'  DISPLAY PF KEY INFO              
         OI    SSTPFKYH+6,X'80'    TRANSMIT                                     
         B     SETUP100                                                         
*                                                                               
SETUP50  DS    0H                  NOTHING TO RETURN TO, CALLSP=0               
         CLI   PFKEY,12            PFKEY=12?                                    
         BNE   *+8                                                              
         MVI   PFKEY,X'FF'         DISABLE IT, ONLY PF4 VALID HERE              
*                                                                               
         MVC   SSTPFKY(11),=C'PF04=MASTER'                                      
         OI    SSTPFKYH+6,X'80'                                                 
SETUP100 DS    0H                                                               
         OC    PFKEY,PFKEY         ANY PF KEY PRESSED?                          
         BZ    SETUPX           IF NO, MAKES NO SENSE TO CALL INITPFKY          
         GOTO1 INITPFKY,DMCB,PFTABLE                                            
SETUPX   B     EXIT                                                             
*                                                                               
*                                                                               
PFTABLE  DS    0H                                                               
*        SHIFT BACK TO MASTER SCREEN                                            
         DC   AL1(PF04X-*,04,PFTCPROG,(PF04X-PF04)/KEYLNQ,0)                    
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'MASTER'              RECORD                                   
         DC   CL8'DISP'                ACTION                                   
PF04     DC   AL1(KEYTYTWA,L'SSTMED-1),AL2(SSTMED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'SSTSTA-1),AL2(SSTSTA-T217FFD)                      
PF04X    EQU  *                                                                 
                                                                                
*        RETURN CALLER                                                          
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC   X'FF'                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *           
* SCANS ALL NETWORKS IN SCBLSEQ, CHECKING IF ANY BELONG IN TOP 24               
* IF ANY SUCH NETWORK IS FOUND - EXIT WITH AN ERROR                             
* TOP 24 PROBLEM                                                                
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *           
CHK24    NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTEQU,ACTCHA                                                    
         JNE   EQXIT                                                            
*                                                                               
         L     R2,AIO                                                           
         USING STARECD,R2                                                       
         LA    R2,SCBLSEQ                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A9E'  GET A(CABLETAB)                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            RF--> A(CABLETAB)                            
         SR    R0,R0                                                            
         IC    R0,5(RF)            GET TABLE ENTRY LENGTH                       
         N     R0,=X'0000007F'     DROP FLAG                                    
         STH   R0,CBLTABLN                                                      
*                                                                               
         LHI   R0,103              MAX NUMBER OF NETWORKS IN SCBLSEQ            
*                                                                               
CHK2410  OC    0(2,R2),0(R2)       END OF LIST?                                 
         JE    EQXIT                                                            
*                                                                               
         CLC   0(2,R2),=X'FFFF'    PLACEHOLDER?                                 
         BE    CHK2420             YES, NEXT ONE                                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,0(R2)          FETCH CABLE NETWORK CODE                     
         BCTR  R1,0                                                             
         MH    R1,CBLTABLN         INDEX INTO THE TABLE                         
         AR    R1,RF                                                            
*                                                                               
         TM    6(R1),X'40'         IS IT A T24 NETWORK?                         
         BO    CHK24ERR                                                         
CHK2420  LA    R2,2(R2)            ADVANCE TO NEXT NETWORK IN SCBLSEQ           
         BCT   R0,CHK2410                                                       
         J     EQXIT                                                            
*                                                                               
CHK24ERR DS    0H                                                               
         MVC   CONHEAD(35),=CL35'TOP 24 ERROR, PLEASE CONTACT DDS'              
         J     MSGERR                                                           
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SUBROUTINE READS MASTER RECORD AND RE-SAVES NETWORK LISTS                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
SVNETS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVSCBLSQ,SVSCBLSQ                                                
         XC    SVSCBL24,SVSCBL24                                                
         L     R1,AIO                                                           
         USING STAREC,R1                                                        
         CLC   STAKLEN,=Y(SCBLSQNQ)                                             
         BL    SVNETSX                                                          
*                                                                               
         MVC   SAVESTK,KEY                                                      
         MVC   SAVEAIO,AIO                                                      
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         L     R1,AIO                                                           
         CLC   SAVESTK,0(R1)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVSCBLSQ,SCBLSEQ                                                 
         MVC   SVSCBL24,SCBL24                                                  
         MVC   AIO,SAVEAIO                                                      
*                                                                               
* COUNT INITIAL NUMBER OF NON-24 NETWORKS                                       
         LA    R1,SCBLSEQ                                                       
         LHI   R0,0                COUNTER                                      
SVNET10  DS    0H                                                               
         CHI   R0,103                                                           
         BNL   SVNET20                                                          
         CLC   0(2,R1),=XL2'0'     END OF NON24 NETWORK LIST?                   
         BE    SVNET20                                                          
         AHI   R0,1                INCREMENT COUNTER                            
         AHI   R1,2                ADVANCE TO NEXT NON24 STATION                
         B     SVNET10                                                          
*                                                                               
SVNET20  DS    0H                                                               
         STC   R0,SVSEQLEN         SAVE ORIGINAL NO. OF NON-24'S                
*                                                                               
SVNETSX  J     EQXIT                                                            
*                                                                               
SAVESTK  DS    XL15                                                             
SAVEAIO  DS    AL4                                                              
         LTORG                                                                  
         DROP  R1                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SETNLIST - CHECKS NEWLY BUILT NETWORK LIST FOR DELETES/ADDS                   
* REPLACES '00'S WITH 'FF'S - PLACEHOLDERS                                      
* COUNTS DELETIONS AND ADDITIONS                                                
*                                                                               
* R2 IS EXPECTED TO ADDRESS AIO                                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
SETNLIST NTR1  BASE=*,LABEL=*                                                   
         USING STARECD,R2                                                       
* INITIALIZE COUNTS OF ADDED, DELETED NON-TOP24 NETWORKS                        
         XC    NUMADD,NUMADD                                                    
         XC    NUMDEL,NUMDEL                                                    
*                                                                               
* OLD, NEW NUMBER OF THE NON-24 NETWORKS - GET THE HIGHEST OF THE TWO           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,SVSEQLEN         OLD NUMBER OF NON-24 NETWORKS                
         CLC   SVSEQLEN,NUMNON24   HIGHER THAN THE NEW?                         
         BH    *+8                 YES - USE IT                                 
         IC    RE,NUMNON24         NO, USE THE NEW NUMBER                       
*                                                                               
         CHI   RE,0                ANY NON-24 NETWORKS TO WORK WITH?            
         JNH   EQXIT               NO - EXIT, LEAVING COUNTS AT ZERO            
*                                                                               
         LA    RF,SCBLSEQ          NEW LIST                                     
         LA    R1,SVSCBLSQ         OLD LIST                                     
*                                                                               
* CHECK EACH NETWORK IN THE *NEW* LIST                                          
* IT WILL EITHER HAVE A X'0000' OR A NETWORK CODE THERE                         
*                                                                               
SETN10   CLC   0(2,RF),=X'0000'    EMTPY SPACE?                                 
         BNE   SETN30              NO - GOT A NETWORK THERE                     
*                                                                               
* GOT EMPTY SPACE HERE                                                          
*                                                                               
         MVC   0(2,RF),=X'FFFF'    TURN IT INTO A PLACEHOLDER                   
         CLC   0(2,R1),=X'FFFF'    WAS IT A PLACEHOLDER BEFORE?                 
         BE    SETN50              YES - CONTINUE THEN                          
*                                  NO, THERE WAS A NETWORK THERE                
         ZIC   R0,NUMDEL           BUT IT WAS DELETED                           
         AHI   R0,1                INCREMENT DELETIONS COUNT                    
         STC   R0,NUMDEL                                                        
         B     SETN50                                                           
*                                                                               
* GOT A NETWORK CODE HERE                                                       
*                                                                               
SETN30   DS    0H                                                               
         CLC   0(2,RF),0(R1)       SAME AS OLD TABLE?                           
         BE    SETN50              YES - IT WAS THERE BEFORE                    
*                                                                               
         CLC   0(2,R1),=X'0000'    OLD TABLE SPOT EMPTY?                        
         BE    *+6                                                              
         DC    H'0'                IT BETTER BE                                 
*                                                                               
* NEW SPOT HAS A NETWORK CODE, OLD ONE IS EMPTY = WE'RE ADDING                  
*                                                                               
         ZIC   R0,NUMADD           INCREMENT ADDITIONS COUNTS                   
         AHI   R0,1                                                             
         STC   R0,NUMADD                                                        
*                                                                               
SETN50   DS    0H                  NEXT NETWORK IN OLD, NEW LISTS               
         LA    RF,2(RF)                                                         
         LA    R1,2(R1)                                                         
         BCT   RE,SETN10                                                        
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* CHKDEL24 - CHECKS TOP24 LIST FOR DELETES/ADDS                                 
* COUNTS DELETIONS AND ADDITIONS                                                
*                                                                               
* R2 IS EXPECTED TO ADDRESS AIO                                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
CHKDEL24 NTR1  BASE=*,LABEL=*                                                   
         USING STARECD,R2                                                       
*                                                                               
         LA    R3,SCBL24                                                        
         LA    R4,SVSCBL24                                                      
*                                                                               
         ICM   R5,7,SCBL24        SAVE OLD, NEW TOP24                           
         ICM   R6,7,SVSCBL24                                                    
*                                                                               
CHK20    DS    0H                                                               
         TM    2(R4),X'01'         LOWEST-ORDER BIT ON IN ORIGINAL 24?          
         BZ    CHK40              IF NOT, SHIFT BOTH BIT PATTERNS RIGHT         
* CHECK IF SAME POSITION BIT IS ON IN NEW TOP24                                 
         TM    2(R3),X'01'                                                      
         BO    CHK50              NOTHING CHANGED - PROCEED TO NEXT             
*                                 BIT NOT THERE - NETWORK DELETED               
         ZIC   R0,NUMDEL          INCREMENT DELETIONS COUNT                     
         AHI   R0,1                                                             
         STC   R0,NUMDEL                                                        
         B     CHK50              ALSO ZERO, NO CHANGE                          
*                                                                               
CHK40    TM    2(R3),X'01'        LOWEST-ORDER BIT ON IN NEW 24?                
         BZ    CHK50              ALSO ZERO, NO CHANGE                          
*                                 BIT ON - A NEW NETWORK HAS BEEN ADDED         
         ZIC   R0,NUMADD          INCREMENT ADDITIONS COUNT                     
         AHI   R0,1                                                             
         STC   R0,NUMADD                                                        
*                                                                               
CHK50    DS    0H                 SHIFT BOTH BIT PATTERNS RIGHT BY ONE          
         SR    R0,R0                                                            
         ICM   R0,7,0(R4)         OLD TOP 24                                    
         SRL   R0,1                                                             
         STCM  R0,7,0(R4)                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,0(R3)         NEW TOP 24                                    
         SRL   R0,1                                                             
         STCM  R0,7,0(R3)                                                       
*                                                                               
         OC    0(3,R4),0(R4)       ANY STATIONS LEFT?                           
         BNZ   CHK20               IF YES, REPEAT                               
*                                                                               
         STCM  R5,7,SCBL24         RESTORE OLD, NEW TOP24                       
         STCM  R6,7,SVSCBL24                                                    
*                                                                               
         J     EQXIT                                                            
         DROP  R2                                                               
         LTORG                                                                  
***********************************************************************         
* INPUT   : DENLIST - LIST OF 3 CHARACTER NETWORKS USER IS TRYING TO  *         
*                     DELETE FROM THE MASTER2 SCREEN                  *         
*                     EXAMPLE = C'AMCBAYLO GAL'                       *         
*           NUMDEL  - NUMBER OF NETWORKS IN DENLIST                   *         
*                                                                     *         
* PROCESS : STEP 1  - PACK THE NETWORKS IN DENLIST                    *         
*           STEP 2  - SORT THE PACKED NETWORKS IN DENLIST             *         
*                     SORTING ALLOWS FOR BETTER OPTIMIZATION          *         
*           STEP 3  - READ THROUGH THE BUYS LOOKING FOR ANY SYSCODE/  *         
*                     NETWORK COMBINATION IN DENLIST                  *         
*                                                                     *         
* RETURN  : CC EQU  - BUY WITH A SYSCODE/NETWORK IN DENLIST FOUND     *         
*           CC NEQ  - NO BUYS FOUND - OK TO DELETE                    *         
***********************************************************************         
READBUYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,SETSPF           SET SPOT FILE DEFINITION                     
*                                                                               
         L     R2,AIO              A(MASTER RECORD)                             
         USING STARECD,R2          MASTER RECORD DSECT                          
*                                                                               
         LA    R4,DELNLIST         DELETED NETWORK LIST                         
         LLC   R5,NUMDEL           NUMBER OF DELETED NETWORKS                   
*                                                                               
RB00     MVC   WORK(4),STAKCALL    SYSCODE                                      
         MVI   WORK+4,C'/'         /                                            
         MVC   WORK+5(3),0(R4)     NETWORK                                      
*                                                                               
         GOTO1 MSPACK,DMCB,=C'0000',WORK,WORK+10                                
*                                                                               
         MVC   0(3,R4),WORK+12     PACKED SYSCODE/NETWORK                       
         LA    R4,3(R4)            BUMP TO NEXT DELETED NETWORK                 
         BCT   R5,RB00             PROCESS NEXT DELETED NETWORK                 
*                                                                               
         MVC   SYSCODE,WORK+12     LAST SYSCODE/NETWORK PROCESSED               
         NI    SYSCODE+2,X'80'     DROP NETWORK BITS                            
         DROP  R2                  DROP STARECD                                 
*                                                                               
         LLC   R5,NUMDEL           SORT THE PACKED NETWORK/SYSCODES             
         GOTO1 XSORT,DMCB,(0,DELNLIST),(R5),3,3,0                               
*                                                                               
         LA    R6,KEY              R6=BUY RECORD KEY                            
         USING BUYKEY,R6           BUY RECORD DSECT                             
         XC    KEY,KEY             CLEAR THE BUY KEY                            
         MVC   BUYKAM,BAGYMD       A/M                                          
*                                                                               
RB10     MVI   RDUPDATE,C'N'       DON'T LOCK FILE                              
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   BUYKAM,BAGYMD       HAVE THE SAME A/M?                           
         BNE   RBNO                NO - SET CC NEQ (BUY DOES NOT EXIST)         
*                                                                               
         CLI   BUYKPRD,X'FF'       POL PRODUCT?                                 
         BE    RB20                YES                                          
         MVI   BUYKPRD,X'FF'       FORCE POL PRODUCT                            
         XC    BUYKMKT(9),BUYKMKT  CLEAR EVERYTHING AFTER PRODUCT               
         B     RB10                AND READ HIGH                                
*                                                                               
RB20     LA    R4,DELNLIST         DELETED NETWORK LIST                         
*                                                                               
         CLI   BUYKSTA,X'E8'       CABLE BUY?                                   
         BL    RB40                NO - BUMP TO OUR SYSCODE                     
         MVC   WORK(3),BUYKSTA     CURRENT CABLE STATION IN BUY KEY             
         NI    WORK+2,X'80'        STRIP NETWORK BITS                           
         CLC   WORK(3),SYSCODE     IS THE BUY IN THIS SYSCODE?                  
         BL    RB40                NO - LESS SO BUMP TO 1ST SYSCD/NETWK         
         BH    RB30                NO - HIGH SO BUMP TO NEXT MARKET             
*                                                                               
         LLC   R5,NUMDEL           NUMBER OF DELETED NETWORKS                   
*                                                                               
RB25     CLC   BUYKSTA,0(R4)       MATCH ON SYSCODE/NETWORK?                    
         BE    RBYES               YES - HAVE A MATCH - DON'T DELETE!           
         BL    RB40                NO - LESS SO BUMP TO THIS SYSCD/NTWK         
         LA    R4,3(R4)            BUMP TO NEXT DELETED NETWORK                 
         BCT   R5,RB25             PROCESS NEXT DELETED NETWORK                 
*                                                                               
RB30     MVC   BUYKSTA(7),XFF      BUYKSTA > SYSCD/NTWKS FORCE NEXT MKT         
         B     RB10                AND READ HIGH                                
*                                                                               
RB40     MVC   BUYKSTA,0(R4)       BUMP TO SYSCODE/NETWORK IN THIS MKT          
         XC    BUYKEST(4),BUYKEST  CLEAR EVERYTHING AFTER STATION               
         B     RB10                AND READ HIGH                                
*                                                                               
RBYES    BAS   RE,SETSDEF          SET FILENAME & OTHER DEFAULTS                
         MVC   ELEM(13),KEY        SAVE OFF BUY KEY FOR ERROR MESSAGE           
         BAS   RE,RESTRKEY         RESTORE KEY AND READHI                       
         J     YES                 CC EQU                                       
*                                                                               
RBNO     BAS   RE,SETSDEF          SET FILENAME & OTHER DEFAULTS                
         BAS   RE,RESTRKEY         RESTORE KEY AND READHI                       
         J     NO                  CC NEQ                                       
*                                                                               
         DROP  R6                  DROP BUY RECORD USING                        
*                                                                               
SETSDEF  MVC   SYSDIR,=C'STATION ' SET TO READ STATION FILE                     
         MVC   SYSFIL,=C'STATION ' SET TO READ STATION FILE                     
         MVI   USEIO,C'Y'          USEIO = Y                                    
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVC   LKEY,=H'15'         SET LENGTH OF STATION KEY                    
         BR    RE                  RETURN                                       
*                                                                               
SETSPF   MVC   SYSDIR,=C'SPTDIR  ' SET TO READ SPOT DIRECTORY                   
         MVC   SYSFIL,=C'SPTFIL  ' SET TO READ SPOT FILE                        
         MVI   USEIO,C'N'          USEIO = N                                    
         MVC   LKEY,=H'13'         SET LENGTH OF KEY                            
         BR    RE                  RETURN                                       
*                                                                               
RESTRKEY L     R2,AIO              AIO HAS CURRENT CHANGES - NO HIGH            
         USING STARECD,R2          MASTER RECORD DSECT                          
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(L'STAKEY),STAKEY KEY FROM THE RECORD                         
         MVC   KEYSAVE,KEY         RESTORE KEYSAVE & LEAVE AIO AS IS!           
*                                                                               
         BR    RE                  RETURN                                       
         DROP  R2                  DROP MASTER RECORD USING                     
*                                                                               
XFF      DC    XL7'FFFFFFFFFFFFFF' XFF'S                                        
*                                                                               
         LTORG                                                                  
*                                                                               
CANTDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVSCBL24,SV24             SAVE OFF SV24                          
         MVC   SVSCBLSQ,SVSEQ            SAVE OFF SVSEQ                         
         XC    CONHEAD,CONHEAD           OUTPUT ERROR MESSAGE HERE              
         MVC   CONHEAD(7),=C'network'    START OF ERROR MESSAGE                 
         GOTO1 MSUNPK,DMCB,(X'80',ELEM+4),WORK+10,WORK+15                       
         MVC   CONHEAD+8(3),WORK+20      NETWORK NAME                           
         LA    R6,CONHEAD+10             IN CASE 2 CHAR NETWORK                 
         CLI   0(R6),X'40'               3 CHAR NETWORK?                        
         BH    *+6                       YES                                    
         BCTR  R6,0                      NO - BACK UP 1                         
         MVC   2(19,R6),=C'has buys for client'                                 
         GOTO1 CLUNPK,DMCB,ELEM+1,22(R6) UNPACK CLIENT FROM BUY KEY             
         LA    R6,24(R6)                 IN CASE 2 CHAR CLIENT CODE             
         CLI   0(R6),X'40'               3 CHAR CLIENT?                         
         BH    *+6                       YES                                    
         BCTR  R6,0                      NO - BACK UP 1                         
         MVC   2(3,R6),=C'est'           ESTIMATE                               
         EDIT  (B1,ELEM+9),(3,6(R6)),ALIGN=LEFT                                 
         LA    R6,9(R6)                  EST CAN HAVE 1, 2 OR 3 DIGITS          
         CLI   0(R6),X'40'               ESTIMATE DIGIT?                        
         BH    *+8                       YES                                    
         BCT   R6,*-8                    NO - BACK UP 1                         
         MVC   2(15,R6),=C'- cannot delete'                                     
         J     EXIT                      RETURN                                 
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPSFMFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM6AD                                                       
         EJECT                                                                  
         ORG   SSTWORK                                                          
*                                                                               
*********                                                                       
NETLNUM  EQU   7                   NO. OF LINES IN CABLE NETWORK LIST           
*********                                                                       
*                                                                               
SVIO     DS    F                                                                
CURRLINE DS    F                                                                
GAPADDR  DS    F                   WE HAVE A GAP ADDRESS                        
NUMPOUND DS    X                   NUMBER OF "###"S                             
NUMSCAN  DS    X                   NUMBER OF LINES RETURNED BY SCANNER          
NUMADD   DS    X                   NUMBER OF NETWORKS ADDED                     
NUMDEL   DS    X                                      DELETED                   
NUMNON24 DS    X                                      DELETED                   
*                                                                               
MYMKT    DS    CL4                                                              
MYFORM   DS    CL4                                                              
QFORM    DS    CL4                                                              
MYEFFDTE DS    CL(L'SEFFDATE)                                                   
CABLE    DS    CL1                                                              
SVSYSNT  DS    XL2                                                              
SVSYSFL  DS    XL1                                                              
MYBYTE   DS    XL1                                                              
SVNETNEW DS    XL1                                                              
SVNETORG DS    XL1                                                              
BITCNTR  DS    XL1                                                              
BYTECNTR DS    XL1                                                              
REACTIVE DS    CL1                                                              
OLDMKTS  DS    CL1                                                              
LINENUM  DS    X                                                                
SVSYSNET DS    CL(16)                                                           
ORIGKEY  DS    CL(STNKLNQ)                                                      
PASSKEY  DS    CL(STNKLNQ)                                                      
MYKEY    DS    CL(STNKLNQ)                                                      
MYKEY2   DS    CL(STNKLNQ)                                                      
MYKEY3   DS    CL(STNKLNQ)                                                      
PSTOUT   DS    CL64                NEED A 64 BYTE FIELD FOR OUTPUT              
*                                                                               
SVNETWK  DS    CL3                 ALPHA NET, 3 CHARACTERS                      
SVSCBL24 DS    CL3                                                              
SVSCBLSQ DS    CL206                                                            
SV24     DS    CL3                                                              
SVSEQ    DS    CL206                                                            
SVSEQLEN DS    X                                                                
SVNTNEW  DS    XL3                                                              
SVNTORG  DS    XL3                                                              
SVSEQNEW DS    CL206                                                            
MYMKTSTA DS    CL5                                                              
COUNT    DS    X                                                                
MYCABTAB DS    CL382                                                            
CBLINDEX DS    X                                                                
DELNLIST DS    CL382                                                            
SYSCODE  DS    XL3                                                              
*                                                                               
*DDDSLIST DS    0C                                                              
*         DSDDL PRINT=YES                                                       
*                                                                               
         ORG   SSTWORK+6000        LEAVE ROOM FOR GENCON'S STUFF                
SCANBLK  DS    CL1024                                                           
*                                                                               
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLST                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* CTGENFILE (NEED CTDMREC)                                                      
       ++INCLUDE CTGENFILE                                                      
                                                                                
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*===================== SPSFM14 (T2175C) SAVE AREA ====================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
MYSYSDIR DS    CL(L'SYSDIR)        SAVED SYSDIR VALUE BEFORE SETDEF RTN         
MYSYSFIL DS    CL(L'SYSFIL)          "   SYSFIL   "     "      "     "          
MYUSEIO  DS    CL(L'USEIO)           "   USEIO    "     "      "     "          
MYACELOP DS    CL(L'ACTELOPT)        "   ACTELOPT "     "      "     "          
MYLKEY   DS    CL(L'LKEY)            "   LKEY     "     "      "     "          
ERRNUM   DS    XL2                                                              
CBLTABLN DS    H                                                                
*                                                                               
CANSTA   DS    H                   CANADIAN STATION NUMBER                      
NOTAUTH  EQU   0175                ERR-NOT AUTHORIZED FOR THIS FUNCTION         
MAXNET   EQU   0825                MAXIMUM CABLE NETWORKS REACHED               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135SPSFM5C   09/14/17'                                      
         END                                                                    
