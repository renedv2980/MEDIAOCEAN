*          DATA SET PPBUY19    AT LEVEL 025 AS OF 10/17/18                      
*PHASE T41119A                                                                  
*                                                                               
         TITLE 'T41119 - BUY CUSTOM COLUMN UPLOAD'                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* KWAN 09/16/15 VALIDATE COLUMNS FOR MEDIA CODES B/V/W                *         
*                                                                     *         
* KWAN 04/26/13 REMOVE "DATA CHANGED" COMPARISON DUE TO FRENCH TEXTS  *         
*                                                                     *         
* BOBY 06/21/10 IDESK  TRACKED COLUMNS FOR MQ MESSAGE                 *         
*                                                                     *         
* KWAN 06/16/10 IDESK INVOICE COMMUNICATION COLUMNS                   *         
*                                                                     *         
* KWAN 08/05/10 IDESK RECONCILIATION COLUMN CONTROL VIA IDK PROFILE   *         
*                                                                     *         
* KWAN 07/29/09 ADD ERROR REPLY RECORD CODE FOR IDESK                 *         
*                                                                     *         
* KWAN 04/23/08 TRACKING FX RATE CHANGE IN CHGIND5                    *         
*                                                                     *         
* KWAN 10/08/07 MODIFICATION FOR IDESK INSERTION UPLOAD               *         
*                                                                     *         
* KWAN 01/03/07 STANDARIZED COLUMN UPLOAD                             *         
*                                                                     *         
* KWAN 06/15/06 FIX DUPLICATE CUSTOM COLUMN ENTRIES IN BUY RECORD     *         
*                                                                     *         
* KWAN 05/16/06 DATA INTEGRITY VERIFICATION FIX FOR TEXT TYPE CC      *         
*                                                                     *         
* KWAN 05/18/05 FIX ADBUYER BUG, IGNORE INVALID CC PAIRS AT REQ END   *         
*                                                                     *         
* KWAN 05/17/05 FIX ADBUYER BUG, IGNORE INVALID CC PAIRS FOR NEW INS  *         
*                                                                     *         
* KWAN 05/11/05 FIX ADBUYER BUG, IGNORE CC FOR DRAFT MODE             *         
*                                                                     *         
* KWAN 04/14/05 NEED TO MARK CC SFM REC TO INDICATE IT IS USED IN BUY *         
*                                                                     *         
* KWAN 01/07/05 ERROR MSG FIX FOR DATE RANGE                          *         
*                                                                     *         
* KWAN 05/26/04 MEDIA VALIDATION FIX                                  *         
*                                                                     *         
* KWAN 08/21/03 CUSTOM COLUNM UPLOAD                                  *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41119 - BUY CUSTOM COLUMN UPLOAD'                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               T41D05 - BUY CUSTOM COLUMN UPLOAD                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  BUY PHASEES (INDIVIDUAL MEDIA)                        *         
*                                                                     *         
*  COMMENTS     VALIDATES CUSTOM COLUMN DATA MAPS                     *         
*                                                                     *         
*  INPUTS       NO SCREENS                                            *         
*                                                                     *         
*  OUTPUTS      ALL DATA WILL BE RETURNED TO WORKER FILE              *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- LOCAL WORKING STORAGE AREA                      *         
*               R9 -- LOCAL WORKING STORAGE AREA                      *         
*               RA -- TWA                                             *         
*               RB -- BASE REGISTER                                   *         
*               RC -- GEND                                            *         
*               RD -- REGISTER CHAIN                                  *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41119 - BUY CUSTOM COLUMN UPLOAD'                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41119   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK19X-WORK19D,T41119,RR=RE                                     
*                                                                               
         LR    R8,RC                                                            
         USING WORK19D,R8          R8, R9 = A(LOCAL STORAGE)                    
         LA    R9,WORK19D+4095                                                  
         LA    R9,1(R9)                                                         
         USING WORK19D+4096,R9                                                  
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         ST    RE,RELO19                                                        
*                                                                               
         CLI   DDLINKSW,C'N'       ADD INSERTION (NEW)?                         
         BE    PRCCC10                                                          
         CLI   DDLINKSW,C'C'       CHG INSERTION?                               
         BE    PRCCC10                                                          
         CLI   DDLINKSW,C'F'       DRAFT INSERTION (ADD)?                       
         BE    PRCCC10                                                          
*                                                                               
         B     EXXMOD                                                           
*                                                                               
PRCCC10  L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         USING LQ_EL,R3                                                         
*                                                                               
         BRAS  RE,CKCCMAPC         CHK FOR CUSTOM COLUMN MAP CODES              
         BNE   EXXMOD                                                           
*                                                                               
         BRAS  RE,INITLZ           INITIALIZE WORKING STORAGE AREAS             
*                                                                               
PRCCC20  CLI   LQ_EL,0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    PRCCC70             DONE                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   PRCCC60                                                          
*                                                                               
         CLC   =AL2(D#CCSEQN),3(R3)                                             
         BNE   PRCCC60                                                          
*                                                                               
         BRAS  RE,GET_DTLN         RF WILL HAVE DATA LENGTH                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   WORK,C'0'           FOR LEADING ZERO                             
         MVC   WORK+1(L'WORK-1),WORK                                            
         LA    R2,5                MAXIMUM IS FIVE DIGITS                       
         SR    R2,RF               DIFFERENCE IN NUMBER OF DIGIT(S)             
         LA    R4,WORK                                                          
         AR    R4,R2               POINT TO START OF CHAR NUMBERS               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),6(R3)                                                    
*                                                                               
         PACK  DUB,WORK(5)                                                      
         CVB   RE,DUB                                                           
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                CC SEQUENCE NUMBER CANNOT BE ZERO!           
*                                                                               
         STCM  RE,3,SVCCSEQ#       SAVE CUSTOM COLUMN SEQ NUMBER                
*                                                                               
         CLC   SVCCSEQ#,=AL2(8208) IDESK RECONCILIATION?                        
         JE    PRCCC21                                                          
         CLC   SVCCSEQ#,=AL2(8209) IDESK RECON STATUS?                          
         JNE   PRCCC22                                                          
PRCCC21  TM    REC+(PBDSTAT2-PBUYREC),X'20'                                     
         BZ    PRCCC24                                                          
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   PRCCC24                                                          
         CLI   SVIDKPRF+00,C'Y'                                                 
         BNE   PRCCC24                                                          
         LHI   R2,IDSKCERR         CANNOT CHG/DEL IDESK INSERTIONS              
         B     PRCCC50                                                          
*                                                                               
PRCCC22  CLC   SVCCSEQ#,=AL2(8210) IDESK MEDIA COMMENTS?                        
         JE    PRCCC22M                                                         
         CLC   SVCCSEQ#,=AL2(8215) IDESK CAMPAIGN ID?                           
         JE    PRCCC22M                                                         
         J     PRCCC24                                                          
PRCCC22M TM    REC+(PBDSTAT2-PBUYREC),X'20'                                     
         JZ    PRCCC24                                                          
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   PRCCC24                                                          
         LHI   R2,IDSKCERR         CANNOT CHG/DEL IDESK INSERTIONS              
         J     PRCCC50                                                          
*                                                                               
PRCCC24  BRAS  RE,NXTWFELM                                                      
         CLC   =AL2(D#CCFDAT),3(R3)                                             
         BE    PRCCC26                                                          
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    PRCCC60                                                          
         CLI   DDLINKSW,C'N'       ADD INSERTION (NEW)?                         
         BE    PRCCC60                                                          
         CLI   DDLINKSW,C'F'       DRAFT INSERTION (ADD)?                       
         BE    PRCCC60                                                          
         DC    H'0'                SOMETHING ELSE DIFFERENT FROM SPEC           
*                                                                               
PRCCC26  CLI   DDLINKSW,C'N'       ADD INSERTION (NEW)?                         
         BE    PRCCC30                                                          
         CLI   DDLINKSW,C'F'       DRAFT INSERTION (ADD)?                       
         BE    PRCCC30                                                          
         CLI   DDLINKSW,C'C'       CHG INSERTION?                               
         BE    PRCCC40                                                          
         DC    H'0'                INVALID UPLOAD REQUEST                       
*                                                                               
PRCCC30  BRAS  RE,GET_DTLN         RF WILL HAVE DATA LENGTH                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,GETCCREC                                                      
         BE    *+12                                                             
PRCC30_E LHI   R2,NFNDERR          RECORD NOT FOUND ERROR                       
         B     PRCCC50                                                          
*                                                                               
         BRAS  RE,VALCCMC                                                       
         BE    *+12                                                             
         LH    R2,HALF             SAVE RETURNED ERROR NUMBER                   
         B     PRCCC50                                                          
*                                                                               
         BRAS  RE,ADD_CCEL         ADD VALIDATED CC ELEM INTO BUY REC           
         BE    *+12                                                             
         LH    R2,HALF             SAVE RETURNED ERROR NUMBER                   
         B     PRCCC50                                                          
*                                                                               
         B     PRCCC60                                                          
*                                                                               
* FOR CHANGE UPLOAD, EMPTY ELEM MEANS TO REMOVE CC ELEM FROM BUY REC            
*                                                                               
PRCCC40  TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   PRCCC49F                                                         
         BRAS  RE,GET_DTLN         RF WILL HAVE DATA LENGTH                     
         BNZ   PRCCC46             BNZ = OLD CC FIELD DATA PRESENT              
         BRAS  RE,NXTWFELM                                                      
         BRAS  RE,GET_DTLN         RF WILL HAVE DATA LENGTH                     
         BNZ   *+6                                                              
         DC    H'0'                NEW CC FIELD DATA MUST PRESENT               
         CLC   =AL2(D#CCFDAT+1),3(R3)                                           
         BE    *+6                                                              
         DC    H'0'                CC DATA FIELD CHG PAIR OUT OF SYNC!          
         BRAS  RE,GETCCREC                                                      
         BE    PRCCC49M            GO VALIDATE NEW CC FIELD DATA                
         B     PRCC30_E            NOT FOUND ERROR                              
*                                                                               
PRCCC46  LA    R5,REC+33                                                        
         USING BYCCELD,R5                                                       
         MVI   ELCODE,BYCCIDQ                                                   
PRCCC46D BRAS  RE,NXTELEM          POINT TO END OF NEWREC                       
         BE    *+12                                                             
PRCCC46M LHI   R2,DCHGDUER         DATA CHANGED BY DIFFERENT USER(S)            
         B     PRCCC50                                                          
*                                                                               
         CLC   BYCCSQN,SVCCSEQ#    SAME CC SEQUENCE NUMBER?                     
         BNE   PRCCC46D                                                         
         BRAS  RE,GETCCREC                                                      
         BE    *+8                                                              
         B     PRCC30_E            NOT FOUND ERROR                              
*                                                                               
         BRAS  RE,VALCCMC          WILL CONVERT "OLD" CC DATA FIELD             
         BE    *+6                                                              
         DC    H'0'                DATA MUST BE CORRECT!                        
         XC    WRKELEM,WRKELEM                                                  
         BRAS  RE,GET_DTLN         RF WILL HAVE DATA LENGTH                     
         BNZ   *+6                                                              
         DC    H'0'                CC FIELD DATA MUST PRESENT                   
*                                                                               
         CLI   WORK,C'T'           TEXT?                                        
         BNE   PRCCC46P                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKELEM(0),6(R3)    GET CC FIELD DATA TEXT                       
         B     PRCCC48                                                          
*                                                                               
PRCCC46P CLI   WORK,C'D'           DATE?                                        
         BNE   *+14                                                             
         MVC   WRKELEM(3),WORK+1                                                
         B     PRCCC48                                                          
*                                                                               
         CLI   WORK,C'P'           PERIOD?                                      
         BNE   *+14                                                             
         MVC   WRKELEM(6),WORK+1                                                
         B     PRCCC48                                                          
*                                                                               
         MVC   WRKELEM(8),WORK+1   ALL NUMBERS ARE PL8                          
*                                                                               
         USING BYCCELD,R5                                                       
PRCCC48  SR    RE,RE                                                            
         IC    RE,BYCCLEN                                                       
         SHI   RE,BYCCHDRL         ELEM OVERHEAD                                
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                CUSTOM COLUMN ELEM CANNOT BE EMPTY!          
*                                                                               
         BCTR  RE,0                FOR EX                                       
         LA    RF,BYCCDATA                                                      
         AR    RF,RE               POINT TO LAST CHAR                           
PRCCC48A CLI   0(RF),0             NULL?                                        
         JE    PRCCC48F                                                         
         CLI   0(RF),C' '          SPACE?                                       
         JE    PRCCC48F                                                         
         CLI   0(RF),X'0D'         CARRIAGE RETURN?                             
         JE    PRCCC48F                                                         
         CLI   0(RF),X'15'         NEW LINE?                                    
         JE    PRCCC48F                                                         
         J     PRCCC48K                                                         
PRCCC48F BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         J     PRCCC48A                                                         
*                                                                               
PRCCC48K DS    0H                                                               
*                                                                               
* COMPARISON REMOVED DUE TO COPIED/PASTED FRENCH TEXTS                          
*                                                                               
* * * *  EX    RE,*+8                                                           
* * * *  B     *+10                                                             
* * * *  CLC   WRKELEM(0),BYCCDATA CUSTOM COLUMN DATA IS STILL SAME?            
* * * *  BNE   PRCCC46M            BNE = DATA HAS BEEN CHANGED                  
*                                                                               
         MVC   WRKCCREM,BYCCSQN    PASS CC CODE TO BE REMOVED                   
         BRAS  RE,SETCCREM         SET CC TO BE REMOVED TABLE                   
*                                                                               
* REMOVE COMPARED OLD CUSTOM COLUMN ELEM FOR NEXT ROUND                         
*                                                                               
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
         DROP  R5                                                               
*                                                                               
         BRAS  RE,NXTWFELM                                                      
*                                                                               
         CLC   =AL2(D#CCFDAT+1),3(R3)                                           
         BE    *+6                                                              
         DC    H'0'                CC DATA FIELD CHG PAIR OUT OF SYNC!          
*                                                                               
PRCCC49F BRAS  RE,GET_DTLN                                                      
         BNZ   PRCCC49K                                                         
*                                  NO DATA FOR CC                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    *+14                                                             
         MVC   WRKCCREM,SVCCSEQ#   PASS CC CODE TO BE REMOVED                   
         BRAS  RE,SETCCREM         SET CC TO BE REMOVED TABLE                   
*                                                                               
         L     R4,FULL             POINT TO CC TABLE ENTRY                      
         USING CCVTD,R4            ESTABLISH TABLE ENTRY                        
*                                                                               
         TM    CCV_TRK,BYCCTRKQ    IF CUSTCOL IS BEING TRACKED                  
         BNO   *+8                                                              
         OI    CHGIND5,PCHGTRKQ       SET CUSTCOL AS CHANGED                    
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     PRCCC60             REMOVING CC ELEM IN BUY RECORD               
*                                                                               
PRCCC49K BRAS  RE,GETCCREC                                                      
         BE    *+8                                                              
         B     PRCC30_E            NOT FOUND ERROR                              
*                                                                               
PRCCC49M BRAS  RE,VALCCMC                                                       
         BE    *+12                                                             
         LH    R2,HALF             SAVE RETURNED ERROR NUMBER                   
         B     PRCCC50                                                          
*                                                                               
         BRAS  RE,ADD_CCEL         ADD VALIDATED CC ELEM INTO BUY REC           
         BE    *+12                                                             
         LH    R2,HALF             SAVE RETURNED ERROR NUMBER                   
         B     PRCCC50                                                          
*                                                                               
         B     PRCCC60                                                          
*                                                                               
PRCCC50  BRAS  RE,BLDDDEL          SHOULD ONLY BUILD ONE DOWNLOAD ELEM          
         ST    R2,FULL             R2 HAS ERROR NUMBER                          
         BRAS  RE,BLDERREL                                                      
*                                                                               
PRCCC60  CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    PRCCC20             NO NEED TO BUMP OVER                         
         BRAS  RE,NXTWFELM                                                      
         B     PRCCC20                                                          
*                                                                               
PRCCC70  CLI   ERRAREA,X'FF'       ERROR OCCURED?                               
         BE    PRCCC80                                                          
         GOTOR MRKCCREC            MARK CC SFM REC AS BEING USED                
*                                                                               
PRCCC80  DS    0H                                                               
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
NXTWFELM SR    R0,R0               POINT TO NEXT WORKER FILE ELEM               
         ICM   R0,3,1(R3)                                                       
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
NXTELEM  SR    R0,R0               R5 POINTS TO FIRST BUY RECORD ELEM           
         IC    R0,1(R5)                                                         
         AR    R5,R0               FIRST ELEM IS ALWAYS X'20'                   
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
GET_ETXT L     R2,FULL             MSG NUMBER FROM CALLER                       
         ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R2),0,(C'E',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
GET_DTLN SR    RF,RF                                                            
         ICM   RF,3,1(R3)                                                       
         SHI   RF,6                DATA LENGTH                                  
         LTR   RF,RF                                                            
         BR    RE                                                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
SETCCREM DS    0H                  SET CC CODES TO BE REMOVED TABLE             
         L     RF,D_CCREMT         DISPLACEMENT TO CC REMOVE TABLE              
         OC    0(L'WRKCCREM,RF),0(RF)                                           
         JZ    *+12                                                             
         LA    RF,L'WRKCCREM(RF)   NEXT ENTRY                                   
         J     *-14                                                             
         MVC   0(L'WRKCCREM,RF),WRKCCREM                                        
         LH    RF,CCREMCNT                                                      
         CHI   RF,CCREMMXQ                                                      
         JNH   *+6                                                              
         DC    H'0'                TABLE IS MAXED                               
         AHI   RF,1                                                             
         STH   RF,CCREMCNT                                                      
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCREADHI NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEYSAVE(L'KEY),KEY                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',KEY,KEY,        +        
               (TERMNAL,0)                                                      
         CLC   KEY(25),KEYSAVE     RECORD FOUND?                                
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCGETREC NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         LA    R5,LWRKREC                                                       
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFILE',KEY+27,(R5),   +        
               (TERMNAL,DMWORK)                                                 
         CLI   8(R1),0                                                          
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCPUTREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,LWRKREC                                                       
         GOTO1 VDATAMGR,DMCB,(0,=C'PUTREC'),=C'PRTFILE',KEY+27,(R5),   +        
               (TERMNAL,DMWORK)                                                 
         CLI   8(R1),0                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* CHECKING FOR CUSTOM COLUMN MAP CODES (R3 POINTS WRK ELEM)           *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCCMAPC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LQ_EL,0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                CORRUPTED WRK ELEM                           
*                                                                               
CKCCMC20 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    CKCCMCX             DONE, NO CC MAP CODES FOUND                  
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   CKCCMC40                                                         
*                                                                               
         CLC   =AL2(D#CCSEQN),3(R3)                                             
         BE    CKCCMCX                                                          
*                                                                               
CKCCMC40 BRAS  RE,NXTWFELM         POINT TO NEXT WRK FILE ELEM                  
         B     CKCCMC20                                                         
*                                                                               
CKCCMCX  DS    0H                                                               
         J     SETCCEQ                                                          
*                                                                               
CKCCMCER DS    0H                                                               
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITLZ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,LWRKREC          LOCAL WORKING STORAGE REC AREA               
         LHI   R1,4096                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,CCVALTAB         TABLE OF CC VALIDATION ENTRIES               
         LHI   R1,MAX_CCQ*CCVTLNQ+(L'CCVALTBX)                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
* TABLE OF CUSTOM COLUMN SEQUENCE NUMBER TO BE REMOVED FOR CHG UPLOAD           
*                                                                               
         LH    RE,=Y(CCREMTAB-GENOLD)                                           
         AR    RE,RC                                                            
         XC    0(2*CCREMMXQ,RE),0(RE)                                           
         ST    RE,D_CCREMT         DISPLACEMENT TO CC REMOVE TABLE              
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVI   WRKSWTCH,0                                                       
         MVI   WRKSW,0                                                          
         XC    CCREMCNT,CCREMCNT   FOR BUILDING CC REMOVAL TABLE                
*                                                                               
         XC    SVIDKPRF,SVIDKPRF   IDESK CONTROL PROFILE VALUES                 
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),BUYMD                                                 
         MVC   WORK+07(3),BUYCL                                                 
         CLI   SVCLTOFC,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFC                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),SVIDKPRF,VDATAMGR                         
*                                                                               
INIZX    DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TO LOOK UP CUSTOM COLUMN RECORD AND STORE IT IN TABLE                         
*                                                                               
* SVCCSEQ# = CC SEQUENCE NUMBER TO BE LOOKED UP                                 
*                                                                               
* FULL     = RETURN ADDRESS OF CC VALIDATION ENTRY IN TABLE                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCCREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,CCVALTAB         POINT TO CC VALIDATION TABLE                 
         USING CCVTD,R4                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
GCCR20   CHI   RF,MAX_CCQ          MAX NUMBER OF CC ENTRIES REACHED?            
         BL    *+6                                                              
         DC    H'0'                TABLE IS TOO SMALL                           
         ICM   RE,3,CCV_SQN                                                     
         CHI   RE,0                END OF TABLE?                                
         BE    GCCR40                                                           
         CH    RE,SVCCSEQ#         ALREADY IN TABLE?                            
         BE    GCCR80                                                           
         LA    R4,CCVTLNQ(R4)      NEXT ENTRY IN TABLE                          
         AHI   RF,1                                                             
         B     GCCR20                                                           
*                                                                               
GCCR40   MVC   WRKSVKEY,KEY        SAVE ORIGINAL KEY                            
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PCOLPKEY,RE                                                      
         MVC   PCOLPAGY,AGYALPHA                                                
         MVI   PCOLPMED,C'A'       ALWAYS "A" (FOR CUSTOM COLUMN)               
         MVI   PCOLPRCD,X'D1'                                                   
         MVC   PCOLPSQN,SVCCSEQ#                                                
         XC    PCOLPSQN,=X'FFFF'                                                
         DROP  RE                                                               
*                                                                               
         BRAS  RE,CCREADHI                                                      
         BE    GCCR46                                                           
*                                                                               
         XC    WRKTEMP1,WRKTEMP1                                                
         LA    RE,WRKTEMP1                                                      
         USING GCOLPKEY,RE                                                      
         MVC   GCOLPRID,=AL3(GCOLPRIQ)                                          
         MVI   GCOLPMED,C'A'                                                    
         MVI   GCOLPRCD,GCOLPRCQ                                                
         MVC   GCOLPSQN,SVCCSEQ#                                                
         XC    GCOLPSQN,=X'FFFF'                                                
         DROP  RE                                                               
         MVC   WRKTEMP2,WRKTEMP1                                                
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',WRKTEMP1,WRKTEMP1            
         CLC   WRKTEMP1(L'GCOLPKEY),WRKTEMP2                                    
         BNE   GCCRERR                                                          
         LA    RF,LWRKREC                                                       
         GOTOR VDATAMGR,DMCB,=C'GETREC',=C'GENFIL',WRKTEMP1+36,        +        
               (RF),WRKTEMP2                                                    
         CLI   DMCB+8,0                                                         
         BNE   GCCRERR                                                          
         LA    R5,LWRKREC                                                       
         LA    R5,(GCOLFRST-GCOLKEY)(R5)                                        
         OI    WRKSW,STDCOLMQ                                                   
         B     GCCR48                                                           
*                                                                               
GCCR46   BRAS  RE,CCGETREC                                                      
         BNE   GCCRERR                                                          
*                                                                               
         LA    R5,LWRKREC                                                       
         LA    R5,PCOLELEM-PCOLKEY(R5)                                          
         USING PCOLELEM,R5                                                      
GCCR48   CLI   PCOLELEM,X'61'      FIRST CC ELEM PRESENT?                       
         BNE   GCCRERR                                                          
         MVC   CCV_SQN,SVCCSEQ#    CUSTOM COLUMN SEQUENCE NUMBER                
         MVC   CCV_MED,PCOLMED     MEDIA CODE INDICATOR                         
         MVC   CCV_MED2,PCOLMED2   MEDIA CODE INDICATOR 2                       
         MVC   CCV_TYP,PCOLTYP     DATA TYPE                                    
         MVC   CCV_LEN,PCOLMLEN    MAX FIELD LENGTH                             
         MVC   CCV_DEC,PCOLDECS    NUMBER OF DECIMAL                            
         MVC   CCV_STA,PCOLSTAT    STATUS BYTE                                  
*                                                                               
         CLI   PCOLTRK,C'Y'        IF TRACKING THIS CUSTCOL                     
         BNE   *+8                                                              
         OI    CCV_TRK,BYCCTRKQ       SET TRACKING INDICATOR                    
*                                                                               
         TM    WRKSW,STDCOLMQ      STANDARIZED COLUMN?                          
         BZ    *+12                                                             
         OI    CCV_STA,X'80'       MARK IT AS USED                              
         NI    WRKSW,X'FF'-STDCOLMQ                                             
*                                                                               
         DROP  R5,R4                                                            
*                                                                               
GCCR80   ST    R4,FULL             PASS BACK ADDRESS OF ENTRY IN TABLE          
*                                                                               
GCCRX    DS    0H                                                               
         MVC   KEY,WRKSVKEY        RESTORE ORIGINAL KEY                         
         J     SETCCEQ                                                          
*                                                                               
GCCRERR  DS    0H                  SOMETHING WRONG WITH CC RECORD               
         MVC   KEY,WRKSVKEY        RESTORE ORIGINAL KEY                         
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CONVERTING CHAR CUSTOM COLUMN FIELD DATA MAP ELEM IN WORKER FILE              
* TO BINARY OR PACK (NOTE: NO CONVERSION IS NEEDED FOR TEXT TYPE)               
*                                                                               
* R3 = CUSTOM COLUMN FIELD DATA MAP ELEM IN WORKER FILE                         
* R4 = CUSTOM COLUMN VALIDATION ENTRY IN TABLE                                  
*                                                                               
* WRKELEM = WILL RETURN CONVERTED DATA (INCLUDING TEXT)                         
*                                                                               
* HALF    = ON ERROR, WILL RETURN ERROR MSG NUMBER                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CVT_WKCC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CCVTD,R4                                                         
         XC    WRKELEM,WRKELEM     PREPARE RETURNING VALUE                      
         XC    HALF,HALF           ON ERROR, NEED TO RETURN ERROR #             
*                                                                               
         BRAS  RE,GET_DTLN         RF WILL HAVE DATA LENGTH                     
         BNZ   *+12                                                             
         LHI   RE,MSSNGERR         MISSING DATA                                 
         B     CWKCCERR                                                         
*                                                                               
* R6 WILL HAVE LENGTH OF MAP DATA                                               
*                                                                               
         LR    R6,RF                                                            
         BCTR  RF,0                FOR EX                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WRKELEM(0),6(R3)    GET CC FIELD DATA                            
*                                                                               
         CLI   CCV_TYP,C'T'        TEXT?                                        
         BE    CWKCCX                                                           
*                                                                               
         CLI   CCV_TYP,C'D'        DATE?                                        
         BNE   CWKCC20                                                          
         GOTO1 VPERVAL,DMCB,((R6),6(R3)),(X'40',WRKELEM)                        
         CLI   DMCB+4,X'01'                                                     
         BNE   *+12                                                             
         LHI   RE,INVDTERR         INVALID DATE FORMAT                          
         B     CWKCCERR                                                         
*                                                                               
         LA    RE,WRKELEM          VALIDATED DATE IS XL3                        
         USING PERVALD,RE                                                       
         MVC   WORK(L'PVALBSTA),PVALBSTA                                        
         XC    WRKELEM,WRKELEM                                                  
         MVC   WRKELEM(L'PVALBSTA),WORK                                         
         DROP  RE                                                               
         B     CWKCC90             VALIDATION IS COMPLETE                       
*                                                                               
CWKCC20  CLI   CCV_TYP,C'P'        PERIOD?                                      
         BNE   CWKCC30                                                          
         GOTO1 VPERVAL,DMCB,((R6),6(R3)),(0,WRKELEM)                            
         CLI   DMCB+4,X'00'                                                     
         BE    *+12                                                             
         LHI   RE,CCNGPRD          INVALID DATE RANGE                           
         B     CWKCCERR                                                         
*                                                                               
         LA    RE,WRKELEM          VALIDATED PERIOD IS XL6                      
         USING PERVALD,RE                                                       
         MVC   WORK(L'PVALBSTA+L'PVALBEND),PVALBSTA                             
         XC    WRKELEM,WRKELEM                                                  
         MVC   WRKELEM(L'PVALBSTA+L'PVALBEND),WORK                              
         DROP  RE                                                               
         B     CWKCC90             VALIDATION IS COMPLETE                       
*                                                                               
CWKCC30  CLI   CCV_TYP,C'N'        NUMERIC?                                     
         BNE   CWKCC40                                                          
         MVC   BYTE,CCV_DEC                                                     
         OI    BYTE,X'80'          CASHVAL OUTPUT WILL BE PL8                   
         GOTO1 VCASHVAL,DMCB,(BYTE,6(R3)),(X'C0',(R6))                          
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         LHI   RE,DATNT#ER         NOT NUMERIC ERROR                            
         B     VCCMCERR                                                         
         MVI   WORK+0,C'N'                                                      
         MVC   WORK+1(8),4(R1)                                                  
         B     CWKCC90             VALIDATION IS COMPLETE                       
*                                                                               
CWKCC40  CLI   CCV_TYP,C'$'        DOLLARS?                                     
         BNE   CWKCC50                                                          
         B     CWKCC90             VALIDATION IS COMPLETE                       
*                                                                               
CWKCC50  CLI   CCV_TYP,C'%'        PERCENT?                                     
         BE    CWKCC60                                                          
         B     CWKCC90             VALIDATION IS COMPLETE                       
*                                                                               
CWKCC60  LHI   RE,INVDTYER         INVALID DATA TYPE                            
         B     CWKCCERR                                                         
*                                                                               
         LHI   RE,DATNT#ER         DATA IS NOT A VALID NUMERIC ENTRY            
         B     CWKCCERR                                                         
*                                                                               
*                                                                               
CWKCC90  DS    0H                                                               
*                                                                               
CWKCCX   DS    0H                                                               
         J     SETCCEQ                                                          
*                                                                               
CWKCCERR DS    0H                  RE HAS ERROR NUMBER ALREADY                  
         STH   RE,HALF                                                          
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUILD AND ADD VALIDATED CUSTOM COLUMN DATA INTO BUY REC (NEWREC)              
*                                                                               
* WORK+0   = ONE BYTE DATA TYPE (NOTE: %,$,N TYPES ARE ALL PL8)                 
* WORK+1   = VALIDATED CUSTOM COLUMN DATA (EXCEPT FOR TEXT)                     
* R3       = CC MAP ELEM (TEXT DATA)                                            
*                                                                               
* HALF    = ON ERROR, WILL RETURN ERROR MSG NUMBER                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADD_CCEL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,NEWREC+33                                                     
         CLI   0(R5),X'20'         FIRST BUY ELEM PRESENT?                      
         BE    *+12                                                             
         LHI   RE,INVRECER         INVALID RECORD                               
         B     ACCELERR                                                         
*                                                                               
         MVI   ELCODE,X'FF'                                                     
         BRAS  RE,NXTELEM          POINT TO END OF NEWREC                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,WRKELEM                                                       
         USING BYCCELD,R4                                                       
         XC    WRKELEM,WRKELEM     BUILD NEW CC ELEM                            
         MVI   WRKELEM+0,BYCCIDQ   ELEM CODE                                    
*                                                                               
         L     RF,FULL             POINT TO CC TABLE ENTRY                      
         USING CCVTD,RF            ESTABLISH TABLE ENTRY                        
*                                                                               
         OC    BYCCSWS,CCV_TRK     UPDATE BYCCELD                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   BYCCSQN,SVCCSEQ#                                                 
         CLI   DDLINKSW,C'C'       CHG INSERTION?                               
         BNE   *+18                                                             
         CLC   BYCCSQN,=X'200F'    FXRATE STANDARD CUSTOM COLUMN CODE?          
         BNE   *+8                                                              
         OI    CHGIND5,PCHFXRTQ                                                 
         SR    RE,RE                                                            
*                                                                               
         CLI   WORK,C'T'           CC FIELD DATA IS TEXT?                       
         BNE   ACCEL30                                                          
         ICM   RE,3,1(R3)                                                       
         SHI   RE,6                CC FIELD DATA LENGTH                         
         AHI   RE,BYCCHDRL         ADD CC ELEM OVERHEAD                         
         STC   RE,WRKELEM+1                                                     
         SHI   RE,BYCCHDRL+1       MINUS OVERHEAD AND ONE FOR EX                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BYCCDATA(0),6(R3)   TEXT TO BE MOVED INTO CC ELEM                
         B     ACCEL60                                                          
*                                                                               
ACCEL30  CLI   WORK,C'N'           CC FIELD DATA IS NUMBER?                     
         BE    ACCEL32                                                          
         CLI   WORK,C'$'           CC FIELD DATA IS DOLLAR?                     
         BE    ACCEL32                                                          
         CLI   WORK,C'%'           CC FIELD DATA IS PERCENT?                    
         BNE   ACCEL36                                                          
ACCEL32  LHI   RE,BYCCHDRL+8       OVERHEAD AND PL8                             
         STC   RE,WRKELEM+1                                                     
         MVC   BYCCDATA(8),WORK+1                                               
         B     ACCEL60                                                          
*                                                                               
ACCEL36  CLI   WORK,C'D'           CC FIELD DATA IS DATE?                       
         BNE   ACCEL40                                                          
         LHI   RE,BYCCHDRL+3       OVERHEAD AND XL3                             
         STC   RE,WRKELEM+1                                                     
         MVC   BYCCDATA(3),WORK+1                                               
         B     ACCEL60                                                          
*                                                                               
ACCEL40  CLI   WORK,C'P'           CC FIELD DATA IS PERIOD?                     
         BNE   ACCEL50                                                          
         LHI   RE,BYCCHDRL+6       OVERHEAD AND XL6                             
         STC   RE,WRKELEM+1                                                     
         MVC   BYCCDATA(6),WORK+1                                               
         B     ACCEL60                                                          
*                                                                               
ACCEL50  LHI   RE,INVDTYER         INVALID DATA TYPE                            
         B     ACCELERR                                                         
*                                                                               
ACCEL60  GOTOR VRECUP,DMCB,(1,NEWREC),WRKELEM,(R5)                              
*                                                                               
         CLI   DDLINKSW,C'C'       CHG INSERTION?                               
         BNE   *+14                                                             
         MVC   WRKCCREM,SVCCSEQ#   PASS CC CODE TO BE REMOVED                   
         BRAS  RE,SETCCREM         SET CC TO BE REMOVED TABLE                   
*                                                                               
         LA    R4,CCVALTAB                                                      
         USING CCVTD,R4                                                         
ACCEL62  OC    CCV_SQN,CCV_SQN                                                  
         BNZ   *+6                                                              
         DC    H'0'                VALIDATED CC MUST BE IN TABLE!               
         CLC   CCV_SQN,SVCCSEQ#    CC SEQ MATCH THAT OF TABLE?                  
         BE    *+12                                                             
         LA    R4,CCVTLNQ(R4)      NEXT ENTRY IN TABLE                          
         B     ACCEL62                                                          
*                                                                               
         OI    CCV_IND,CCIUSEDQ    CUSTOM COLUMN IS USED                        
*                                                                               
         CLI   DDLINKSW,C'N'       IF ADD INSERTION (NEW)?                      
         BE    *+8                                                              
         CLI   DDLINKSW,C'C'       OR CHG INSERTION?                            
         BNE   ACCEL85                                                          
*                                                                               
         TM    CCV_TRK,BYCCTRKQ    IF CUSTCOL IS BEING TRACKED                  
         BNO   *+8                                                              
         OI    CHGIND5,PCHGTRKQ       SET CUSTCOL AS CHANGED                    
*                                                                               
ACCEL85  DS    0H                                                               
*                                                                               
ACCELX   DS    0H                                                               
         J     SETCCEQ                                                          
*                                                                               
ACCELERR DS    0H                  RE HAS ERROR NUMBER ALREADY                  
         STH   RE,HALF                                                          
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE CUSTOM COLUMN MAP CODES                                              
*                                                                               
* FULL = ADDRESS OF CC VALIDATION ENTRY IN TABLE                                
* R3   = ADDRESS OF CC FIELD DATA MAP CODE ELEM TO BE VALIDATED                 
*                                                                               
* HALF = WILL RETURN ERROR MSG NUMBER ON ERROR (CC IS SET TO NOT EQUAL)         
*                                                                               
* WORK+0 = T(TEXT), N(NUMBER), D(DATE), P(PERIOD)                               
* WORK+1 = PL8   FOR NUMBERS       (CC IS SET TO EQUAL)                         
* WORK+1 = XL3   FOR DATES         (CC IS SET TO EQUAL)                         
* WORK+1 = XL6   FOR DATE RANGES   (CC IS SET TO EQUAL)                         
*                                                                               
* NOTE: FOR TEXT, ACTUAL DATA IS STILL POINTED TO BY R3, IT IS NOT              
*       PASSED BACK IN WORK (NO NEED TO)                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALCCMC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK           INIT RETURN VALUE(S)                         
*                                                                               
         L     R4,FULL                                                          
         USING CCVTD,R4                                                         
*                                                                               
         CLI   CCV_MED,CC_ALLMQ    THIS CC DEFINED FOR ALL MEDIA?               
         BE    VCCMC20                                                          
         LA    RF,VCCMTAB                                                       
VCCMC12  CLI   0(RF),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                MEDIA MUST BE IN TABLE                       
         CLC   BUYMD(1),1(RF)      MEDIA MATCHED THAT IN TABLE?                 
         BE    *+12                                                             
         LA    RF,2(RF)            POINT TO NEXT MEDIA ENTRY                    
         B     VCCMC12                                                          
         MVC   BYTE,CCV_MED                                                     
         OC    BYTE,0(RF)                                                       
         CLC   BYTE,CCV_MED        CC ALLOWED FOR THIS MEDIA?                   
         BE    VCCMC20                                                          
         MVC   BYTE,CCV_MED2       CHECK MEDIA CODE INDICATOR 2                 
         OC    BYTE,0(RF)                                                       
         CLC   BYTE,CCV_MED2       CC ALLOWED FOR THIS MEDIA?                   
         BE    VCCMC20                                                          
         LHI   RE,NVMEDERR         NOT VALID FOR THIS MEDIA ERROR               
         B     VCCMCERR                                                         
*                                                                               
* R6 WILL HAVE LENGTH OF MAP DATA                                               
*                                                                               
VCCMC20  LHI   RE,DEMAXERR         DATA EXCEEDED MAX LENGTH ERROR               
         SR    R6,R6                                                            
         ICM   R6,3,1(R3)                                                       
         SHI   R6,6                MINUS OVER HEAD TO GET DATA LENGTH           
         CHI   R6,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         CLI   CCV_TYP,C'D'        DATE?                                        
         BE    VCCMC52             NO NEED TO CK FOR LENGTH                     
         CLI   CCV_TYP,C'P'        PERIOD?                                      
         BE    VCCMC62             NO NEED TO CK FOR LENGTH                     
         CHI   R6,255                                                           
         BH    VCCMCERR                                                         
         SR    R1,R1                                                            
         IC    R1,CCV_LEN                                                       
         CR    R6,R1               FIELD LENGTH IS > ALLOWED LENGTH?            
         BH    VCCMCERR                                                         
*                                                                               
         CLI   CCV_TYP,C'T'        TEXT?                                        
         BNE   *+12                                                             
         MVI   WORK,C'T'           INDICATOR FOR TEXT                           
         B     VCCMC90             VALIDATION IS COMPLETE                       
*                                                                               
         CLI   CCV_TYP,C'N'        NUMERIC?                                     
         BNE   VCCMC30                                                          
         BRAS  RE,VCC_DECS         WILL VALIDATE # OF DECIMAL PLACES            
         MVC   BYTE,CCV_DEC                                                     
         OI    BYTE,X'80'          CASHVAL OUTPUT WILL BE PL8                   
         GOTO1 VCASHVAL,DMCB,(BYTE,6(R3)),(X'C0',(R6))                          
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         LHI   RE,DATNT#ER         NOT NUMERIC ERROR                            
         B     VCCMCERR                                                         
         MVI   WORK+0,C'N'                                                      
         MVC   WORK+1(8),4(R1)                                                  
         B     VCCMC90             VALIDATION IS COMPLETE                       
*                                                                               
VCCMC30  CLI   CCV_TYP,C'$'        DOLLARS?                                     
         BNE   VCCMC40                                                          
         BRAS  RE,VCC_DECS         WILL VALIDATE # OF DECIMAL PLACES            
         MVC   BYTE,CCV_DEC                                                     
         OI    BYTE,X'80'          CASHVAL OUTPUT WILL BE PL8                   
         GOTO1 VCASHVAL,DMCB,(BYTE,6(R3)),(X'40',(R6))                          
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         LHI   RE,CCNGCSH          MUST BE DOLLAR AMOUNT ERROR                  
         B     VCCMCERR                                                         
         MVI   WORK+0,C'$'                                                      
         MVC   WORK+1(8),4(R1)                                                  
         B     VCCMC90             VALIDATION IS COMPLETE                       
*                                                                               
VCCMC40  CLI   CCV_TYP,C'%'        PERCENT?                                     
         BNE   VCCMC50                                                          
         BRAS  RE,VCC_DECS         WILL VALIDATE # OF DECIMAL PLACES            
         XC    WRKELEM,WRKELEM                                                  
         LR    R2,R6                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   WRKELEM(0),6(R3)                                                 
         LR    R2,R6               INPUT LENGTH                                 
         LA    RE,WRKELEM          POINT TO MAP DATA                            
         AR    RE,R6               POINT TO END OF MAP DATA                     
         SHI   RE,1                POINT TO LAST CHAR OF MAP DATA               
         CLI   0(RE),C'%'          PERCENT SIGN?                                
         BNE   *+10                                                             
         MVI   0(RE),0             WIPE OUT % SIGN                              
         BCTR  R2,0                DECREASE INPUT LENGTH BY ONE                 
         MVC   BYTE,CCV_DEC                                                     
         OI    BYTE,X'80'          CASHVAL OUTPUT WILL BE PL8                   
         GOTO1 VCASHVAL,DMCB,(BYTE,WRKELEM),(X'C0',(R2))                        
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         LHI   RE,CCNGPCT          MUST BE PERCENT ERROR                        
         B     VCCMCERR                                                         
         MVI   WORK+0,C'%'                                                      
         MVC   WORK+1(8),4(R1)                                                  
         B     VCCMC90             VALIDATION IS COMPLETE                       
*                                                                               
VCCMC50  CLI   CCV_TYP,C'D'        DATE?                                        
         BNE   VCCMC60                                                          
VCCMC52  GOTO1 VPERVAL,DMCB,((R6),6(R3)),(X'40',WRKELEM)                        
         CLI   DMCB+4,X'01'                                                     
         BNE   *+12                                                             
         LHI   RE,INVDTERR         INVALID DATE FORMAT                          
         B     VCCMCERR                                                         
*                                                                               
         LA    RE,WRKELEM          VALIDATED DATE IS XL3                        
         USING PERVALD,RE                                                       
         MVI   WORK+0,C'D'                                                      
         MVC   WORK+1(L'PVALBSTA),PVALBSTA                                      
         DROP  RE                                                               
         B     VCCMC90             VALIDATION IS COMPLETE                       
*                                                                               
VCCMC60  CLI   CCV_TYP,C'P'        PERIOD?                                      
         BNE   VCCMC70                                                          
VCCMC62  GOTO1 VPERVAL,DMCB,((R6),6(R3)),(0,WRKELEM)                            
         CLI   DMCB+4,X'00'                                                     
         BE    *+12                                                             
         LHI   RE,CCNGPRD          INVALID DATE RANGE                           
         B     VCCMCERR                                                         
*                                                                               
         LA    RE,WRKELEM          VALIDATED PERIOD IS XL6                      
         USING PERVALD,RE                                                       
         MVI   WORK+0,C'P'                                                      
         MVC   WORK+1(L'PVALBSTA+L'PVALBEND),PVALBSTA                           
         DROP  RE                                                               
         B     VCCMC90             VALIDATION IS COMPLETE                       
*                                                                               
VCCMC70  LHI   RE,INVDTYER         INVALID DATA TYPE                            
         B     VCCMCERR                                                         
*                                                                               
VCCMC90  DS    0H                                                               
*                                                                               
VCCMCX   DS    0H                                                               
         J     SETCCEQ                                                          
*                                                                               
VCCMCERR DS    0H                  CUSTOM COLUMN FIELD DATA IS INVALID          
         STH   RE,HALF                                                          
         J     SETCCNEQ                                                         
*                                                                               
* FOR NUMBERS, NEED TO VALIDATE NUMBER OF DECIMAL PLACES                        
*                                                                               
VCC_DECS ST    RE,DUB              SAVE RETURN ADDRESS                          
         LR    RE,R6               GET SAVED FIELD DATA LENGTH                  
         SR    R1,R1               COUNTER FOR DECIMAL PLACES                   
         LA    RF,6(R3)            POINT TO BEGINNING OF FIELD DATA             
VCC_DE10 CLI   0(RF),C'.'          DECIMAL POINT?                               
         BE    VCC_DE22                                                         
         LA    RF,1(RF)            POINT TO NEXT FIELD DATA CHAR                
         BCT   RE,VCC_DE10                                                      
         B     VCC_DE30                                                         
VCC_DE20 AHI   R1,1                COUNTING NUMBER OF DECIMALS                  
VCC_DE22 LA    RF,1(RF)            POINT TO NEXT FIELD DATA CHAR                
         BCT   RE,VCC_DE20                                                      
VCC_DE30 SR    RE,RE                                                            
         IC    RE,CCV_DEC                                                       
         CR    R1,RE               # OF DECIMALS > THAN MAX?                    
         BNH   VCC_DEX                                                          
         LHI   RE,DEC#PERR         INVALID # OF DECIMAL PLACES ERROR            
         B     VCCMCERR                                                         
VCC_DEX  L     RE,DUB                                                           
         BR    RE                                                               
*                                                                               
VCCMTAB  DC    AL1(PCOLM_IQ),C'I'                                               
         DC    AL1(PCOLM_MQ),C'M'                                               
         DC    AL1(PCOLM_NQ),C'N'                                               
         DC    AL1(PCOLM_OQ),C'O'                                               
         DC    AL1(PCOLM_SQ),C'S'                                               
         DC    AL1(PCOLM_TQ),C'T'                                               
         DC    AL1(PCOLM_LQ),C'L'                                               
         DC    AL1(PCOLM_BQ),C'B'                                               
         DC    AL1(PCOLM_VQ),C'V'                                               
         DC    AL1(PCOLM_WQ),C'W'                                               
         DC    AL1(PCOLM_DQ),C'D'                                               
         DC    X'00'                                                            
*                                                                               
CC_ALLMQ EQU   PCOLM_AQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MRKCCREC NTR1  BASE=*,LABEL=*      MARK CC SFM REC AS BEING USED                
*                                                                               
         LA    R4,CCVALTAB                                                      
         USING CCVTD,R4                                                         
         MVC   WRKSVKEY,KEY        SAVE ORIGINAL KEY                            
*                                                                               
MRKCCR20 OC    CCV_SQN,CCV_SQN     END OF TABLE?                                
         BZ    MRKCCR90                                                         
         TM    CCV_STA,X'80'       ALREADY MARKED AS USED?                      
         BZ    *+12                                                             
MRKCCR22 LA    R4,CCVTLNQ(R4)      NEXT ENTRY IN TABLE                          
         B     MRKCCR20                                                         
         TM    CCV_IND,CCIUSEDQ    CUSTOM COLUMN IS USED?                       
         BZ    MRKCCR22                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PCOLPKEY,RE                                                      
         MVC   PCOLPAGY,AGYALPHA                                                
         MVI   PCOLPMED,C'A'       ALWAYS "A" (FOR CUSTOM COLUMN)               
         MVI   PCOLPRCD,X'D1'                                                   
*****    MVC   PCOLPSQN,SVCCSEQ#                                                
         MVC   PCOLPSQN,CCV_SQN                                                 
         XC    PCOLPSQN,=X'FFFF'                                                
*                                                                               
         BRAS  RE,CCREADHI                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,CCGETREC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,LWRKREC                                                       
         LA    R5,PCOLELEM-PCOLREC(R5)                                          
         CLI   0(R5),X'61'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID CUSTOM COLUMN RECORD                 
         USING PCOLELEM,R5                                                      
         OI    PCOLSTAT,X'80'      MARK IT AS BEING USED                        
*                                                                               
         BRAS  RE,CCPUTREC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    WRKSW,CCRECWRQ      CUSTOM COLUMN RECORD IS UPDATED              
         B     MRKCCR22            PROCESS NEXT ENTRY IN TABLE                  
*                                                                               
MRKCCR90 TM    WRKSW,CCRECWRQ      NEED TO RESTORE DATAMGR SEQ?                 
         BZ    MRKCCR_X                                                         
         MVC   KEY,WRKSVKEY        RESTORE ORIGINAL KEY                         
         OC    KEY(25),KEY                                                      
         BNZ   *+10                                                             
         MVC   KEY(25),NEWREC                                                   
         BRAS  RE,CCREADHI                                                      
         BRAS  RE,CCGETREC                                                      
         MVC   KEY,WRKSVKEY        RESTORE ORIGINAL KEY                         
*                                                                               
MRKCCR_X J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RE                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ONLY NEED TO REPLY ONE DOWNLOAD DATA ELEM                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDDDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    WRKSWTCH,DLELBLDQ   ALREADY BUILT?                               
         BO    BDDDX                                                            
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         XC    WRKWORK,WRKWORK                                                  
         XC    WRKABKEY,WRKABKEY                                                
         MVI   WRKSW,0                                                          
*                                                                               
BDDD20   CLI   0(R3),0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    BDDD25                                                           
*                                                                               
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   BDDD25                                                           
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         BNE   BDDD25                                                           
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         CHI   RE,255                                                           
         BNH   *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (>255)                 
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (<0)                   
         STC   RE,WRKWORK                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKWORK+1(0),6(R3)                                               
*                                                                               
BDDD25   CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDDD26                                                           
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   BDDD26                                                           
         CLC   3(2,R3),=AL2(D#ADBKEY)                                           
         BNE   BDDD26                                                           
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         CHI   RE,6+20             "ADBUYER ONLY" KEY IS <20?                   
         BNH   *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (>20)             
         CHI   RE,6                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (<0)              
         STC   RE,WRKABKEY                                                      
         SHI   RE,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKABKEY+1(0),6(R3)                                              
*                                                                               
BDDD26   CLI   0(R3),LQ_DLDDQ      DOWLOAD DATA ELEM?                           
         BNE   *+8                                                              
         OI    WRKSW,DLDATABQ      DOWNLOAD DATA ALREADY CONSTRUCTED            
         CLI   0(R3),LQ_RAWDQ      REPLY DATA ELEM?                             
         BNE   BDDD27                                                           
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         BNE   *+8                                                              
         OI    WRKSW,INSKEYBQ      REPLY INS. KEY ALREADY CONSTRUCTED           
         CLC   3(2,R3),=AL2(D#DACTN)                                            
         BNE   *+8                                                              
         OI    WRKSW,DRFTELBQ      DRAFT CHG ELEM ALREADY CONSTRUCTED           
BDDD27   CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    BDDD30                                                           
         BRAS  RE,NXTWFELM                                                      
         B     BDDD20                                                           
*                                                                               
BDDD30   MVC   WRKHALF,1(R3)       SAVE RETURNED DATA HDR ELEM LENGTH           
         LH    RE,WRKHALF                                                       
         CHI   RE,100              ENOUGH ROOM?                                 
         BNL   *+6                                                              
         DC    H'0'                RETURN RECORD IS TOO SMALL                   
*                                                                               
         TM    WRKSW,DLDATABQ      DOWNLOAD DATA ELEM BUILD?                    
         BO    BDDD35              YES, NO NEED TO BUILD ANOTHER                
*                                                                               
         MVI   0(R3),LQ_DLDDQ      DOWNLOAD DATA ELEM CODE                      
         LHI   RE,5                ELEM LENGTH                                  
         STCM  RE,3,1(R3)                                                       
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    *+14                                                             
         MVC   3(2,R3),=AL2(E#IDKRPY)                                           
         B     BDDD33                                                           
*                                                                               
         CLI   DDLINKSW,C'N'       NEW INS?                                     
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSADD)                                           
         CLI   DDLINKSW,C'C'       CHANGE INS?                                  
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSCHA)                                           
*                                                                               
BDDD33   LH    RE,WRKHALF                                                       
         SHI   RE,5                RECALCULATE RETURNED DATE HDR EL LEN         
         STH   RE,WRKHALF                                                       
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDDD35   TM    WRKSW,DRFTELBQ      DRAFT CHANGE ELEM BUILD?                     
         BO    BDDD40              YES, NO NEED TO BUILD ANOTHER                
         CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDDD40                                                           
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY DRAFT CHANGE ACTION                    
         MVI   1(R3),0                                                          
         MVI   2(R3),7             LENGTH                                       
         MVC   3(2,R3),=AL2(D#DACTN)                                            
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVI   6(R3),C'D'                                                       
         BRAS  RE,NXTWFELM                                                      
         LH    RF,WRKHALF                                                       
         SHI   RF,6+1              RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF                                                       
*                                                                               
         OC    WRKABKEY,WRKABKEY   NEED TO REPLY "ADBUYER ONLY" KEY?            
         BZ    BDDD40                                                           
         MVI   0(R3),LQ_RAWDQ      REPLY "ADBUYER ONLY" KEY                     
         SR    RE,RE                                                            
         IC    RE,WRKABKEY                                                      
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF                                                       
         SR    RF,RE               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF                                                       
         MVC   3(2,R3),=AL2(D#ADBKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         SHI   RE,6+7              6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKABKEY+1  INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
* NOTE THAT REPLY INS KEY ELEM FOR ADD INS UPLOAD IS NOT BUILD HERE             
* BECAUSE ON A SUCCESSFUL ADD, REPLY INS KEY ELEM WILL BE BUILD IN              
* T41117.  REPLY INS KEY ELEM CANNOT BE BUILT IF ERROR OCCURED                  
* WHEN DOING AN ADD (I.E. THERE'S NO SERIAL NUMBER!)                            
*                                                                               
BDDD40   TM    WRKSW,INSKEYBQ      REPLY INS. KEY ELEM BUILD?                   
         BO    BDDD80              YES, NO NEED TO BUILD ANOTHER                
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    BDDD80                                                           
*                                                                               
* NEED TO REPLY INS. KEY FOR CHANGE UPLOAD MODES                                
*                                                                               
BDDD50   MVI   0(R3),LQ_RAWDQ      REPLY INSERTION KEY                          
         SR    RE,RE                                                            
         IC    RE,WRKWORK                                                       
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF                                                       
         SR    RF,RE               RECALCULATE RETURNED DATE HDR EL LEN         
         STH   RF,WRKHALF                                                       
         MVC   3(2,R3),=AL2(D#INSKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         SHI   RE,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKWORK+1   INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDDD80   MVI   0(R3),LQ_RDATQ      REBUILD RETURNED DATA HEADER ELEM            
         MVC   1(2,R3),WRKHALF                                                  
         OI    WRKSWTCH,DLELBLDQ   SET SWITCH TO ALREADY BUILT                  
*                                                                               
BDDDX    DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FULL = ERROR MSG NUMBER (MAX IS HALF WORD)                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDERREL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
*                                                                               
BDERR20  CLI   0(R3),0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    *+12                                                             
         BRAS  RE,NXTWFELM                                                      
         B     BDERR20                                                          
*                                                                               
         MVC   WRKHALF,1(R3)       ELEM LENGTH                                  
*                                                                               
         CLI   PCVERSN#,X'04'      HIGHER THAN 4.X.X.X?                         
         JL    BDERR22                                                          
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         JNE   BDERR22                                                          
         CLC   REQTOKEN,SPACES     HAVE REQUEST TOKEN?                          
         JNH   BDERR22                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY TOKEN                                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'REQTOKEN  LENGTH                                       
         MVC   3(2,R3),=AL2(D#QTOKEN)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(L'REQTOKEN,R3),REQTOKEN                                        
         BRAS  RE,NXTWFELM                                                      
         LH    RE,WRKHALF                                                       
         SHI   RE,L'REQTOKEN                                                    
         STH   RE,WRKHALF          NEW RETURNED DATA HEADER ELEM LENGTH         
*                                                                               
BDERR22  MVI   0(R3),LQ_RAWDQ      REPLY ERROR FIELD IDENTIFIER                 
         LHI   RE,8                                                             
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF                                                       
         SHI   RF,8                                                             
         STH   RF,WRKHALF          NEW RETURNED DATA HEADER ELEM LENGTH         
         LHI   RE,D#ERRFID                                                      
         STCM  RE,3,3(R3)                                                       
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         MVC   6(2,R3),SVCCSEQ#    CUSTOM COLUMN SEQUENCE NUMBER                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY ERROR FIELD NUMBER                     
         LHI   RE,8                                                             
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF                                                       
         SHI   RF,8                                                             
         STH   RF,WRKHALF          NEW RETURNED DATA HEADER ELEM LENGTH         
         LHI   RE,D#ERRNUM                                                      
         STCM  RE,3,3(R3)                                                       
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         LHI   RE,D#CCFDAT                                                      
         STCM  RE,3,6(R3)          CUSTOM COLUMN FIELD DATA                     
         BRAS  RE,NXTWFELM                                                      
*                                                                               
         BRAS  RE,GET_ETXT         GET ERROR MSG IN BUYMSG                      
         OC    BUYMSG,SPACES       MAKE SURE NO TRAILING NULLS                  
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY ERROR MESSGE (TEXT)                    
         LHI   RE,D#ERRDSC                                                      
         STCM  RE,3,3(R3)                                                       
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
*                                                                               
         LHI   R4,L'BUYMSG                                                      
         LA    RF,BUYMSG+60-1      POINT TO LAST CHAR OF MSG                    
         CLI   0(RF),C' '                                                       
         BNE   *+12                                                             
         BCTR  R4,0                TRAILING SPACES ARE STRIPPED                 
         BCTR  RF,0                                                             
         B     *-12                                                             
         BCTR  R4,0                                                             
         CHI   R4,0                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID MSG LENGTH                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),BUYMSG                                                   
         LH    RE,WRKHALF                                                       
         AHI   R4,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         STCM  R4,3,1(R3)          LENGTH                                       
         SR    RE,R4                                                            
         STH   RE,WRKHALF          NEW RETURNED DATA HEADER ELEM LENGTH         
*                                                                               
         MVI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM CODE               
         MVC   1(2,R3),WRKHALF     NEW LENGTH                                   
*                                                                               
         MVI   ERRAREA,X'FF'       NO ADD/CHG WHEN ERROR ENCOUNTERED            
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP                                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK19D  DSECT                                                                  
*                                                                               
SPACES   DS    CL(L'BUYMSG)        C' '                                         
*                                                                               
CCVALTAB DS    (MAX_CCQ)XL(CCVTLNQ)                                             
CCVALTBX DS    XL(L'PCOLSQN)       END OF TABLE                                 
*                                                                               
CCVTLNQ  EQU   CCVTX-CCVTD         LENGTH OF ONE CC VALIDATION ENTRY            
MAX_CCQ  EQU   100                 WILL ONLY PROCESS THIS MAX FOR NOW           
*                                                                               
RELO19   DS    F                   RELOACTION FACTOR                            
D_CCREMT DS    F                   DISPLACEMENT TO CC REMOVE TABLE              
*                                                                               
DMGRCMND DS    CL(L'COMMAND)                                                    
SVCCSEQ# DS    XL(L'PCOLSQN)       SAVED CUSTOM COLUMN SEQ NUMBER               
WRKCCREM DS    XL(L'BYCCSQN)       CC CODE TO BE REMOVED                        
WRKSVKEY DS    XL(L'KEY)                                                        
WRKELEM  DS    CL255                                                            
*                                                                               
WRKSWTCH DS    X                   WORKING ERROR SWITCH                         
DLELBLDQ EQU   X'80'               DOWNLOAD ELEM IS BUILT                       
*                                                                               
WRKHALF  DS    H                                                                
WRKSVRE  DS    F                   SAVE RE                                      
WRKWORK  DS    XL50                                                             
WRKABKEY DS    XL20                                                             
WRKTEMP1 DS    XL80                                                             
WRKTEMP2 DS    XL80                                                             
*                                                                               
WRKSW    DS    X                                                                
DLDATABQ EQU   X'80'               DOWNLOAD DATA ELEM IS BUILT                  
INSKEYBQ EQU   X'40'               REPLY KEY ELEM IS BUILT                      
DRFTELBQ EQU   X'20'               DRAFT CHANGE ELEM IS BUILT                   
CCRECWRQ EQU   X'10'               CUSTOM COLUMN RECORD IS UPDATED              
STDCOLMQ EQU   X'08'               STANDARIZED COLUMN                           
*                                                                               
CCREMCNT DS    H                   COUNTER FOR CC REMOVAL TABLE                 
*                                                                               
SVIDKPRF DS    XL16                IDESK CONTROL PROFILE VALUES                 
*                                                                               
LWRKREC  DS    XL4096              LOCAL WORKING REC AREA                       
*                                                                               
WORK19X  EQU   *                   END OF LOCAL WORKING STORAGE AREA            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCVTD    DSECT                     CC VALIDATION TABLE ENTRY                    
CCV_SQN  DS    XL(L'PCOLSQN)       CUSTOM COLUMN SEQUENCE NUMBER                
CCV_MED  DS    XL(L'PCOLMED)       MEDIA CODE INDICATOR                         
CCV_MED2 DS    XL(L'PCOLMED2)      MEDIA CODE INDICATOR 2                       
CCV_TYP  DS    XL(L'PCOLTYP)       DATA TYPE                                    
CCV_LEN  DS    XL(L'PCOLMLEN)      MAX FIELD LENGTH                             
CCV_DEC  DS    XL(L'PCOLDECS)      NUMBER OF DECIMAL                            
CCV_STA  DS    XL(L'PCOLSTAT)      STATUS BYTE                                  
CCV_TRK  DS    XL(L'PCOLTRK)       TRACKING INDICATOR                           
*                                                                               
CCV_IND  DS    X                   INDICATOR BYTE                               
CCIUSEDQ EQU   X'80'               CUSTOM COLUMN IS USED                        
*                                                                               
CCVTX    DS    0X                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025PPBUY19   10/17/18'                                      
         END                                                                    
