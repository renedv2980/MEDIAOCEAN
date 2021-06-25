*          DATA SET SPSFM2A    AT LEVEL 021 AS OF 09/01/11                      
*PHASE T2172AA                                                                  
*INCLUDE BRDMON                                                                 
*INCLUDE GETDAY                                                                 
*                                                                               
* *********************************************************************         
*                                                                     *         
*  TITLE:        SPSFM2A - CLEARANCE STATUS PROGRAM                   *         
*                                                                     *         
*  COMMENTS:     DISPLAY'S X'01'=CLEARANCE ELEMENTS                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON, WHICH CALLS THIS PROGRAM.                  *         
*                                                                     *         
*  INPUTS:       SCREENS T217C4  (MAINTENANCE)                        *         
*                        T217CC  (LIST)                               *         
*                        T217D9  (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED CLEARANCE DETAILS, LIST, REPORT.             *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPSFM2A - SCROLLER DATATYPE RECORD MAINT/LIST/REPORT'           
T2172A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CLRS**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T217FFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         OI    GLSTSTAT,NOSELFLD   TAKE OUT THE SELECT FIELDS                   
         L     RE,=V(BRDMON)                                                    
         A     RE,RELO                                                          
         ST    RE,ABRDMON                                                       
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* *********************************************************************         
* VALIDATE KEY                                                                  
* *********************************************************************         
VK       DS    0H                                                               
         LA    R2,CONACTH                                                       
         CLC   =C'REPORT',8(R2)                                                 
         BE    ERRACT                                                           
         MVI   NEWLIST,C'Y'                                                     
*                                                                               
         LA    R2,LSTMEDH          START CHECKING WITH MEDIA FIELD              
         LA    R3,LSTOPTH          END CHECKING WITH OPTIONS FIELD              
*                                                                               
VK00     TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BZ    VK03                NO - GO VALIDATE                             
         CR    R2,R3               EQUAL TO OR PASSED OPTIONS FIELD?            
         BNL   VK02                YES                                          
VK01     SR    R0,R0               BUMP TO NEXT FIELD                           
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BZ    VK00                NO                                           
         B     VK01                                                             
*                                                                               
VK02     MVI   NEWLIST,C'N'        CONTINUE LISTING THE RECORDS                 
         B     VKX                                                              
*                                                                               
VK03     XC    SEQNUM,SEQNUM       INIT SOME VARS                               
         XC    LOWDATE,LOWDATE                                                  
         XC    BCLT,BCLT                                                        
         XC    BMKT,BMKT                                                        
         XC    BSTA,BSTA                                                        
*                                                                               
VKLST    DS    0H                  LIST ACTION                                  
         LA    R2,LSTMEDH          VALIDATE MEDIA - REQUIRED FIELD              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         MVI   SVA0GRS,C'G'        DEFAULT TO GROSS                             
         LA    R2,LSTCLTH          VALIDATE CLIENT - REQUIRED FIELD             
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VKCLT               YES                                          
         OC    CONWHEN(3),CONWHEN  ARE WE TRYING TO REPORT?                     
         BNZ   VKCLT05             YES, NO INPUT IS OK                          
*                                                                               
VKCLT    CLC   =C'ALL',8(R2)       WANT CLIENT 'ALL'?                           
         BE    VKCLT05             YES, ONLY FOR OPT 'UNC'                      
*                                                                               
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         XC    WORK,WORK           GET AGENCY/OFFICE LEVEL A0                   
         MVC   WORK(4),=C'S0A0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   KEY(6),WORK         SAVE KEY                                     
         MVC   WORK+6(1),LSTMED                                                 
         MVC   WORK+7(3),=X'FFFFFF'     FAKE THE CLIENT OR GETPROF WILL         
         MVI   WORK+10,C'*'             RETURN THE MEDIA LEVEL THINKING         
         MVC   WORK+11(1),SVOFFC        IT MATCHES THE BLANK CLIENT             
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK+24,DATAMGR                                   
         MVC   SVA0GRS,WORK+24     NOTE 1 BYTE SAVED                            
*                                                                               
VKCLT05  LA    R2,LSTSTAH          VALIDATE STATION                             
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VKSTA               YES                                          
         OC    CONWHEN(3),CONWHEN  ARE WE TRYING TO REPORT?                     
         BNZ   VKLST14             YES, NO INPUT IS OK                          
         CLC   =C'ALL',LSTCLT      WANT ALL CLIENTS?                            
         BE    VKLST14             YES, DON'T NEED THE STATION                  
*                                                                               
VKSTA    GOTO1 ANY                 INPUT REQUIRED                               
         MVI   STAFLG,C'N'         READ FOR THIS STATN ONLY                     
         CLI   LSTSTA,C'='         IS THIS A FILTER?                            
         BNE   VKLST05             NO                                           
         ZIC   R1,LSTSTAH+5        REDUCE LENGTH BY 1 FOR THE '=' SIGN          
         BCTR  R1,0                                                             
         STC   R1,LSTSTAH+5                                                     
         BCTR  R1,0                DECR FOR EXMVC                               
         XC    LSTSTA,LSTSTA                                                    
         EXMVC R1,LSTSTA,WORK+1                                                 
         MVC   WORK(8),LSTSTA                                                   
         NC    WORK(8),=8X'F0'                                                  
         EXCLC R1,WORK,=8X'F0'                                                  
         BNE   *+8                                                              
         OI    4(R2),X'08'         SET NUMERIC FIELD BIT OF FLDHDR              
         MVI   STAFLG,C'Y'         READ FOR THIS STATN ONLY                     
         OI    LSTSTAH+6,X'80'     TRANSMIT                                     
*                                                                               
VKLST05  TM    4(R2),X'08'         NUMERIC INPUT?                               
         BO    VKLST10             YES- INPUT IS A MARKET                       
         GOTO1 VALISTA             NO-  INPUT IS STATION (INCLUDES MKT)         
         B     VKLST12                                                          
*                                                                               
VKLST10  GOTO1 VALIMKT             R2 POINTS TO NUMERIC STATION FLDHDR          
         XC    BSTA,BSTA           ONLY MARKET WAS SPECIFIED                    
*                                                                               
VKLST12  CLI   STAFLG,C'Y'         THIS STATION ONLY FILTER?                    
         BNE   VKLST14                                                          
         ZIC   R1,LSTSTAH+5        RESTORE THE '=' SIGN FILTER MARKER           
         BCTR  R1,0                                                             
         MVC   WORK(L'LSTSTA),LSTSTA                                            
         EXMVC R1,LSTSTA+1,WORK                                                 
         MVI   LSTSTA,C'='                                                      
         LA    R1,1(R1)            ONE MORE FOR THE '='                         
*                                                                               
VKLST14  CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BE    VKLST16                                                          
         CLI   BSTA,X'E8'          TEST CABLE                                   
         BL    *+8                 NO                                           
         NI    BSTA+2,X'80'        YES - DROP NETWORK                           
         B     VKLST18                                                          
*                                                                               
VKLST16  CLI   LSTMED,C'N'         TEST NETWORK                                 
         BNE   *+8                                                              
         MVI   BSTA+2,0            DROP LAST BYTE                               
*                                                                               
VKLST18  OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         XC    STARTDAT,STARTDAT   CLEAR START AND END DATE FILTERS             
         XC    ENDDATE,ENDDATE                                                  
         LA    R2,LSTDATEH                                                      
         CLI   5(R2),0             ANYTHING INPUT?                              
         BNE   VKLST26             YES                                          
***                                                                             
* IF NO DATE IS INPUT , DO ONE YEAR ENDING TODAY                                
***                                                                             
         GOTO1 DATCON,DMCB,(5,0),WORK                                           
         GOTO1 (RF),(R1),WORK,(2,ENDDATE)                                       
*                                                                               
         LHI   R0,-1                                                            
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,(R0)                               
         GOTO1 DATCON,DMCB,WORK+6,(2,STARTDAT)                                  
         B     VKLST35                                                          
*                                                                               
VKLST26  MVI   ERROR,INVDATE                                                    
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(LSTDATEH+5,LSTDATE),WORK                            
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID?                              
         BE    ERRX                NO                                           
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID?                              
         BE    ERRX                NO                                           
*                                                                               
         LA    R5,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R5                                                       
         MVC   STARTDAT,PVALCSTA   SAVE COMPRESSED START DATE                   
         MVC   ENDDATE,PVALCEND    SAVE COMPRESSED END DATE                     
         DROP  R5                                                               
*                                                                               
VKLST35  MVC   LSTDATE,SPACES                                                   
         OI    LSTDATEH+6,X'80'                                                 
         OI    LSTDATEH+4,X'20'    SET DATES VALIDATED                          
         GOTO1 DATCON,DMCB,(2,STARTDAT),(8,LSTDATE)                             
         MVI   LSTDATE+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(2,ENDDATE),(8,LSTDATE+9)                            
*                                                                               
VKLST40  XC    PAYFLG,PAYFLG       PAYEE FILTER                                 
         MVI   ERROR,INVALID                                                    
         LA    R2,LSTPAYH                                                       
         CLI   5(R2),0                                                          
         BE    VKLST50                                                          
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXCLC R1,WORK,=C'DIRECT'                                               
         BNE   *+14                                                             
         MVC   PAYFLG,=C'DIR '                                                  
         B     VKLST50                                                          
         CLI   5(R2),4                                                          
         BNE   ERRX                                                             
         CLI   WORK,C'P'                                                        
         BE    *+12                                                             
         CLI   WORK,C'S'                                                        
         BNE   ERRX                                                             
         MVC   PAYFLG,WORK                                                      
*                                                                               
VKLST50  OI    4(R2),X'20'         SET PAYEE VALIDATED                          
*                                                                               
         XC    OPTFLAG,OPTFLAG     CLEAR THE OPTIONS FLAG                       
         XC    FLTFLG,FLTFLG       CLEAR THE OPTIONS FLAG                       
         XC    SVMOSF,SVMOSF       CLEAR MOS FILTER                             
         LA    R2,LSTOPTH          OPTIONS FILTER                               
         CLI   5(R2),0             ANY INPUT                                    
         BE    *+8                 NOPE                                         
         BAS   RE,EDTOPTS          EDIT THE OPTIONS                             
         CLC   =C'ALL',LSTCLT      HAVE CLIENT 'ALL'?                           
         BNE   VKLST55                                                          
         TM    OPTFLAG,OPTUNCLR    HAVE UNLEARED OPTION?                        
         BZ    ERRUNCL2            NO, MUST HAVE OPT UNC FOR CLT 'ALL'          
*                                                                               
VKLST55  OI    4(R2),X'20'         SET OPTIONS VALIDATED                        
*                                                                               
         OC    CONWHEN(3),CONWHEN  ARE WE TRYING TO REPORT?                     
         BZ    VKLST60             NO                                           
         TM    OPTFLAG,OPTUNCLR    HAVE UNLEARED OPTION?                        
         BZ    ERRUNCL             NO, MUST HAVE OPT UNC FOR REPORT             
*                                                                               
VKLST60  MVC   LSTLHL1+5(4),=X'C394A340' CLT IN LOWERCASE                       
         CLC   =C'ALL',LSTCLT            HAVE CLIENT 'ALL'?                     
         BE    *+10                                                             
         MVC   LSTLHL1+5(4),=X'E2A381A3'  STAT IN LOWERCASE                     
         OI    LSTLHL1H+6,X'80'                                                 
*                                                                               
VKKEY    DS    0H                  BUILD CLEARANCE STATUS RECD KEY              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLRSTATD,R4                                                      
         MVC   CLSKTYPE,=X'0D76'                                                
*                                                                               
         TM    OPTFLAG,OPTUNCLR    TEST UNCLEARED ONLY                          
         BZ    *+8                                                              
         OI    CLSKTYPE+1,X'80'    READ 0DF6 POINTERS                           
*                                                                               
         MVC   CLSKAGMD,BAGYMD                                                  
         MVC   CLSKCLT,BCLT                                                     
         MVC   CLSKMKT,BMKT                                                     
         MVC   CLSKSTA,BSTA                                                     
         MVC   CLSKDATE,LOWDATE    ONLY DEFINED FOR MAINT- ELSE ZERO            
         MVC   CLSKSEQ,SEQNUM      ONLY DEFINED FOR MAINT- ELSE ZERO            
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
VKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* VALIDATE RECORD                                                               
* ********************************************************************          
                                                                                
VR       L     R4,AIO              BUILD RECORD                                 
         USING CLRSTATD,R4                                                      
*                                                                               
VR20     B     DR                  DISPLAY THE RECORD                           
         DROP  R4                                                               
         EJECT                                                                  
* ********************************************************************          
* DISPLAY RECORD                                                                
* ********************************************************************          
                                                                                
DR       DS    0H                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* DISPLAY KEY                                                                   
* ********************************************************************          
                                                                                
DK       L     R4,AIO              SELECTED RECORD                              
         USING CLRSTATD,R4                                                      
*                                                                               
DKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* *******************************************************************           
* ON-SCREEN LIST                                                                
* *******************************************************************           
                                                                                
LR       LA    R4,KEY                                                           
         LA    R2,LSTLST1H         SETUP THIS LIST SO IT DOESN'T GO FOR         
         ST    R2,ATHISLST         ...THE FIRST UNPROTECTED FIELD               
         MVI   NLISTS,12            #LIST LINES IS < THE DEFAULT                
         USING CLRSTATD,R4                                                      
         XC    LASTCLT,LASTCLT                                                  
         MVC   PREVMKT,=X'FFFF'                                                 
*                                                                               
         MVC   LSTLHL2+20(8),=X'C388928481A38540' CHKDATE IN LOWERCASE          
         OI    LSTLHL2H+6,X'80'     TRANSMIT                                    
         TM    OPTFLAG,OPTCLR       CLEARED OPTION?                             
         BNO   *+10                 NO                                          
         MVC   LSTLHL2+20(8),=X'C39440C281959240' CL BANK IN LOWERCASE          
*                                                                               
         CLI   NEWLIST,C'Y'        WANT TO START LISTING FROM NEW KEY?          
         BE    LR05                YES                                          
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
LR05     ZAP   PACKDEC,=P'0'                                                    
         MVC   KEY,ORIGKEY         RESET TO ORIG KEY BUILT IN VALKEY            
         MVC   BMKT,CLSKMKT                                                     
         MVC   BSTA,CLSKSTA                                                     
         XC    SVEL01,SVEL01       INITIALIZE ELEMENT DISPLACEMENT              
         XC    SVEL03,SVEL03                                                    
         MVC   LASTKEY,KEY                                                      
         MVI   REPFLG,0            REPORT FLAG                                  
         MVI   NEWLIST,C'N'                                                     
*                                                                               
LR10     OC    SVEL01,SVEL01       HAD WE READ ALL OF LAST REC'S ELEMS?         
         BZ    *+10                YES                                          
         MVC   KEY,LASTKEY         NO, RESTORE KEY OF LAST RECD                 
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVEL01,SVEL01       DO WE HAVE TO BUMP TO RT ELEMENT?            
         BZ    LR30                NO,                                          
         GOTO1 GETREC              YES. GET THE RECORD                          
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO              START OF RECORD ADDRESS                      
         A     R6,SVEL01           R6 PTS TO NEXT ELEMENT                       
         MVC   DATADSP,=Y(CLSELEMS-CLRSTATD)  INIT FOR NEXTEL LATER ON          
         MVI   ELCODE,X'01'                                                     
         CLI   0(R6),0             END OF RECORD?                               
         BNE   LR34                NO,                                          
*                                                                               
LR20     LA    R4,KEY              GET NEXT RECORD                              
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    SVEL01,SVEL01       NEW RECD->INITIALIZE ELMNT CNTR              
         XC    SVEL03,SVEL03                                                    
         MVC   LASTKEY,KEY         SAVE KEY                                     
*                                                                               
LR30     LA    R1,4                COMPARE KEY FOR 5 UNLESS NO CLT              
         OC    ORIGKEY+3(2),ORIGKEY+3               FILTER ON CLT?              
         BNZ   *+8                                  YES                         
         AHI   R1,-2               COMPARE ONLY FOR 3                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),ORIGKEY      SAME RECD TYP/AGY/MED/CLT?                   
         BNE   LRX                 NO MORE RECDS TO LIST                        
*                                                                               
         CLI   STAFLG,C'Y'                                                      
         BNE   LR32                                                             
         CLC   CLSKMKT,BMKT                                                     
         BNE   LRX                                                              
         OC    BSTA,BSTA           STATION FILTER?                              
         BZ    LR32                                                             
         CLC   CLSKSTA,BSTA                                                     
         BNE   LRX                                                              
*                                                                               
LR32     GOTO1 GETREC              GET THE RECORD                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR34     CLC   =C'ALL',LSTCLT      HAVE CLIENT 'ALL'?                           
         BNE   LR40                NO                                           
*                                                                               
         MVC   SAVEKEY,KEY         BUILD KEY FOR STN MKT RECD                   
         GOTO1 CLUNPK,DMCB,CLSKCLT,LSTA+2                                       
         MVC   FAKEFLDH,LSTCLTH                                                 
         MVC   FAKEFLD,LSTA+2                                                   
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALICLT             AND "VALIDATE IT"                            
         MVC   KEY,SAVEKEY         RESTORE OLD KEY FOR 0D76 RECD                
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
LR40     L     R4,AIO                                                           
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         CLC   PREVMKT,CLSKMKT     ONLY PRT MKT IF CHANGED                      
         BE    LR42                                                             
         MVC   PREVMKT,CLSKMKT                                                  
*                                                                               
         MVI   TACT,C'U'                                                        
         MVC   TMKST,CLSKMKT                                                    
         BAS   RE,SETMSP                                                        
         MVC   LMKT,TMKT                                                        
*                                                                               
         MVC   SAVEKEY,KEY         BUILD KEY FOR STN MKT RECD                   
         XC    KEY,KEY                                                          
         MVC   AIO,AIO2                                                         
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),TMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',KEY,AIO                  
         L     R1,AIO                                                           
         MVC   SVMKTNM,MKTNAME-MKTREC(R1) DON'T PRINT TILL KNOW ACTIVE          
*                                                                               
         MVC   KEY,SAVEKEY         RESTORE OLD KEY FOR 0D76 RECD                
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
LR42     OC    SVEL01,SVEL01       IN THE MIDDLE OF A RECD?                     
         BNZ   LR44                YES                                          
*                                                                               
         L     R6,AIO                                                           
         USING CLSTEL01,R6         NAME ELEMENT'S DSECT                         
         MVC   DATADSP,=Y(CLSELEMS-CLRSTATD)                                    
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL            FIND CLERANCE DETAILS ELEMENTS               
         BE    *+6                 IF THERE, CONTINUE, ELSE BUG                 
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         LR    R1,R6                                                            
         S     R1,AIO                                                           
         ST    R1,SVEL01           SAVE DISP TO 1ST ELEMENT                     
         XC    SVEL03,SVEL03                                                    
*                                                                               
LR44     TM    OPTFLAG,OPTUNCLR    TEST UNCLEARANCES ONLY                       
         BZ    *+12                                                             
         TM    CLSTSTAT,X'01'      TEST UNCLEARANCE                             
         BZ    LR48                NO - SKIP                                    
*                                                                               
         CLC   CLSTCLRD,ENDDATE                                                 
         BH    LR48                                                             
         CLC   CLSTCLRD,STARTDAT                                                
         BL    LR48                DATE NO GOOD                                 
*                                                                               
         CLI   SVMOSF,0            MOS FILTER?                                  
         BE    LR44C                                                            
         GOTO1 DATCON,DMCB,(2,CLSTSTDT),(0,DUB)                                 
         OC    DMCB,DMCB                                                        
         BZ    ERRSYN                                                           
         GOTO1 ABRDMON,DMCB,DUB,DUB+6                                           
         GOTO1 DATCON,DMCB,(0,DUB+6),(6,DUB)                                    
         CLC   SVMOSF,DUB                                                       
         BNE   LR48                                                             
*                                                                               
LR44C    DS    0H                                                               
         OC    PAYFLG,PAYFLG       ANY PAYEE FILTER?                            
         BZ    LR54                                                             
         CLC   PAYFLG,=C'DIR '     DIRECT FLAG (PYEE=X'00')                     
         BNE   LR46                                                             
         OC    CLSTPYEE,CLSTPYEE   DIRECT IF PYEE=X'00'                         
         BZ    LR54                YES, IT'S GOOD                               
         CLC   CLSTPYEE,=C'000'    DIRECT IF PYEE=C'00' (X'F0F0')               
         BE    LR54                YES, IT'S GOOD                               
         B     LR48                NO,  NO   GOOD                               
*                                                                               
LR46     CLC   CLSTREPT(4),PAYFLG                                               
         BE    LR54                ELEM IS GOOD- GO PRINT IT                    
*                                                                               
LR48     SR    R0,R0               LOOP TILL YOU FIND 1ST GD ELMNT              
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    LR20                NO MORE- READ NEXT RECORD                    
*                                                                               
LR49     CLI   0(R6),1                                                          
         BNE   LR48                ELSE TRY FOR ANOTHER ELEMENT                 
         LR    R1,R6                                                            
         S     R1,AIO                                                           
         ST    R1,SVEL01                                                        
         XC    SVEL03,SVEL03                                                    
         B     LR44                                                             
*                                                                               
LR50     DS    0H                  FILTER 01 ELEMTS ON DATE                     
         CLC   CLSTCLRD,ENDDATE                                                 
         BH    LR150                                                            
         CLC   CLSTCLRD,STARTDAT                                                
         BL    LR150                                                            
*                                                                               
         CLI   SVMOSF,0            MOS FILTER?                                  
         BE    LR50C                                                            
         GOTO1 DATCON,DMCB,(2,CLSTSTDT),(0,DUB)                                 
         OC    DMCB,DMCB                                                        
         BZ    ERRSYN                                                           
         GOTO1 ABRDMON,DMCB,DUB,DUB+6                                           
         GOTO1 DATCON,DMCB,(0,DUB+6),(6,DUB)                                    
         CLC   SVMOSF,DUB                                                       
         BNE   LR150                                                            
*                                                                               
LR50C    OI    FLAG,VAL01                                                       
         TM    OPTFLAG,OPTUNCLR    TEST UNCLEARANCES ONLY                       
         BZ    *+12                                                             
         TM    CLSTSTAT,X'01'      TEST UNCLEARANCE                             
         BZ    LR150               NO - SKIP                                    
*                                                                               
         OC    PAYFLG,PAYFLG       ANY PAYEE FILTER?                            
         BZ    LR54                                                             
         CLC   PAYFLG,=C'DIR '     DIRECT FLAG (PYEE=X'00')                     
         BNE   LR52                                                             
         OC    CLSTPYEE,CLSTPYEE   DIRECT IF PYEE=X'00'                         
         BZ    LR54                YES, IT'S GOOD                               
         CLC   CLSTPYEE,=C'000'    DIRECT IF PYEE=C'00' (X'F0F0F0')             
         BE    LR54                YES, IT'S GOOD                               
         B     LR150               NO,  NO   GOOD                               
*                                                                               
LR52     CLC   CLSTREPT(4),PAYFLG                                               
***      BNE   LR48                                                             
         BNE   LR150                                                            
*                                                                               
LR54     MVC   LISTAR,SPACES                                                    
         OC    SVMKTNM,SVMKTNM     MKTNAME TO SCREEN ON FIRST                   
         BZ    LR120               ACTIVITY                                     
         MVC   LMKT(24),SVMKTNM                                                 
         XC    SVMKTNM,SVMKTNM                                                  
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
*                                                                               
LR120    MVI   TACT,C'U'                                                        
         MVC   TMKST,CLSKMKT                                                    
         BAS   RE,SETMSP                                                        
         MVC   WORK,TMKT                                                        
         MVC   LSTA,TSTA                                                        
         MVC   LREPT,CLSTREPT      1 CHAR REPT TYPE CODE                        
         MVC   LPYEE,CLSTPYEE      PAYEE                                        
         OC    CLSTPYEE,CLSTPYEE   DIRECT IF PYEE=X'00'                         
         BZ    *+14                                                             
         CLC   CLSTPYEE,=C'000'    DIRECT IF PYEE=C'000'                        
         BNE   *+10                                                             
         MVC   LREPT(4),=C'DIR '                                                
         EDIT  CLSTCLSQ,(4,WRK)                                                 
         OC    WRK,=4X'F0'                                                      
         MVC   LSEQ,WRK+1                                                       
*                                                                               
         TM    CLSTSTAT,X'80'       RECONCILED?                                 
         BNO   *+8                                                              
         MVI   LCKRCN,C'*'                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,CLSTCLRD),(8,LCLRDT)                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,CLSTSTDT),(0,DUB)                                 
         GOTO1 ABRDMON,DMCB,DUB,DUB+6                                           
         GOTO1 DATCON,DMCB,(0,DUB+6),(6,SVMOS)                                  
*                                                                               
         TM    OPTFLAG,OPTOLE      OLDEL OPTION TO FORCE DISPLAY                
         BO    LR121X                                                           
         TM    OPTFLAG,OPTNWE      NEWEL OPTION TO FORCE DISPLAY                
         BO    LR121A                                                           
         CLC   CLSTCLRD,=X'D6C5'   IF CLEARED BEFORE JUN5/07                    
         BL    LR121X              SKIP NEW ELEMS AS SAX WAS NOT                
         MVI   SHOW03,C'Y'         WRITING CHECK INFO BACK TO THEM YET          
*                                                                               
LR121A   TM    CLSTSTAT,X'02'      TEST NEW CLRST (BY INVOICE)                  
         BZ    LR121X                                                           
LR121    BRAS  RE,PROCINV          FIND/PROCESS 03 ELEMENTS                     
         B     PCLR20                                                           
*                                                                               
LR121X   MVC   WRK,CLSTGRS                                                      
         TM    OPTFLAG,OPTGRS      FIRST CHECK OPTION ENTERED                   
         BO    LR124                                                            
         TM    OPTFLAG,OPTNET                                                   
         BO    LR122                                                            
         CLI   SVA0GRS,C'G'        THEN CHECK A0 PROFILE                        
         BE    LR124                                                            
LR122    MVC   WRK,CLSTNET                                                      
*                                                                               
LR124    TM    WHEN,X'40'          NOW REPORT?                                  
         BZ    LR126               NO                                           
         ICM   R3,15,WRK           ADD TOTAL UNCLEARANCES                       
         CVD   R3,DUB              CONVERT R3 TO A PACKED DECIMAL               
         AP    PACKDEC,DUB                                                      
*                                                                               
LR126    EDIT  (4,WRK),(11,LAMT),2,MINUS=YES                                    
*                                                                               
         TM    CLSTSTAT,X'01'      TEST UNCLEARANCE                             
         BZ    *+8                                                              
         MVI   LAMT-1,C'U'         SET UNCLEARED FLAG                           
*                                                                               
*** CLEARED OPTION PROCESSING (PUT IN 02/11/02)                                 
*                                                                               
         MVC   LCKNUM,CLSTCHK       MOVE CHECK NUMBER TO SCREEN                 
         CLI   CLSTEL01+1,38        LONG ELEMENT?                               
         BNE   LR128                NO...OLD SHORTER ELEM W/0 CLR DATE          
         OC    CLSTBKDT,CLSTBKDT    DOES BANK CLEARED DATE EXIST?               
         BZ    LR128                NO                                          
         MVI   LAMT+11,C'C'         MOVE 'C' IN BEFORE CHECK NUMBER             
*                                                                               
LR128    TM    OPTFLAG,OPTCLR       CLEARED OPTION ON?                          
         BO    LR130                YES                                         
         GOTO1 DATCON,DMCB,(2,CLSTCHDT),(8,LCKDT)                               
         B     PCLR20                                                           
*                                                                               
LR130    CLI   CLSTEL01+1,38        LONG ELEMENT?                               
         BL    PCLR30               NO...OLD SHORTER ELEM W/0 CLR DATE          
         OC    CLSTBKDT,CLSTBKDT    DOES BANK CLEARED DATE EXIST?               
         BZ    PCLR20               NO                                          
         GOTO1 DATCON,DMCB,(2,CLSTBKDT),(8,LCKDT)                               
*                                                                               
PCLR20   XC    SVPID,SVPID                                                      
         XC    SVACC,SVACC                                                      
         TM    FLTFLG,FLTPID+FLTACC       Need Pid/Acc Agy?                     
         JZ    PCLR30                                                           
         CLI   CLST01LN,CL01ELN3   Test For Longer 01 Element                   
         JNE   PCLR30                                                           
         MVC   SVACC,CLSTACC       Set ACC agency                               
*                                                                               
         LA    R4,PIDKEY                                                        
         XC    PIDKEY,PIDKEY                                                    
         USING SA0REC,PIDKEY                                                    
         MVI   SA0KTYP,SA0KTYPQ    C'0' - Personal auth. record                 
         MVC   SA0KAGY,AGENCY      Alpha ID                                     
         MVC   SA0KNUM,CLSTPID                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',PIDKEY,AIO3                  
         L     R4,AIO3                                                          
         CLC   PIDKEY(L'SA0KEY),0(R4)                                           
         BNE   PCLR30                                                           
         TM    SA0STAT,X'20'       Test locked                                  
         BO    PCLR30                                                           
*                                                                               
         AHI   R4,28                                                            
PCLR22   CLI   0(R4),0                                                          
         BE    PCLR30                                                           
         CLI   0(R4),X'C3'                                                      
         BE    PCLR24                                                           
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     PCLR22                                                           
         USING SAPALD,R4                                                        
*                                                                               
PCLR24   MVC   SVPID,SAPALPID      Set PID                                      
         DROP  R4                                                               
*                                                                               
PCLR30   DS    0H          NOTE - NET WENT TO LISTMON HERE                      
         L     R4,AIO      RESET R4 TO CLRSTREC                                 
         USING CLRSTATD,R4                                                      
*                                                                               
*--GET PRODUCT-PARTNER CODE                                                     
*                                                                               
LR132    CLI   0(R6),1             TEST PROCESSING 01                           
         BNE   LR138               NO - MUST BE 03                              
*                                                                               
         LA    R1,SVCLIST                                                       
*                                                                               
LR134    CLI   0(R1),X'00'                                                      
         BE    LR136                                                            
*                                                                               
         CLC   CLSTPRD,3(R1)       FIND PRD CODE IN LIST                        
         BNE   *+10                                                             
         MVC   LPRD(3),0(R1)                                                    
*                                                                               
         CLC   CLSTPRD2,3(R1)                                                   
         BNE   *+14                                                             
         MVI   LPRD+3,C'-'                                                      
         MVC   LPRD+4(3),0(R1)                                                  
*                                                                               
         LA    R1,4(R1)            BUMP TO THE NEXT ENTRY                       
         B     LR134                                                            
*                                                                               
LR136    CLI   CLSTPRD2,0          TEST SECOND PRD                              
         BNE   LR138               YES - SO NO EST                              
         CLI   CLSTEL01+1,CL01ELLN TEST BIG ELEM                                
         BL    LR138               NO - CAN'T HAVE EST                          
         SR    R0,R0                                                            
         ICM   R0,1,CLSTEST                                                     
         BZ    LR138               NOT PAID BY ESTIMATE                         
         MVI   LPRD+3,C'/'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LPRD+4(3),DUB                                                    
*                                                                               
LR138    TM    WHEN,X'40'          NOW REPORT?                                  
         BZ    LR140               NO                                           
         LA    R2,LSTOPTH          OPTION HEADER (IN CASE OF ERR)               
         TM    OPTFLAG,OPTUNCLR    UNCLEARED ONLY?                              
         BZ    ERRUNCL             NO, ERROR                                    
         OI    REPFLG,REPDATA      HAVE SOME DATA ON REPORT                     
         BAS   RE,PR               REPORT                                       
*                                                                               
LR140    CLC   =C'ALL',LSTCLT      HAVE CLIENT 'ALL'?                           
         BNE   LR142               NO                                           
         GOTO1 MSUNPK,DMCB,CLSKMKT,LMKT,PRTSTA                                  
         GOTO1 CLUNPK,DMCB,CLSKCLT,LSTA+2                                       
         B     LR144                                                            
*                                                                               
LR142    GOTO1 MSUNPK,DMCB,CLSKMKT,LMKT,LSTA                                    
*                                                                               
LR144    GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
*        SEE IF OPTIONAL DATA IS TO BE DISPLAYED                                
*                                                                               
         OC    SVPID,SVPID                                                      
         BNZ   LR144A                                                           
         OC    SVACC,SVACC                                                      
         BNZ   LR144A                                                           
         TM    FLTFLG,FLTMOS      ASKED TO DISPLAY MOS?                         
         BZ    LR144X             NO - SKIP                                     
         OC    SVMOS,SVMOS                                                      
         BNZ   LR144A                                                           
         B     LR144X                                                           
*                                                                               
LR144A   LA    RE,LREPT            START IN REP FIELD                           
         TM    FLTFLG,FLTPID                                                    
         BZ    LR144C                                                           
         OC    SVPID,SVPID         SEE IF I'M TO DISPLAY THE PID                
         BZ    LR144C                                                           
         MVC   0(4,RE),=C'Pid='                                                 
         MVC   4(L'SVPID,RE),SVPID                                              
         LA    RE,4(RE)           PAST PID=                                     
         LA    RE,L'SVPID(RE)                                                   
         LA    RE,2(RE)                                                         
*                                                                               
LR144C   OC    SVACC,SVACC        SEE IF I'M TO DISPLAY THE ACC AGY             
         BZ    LR144E                                                           
         TM    FLTFLG,FLTACC                                                    
         BZ    LR144E                                                           
         MVC   0(4,RE),=C'Acc='                                                 
         MVC   4(L'SVACC,RE),SVACC                                              
         LA    RE,4(RE)           PAST ACC=                                     
         LA    RE,L'SVACC(RE)                                                   
         LA    RE,2(RE)                                                         
*                                                                               
LR144E   OC    SVMOS,SVMOS        SEE IF I'M TO DISPLAY THE MOS                 
         BZ    LR144G                                                           
         TM    FLTFLG,FLTMOS      ASKED TO DISPLAY MOS?                         
         BZ    LR144G             NO - THEN SKIP                                
         MVC   0(4,RE),=C'Mos='                                                 
         MVC   4(L'SVMOS,RE),SVMOS                                              
*                                                                               
LR144G   DS    0H                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
LR144X   SR    R0,R0                                                            
         ICM   R6,15,SVEL03        TEST PROCESSING 03/05 ELEMS                  
         BNZ   LR160                                                            
*                                                                               
         L     R6,SVEL01                                                        
         A     R6,AIO                                                           
LR150    SR    R0,R0                                                            
*                                                                               
         IC    R0,1(R6)            WAS LR150                                    
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    LR20                NO MORE- READ NEXT RECORD                    
*                                                                               
LR152    CLI   0(R6),1                                                          
         BNE   LR150               ELSE TRY FOR ANOTHER ELEMENT                 
         LR    R0,R6                                                            
         S     R0,AIO                                                           
         ST    R0,SVEL01                                                        
         XC    SVEL03,SVEL03                                                    
         MVI   SHOW03,C'N'                                                      
         B     LR50                GO PROCESS 01                                
*                                                                               
LR160    A     R6,AIO              POINT TO 03 ELEM                             
*                                                                               
LR162    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    LR20                                                             
         CLI   0(R6),X'01'                                                      
         BE    LR152                                                            
         CLI   0(R6),X'03'                                                      
         BNE   LR162                                                            
*                                                                               
         CLI   SHOW03,C'Y'         N=SKIP DISPLAY OF 03/05 ELEMS                
         BNE   LR162                                                            
         LR    R0,R6               SAVE DSPL TO 03 ELEMENT                      
         S     R0,AIO                                                           
         ST    R0,SVEL03                                                        
         B     LR121               GO PROCESS THIS 03                           
*                                                                               
LRX      TM    WHEN,X'40'          NOW REPORT?                                  
         BZ    LRXX                NO                                           
         BAS   RE,PRTOTAL                                                       
LRXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*==================================================================             
* PROCESS A NEW-STYLE INVOICE ELEMENT                                           
*==================================================================             
                                                                                
PROCINV  NTR1                                                                   
*                                                                               
         MVI   SHOW03,C'Y'                                                      
         ICM   R6,15,SVEL03        HAVE AN 03 ELEM NOW                          
         BZ    PRINV2              NO                                           
         A     R6,AIO              POINT TO ELEMENT                             
         B     PRINV10                                                          
*                                                                               
PRINV2   L     R6,SVEL01                                                        
         A     R6,AIO                                                           
         SR    R0,R0               MUST HAVE AN 01 ELEM                         
         IC    R0,1(R6)            SO POINT TO NEXT ELEM                        
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),3             03 MUST FOLLOW                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R6                                                            
         S     R0,AIO                                                           
         ST    R0,SVEL03                                                        
*                                                                               
         USING CLSTEL03,R6                                                      
PRINV10  MVC   LINV,CLS3INV                                                     
         DROP  R6                                                               
*                                                                               
         SR    R0,R0               NOW FIND 05 ELEM                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),5                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CLSTEL05,R6                                                      
****  EITHER CR OR CK FOR NOW                                                   
         XC    LSTAT,LSTAT                                                      
         TM    CLS5STAT,CLS5STAT_CR   X'40' - CREDIT MEMO                       
         BZ    *+10                                                             
         MVC   LSTAT,=C'CR'                                                     
         TM    CLS5STAT,CLS5STAT_CK   X'20' - CHECK (CASH RECEIPT)              
         BZ    *+10                                                             
         MVC   LSTAT,=C'CK'                                                     
****  EITHER CR OR CK FOR NOW                                                   
         MVC   WRK,CLS5GRS                                                      
         TM    OPTFLAG,OPTGRS      FIRST CHECK OPTION ENTERED                   
         BO    PRINV14                                                          
         TM    OPTFLAG,OPTNET                                                   
         BO    PRINV12                                                          
         CLI   SVA0GRS,C'G'        THEN CHECK A0 PROFILE                        
         BE    PRINV14                                                          
*                                                                               
PRINV12  MVC   WRK,CLS5NET                                                      
*                                                                               
PRINV14  TM    WHEN,X'40'          NOW REPORT?                                  
         BZ    PRINV16             NO                                           
         ICM   R3,15,WRK           ADD TOTAL UNCLEARANCES                       
         CVD   R3,DUB              CONVERT R3 TO A PACKED DECIMAL               
         AP    PACKDEC,DUB                                                      
*                                                                               
PRINV16  EDIT  (4,WRK),(11,LAMT),2,MINUS=YES                                    
*                                                                               
         L     R1,SVEL01                                                        
         A     R1,AIO                                                           
         TM    CLSTSTAT-CLSTEL01(R1),X'01' TEST UNCLEARANCE                     
         BZ    *+8                                                              
         MVI   LAMT-1,C'U'                 SET UNCLEARED FLAG                   
*                                                                               
*** CLEARED OPTION PROCESSING (PUT IN 02/11/02)                                 
*                                                                               
         MVC   LCKNUM,CLS5CHK       MOVE CHECK NUMBER TO SCREEN                 
         OC    CLS5BKDT,CLS5BKDT    DOES BANK CLEARED DATE EXIST?               
         BZ    PRINV18              NO                                          
         MVI   LAMT+11,C'C'         MOVE 'C' IN BEFORE CHECK NUMBER             
*                                                                               
PRINV18  TM    OPTFLAG,OPTCLR       CLEARED OPTION ON?                          
         BO    PRINV20              YES                                         
         GOTO1 DATCON,DMCB,(2,CLS5CHDT),(8,LCKDT)                               
         B     PRINVX                                                           
*                                                                               
PRINV20  OC    CLS5BKDT,CLS5BKDT    DOES BANK CLEARED DATE EXIST?               
         BZ    PRINVX               NO                                          
         GOTO1 DATCON,DMCB,(2,CLS5BKDT),(8,LCKDT)                               
*                                                                               
PRINVX   XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* PR- PRINT AN UNCLRST REPORT                                                   
***********************************************************************         
PR       NTR1                                                                   
         USING CLRSTATD,R4                                                      
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         OC    LASTCLT,LASTCLT     FIRST TIME THROUGH?                          
         BZ    PR10                YES                                          
         CLC   LASTCLT,CLSKCLT     SAME AS LAST CLIENT?                         
         BE    PR15                YES                                          
         MVI   FORCEHED,C'Y'       NO, FORCE PAGE BREAK                         
PR10     MVC   LASTCLT,CLSKCLT                                                  
*                                                                               
PR15     GOTO1 MSUNPK,DMCB,CLSKMKT,PRTMKT,PRTSTA                                
*                                                                               
         MVC   PRTREPAY,LREPT                                                   
         MVC   PRTCLRDT,LCLRDT                                                  
         MVC   PRTSEQ,LSEQ                                                      
         MVC   PRTPRD,LPRD                                                      
         MVC   PRTEST,LPRD+4                                                    
         MVC   PRTAMT,LAMT                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
*                                                                               
PRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* PRTOTAL- PRINT TOTAL UNCLEARANCES FOR REPORT                                  
***********************************************************************         
PRTOTAL  NTR1                                                                   
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVC   P(20),=C'UNCLEARANCE TOTAL = '                                   
         EDIT  (P8,PACKDEC),(10,P+21),2,MINUS=YES                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT UNCLEARANCE TOTAL                      
*                                                                               
PTX      B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*                             EDTOPTS                               *           
*********************************************************************           
EDTOPTS  NTR1                                                                   
*                                                                               
         LA    R2,LSTOPTH                                                       
         XC    SCANBLK(160),SCANBLK                                             
         GOTO1 SCANNER,DMCB,(R2),SCANBLK                                        
*                                                                               
         CLI   DMCB+4,0             INVALID SYNTAX?                             
         BE    ERRSYN               YES...ERROR                                 
*                                                                               
         LA    R5,SCANBLK-32                                                    
         USING SCANBLKD,R5                                                      
         SR    R1,R1                CLEAR COUNTER                               
*                                                                               
EDOPT10  LA    R5,32(R5)            NEXT OPTION                                 
         CLI   0(R5),0              TEST NO MORE                                
         BE    EDOPTX                                                           
         AHI   R1,1                 BUMP OPTION COUNTER                         
         SR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
*                                                                               
         EX    RE,TESTCLR                                                       
         BE    EDOPT20                                                          
         EX    RE,TESTUNC                                                       
         BE    EDOPT30                                                          
         EX    RE,TESTGRS                                                       
         BE    EDOPT40                                                          
         EX    RE,TESTNET                                                       
         BE    EDOPT50                                                          
         EX    RE,TESTNWE                                                       
         BE    EDOPT60                                                          
         EX    RE,TESTOLE                                                       
         BE    EDOPT70                                                          
         EX    RE,TESTDA                                                        
         BE    VOPT70                                                           
         EX    RE,TESTPID                                                       
         BE    VOPT80                                                           
         EX    RE,TESTACC                                                       
         BE    VOPT90                                                           
         EX    RE,TESTMOS                                                       
         BE    VOPT100                                                          
         EX    RE,TESTMOSF                                                      
         BE    VOPT110                                                          
*                                                                               
ERROPTS  CHI   R1,1                                                             
         BE    ERROP1                                                           
         CHI   R1,2                                                             
         BE    ERROP2                                                           
         B     ERROP3                                                           
*                                                                               
TESTCLR  CLC   SC1STFLD(0),=C'CLEARED'                                          
TESTUNC  CLC   SC1STFLD(0),=C'UNCLEARED'                                        
TESTGRS  CLC   SC1STFLD(0),=C'GROSS'                                            
TESTNET  CLC   SC1STFLD(0),=C'NET'                                              
TESTNWE  CLC   SC1STFLD(0),=C'NEWEL'                                            
TESTOLE  CLC   SC1STFLD(0),=C'OLDEL'                                            
TESTDA   CLC   SC1STFLD(0),=C'DA'                                               
TESTPID  CLC   SC1STFLD(0),=C'PID'                                              
TESTACC  CLC   SC1STFLD(0),=C'ACCAGY'                                           
TESTMOS  CLC   SC1STFLD(0),=C'MOS'                                              
TESTMOSF CLC   SC1STFLD(0),=C'MOSF'                                             
*                                                                               
EDOPT20  CLI   SC2NDLEN,0           L'RIGHT OF SEPARATOR EMPTY?                 
         BNE   ERROP1               YES...SHOULD BE                             
         TM    OPTFLAG,OPTCLR       IS THE OPTION ON ALREADY?                   
         BO    ERRDUP               YES...ERROR                                 
         OI    OPTFLAG,OPTCLR       TURN THE OPTION ON                          
         B     EDOPT10                                                          
*                                                                               
EDOPT30  OI    OPTFLAG,OPTUNCLR                                                 
         B     EDOPT10                                                          
*                                                                               
EDOPT40  OI    OPTFLAG,OPTGRS                                                   
         B     EDOPT10                                                          
*                                                                               
EDOPT50  OI    OPTFLAG,OPTNET                                                   
         B     EDOPT10                                                          
*                                                                               
EDOPT60  OI    OPTFLAG,OPTNWE      SHOW NEW 03/05 ELEMS                         
         B     EDOPT10                                                          
*                                                                               
EDOPT70  OI    OPTFLAG,OPTOLE      SHOW OLD 01 ELEMS                            
         B     EDOPT10                                                          
*                                                                               
VOPT70   OI    FLTFLG,FLTDA                                                     
         B     EDOPT10                                                          
*                                                                               
VOPT80   OI    FLTFLG,FLTPID        CHECKS NO-OPED TO ALLOW                     
***      TM    FLTFLG,FLTACC        FOR MULTIPLE ENTRIES                        
***      BO    ERRSYN               SPOT CAN DISPLAY MORE                       
         B     EDOPT10                                                          
*                                                                               
*                                                                               
VOPT90   OI    FLTFLG,FLTACC                                                    
***      TM    FLTFLG,FLTPID                                                    
***      BO    ERRSYN                                                           
         B     EDOPT10                                                          
*                                                                               
VOPT100  OI    FLTFLG,FLTMOS                                                    
***      TM    FLTFLG,FLTPID+FLTACC                                             
***      BNZ   ERRSYN                                                           
         B     EDOPT10                                                          
*                                                                               
VOPT110  GOTO1 DATVAL,DMCB,(2,SC2NDFLD),(0,DUB)      MOSF -> YYMMDD             
         OC    DMCB,DMCB                                                        
         BZ    ERRSYN                                                           
         GOTO1 DATCON,DMCB,DUB,(6,SVMOSF)            YYMNMDD> MMM/YY            
         B     EDOPT10                                                          
*                                                                               
EDOPTX   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
* *******************************************************************           
* MISC                                                                          
* *******************************************************************           
*                                                                               
ERRX     GOTO1 ERREX               ERROR CODE SET. DON'T RETURN                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                           ERROR MESSAGES                            *         
***********************************************************************         
ERROP1   MVC   ERRNUM,=AL2(871)     OPTION 1 NOT VALID                          
         B     SPERREX                                                          
ERROP2   MVC   ERRNUM,=AL2(872)     OPTION 2 NOT VALID                          
         B     SPERREX                                                          
ERROP3   MVC   ERRNUM,=AL2(873)     OPTION 3 NOT VALID                          
         B     SPERREX                                                          
ERRSYN   MVC   ERRNUM,=AL2(870)     INVALID SYNTAX FOR OPTIONS                  
         B     SPERREX                                                          
ERRDUP   MVC   ERRNUM,=AL2(854)     DUPLICATE FILTER                            
         B     SPERREX                                                          
ERRUNCL  MVC   ERRNUM,=AL2(1179)    UNC MUST BE IN OPTION FOR REPORT            
         B     SPERREX                                                          
ERRACT   MVC   ERRNUM,=AL2(1180)    PLEASE USE ACTION LIST FOR REPORT           
         B     SPERREX                                                          
ERRUNCL2 MVC   ERRNUM,=AL2(1182)    UNC MUST BE IN OPTION FOR CLT 'ALL'         
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
*        SET UP CALL FOR MSPACK/MSUNPK TO STAPACK                               
*                                                                               
SETMSP   NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVC   STAPACT,TACT                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPMKST,TMKST                                                   
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,WORK                                                    
         MVC   TMKST,STAPMKST                                                   
         MVC   TMKT,STAPQMKT                                                    
         MVC   TSTA,STAPQSTA                                                    
         MVC   BYTE,STAPERR                                                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* *******************************************************************           
* HEAD HOOK ROUTINE                                                             
* *******************************************************************           
*                                                                               
HDHOOK   NTR1                                                                   
*                                                                               
         USING CLRSTATD,R4                                                      
         MVC   H1(5),=C'MEDIA'                                                  
         MVC   H1+7(1),LSTMED                                                   
         MVC   H2(6),=C'CLIENT'                                                 
         MVC   H2+7(3),LSTCLT      MOVE IN THE CLIENT FROM SCREEN               
         TM    REPFLG,REPDATA      DID WE HAVE ANY DATA FROM THIS REP?          
         BZ    HOOK10              NOPE, DONT SHOW CLT FROM BAD KEY!            
*                                                                               
         GOTO1 CLUNPK,DMCB,CLSKCLT,H2+7                                         
*                                                                               
HOOK10   MVC   H3+45(25),=CL25'UNCLEARANCE STATUS REPORT'                       
         MVI   H4+45,X'BF'         UNDERLINE CHARACTER                          
         MVC   H4+46(25),H4+45                                                  
         DROP  R4                                                               
*                                                                               
HDHOOKX  B     XIT                                                              
         EJECT                                                                  
HEDSPECS DS    0H                                                               
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H7,2,C'   MARKET  STATION  REP  CLEAR DATE SEQ PRD EST  +        
                 AMOUNT'                                                        
         SSPEC H8,2,C'   ------  -------  ---  ---------- --- --- ---  +        
               ----------'                                                      
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADSP,ELCODE                                                
         SPACE 2                                                                
RELO     DS    F                                                                
TACT     DS    CL1                 ACTION                                       
TMKST    DS    XL5                 BINARY MKT/STA                               
TMKT     DS    CL4                 MARKET NUMBER                                
TSTA     DS    CL5                 STATION                                      
TNET     DS    CL3                 NETWORK                                      
ABRDMON  DS    A                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPSFMFFD                                                                      
* SCSFMCCD                                                                      
* DDGENTWA                                                                      
* DDPERVALD                                                                     
* SPGENCLRST                                                                    
* DDSCANBLKD                                                                    
* FAGETTXTD                                                                     
* SPSFMWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFMCCD          LIST SCREEN                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENCLRST                                                     
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* *******************************************************************           
* START OF SAVED STORAGE (6144)                                                 
* *******************************************************************           
         ORG   SYSSPARE                                                         
SEQNUM   DS    X                                                                
STAFLG   DS    X                   YES I KNOW "USE BITS FOR FLAGS"              
PAYFLG   DS    CL4                      "                                       
ORIGKEY  DS    XL(L'KEY)           SPOT KEY                                     
SAVEKEY  DS    XL(L'KEY)                                                        
LASTKEY  DS    XL(L'KEY)           KEY OF LAST RECD ON LAST LIST SCRN           
         DS    0H                                                               
STARTDAT DS    XL2                                                              
ENDDATE  DS    XL2                                                              
LOWDATE  DS    XL2                                                              
DATADSP  DS    H                                                                
WRK      DS    CL4                                                              
WRK2     DS    CL80                                                             
PREVMKT  DS    H                                                                
SVEL01   DS    F                   DSPL TO CURRENT 01 ELEM                      
SVEL03   DS    F                   DSPL TO CURRENT 05 ELEM                      
*                                                                               
PIDKEY   DS    XL25                                                             
*                                                                               
SVPID    DS    CL8                  PID                                         
SVACC    DS    CL2                  ACC AGENCY                                  
SVMOS    DS    CL6                  MOS                                         
SVMOSF   DS    CL6                  MOS FILTER                                  
*                                                                               
FLAG     DS    XL1                                                              
VAL01    EQU   X'01'                FOUND VALID CLEARNACE ELEMENT               
SCANBLK  DS    5CL32               SCANNER BLOCK                                
OPTFLAG  DS    X                   OPTIONS FLAG                                 
OPTCLR   EQU   X'80'               CLEARED                                      
OPTUNCLR EQU   X'40'               UNCLEARED                                    
OPTGRS   EQU   X'20'               SHOW GROSS                                   
OPTNET   EQU   X'10'               SHOW NET                                     
OPTNWE   EQU   X'08'               SHOW NEW 03/05 ELEMS                         
OPTOLE   EQU   X'04'               SHOW OLD 01 ELEMS                            
*                                                                               
FLTFLG   DS    X                                                                
FLTDA    EQU   X'10'               RETURN DISK ADDRESS                          
FLTPID   EQU   X'20'               RETURN PID                                   
FLTACC   EQU   X'40'               RETURN ACC AGY                               
FLTMOS   EQU   X'80'               RETURN MOS                                   
*                                                                               
ERRNUM   DS    XL2                                                              
LASTCLT  DS    XL2                 LAST CLIENT FOR PAGE BREAK IN REPORT         
FAKEFLDH DS    XL8                                                              
FAKEFLD  DS    XL3                                                              
PACKDEC  DS    PL8                                                              
REPFLG   DS    X                   VARIOUS FLAGS FOR REPORTING                  
REPDATA  EQU   X'80'               HAVE SOME DATA TO REPORT                     
SVA0GRS  DS    CL1                 PAY GROSS OR NET FROM A0 PROFILE             
NEWLIST  DS    CL1                 FOR LIST FROM NEW KEY                        
SHOW03   DS    CL1                                                              
SVMKTNM  DS    CL24                                                             
         EJECT                                                                  
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMKT     DS    CL4                 MKTNUM (NAME ON LINE 2)                      
         DS    CL1                                                              
LSTA     DS    CL5                 STATION                                      
         DS    CL1                                                              
LREPT    DS    CL1                 REP TYPE                                     
LPYEE    DS    CL3                 PAYEE                                        
         DS    CL1                                                              
LCLRDT   DS    CL8                 CLEARANCE DATE                               
         DS    CL1                                                              
LSEQ     DS    CL3                 SEQUENCE                                     
         DS    CL1                                                              
LPRD     DS    CL7                 PRODUCT                                      
         DS    CL1                                                              
LINV     DS    CL11                INVOICE                                      
         DS    CL1                                                              
LAMT     DS    CL10                AMOUNT                                       
         DS    CL1                                                              
LCKNUM   DS    CL6                 CHECK NUMBER                                 
LCKRCN   DS    CL1                 RECONCILIATION INDICATOR                     
         DS    CL1                                                              
LCKDT    DS    CL8                 CHECK DATE                                   
         DS    CL1                                                              
LSTAT    DS    CL2                 CR OR CK FOR NOW                             
*                                                                               
         SPACE 5                                                                
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL5                                                              
PRTMKT   DS    CL4                 MARKET/MARKET NAME                           
         DS    CL4                                                              
PRTSTA   DS    CL5                 STATION                                      
         DS    CL3                                                              
PRTREPAY DS    CL4                 REP TYPE/PAYEE                               
         DS    CL2                                                              
PRTCLRDT DS    CL8                 CLEARANCE DATE                               
         DS    CL2                                                              
PRTSEQ   DS    CL3                 SEQUENCE                                     
         DS    C                                                                
PRTPRD   DS    CL3                 PRODUCT                                      
         DS    C                                                                
PRTEST   DS    CL3                 ESTIMATE                                     
         DS    C                                                                
PRTAMT   DS    CL11                AMOUNT                                       
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPSFM2A   09/01/11'                                      
         END                                                                    
