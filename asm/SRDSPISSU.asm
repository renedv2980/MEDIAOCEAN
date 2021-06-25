*          DATA SET SRDSPISSU  AT LEVEL 054 AS OF 10/22/98                      
*PHASE PPSD02C,+0,NOAUTO                                                        
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'PPSD02 - CREATE GENFILE SRDS PUB RECORDS (FLAT FILE)'           
*                                                                               
*                                                                               
*      QOPT4     Y= PRINT ALL ERROR MESSAGES                                    
*             FRACTION, SIZE CODE, ETC., ERRORS WILL                            
*             ALWAYS PRINT EVEN IF QOPT4 IS NOT Y                               
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 10 RECORDS                                       
*      QOPT7     Y= DO NOT PRINT BLEE SIZE CODE ERRORS                          
*                                                                               
PPSD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPSD02,R9         NOTE R9 AS SECOND BASE                       
*                                   (R7 IS AVAILABLE)                           
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPSDWRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   MAINCNT,=P'0'                                                    
         ZAP   HEADCNT,=P'0'                                                    
         ZAP   TITLCNT,=P'0'                                                    
         ZAP   CORPCNT,=P'0'                                                    
         ZAP   PERSCNT,=P'0'                                                    
         ZAP   SHIPCNT,=P'0'                                                    
         ZAP   DIMECNT,=P'0'                                                    
         ZAP   BLEECNT,=P'0'                                                    
         ZAP   MECHCNT,=P'0'                                                    
         ZAP   SAFECNT,=P'0'                                                    
         ZAP   PRINCNT,=P'0'                                                    
         ZAP   ISSUCNT,=P'0'                                                    
         ZAP   DIGICNT,=P'0'                                                    
         ZAP   MAINSKP,=P'0'                                                    
         ZAP   HEADSKP,=P'0'                                                    
         ZAP   TITLSKP,=P'0'                                                    
         ZAP   CORPSKP,=P'0'                                                    
         ZAP   PERSSKP,=P'0'                                                    
         ZAP   SHIPSKP,=P'0'                                                    
         ZAP   DIMESKP,=P'0'                                                    
         ZAP   BLEESKP,=P'0'                                                    
         ZAP   MECHSKP,=P'0'                                                    
         ZAP   SAFESKP,=P'0'                                                    
         ZAP   PRINSKP,=P'0'                                                    
         ZAP   ISSUSKP,=P'0'                                                    
         ZAP   DIGISKP,=P'0'                                                    
         ZAP   WRTCNT,=P'0'        RECORDS TO BE WRITTEN                        
         ZAP   OUTCNT,=P'0'        RECORDS ACTUALLY WRITTEN                     
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   ISSULNG,=P'0'                                                    
         ZAP   ISSUMNY,=P'0'                                                    
         ZAP   TITLBUS,=P'0'                                                    
         ZAP   TITLCON,=P'0'                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST FOR REQUEST                            
         MVI   RCWRITE,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVC   DATADISP,=H'42'     FOR GETEL MACRO                              
         MVC   SYSFIL,=CL8'GENFIL'                                              
*                                                                               
**       MUST SWITCH TO THE CONTROL SYSTEM FOR GENDIR-GENFIL                    
*NOP*    L     RF,UTL                                                           
*NOP*    MVI   4(RF),X'0A'                                                      
*                                                                               
         OPEN  (XMAIN,INPUT,                                           X        
               XHEAD,INPUT,                                            X        
               XTITL,INPUT,                                            X        
               XCORP,INPUT,                                            X        
               XPERS,INPUT,                                            X        
               XSHIP,INPUT,                                            X        
               XDIME,INPUT,                                            X        
               XBLEE,INPUT,                                            X        
               XMECH,INPUT,                                            X        
               XSAFE,INPUT,                                            X        
               XPRIN,INPUT,                                            X        
               XISSU,INPUT,                                            X        
               XDIGI,INPUT,OUT,OUTPUT)                                          
*                                                                               
         XC    INPREC(22),INPREC                                                
         XC    MAINREC(22),MAINREC                                              
         XC    HEADREC(22),HEADREC                                              
         XC    TITLREC(22),TITLREC                                              
         XC    CORPREC(22),CORPREC                                              
         XC    PERSREC(22),PERSREC                                              
         XC    SHIPREC(22),SHIPREC                                              
         XC    DIMEREC(22),DIMEREC                                              
         XC    BLEEREC(22),BLEEREC                                              
         XC    MECHREC(22),MECHREC                                              
         XC    SAFEREC(22),SAFEREC                                              
         XC    PRINREC(22),PRINREC                                              
         XC    ISSUREC(22),ISSUREC                                              
         XC    DIGIREC(22),DIGIREC                                              
*                                                                               
**********************************************************************          
***         PROCESS PUB MAIN REC - BOTH MAIN AND HEAD RECORDS NEEDED            
***             FOR THE X'10' AND X'11' MAIN SRDS PUB ELEMENTS                  
***         TITL RECORD NEEDED TO SET CONSUMER OR BUSINESS KEY CODE             
**********************************************************************          
MAINP    DS    0H                                                               
         MVI   WORK,C'M'           READ MAIN RECORD                             
         GOTO1 =A(GETSREC)                                                      
MAINP10  MVI   WORK,C'H'           READ HEAD RECORD                             
MAINP20  GOTO1 =A(GETSREC)                                                      
         CLI   MAINREC,X'FF'       NO MORE MAIN RECORDS ?                       
         BE    EXIT                                                             
         CLC   MAINREC(9),HEADREC                                               
         BE    MAINP40             GO CHECK FOR TITLREC                         
         BH    MAINP30                                                          
*                              ERROR FLAG (PRINT) AND SKIP MAINREC              
         AP    MAINSKP,=P'1'                                                    
         MVC   P(4),=C'MAIN'                                                    
         MVC   P+7(9),=C'*NO HEAD*'                                             
         OC    MAINREC(80),SPACES                                               
         MVC   P+18(80),MAINREC                                                 
         BAS   RE,RPRT             PRINT THIS RECORD                            
         MVI   WORK,C'M'                                                        
*****    GOTO1 =A(GETSREC)         SKIP TO NEXT MAINREC                         
         B     MAINP20             SKIP TO NEXT MAINREC                         
MAINP30  DS    0H              ERROR FLAG (PRINT) AND SKIP HEADREC              
         AP    HEADSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   MAINP10             NO - SKIP TO NEXT HEADREC                    
         MVC   P(4),=C'HEAD'                                                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    HEADREC(112),SPACES                                              
         MVC   P+18(112),HEADREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     MAINP10             SKIP TO NEXT HEADREC                         
*                                                                               
MAINP40  MVI   WORK,C'F'           READ TITL RECORD                             
         GOTO1 =A(GETSREC)                                                      
         CLC   MAINREC(9),TITLREC                                               
         BE    MAINP60             OK - GO PROCESS                              
         BH    MAINP50             NO MAINREC - SKIP TITLREC                    
*                              ERROR FLAG (PRINT) AND SKIP MAINREC              
         AP    MAINSKP,=P'1'                                                    
         AP    HEADSKP,=P'1'                                                    
         MVC   P(4),=C'MAIN'                                                    
         MVC   P+7(9),=C'*NO TITL*'                                             
         OC    MAINREC(80),SPACES                                               
         MVC   P+18(80),MAINREC                                                 
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     MAINP               SKIP MAINREC AND HEADREC                     
MAINP50  DS    0H              ERROR FLAG (PRINT) AND SKIP TITLREC              
         AP    TITLSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   MAINP40             NO - SKIP TO NEXT TITLREC                    
         MVC   P(4),=C'TITL'                                                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    TITLREC(112),SPACES                                              
         MVC   P+18(112),TITLREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     MAINP40             SKIP TO NEXT TITLREC                         
*                                                                               
MAINP60  XC    GKEY,GKEY                                                        
         LA    R4,GKEY                                                          
         USING GPUBKEYD,R4                                                      
         LA    R5,MAINREC                                                       
         USING MAIN,R5                                                          
         MVI   GPUBTYP,GPUBTYPQ    RECORD TYPE CODE                             
         CLI   TITLTYP,C' '        BLANK ?                                      
         BH    *+6                 NO                                           
         DC    H'0'                SHOULD NEVER HAPPEN                          
         MVC   GPUBPUBT,TITLTYP    SRDS PUB TYPE (B OR C)                       
         MVC   GPUBPUB,MAPNUM      SRDS PUB NUMBER                              
         MVC   GPUBAGY,=C'ZZ'      SRDS AGENCY                                  
         MVC   GKEYSAVE,GKEY                                                    
*                                                                               
         AP    WRTCNT,=P'1'        RECORDS TO BE WRITTEN                        
*                                                                               
*    *****************************************************************          
*    * ** NOTE ** - PCONREC USED AS I/O AREA FOR GENFIL SIZE RECORD  *          
*    *****************************************************************          
*                                                                               
         XCEFL PCONREC-4,2004      CLEAR RECORD AREA                            
*                                                                               
         MVC   GKEY,GKEYSAVE                                                    
         MVC   PCONREC(32),GKEYSAVE                                             
*                                                                               
MAINP70  DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBHD,R6                                                        
         MVI   GPUBHEL,GPUBHEQU    X'10'                                        
         MVI   GPUBHLN,GPUBHLNQ    ELEMENT LENGTH                               
         OC    MANAME,SPACES                                                    
         MVC   GPUBHALN,MANAME     PUB NAME FROM XMAIN                          
         MVC   PUBNMSAV,MANAME     SAVE PUB NAME FOR ERROR PRINTING             
         DROP  R5,R6                                                            
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
*                                  ADD SECOND MAIN PUB ELEMENT                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBRD,R6                                                        
         MVI   GPUBREL,GPUBREQU    X'11'                                        
         MVI   GPUBRLN,GPUBRLNQ    ELEMENT LENGTH                               
         LA    R5,HEADREC                                                       
         USING HEAD,R5                                                          
         MVC   GPUBRAL1,HDAL1      ADDRESS LINE 1                               
         MVC   GPUBRAL2,HDAL2      ADDRESS LINE 2                               
         MVC   GPUBRAL3,HDAL3      ADDRESS LINE 3                               
         MVC   GPUBRCTY,HDCITY     CITY                                         
         MVC   GPUBRST,HDSTATE     STATE                                        
         MVC   GPUBRZIP,HDZIP      ZIP CODE                                     
         MVC   GPUBRPRV,HDPROV     PROVINCE                                     
         OC    GPUBRAL1(GPUBRAL3-GPUBRAL1),SPACES                               
         OC    GPUBRAL3(GPUBRST-GPUBRAL3),SPACES                                
         OC    GPUBRST(42),SPACES                                               
                                                                                
         DROP  R5,R6                                                            
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    CORPP10             NEXT RECORD TO TEST                          
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
CORPP10  DS    0H                                                               
         CLI   CORPREC,X'00'       FIRST TIME ?                                 
         BE    CORPPGET            YES                                          
         CLI   CORPREC,X'FF'       NO MORE RECORDS ?                            
         BE    PERSP10             YES - CHECK PERSONNEL FILE                   
         CLC   CORPREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    PERSP10             LEAVE FOR "LATER" PROCESSING                 
         BE    CORPP30             PROCESS THIS RECORD                          
*                        LOW - ERROR FLAG (PRINT) AND SKIP RECORD               
         AP    CORPSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   CORPPGET            NO - SKIP TO NEXT CORPREC                    
         MVC   P(4),=C'CORP'                                                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    CORPREC(112),SPACES                                              
         MVC   P+18(112),CORPREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     CORPPGET            SKIP TO NEXT CORPREC                         
*                                                                               
CORPP30  LA    R6,PCONREC+42       POINT TO FIRST (NAME) ELEMENT                
         USING GPUBHD,R6                                                        
         LA    R5,CORPREC                                                       
         USING CORP,R5                                                          
         MVC   GPUBHCOR,COCORP     CORPORATE OWNER FROM XCORP                   
         OC    GPUBHCOR,SPACES                                                  
         DROP  R5,R6                                                            
*                                                                               
CORPPGET DS    0H                                                               
         MVI   WORK,C'C'           READ CORP RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     CORPP10                                                          
*                                                                               
**********************************************************************          
*                                                                               
* NOTE: ONLY PROCESSING "SEQUENCE" 1 AND 2 PERS RECORDS HERE (7/23/98)          
*                                                                               
**********************************************************************          
*        PERSONNEL AND PERSONNEL EXTENSION ELEMENTS                  *          
**********************************************************************          
PERSP10  DS    0H                                                               
         CLI   PERSREC,X'00'       FIRST TIME ?                                 
         BE    PERSPGET            YES                                          
         CLI   PERSREC,X'FF'       NO MORE RECORDS ?                            
         BE    SHIPP10             YES - CHECK SHIP FILE                        
         CLC   PERSREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    SHIPP10             LEAVE FOR "LATER" PROCESSING                 
         BE    PERSP30             PROCESS THIS RECORD                          
*                        LOW - ERROR FLAG (PRINT) AND SKIP RECORD               
         AP    PERSSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   PERSPGET            NO - SKIP TO NEXT PERSREC                    
         MVC   P(4),=C'PERS'                                                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    PERSREC(112),SPACES                                              
         MVC   P+18(112),PERSREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     PERSPGET            SKIP TO NEXT PERSREC                         
*                                                                               
PERSP30  DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM+1,GPUBELNQ     ELEMENT LENGTH                               
         LA    R6,ELEM                                                          
         USING GPUBED,R6                                                        
         LA    R5,PERSREC                                                       
         USING PERSON,R5                                                        
         MVI   ELEM,GPUBEEQU       X'12' (PERSONNEL ELEMENT)                    
         CLI   PESEQ,C'1'          "SEQUENCE" 1 ?                               
         BE    *+8                 YES - ELSE MUST BE "SEQUENCE" 2              
         MVI   ELEM,GPUBLEQU         SO MAKE X'13' (EXTENSION ELEM)             
         MVC   GPUBEN1,PEFN        FIRST NAME                                   
         MVC   GPUBEN2,PEMN        MIDDLE NAME                                  
         MVC   GPUBEN3,PELN        LAST NAME                                    
         MVC   GPUBEAC,PEAC        AREA  CODE                                   
         MVC   GPUBEEX,PEEX        EXCHANGE                                     
         MVC   GPUBENUM,PENUM      NUMBER                                       
         MVC   GPUBEEXT,PEEXT      EXTENSION                                    
         MVC   GPUBETLE,PETITLE    TITLE                                        
         OC    GPUBEN1(GPUBETLE-GPUBEN1),SPACES                                 
         OC    GPUBETLE(L'GPUBETLE),SPACES                                      
         DROP  R5,R6                                                            
*                                  DELETE THE ELEMENT                           
         GOTO1 =V(HELLO),DMCB,(C'D',SYSFIL),(ELEM,PCONREC),0,0                  
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    PERSPGET            CHECK FOR ANOTHER REC                        
         DC    H'0'                                                             
*                                                                               
PERSPGET DS    0H                                                               
         MVI   WORK,C'P'           READ PERS RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     PERSP10                                                          
*                                                                               
**********************************************************************          
*        SHIPPING NAME AND SHIPPING ADDRESS ELEMENTS                 *          
**********************************************************************          
SHIPP10  DS    0H           NOTE - TWO ELEMENTS (X'14' (SHIP NAMES)             
*                                            AND X'15' (SHIP ADDRESS))          
*                                  WILL BE CREATED FROM EACH SHIPREC            
         CLI   SHIPREC,X'00'       FIRST TIME ?                                 
         BE    SHIPPGET            YES                                          
         CLI   SHIPREC,X'FF'       NO MORE RECORDS ?                            
         BE    DIMEP10             YES - CHECK DIMENSION FILE                   
         CLC   SHIPREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    DIMEP10             LEAVE FOR "LATER" PROCESSING                 
         BE    SHIPP30             PROCESS THIS RECORD                          
*                        LOW - ERROR FLAG (PRINT) AND SKIP RECORD               
         AP    SHIPSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   SHIPPGET            NO - SKIP TO NEXT SHIPREC                    
         MVC   P(4),=C'SHIP'                                                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    SHIPREC(112),SPACES                                              
         MVC   P+18(112),SHIPREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     SHIPPGET            SKIP TO NEXT SHIPREC                         
*                                                                               
SHIPP30  DS    0H                  PROCESS THIS RECORD                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBSD,R6                                                        
         MVI   GPUBSEL,GPUBSEQU    ELEMENT CODE (X'14')                         
         MVI   GPUBSLN,GPUBSLNQ    ELEMENT LENGTH                               
         LA    R5,SHIPREC                                                       
         USING SHIP,R5                                                          
         MVC   GPUBSN1,SHFN        FIRST NAME                                   
         MVC   GPUBSN2,SHMN        MIDDLE NAME                                  
         MVC   GPUBSN3,SHLN        LAST NAME                                    
         MVC   GPUBSTLE,SHTITLE    TITLE                                        
         MVC   GPUBSCOM,SHCOMP     COMPANY NAME                                 
         OC    GPUBSN1(GPUBSCOM-GPUBSLN),SPACES                                 
         OC    GPUBSCOM(L'GPUBSCOM),SPACES                                      
         DROP  R5,R6                                                            
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    SHIPP50             ADD SHIP ADDRESS ELEMENT                     
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
SHIPPGET DS    0H                                                               
         MVI   WORK,C'T'           READ SHIP RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     SHIPP10                                                          
*                                                                               
*                                                                               
SHIPP50  DS    0H                  SHIP ADDRESS (X'15') ELEMENT                 
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBTD,R6                                                        
         MVI   GPUBTEL,GPUBTEQU    ELEMENT CODE (X'15')                         
         MVI   GPUBTLN,GPUBTLNQ    ELEMENT LENGTH                               
         LA    R5,SHIPREC                                                       
         USING SHIP,R5                                                          
         MVC   GPUBTADR,SHADDR     STREET ADDRESS                               
         MVC   GPUBTCTY,SHCITY     CITY                                         
         MVC   GPUBTST,SHSTATE     STATE                                        
         MVC   GPUBTZIP,SHZIP      ZIP CODE                                     
         OC    GPUBTADR(GPUBTZIP-GPUBTLN),SPACES                                
         DROP  R5,R6                                                            
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    SHIPPGET            CHECK FOR ANOTHER REC                        
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
**********************************************************************          
*        AD DIMENSION ELEMENT                                        *          
**********************************************************************          
DIMEP10  DS    0H                  AD DIMENSION ELEMENT                         
         CLI   DIMEREC,X'00'       FIRST TIME ?                                 
         BE    DIMEPGET            YES                                          
         CLI   DIMEREC,X'FF'       NO MORE RECORDS ?                            
         BE    BLEEP10             YES - CHECK BLEED FILE                       
         CLC   DIMEREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    BLEEP10             LEAVE FOR "LATER" PROCESSING                 
         MVC   P(4),=C'DIME'       FOR POSSIBLE ERROR                           
         BE    DIMEP20             PROCESS THIS RECORD                          
*                        LOW - ERROR FLAG (PRINT) AND SKIP RECORD               
         AP    DIMESKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   DIMEPGET            NO - SKIP TO NEXT DIMEREC                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    DIMEREC(112),SPACES                                              
         MVC   P+18(112),DIMEREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     DIMEPGET            SKIP TO NEXT DIMEREC                         
*                                                                               
DIMEP20  DS    0H                  PROCESS THIS RECORD                          
         XC    WORK,WORK                                                        
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBAD,R6                                                        
         MVI   GPUBAEL,GPUBAEQU    X'20'                                        
         MVI   GPUBALN,GPUBALNQ    ELEMENT LENGTH (NO GPUBADES)                 
         LA    R5,DIMEREC                                                       
         USING DIMENS,R5                                                        
*                                                                               
         CLI   DITC,C'J'           J, S, OR T TABLE CODE ?                      
         BE    DIMEP30             YES - OK                                     
         CLI   DITC,C'S'           J, S, OR T TABLE CODE ?                      
         BE    DIMEP30             YES - OK                                     
         CLI   DITC,C'T'           J, S, OR T TABLE CODE ?                      
         BNE   DIMEP92             NOT J, S, OR T - REJECT                      
*                                                                               
DIMEP30  MVC   GPUBACD(1),DITC     TABLE CODE                                   
         MVC   WORK(4),DISC        SIZE CODE                                    
         BAS   RE,DOUNITS          RIGHT-JUSTIFY, ZERO-FILL                     
         LTR   R3,R3               SIZE CODE ZERO ?                             
         BZ    DIMEP92             YES - REJECT                                 
         MVC   GPUBACD+1(2),WORK+2   LAST 2 DIGITS OF SIZE CODE                 
         MVI   GPUBAUI,C'I'        UNITS INDICATOR (INCHES)                     
*                                                                               
         MVC   WORK(4),DIWU        WIDTH UNITS                                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBAWU        WIDTH UNITS TO ELEM                          
         MVC   WORK(4),DIWN        WIDTH NUMERATOR                              
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBAWN          WIDTH NUMERATOR TO ELEM                      
         MVC   WORK(4),DIWD        WIDTH DENOMINATOR                            
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBAWD          WIDTH DENOMINATOR TO ELEM                    
         ICM   R3,15,GPUBAWU       WIDTH, NUM, DENOM TO REGISTER 3              
         BAS   RE,DOFRACT          EDIT DIMENSIONS                              
         CLI   WORK,X'FF'          "BAD" ?                                      
         BE    DIMEP90             YES - PRINT AND SKIP RECORD                  
*                                                                               
         MVC   WORK(4),DIDU        DEPTH UNITS                                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBADU        DEPTH UNITS TO ELEM                          
         MVC   WORK(4),DIDN        DEPTH NUMERATOR                              
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBADN          DEPTH NUMERATOR TO ELEM                      
         MVC   WORK(4),DIDD        DEPTH DENOMINATOR                            
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBADD          DEPTH DENOMINATOR TO ELEM                    
         ICM   R3,15,GPUBADU       DEPTH, NUM, DENOM TO REGISTER 3              
         BAS   RE,DOFRACT          EDIT DIMENSIONS                              
         CLI   WORK,X'FF'          "BAD" ?                                      
         BE    DIMEP90             YES - PRINT AND SKIP RECORD                  
*                                                                               
         CLC   GPUBACD+1(2),=C'53' LAST 2 DIGITS OF SIZE CODE > 53 ?            
         BH    DIMEP60             YES                                          
         CLC   DIOTHER(17),SPACES  OTHER DESCRIPTION BLANK ?                    
         BH    DIMEP92             NO - PRINT AND SKIP RECORD                   
         B     DIMEP70             YES - ADD ELEM                               
DIMEP60  MVC   GPUBADES,DIOTHER    MOVE 1ST 17 BYTES OF OTHER DESCRIPT          
         OC    GPUBADES,SPACES     MAKE UPPER CASE                              
         MVI   GPUBALN,GPUBALNE    ELEMENT LENGTH (INCLUDING GPUBADES)          
*                                                                               
         DROP  R5,R6                                                            
*                                                                               
DIMEP70  DS    0H                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    DIMEPGET            CHECK FOR MORE RECS                          
         DC    H'0'                SOMETHING WRONG                              
*                                  PRINT AND SKIP RECORD                        
DIMEP90  DS    0H                                                               
         MVC   P+7(9),=C'BAD FRAC*'                                             
         B     DIMEPRT                                                          
DIMEP92  MVC   P+7(9),=C'SIZE COD*'                                             
DIMEPRT  OC    DIMEREC(112),SPACES                                              
         MVC   P+18(112),DIMEREC                                                
         MVC   PSECOND+4(50),PUBNMSAV       PUBLICATION NAME                    
         BAS   RE,RPRT             PRINT RECORD                                 
         AP    DIMESKP,=P'1'                                                    
*****    B     DIMEPGET            SKIP THIS RECORD                             
*                                                                               
DIMEPGET DS    0H                                                               
         MVI   WORK,C'A'           READ DIME RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     DIMEP10                                                          
*                                                                               
**********************************************************************          
*        BLEED AD DIMENSION ELEMENT                                  *          
**********************************************************************          
BLEEP10  DS    0H                  BLEED AD DIMENSION ELEMENT                   
         CLI   BLEEREC,X'00'       FIRST TIME ?                                 
         BE    BLEEPGET            YES                                          
         CLI   BLEEREC,X'FF'       NO MORE RECORDS ?                            
         BE    MECHP10             YES - CHECK MECHANICALS FILE                 
         CLC   BLEEREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    MECHP10             LEAVE FOR "LATER" PROCESSING                 
         MVC   P(4),=C'BLEE'       FOR POSSIBLE ERROR                           
         BE    BLEEP20             PROCESS THIS RECORD                          
*                        LOW - ERROR FLAG (PRINT) AND SKIP RECORD               
         AP    BLEESKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   BLEEPGET            NO - SKIP TO NEXT BLEEREC                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    BLEEREC(52),SPACES                                               
         MVC   P+18(52),BLEEREC                                                 
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     BLEEPGET            SKIP TO NEXT BLEEREC                         
*                                                                               
BLEEP20  DS    0H                  PROCESS THIS RECORD                          
         XC    WORK,WORK                                                        
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBBD,R6                                                        
         MVI   GPUBBEL,GPUBBEQU    X'30'                                        
         MVI   GPUBBLN,GPUBBLNQ    ELEMENT LENGTH                               
         LA    R5,BLEEREC                                                       
         USING BLEED,R5                                                         
*                                                                               
         CLI   BLTC,C'J'           J, S, OR T TABLE CODE ?                      
         BE    BLEEP30             YES - OK                                     
         CLI   BLTC,C'S'           J, S, OR T TABLE CODE ?                      
         BE    BLEEP30             YES - OK                                     
         CLI   BLTC,C'T'           J, S, OR T TABLE CODE ?                      
         BNE   BLEEP92             NOT J, S, OR T - REJECT                      
*                                                                               
BLEEP30  MVC   GPUBBCD(1),BLTC     TABLE CODE                                   
         MVC   WORK(4),BLSC        SIZE CODE                                    
         BAS   RE,DOUNITS          RIGHT-JUSTIFY, ZERO-FILL                     
         LTR   R3,R3               SIZE CODE ZERO ?                             
         BZ    BLEEP92             YES - REJECT                                 
         CLC   WORK+2(2),=C'53'    SIZE CODE GT 53 ?                            
         BNH   BLEEP50             NO - CONTINUE                                
*                                  YES - TEST DIMENSION ELEMENT                 
         MVI   ELCODE,GPUBAEQU     X'20' ELEMENT CODE                           
         LA    R2,PCONREC+42       POINT TO FIRST ELEMENT                       
BLEEP35  BAS   RE,NEXTEL           LOOK FOR DIMENSION ELEMENT                   
         BNE   BLEEP92             NOT FOUND - REJECT BLEEREC                   
         CLC   BLTC(3),2(R2)       SAME SIZE CODE ?                             
         BNE   BLEEP35             NO - LOOK FOR ANOTHER DIME ELEM              
BLEEP40  MVC   GPUBBDES,16(R2)     DESCRIPTION FROM DIMENSION ELEMENT           
         MVI   GPUBBLN,GPUBBLNE    "LONGER" ELEMENT LENGTH                      
*                                                                               
         ZAP   DUMPCNT,=P'9'       ************  TESTING  ************          
*                                                                               
BLEEP50  MVC   GPUBBCD+1(2),WORK+2   LAST 2 DIGITS OF SIZE CODE                 
         MVI   GPUBBUI,C'I'        UNITS INDICATOR (INCHES)                     
*                                                                               
         MVC   WORK(4),BLWU        WIDTH UNITS                                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBBWU        WIDTH UNITS TO ELEM                          
         MVC   WORK(4),BLWN        WIDTH NUMERATOR                              
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBBWN          WIDTH NUMERATOR TO ELEM                      
         MVC   WORK(4),BLWD        WIDTH DENOMINATOR                            
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBBWD          WIDTH DENOMINATOR TO ELEM                    
         ICM   R3,15,GPUBBWU       WIDTH, NUM, DENOM TO REGISTER 3              
         BAS   RE,DOFRACT          EDIT DIMENSIONS                              
         CLI   WORK,X'FF'          "BAD" ?                                      
         BE    BLEEP90             YES - PRINT AND SKIP RECORD                  
*                                                                               
         MVC   WORK(4),BLDU        DEPTH UNITS                                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBBDU        DEPTH UNITS TO ELEM                          
         MVC   WORK(4),BLDN        DEPTH NUMERATOR                              
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBBDN          DEPTH NUMERATOR TO ELEM                      
         MVC   WORK(4),BLDD        DEPTH DENOMINATOR                            
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBBDD          DEPTH DENOMINATOR TO ELEM                    
         ICM   R3,15,GPUBBDU       DEPTH, NUM, DENOM TO REGISTER 3              
         BAS   RE,DOFRACT          EDIT DIMENSIONS                              
         CLI   WORK,X'FF'          "BAD" ?                                      
         BE    BLEEP90             YES - PRINT AND SKIP RECORD                  
*                                                                               
         DROP  R5,R6                                                            
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    BLEEPGET            CHECK FOR MORE RECORDS                       
         DC    H'0'                SOMETHING WRONG                              
*                                  PRINT AND SKIP RECORD                        
BLEEP90  DS    0H                                                               
         MVC   P+7(9),=C'BAD FRAC*'                                             
         B     BLEEPRT                                                          
BLEEP92  CLI   QOPT7,C'Y'          PRINT BLEE SIZE COD ERRORS ?                 
         BE    BLEEPRX             NO - SKIP TO NEXT BLEEREC                    
         MVC   P+7(9),=C'SIZE COD*'                                             
BLEEPRT  OC    BLEEREC(51),SPACES                                               
         MVC   P+18(51),BLEEREC                                                 
         MVC   PSECOND+4(50),PUBNMSAV       PUBLICATION NAME                    
         BAS   RE,RPRT             PRINT RECORD                                 
BLEEPRX  AP    BLEESKP,=P'1'                                                    
*****    B     BLEEPGET            SKIP THIS RECORD                             
*                                                                               
BLEEPGET DS    0H                                                               
         MVI   WORK,C'B'           READ BLEE RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     BLEEP10                                                          
*                                                                               
**********************************************************************          
*        MECHANICALS ELEMENT                                         *          
**********************************************************************          
MECHP10  DS    0H                  MECHANICALS ELEMENT                          
         CLI   MECHREC,X'00'       FIRST TIME ?                                 
         BE    MECHPGET            YES                                          
         CLI   MECHREC,X'FF'       NO MORE RECORDS ?                            
         BE    SAFEP10             YES - CHECK SAFETY FILE                      
         CLC   MECHREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    SAFEP10             LEAVE FOR "LATER" PROCESSING                 
         MVC   P(4),=C'MECH'       FOR POSSIBLE ERROR                           
         BE    MECHP20             PROCESS THIS RECORD                          
*                        LOW - ERROR FLAG (PRINT) AND SKIP RECORD               
         AP    MECHSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   MECHPGET            NO - SKIP TO NEXT MECHREC                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    MECHREC(112),SPACES                                              
         MVC   P+18(112),MECHREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     MECHPGET            SKIP TO NEXT MECHREC                         
*                                                                               
MECHP20  DS    0H                  PROCESS THIS RECORD                          
         XC    WORK,WORK                                                        
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBMD,R6                                                        
         MVI   GPUBMEL,GPUBMEQU    X'40'                                        
         MVI   GPUBMLN,GPUBMLNQ    ELEMENT LENGTH                               
         LA    R5,MECHREC                                                       
         USING MECH,R5                                                          
*                                  ???????????????????????????                  
*                                                                               
*   NOTE: THE LOGIC BELOW IS TESTING ONLY FOR 'X' IN                            
*         SINGLE-BYTE FIELDS AS A "POSITIVE" INDICATOR.                         
*                                                                               
*         TESTING FOR ANYTHING IN MULTI-BYTE DESCRIPTIVE FIELDS                 
*         (LIKE MEBDO - "BINDING - OTHER") IS DONE AGAINST ONLY                 
*         THE FIRST BYTE IN THE FIELD.                                          
*         ANYTHING GREATER THAN X'40' IS TAKEN AS POSITIVE                      
*                                                                               
         CLI   MEFR1,C'X'          FULL RUN 1 ?                                 
         BNE   *+8                 NO                                           
         OI    GPUBM1,X'01'        SET FULL RUN                                 
         CLI   MERG1,C'X'          REGIONAL 1 ?                                 
         BNE   *+8                 NO                                           
         OI    GPUBM1,X'02'        SET REGIONAL                                 
         CLI   MECV1,C'X'          COVER 1 ?                                    
         BNE   *+8                 NO                                           
         OI    GPUBM1,X'04'        SET COVER                                    
         CLI   MEFR2,C'X'          FULL RUN 2 ?                                 
         BNE   *+8                 NO                                           
         OI    GPUBM2,X'01'        SET FULL RUN                                 
         CLI   MERG2,C'X'          REGIONAL 2 ?                                 
         BNE   *+8                 NO                                           
         OI    GPUBM2,X'02'        SET REGIONAL                                 
         CLI   MECV2,C'X'          COVER 2 ?                                    
         BNE   *+8                 NO                                           
         OI    GPUBM2,X'04'        SET COVER                                    
         CLI   MEFR3,C'X'          FULL RUN 3 ?                                 
         BNE   *+8                 NO                                           
         OI    GPUBM3,X'01'        SET FULL RUN                                 
         CLI   MERG3,C'X'          REGIONAL 3 ?                                 
         BNE   *+8                 NO                                           
         OI    GPUBM3,X'02'        SET REGIONAL                                 
         CLI   MECV3,C'X'          COVER 3 ?                                    
         BNE   *+8                 NO                                           
         OI    GPUBM3,X'04'        SET COVER                                    
*                                  *** BINDING                                  
         CLI   MEBDF,C'X'          BINDING - FOLD ?                             
         BNE   *+8                 NO                                           
         OI    GPUBMBD,X'01'       SET FOLD                                     
         CLI   MEBDP,C'X'          BINDING - PERFECT ?                          
         BNE   *+8                 NO                                           
         OI    GPUBMBD,X'02'       SET PERFECT                                  
         CLI   MEBDS,C'X'          BINDING - STITCHED ?                         
         BNE   *+8                 NO                                           
         OI    GPUBMBD,X'04'       SET STITCHED                                 
         CLI   MEBDSW,C'X'         BINDING - SIDE WIRE ?                        
         BNE   *+8                 NO                                           
         OI    GPUBMBD,X'08'       SET SIDE WIRE                                
         CLI   MEBDO,C' '          BINDING - OTHER ?                            
         BNH   *+8                 NO                                           
         OI    GPUBMBD,X'10'       SET OTHER                                    
*                                  *** COLOR                                    
         CLI   MECL1,C'X'          COLOR 1 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'01'       SET COLOR 1                                  
         CLI   MECL2,C'X'          COLOR 2 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'02'       SET COLOR 2                                  
         CLI   MECL3,C'X'          COLOR 3 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'04'       SET COLOR 3                                  
         CLI   MECL4,C'X'          COLOR 4 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'08'       SET COLOR 4                                  
         CLI   MECL5,C'X'          COLOR 5 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'10'       SET COLOR 5                                  
         CLI   MECL6,C'X'          COLOR 6 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'20'       SET COLOR 6                                  
         CLI   MECL7,C'X'          COLOR 7 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'40'       SET COLOR 7                                  
         CLI   MECL8,C'X'          COLOR 8 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCL,X'80'       SET COLOR 8                                  
*                                  *** COVER                                    
         CLI   MECOV1,C'X'         COVER 1 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCV,X'01'       SET COVER 1                                  
         CLI   MECOV2,C'X'         COVER 2 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCV,X'02'       SET COVER 2                                  
         CLI   MECOV3,C'X'         COVER 3 FLAG ?                               
         BNE   *+8                 NO                                           
         OI    GPUBMCV,X'04'       SET COVER 3                                  
         CLI   MECOVO,C' '         COVER - OTHER ?                              
         BNH   *+8                 NO                                           
         OI    GPUBMCV,X'80'       SET COVER - OTHER                            
*                                                                               
         MVI   GPUBMTUI,C'I'       TRIM UNITS INDICATOR (INCHES)                
*                                                                               
*    NOTE: ************    SEE NOTE ABOVE   *********************               
*                                                                               
*              TEST FOR ALL ZEROS IN UNITS AND FRACTION FIELDS                  
*              IF ALL SIX OF THESE ARE ZERO LEAVE APPROPRIATE ELEMENT           
*              FIELDS AT ZERO - DO NOT REJECT THE INPUT RECORD                  
*                                                                               
         MVC   WORK(4),MEWU        WIDTH UNITS                                  
         BAS   RE,DOUNITS                                                       
         LTR   R3,R3               FIELD EQUAL TO 0 ?                           
         BNZ   MECHP70             NO - CONTINUE EDIT                           
         MVC   WORK(4),MEWN        WIDTH NUMERATOR                              
         BAS   RE,DOUNITS                                                       
         LTR   R3,R3               FIELD EQUAL TO 0 ?                           
         BNZ   MECHP70             NO - CONTINUE EDIT                           
         MVC   WORK(4),MEWD        WIDTH DENOMINATOR                            
         BAS   RE,DOUNITS                                                       
         LTR   R3,R3               FIELD EQUAL TO 0 ?                           
         BNZ   MECHP70             NO - CONTINUE EDIT                           
         MVC   WORK(4),MEDU        DEPTH UNITS                                  
         BAS   RE,DOUNITS                                                       
         LTR   R3,R3               FIELD EQUAL TO 0 ?                           
         BNZ   MECHP70             NO - CONTINUE EDIT                           
         MVC   WORK(4),MEDN        DEPTH NUMERATOR                              
         BAS   RE,DOUNITS                                                       
         LTR   R3,R3               FIELD EQUAL TO 0 ?                           
         BNZ   MECHP70             NO - CONTINUE EDIT                           
         MVC   WORK(4),MEDD        DEPTH DENOMINATOR                            
         BAS   RE,DOUNITS                                                       
         LTR   R3,R3               FIELD EQUAL TO 0 ?                           
         BZ    MECHP75             YES - ALL ZEROS - ADD ELEMENT                
*                                                                               
MECHP70  DS    0H                                                               
         MVC   WORK(4),MEWU        WIDTH UNITS                                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBMWU        WIDTH UNITS TO ELEM                          
         MVC   WORK(4),MEWN        WIDTH NUMERATOR                              
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBMWN          WIDTH NUMERATOR TO ELEM                      
         MVC   WORK(4),MEWD        WIDTH DENOMINATOR                            
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBMWD          WIDTH DENOMINATOR TO ELEM                    
         ICM   R3,15,GPUBMWU       WIDTH, NUM, DENOM TO REGISTER 3              
         BAS   RE,DOFRACT          EDIT DIMENSIONS                              
         CLI   WORK,X'FF'          "BAD" ?                                      
         BE    MECHP90             YES - PRINT AND SKIP RECORD                  
*                                                                               
         MVC   WORK(4),MEDU        DEPTH UNITS                                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBMDU        DEPTH UNITS TO ELEM                          
         MVC   WORK(4),MEDN        DEPTH NUMERATOR                              
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBMDN          DEPTH NUMERATOR TO ELEM                      
         MVC   WORK(4),MEDD        DEPTH DENOMINATOR                            
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBMDD          DEPTH DENOMINATOR TO ELEM                    
         ICM   R3,15,GPUBMDU       DEPTH, NUM, DENOM TO REGISTER 3              
         BAS   RE,DOFRACT          EDIT DIMENSIONS                              
         CLI   WORK,X'FF'          "BAD" ?                                      
         BE    MECHP90             YES - PRINT AND SKIP RECORD                  
*                                                                               
         DROP  R5,R6                                                            
MECHP75  DS    0H                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    MECHPGET            CHECK FOR MORE RECORDS                       
         DC    H'0'                SOMETHING WRONG                              
*                                  PRINT AND SKIP RECORD                        
MECHP90  DS    0H                                                               
         MVC   P+7(9),=C'BAD FRAC*'                                             
MECHPRT  OC    MECHREC(112),SPACES                                              
*****    MVC   P+18(112),MECHREC                                                
         MVC   P+18(22),MECHREC                                                 
         OC    MECHREC+111(45),SPACES                                           
         MVC   P+40(45),MECHREC+111                                             
         MVC   PSECOND+4(50),PUBNMSAV       PUBLICATION NAME                    
         BAS   RE,RPRT             PRINT RECORD                                 
         AP    MECHSKP,=P'1'                                                    
*****    B     MECHPGET            SKIP THIS RECORD                             
*                                                                               
MECHPGET DS    0H                                                               
         MVI   WORK,C'E'           READ MECH RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     MECHP10                                                          
*                                                                               
**********************************************************************          
*        SAFETY ELEMENT                                              *          
**********************************************************************          
SAFEP10  DS    0H                  SAFETY ELEMENT                               
         CLI   SAFEREC,X'00'       FIRST TIME ?                                 
         BE    SAFEPGET            YES                                          
         CLI   SAFEREC,X'FF'       NO MORE RECORDS ?                            
         BE    PRINP10             YES - CHECK PRINTING FILE                    
         CLC   SAFEREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    PRINP10             LEAVE FOR "LATER" PROCESSING                 
         MVC   P(4),=C'SAFE'       FOR POSSIBLE ERROR                           
         BE    SAFEP20             PROCESS THIS RECORD                          
*                        LOW - ERROR FLAG (PRINT) AND SKIP RECORD               
         AP    SAFESKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   SAFEPGET            NO - SKIP TO NEXT SAFEREC                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    SAFEREC(50),SPACES                                               
         MVC   P+18(50),SAFEREC                                                 
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     SAFEPGET            SKIP TO NEXT SAFEREC                         
*                                                                               
SAFEP20  DS    0H                  PROCESS THIS RECORD                          
         XC    WORK,WORK                                                        
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBFD,R6                                                        
         MVI   GPUBFEL,GPUBFEQU    X'42'                                        
         MVI   GPUBFLN,GPUBFLNQ    ELEMENT LENGTH                               
         LA    R5,SAFEREC                                                       
         USING SAFETY,R5                                                        
*                                                                               
         MVC   GPUBFCD,SFC         SAFETY CODE  (1-6)                           
         MVI   GPUBFUI,C'I'        UNITS INDICATOR (INCHES)                     
*                                                                               
         MVC   WORK(4),SFU         SAFETY UNITS                                 
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBFU         SAFETY UNITS TO ELEM                         
         MVC   WORK(4),SFN         SAFETY NUMERATOR                             
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBFNU          SAFETY NUMERATOR TO ELEM                     
         MVC   WORK(4),SFD         SAFETY DENOMINATOR                           
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STC   R3,GPUBFDE          SAFETY DENOMINATOR TO ELEM                   
         ICM   R3,15,GPUBFU        UNITS, NUM, DENOM TO REGISTER 3              
         BAS   RE,DOFRACT          EDIT DIMENSIONS                              
         CLI   WORK,X'FF'          "BAD" ?                                      
         BE    SAFEP90             YES - PRINT AND SKIP RECORD                  
*                                                                               
         DROP  R5,R6                                                            
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    SAFEPGET            CHECK FOR MORE RECORDS                       
         DC    H'0'                SOMETHING WRONG                              
*                                  PRINT AND SKIP RECORD                        
SAFEP90  MVC   P(4),=C'SAFE'                                                    
         MVC   P+7(9),=C'BAD FRAC*'                                             
         OC    SAFEREC(50),SPACES                                               
         MVC   P+18(50),SAFEREC                                                 
         MVC   PSECOND+4(50),PUBNMSAV       PUBLICATION NAME                    
         BAS   RE,RPRT             PRINT RECORD                                 
         AP    SAFESKP,=P'1'                                                    
*****    B     SAFEPGET            SKIP THIS RECORD                             
*                                                                               
*                                                                               
SAFEPGET DS    0H                                                               
         MVI   WORK,C'S'           READ SAFE RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     SAFEP10                                                          
*                                                                               
**********************************************************************          
*        PRINTING INFORMATION ELEMENT                                *          
**********************************************************************          
PRINP10  DS    0H                  PRINTING INFORMATION ELEMENT                 
         CLI   PRINREC,X'00'       FIRST TIME ?                                 
         BE    PRINPGET            YES                                          
         CLI   PRINREC,X'FF'       MORE RECORDS ?                               
         BNE   PRINP15             YES                                          
         BAS   RE,PUBOUTP          NO - FINISH SRDS PUB RECORD                  
         B     ISSUPROC            PROCESS ISSUE RECORD                         
PRINP15  CLC   PRINREC(9),GPUBPUB  TEST PUB NUMBER                              
         BE    PRINP30             PROCESS THIS RECORD                          
         BL    PRINP20             ERROR                                        
         BAS   RE,PUBOUTP          LEAVE FOR "LATER" PROCESSING                 
         B     ISSUPROC            PROCESS ISSUE RECORD                         
PRINP20  DS    0H            LOW - ERROR FLAG (PRINT) AND SKIP RECORD           
         AP    PRINSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   PRINPGET            NO - SKIP TO NEXT PRINREC                    
         MVC   P(4),=C'PRIN'                                                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    PRINREC(112),SPACES                                              
         MVC   P+18(112),PRINREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     PRINPGET            SKIP TO NEXT PRINREC                         
*                                                                               
PRINP30  DS    0H                  PROCESS THIS RECORD                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBPD,R6                                                        
         MVI   GPUBPEL,GPUBPEQU    X'44'                                        
         MVI   GPUBPLN,GPUBPLNQ    ELEMENT LENGTH                               
         LA    R5,PRINREC                                                       
         USING PRINTD,R5                                                        
*                                  *** ROP                                      
         CLI   PRROPP,C'X'         ROP - POSITIVE ?                             
         BNE   *+8                 NO                                           
         OI    GPUBPROP,X'01'      SET POSITIVE                                 
         CLI   PRROPN,C'X'         ROP - NEGATIVE ?                             
         BNE   *+8                 NO                                           
         OI    GPUBPROP,X'02'      SET NEGATIVE                                 
         CLI   PRROPSU,C'X'        ROP - SIDE UP ?                              
         BNE   *+8                 NO                                           
         OI    GPUBPROP,X'04'      SET SIDE UP                                  
         CLI   PRROPSD,C'X'        ROP - SIDE DOWN ?                            
         BNE   *+8                 NO                                           
         OI    GPUBPROP,X'08'      SET SIDE DOWN                                
         CLI   PRROPUD,C'X'        ROP - SIDE UP OR DOWN ?                      
         BNE   *+8                 NO                                           
         OI    GPUBPROP,X'10'      SET SIDE UP OR DOWN                          
         MVC   GPUBPSW,PRROPSW     ??????????????????????                       
*                                  *** 2 COLOR                                  
         MVC   WORK(4),PR2CRS      2 COLOR - RECOMMENDED SCREEN                 
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP2CS       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR2CMS      2 COLOR - MAXIMUM SCREEN                     
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP2CM       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR2CTD      2 COLOR - TONE DENSITY PCT.                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP2CT       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR2C2MD     2 COLOR - 2ND MAXIMUM DENSITY PCT.           
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP2C2       CONVERTED FIELD TO ELEM                      
*                                  *** 4 COLOR                                  
         MVC   WORK(4),PR4CRS      4 COLOR - RECOMMENDED SCREEN                 
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP4CS       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR4CMS      4 COLOR - MAXIMUM SCREEN                     
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP4CM       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR4CTD      4 COLOR - TONE DENSITY PCT.                  
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP4CT       CONVERTED FIELD TO ELEM                      
*                                  *** 4 COLOR - COLOR PCT                      
         MVC   WORK(4),PR4CY       4 COLOR - YELLOW PCT.                        
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP4CY       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR4CM       4 COLOR - MAGENTA PCT.                       
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP4CG       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR4CC       4 COLOR - CYAN PCT.                          
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP4CC       CONVERTED FIELD TO ELEM                      
         MVC   WORK(4),PR4CB       4 COLOR - BLACK PCT.                         
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         STCM  R3,3,GPUBP4CB       CONVERTED FIELD TO ELEM                      
*                                                                               
*                                  *** COLOR ROTATION CODES (GPUBPCRC)          
*  DEPENDING ON WHAT (AND HOW MANY) COLORS ARE FOUND IN THE 50-BYTE             
*  PRCRCD INPUT FIELD, THE 4-BYTE GPUBPCRC FIELD WILL CONTAIN:                  
*           "B" FOR BLACK                                                       
*    AND/OR "C" FOR CYAN                                                        
*    AND/OR "M" FOR MAGENTA                                                     
*    AND/OR "Y" FOR YELLOW     IN THE ORDER IN WHICH THEY WERE FOUND            
*                                                                               
         LA    R0,47               LUP COUNTER                                  
         OC    PRCRCD,SPACES       MAKE UPPER CASE                              
         LA    RE,PRCRCD           I/P - ROTATION - OTHER DESCRIPT.             
         LA    RF,GPUBPCRC         ELEMENT COLOR ROTATION CODES                 
PRINP50  CLC   =C'BLACK',0(RE)                                                  
         BNE   *+12                                                             
         MVI   0(RF),C'B'                                                       
         LA    RF,1(RF)            BUMP TO NEXT "CODE" POSITION                 
         CLC   =C'CYAN',0(RE)                                                   
         BNE   *+12                                                             
         MVI   0(RF),C'C'                                                       
         LA    RF,1(RF)            BUMP TO NEXT "CODE" POSITION                 
         CLC   =C'MAGENTA',0(RE)                                                
         BNE   *+12                                                             
         MVI   0(RF),C'M'                                                       
         LA    RF,1(RF)            BUMP TO NEXT "CODE" POSITION                 
         CLC   =C'YELLOW',0(RE)                                                 
         BNE   *+12                                                             
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)            BUMP TO NEXT "CODE" POSITION                 
         LA    RE,1(RE)            BUMP TO NEXT DESCRIPTION POSITION            
         BCT   R0,PRINP50          CHECK AGAIN FOR A COLOR                      
*                                                                               
*                                  CHECK FOR DUPLICATE COLOR (CODES)            
         MVC   WORK(20),SPACES                                                  
         MVC   WORK(4),GPUBPCRC    ELEMENT COLOR ROTATION CODES                 
         LA    R0,3                LUP COUNTER                                  
         LA    RF,WORK                                                          
PRINP60  CLI   0(RF),C' '          ANY COLOR FOUND ?                            
         BNH   PRINP65             NO - DONE                                    
         CLC   0(1,RF),1(RF)       DUPLICATE COLOR CODE ?                       
         BE    PRINP82             YES - ERROR                                  
         CLC   0(1,RF),2(RF)       DUPLICATE COLOR CODE ?                       
         BE    PRINP82             YES - ERROR                                  
         CLC   0(1,RF),3(RF)       DUPLICATE COLOR CODE ?                       
         BE    PRINP82             YES - ERROR                                  
         LA    RF,1(RF)            BUMP TO NEXT "CODE" POSITION                 
         BCT   R0,PRINP60          CHECK NEXT POSITIONS                         
*                                                                               
PRINP65  MVC   WORK(4),PRPROG      PROGRESSIVE                                  
         BAS   RE,DOUNITS          CONVERT TO BINARY                            
         STC   R3,GPUBPPRG         CONVERTED FIELD TO ELEM (1 BYTE)             
         MVC   WORK(4),PRCOMP      COMPREHENSIVE                                
         BAS   RE,DOUNITS          CONVERT TO BINARY                            
         STC   R3,GPUBPCOM         CONVERTED FIELD TO ELEM (1 BYTE)             
         MVC   WORK(4),PRCROM      CROMALIN                                     
         BAS   RE,DOUNITS          CONVERT TO BINARY                            
         STC   R3,GPUBPCRO         CONVERTED FIELD TO ELEM (1 BYTE)             
         MVC   WORK(4),PRMP        MATCH PRINT                                  
         BAS   RE,DOUNITS          CONVERT TO BINARY                            
         STC   R3,GPUBPMPT         CONVERTED FIELD TO ELEM (1 BYTE)             
*                                  *** OTHER                                    
         CLI   PROPOS,C'X'         OTHER - POSITIVE ?                           
         BNE   *+8                 NO                                           
         OI    GPUBPO,X'01'        SET POSITIVE                                 
         CLI   PRONEG,C'X'         OTHER - NEGATIVE ?                           
         BNE   *+8                 NO                                           
         OI    GPUBPO,X'02'        SET NEGATIVE                                 
         CLI   PROSU,C'X'          OTHER - SIDE UP ?                            
         BNE   *+8                 NO                                           
         OI    GPUBPO,X'04'        SET SIDE UP                                  
         CLI   PROSD,C'X'          OTHER - SIDE DOWN ?                          
         BNE   *+8                 NO                                           
         OI    GPUBPO,X'08'        SET SIDE DOWN                                
         CLI   PROSUD,C'X'         OTHER - SIDE UP OR DOWN ?                    
         BNE   *+8                 NO                                           
         OI    GPUBPO,X'10'        SET SIDE UP OR DOWN                          
*                                                                               
         DROP  R5,R6                                                            
*                                  ADD THE ELEMENT                              
         GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    PRINPGET    ??????  CHECK FOR MORE RECORDS  ??????????           
         DC    H'0'                SOMETHING WRONG                              
*                                  PRINT AND SKIP RECORD                        
PRINP82  MVC   P(4),=C'PRIN'                                                    
         MVC   P+7(9),=C'DUP COLR*'                                             
         OC    PRINREC(27),SPACES                                               
         MVC   P+18(27),PRINREC                                                 
         OC    PRINREC+87(83),SPACES                                            
         MVC   P+47(83),PRINREC+87                                              
         MVC   PSECOND+4(50),PUBNMSAV       PUBLICATION NAME                    
         BAS   RE,RPRT             PRINT RECORD                                 
         AP    PRINSKP,=P'1'                                                    
*****    B     PRINPGET            SKIP THIS RECORD                             
*                                                                               
*                                                                               
PRINPGET DS    0H                                                               
         MVI   WORK,C'R'           READ PRIN RECORD                             
         GOTO1 =A(GETSREC)                                                      
         B     PRINP10                                                          
         EJECT                                                                  
*                                                                               
**********************************************************************          
*    CREATE RECORD SUB-TYPE X'01' ISSUE DATE RECORDS HERE            *          
**********************************************************************          
*                                                                               
ISSUPROC DS    0H                                                               
*                                                                               
*NOP*    B     MAINP               TESTING *********************                
*                                                                               
         ZAP   ISSUNUM,=P'0'       NO OF ELEMENTS                               
         XCEFL PCONREC+26,1974     CLEAR RECORD AFTER AGENCY                    
         MVI   PCONREC+26,1        ISSUE DATE RECORD SUB-TYPE                   
         MVC   GKEY,PCONREC                                                     
         MVC   GKEYSAVE,GKEY                                                    
         MVI   TESTSW,0                                                         
*                                                                               
**********************************************************************          
*        ISSUE DATE ELEMENT                                          *          
**********************************************************************          
ISSUP10  DS    0H                  ISSUE DATE ELEMENT                           
         CLI   ISSUREC,X'00'       FIRST TIME ?                                 
         BE    ISSUPGET            YES                                          
         CLI   ISSUREC,X'FF'       MORE RECORDS ?                               
         BE    ISSUPEND            NO - CHECK FOR WRITE                         
         CLC   ISSUREC(9),GPUBPUB  TEST PUB NUMBER                              
         BH    ISSUPEND            CHECK FOR WRITE                              
         BL    ISSUP20                                                          
*                                                                               
         CLC   ISSUREC(9),=C'000913000'                                         
         BE    ISSUPTST                                                         
         CLC   ISSUREC(9),=C'005116000'                                         
         BE    ISSUPTST                                                         
         CLC   ISSUREC(9),=C'032948000'                                         
         BE    ISSUPTST                                                         
         CLC   ISSUREC(9),=C'041132000'                                         
         BE    ISSUPTST                                                         
         CLC   ISSUREC(9),=C'109328000'                                         
         BE    ISSUPTST                                                         
         CLC   ISSUREC(9),=C'213676000'                                         
         BNE   ISSUP30                                                          
*                                                                               
ISSUPTST DS    0H                                                               
*                                  PRINT AND SKIP RECORD                        
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         CLI   TESTSW,0                                                         
         BNE   ISSUPTC                                                          
         MVC   P+4(50),PUBNMSAV       PUBLICATION NAME                          
         MVI   TESTSW,1                                                         
         BAS   RE,RPRT             PRINT RECORD                                 
ISSUPTC  MVI   TESTSW,1                                                         
         MVC   P+7(9),=C'TOO MANY*'                                             
         MVC   P(4),=C'ISSU'                                                    
         OC    ISSUREC(112),SPACES                                              
         MVC   P+18(34),ISSUREC                                                 
         OC    ISSUREC+50(112),SPACES                                           
         MVC   P+55(76),ISSUREC+85                                              
         BAS   RE,RPRT             PRINT RECORD                                 
         AP    ISSUSKP,=P'1'                                                    
*****    B     ISSUPGET            SKIP THIS RECORD                             
*                                                                               
*                                                                               
         MVI   WORK,C'I'           READ ISSUE RECORD                            
         GOTO1 =A(GETSREC)                                                      
         B     ISSUP10                                                          
*                                                                               
*                            LOW - ERROR FLAG (PRINT) AND SKIP RECORD           
ISSUP20  AP    ISSUSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   ISSUPGET            NO - SKIP TO NEXT ISSUREC                    
         MVC   P(4),=C'ISSU'                                                    
         MVC   P+7(9),=C'*NO MAIN*'                                             
         OC    ISSUREC(112),SPACES                                              
         MVC   P+18(112),ISSUREC                                                
         BAS   RE,RPRT             PRINT THIS RECORD                            
         B     ISSUPGET            SKIP TO NEXT ISSUREC                         
*                                                                               
ISSUP30  DS    0H                  PROCESS THIS RECORD                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GPUBID,R6                                                        
         MVI   GPUBIEL,GPUBIEQU    X'60'                                        
         MVI   GPUBILN,GPUBILNQ    ELEMENT LENGTH                               
         LA    R5,ISSUREC                                                       
         USING ISSUE,R5                                                         
*                                                                               
         OC    ISID,SPACES                                                      
         CLC   ISID(3),SPACES      ANY DESCRIPTION ?                            
         BNH   ISSUP96             NO - SKIP                                    
         OC    ISOSD(48),SPACES                                                 
         CLC   ISOSD(48),SPACES    ANY DATES ?                                  
         BH    ISSUP34             YES - CONTINUE                               
         MVC   P+7(9),=C'NO DATES*'                                             
         B     ISSUP92             PRINT AND SKIP RECORD                        
ISSUP34  MVC   GPUBIID,ISID        20 CHARACTERS OF ISSUE DESCRIPTION           
         CLC   ISID+20(3),SPACES   DESCRIPTION GT 20 CHARACTERS ?               
         BNH   ISSUP40             NO                                           
         MVC   P(4),=C'ISSU'                                                    
         MVC   P+7(9),=C'LONG DESC'                                             
         OC    ISSUREC(112),SPACES                                              
         MVC   P+18(112),ISSUREC                                                
         MVC   PSECOND+4(50),PUBNMSAV       PUBLICATION NAME                    
         BAS   RE,RPRT             PRINT RECORD - DO NOT SKIP IT                
         AP    ISSULNG,=P'1'                                                    
ISSUP40  DS    0H                                                               
         LA    R0,6                LUP COUNTER                                  
         LA    R1,ISOSD       START OF 6 4-BYTE DAY/4-BYTE MONTH FIELDS         
         LA    RF,GPUBIOSD    START OF 6 2-BYTE M/D FIELDS (O/P)                
ISSUP45  MVC   WORK(4),0(R1)       4-BYTE DAY                                   
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         CH    R3,=H'31'           DAY GREATER THAN 31 ?                        
         BH    ISSUP90             YES - ERROR                                  
         STC   R3,1(RF)            DAY TO OUTPUT                                
         MVC   WORK(4),4(R1)       4-BYTE MONTH                                 
         BAS   RE,DOUNITS          CONVERT TO 2-BYTE BINARY                     
         CH    R3,=H'12'           MONTH GREATER THAN 12 ?                      
         BH    ISSUP90             YES - ERROR                                  
         STC   R3,0(RF)            MONTH TO OUTPUT                              
         LA    R1,8(R1)            BUMP TO NEXT "PAIR"                          
         LA    RF,2(RF)            BUMP TO NEXT OUTPUT                          
         BCT   R0,ISSUP45                                                       
*                                                                               
         DROP  R4,R5,R6                                                         
*                                                                               
         AP    ISSUNUM,=P'1'                                                    
         CP    ISSUNUM,=P'53'      52 IS MAXIMUM                                
         BL    ISSUP80             OK                                           
         AP    ISSUMNY,=P'1'                                                    
         MVC   P+7(9),=C'TOO MANY*'                                             
         B     ISSUP92                                                          
*                                                                               
*                                  ADD THE ELEMENT                              
ISSUP80  GOTO1 =V(HELLO),DMCB,(C'P',SYSFIL),PCONREC,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    ISSUPGET            CHECK FOR MORE RECORDS                       
         DC    H'0'                SOMETHING WRONG                              
*                                  PRINT AND SKIP RECORD                        
ISSUP90  MVC   P+7(9),=C'BAD DATE*'                                             
ISSUP92  MVC   P(4),=C'ISSU'                                                    
         OC    ISSUREC(112),SPACES                                              
         MVC   P+18(34),ISSUREC                                                 
         OC    ISSUREC+50(112),SPACES                                           
         MVC   P+55(76),ISSUREC+85                                              
         MVC   PSECOND+4(50),PUBNMSAV       PUBLICATION NAME                    
         BAS   RE,RPRT             PRINT RECORD                                 
ISSUP96  AP    ISSUSKP,=P'1'                                                    
*****    B     ISSUPGET            SKIP THIS RECORD                             
*                                                                               
*                                                                               
ISSUPGET DS    0H                                                               
         MVI   WORK,C'I'           READ ISSUE RECORD                            
         GOTO1 =A(GETSREC)                                                      
         B     ISSUP10                                                          
*                                                                               
ISSUPEND DS    0H                  CHECK FOR OUTPUT                             
         MVI   TESTSW,0                                                         
         CLI   PCONREC+42,0        ANY ELEMENTS ADDED ?                         
*NOP*    BNH   DIGIPROC            NO - CHECK DIGITAL DATA RECORDS              
         BNH   MAINP               NO                                           
         BAS   RE,PUBOUTP          WRITE SRDS PUB RECORD                        
*NOP*    B     DIGIPROC                                                         
         B     MAINP                                                            
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
RUNL     DS    0H                                                               
         CLOSE (XMAIN,,XHEAD,,XCORP,,XPERS,,XSHIP,,XDIME,,XBLEE,,      X        
               XMECH,,XSAFE,,XPRIN,,XISSU,,XTITL,,XDIGI,,OUT)                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
RUNL10   DS    0H                                                               
*                                                                               
RUNL40   GOTO1 REPORT                                                           
         LA    R4,COUNTHDR                                                      
         LA    R5,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNLX                                                            
         MVC   P(04),0(R4)                                                      
         OI    3(R5),X'0F'                                                      
         UNPK  P+08(07),0(4,R5)                                                 
         OI    07(R5),X'0F'                                                     
         UNPK  P+18(07),4(4,R5)                                                 
         CLC   =C'ISSU',0(R4)                                                   
         BNE   RUNL60                                                           
         OI    ISSULNG+3,X'0F'                                                  
         UNPK  P+28(07),ISSULNG                                                 
         OI    ISSUMNY+3,X'0F'                                                  
         UNPK  P+38(07),ISSUMNY                                                 
RUNL60   CLC   =C'TITL',0(R4)                                                   
         BNE   RUNL90                                                           
         OI    TITLBUS+3,X'0F'                                                  
         UNPK  P+28(07),TITLBUS                                                 
         OI    TITLCON+3,X'0F'                                                  
         UNPK  P+38(07),TITLCON                                                 
RUNL90   GOTO1 REPORT                                                           
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         B     RUNL50                                                           
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
**********************************************************************          
*        OUTPUT SRDS PUB RECORD TO CONTROL FILE                      *          
**********************************************************************          
PUBOUTP  NTR1                      WRITE SRDS PUB RECORD TO FILE                
*                                                                               
         MVC   HALF,PCONREC+32     LENGTH OF RECORD                             
         LH    R1,HALF                                                          
         LA    R1,4(R1)            TOTAL LENGTH OF RECORD                       
         STCM  R1,3,PCONREC-4                                                   
******************  TESTING BELOW  ****************************                 
         CP    OUTCNT,=P'49'       50 RECORDS WRITTEN ?                         
         BH    POP44               YES - DON'T WRITE MORE                       
         AP    OUTCNT,=P'1'                                                     
******************  TESTING ABOVE  ****************************                 
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    POP44               NO                                           
         PUT   OUT,PCONREC-4                                                    
*                                                                               
POP44    CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   POP50               NO                                           
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    POP50               YES                                          
*                                                                               
POP46    MVC   P(11),=C'*** NEW ***'                                            
*NOP*    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
POP50    DS    0H                  RETURN                                       
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
*                             LINK TO REPORT                                    
         DS    0F                                                               
RPRT     NTR1                                                                   
         CLI   TESTSW,1                                                         
         BNE   RPRTXIT                                                          
         MVI   RCSUBPRG,0                                                       
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
RPRTXIT  MVC   PSECOND,SPACES                                                   
         B     EXIT                                                             
*                                                                               
COUNTHDR DS    0C                                                               
         DC    CL4'MAIN'                                                        
         DC    CL4'HEAD'                                                        
         DC    CL4'TITL'                                                        
         DC    CL4'CORP'                                                        
         DC    CL4'PERS'                                                        
         DC    CL4'SHIP'                                                        
         DC    CL4'DIME'                                                        
         DC    CL4'BLEE'                                                        
         DC    CL4'MECH'                                                        
         DC    CL4'SAFE'                                                        
         DC    CL4'PRIN'                                                        
         DC    CL4'ISSU'                                                        
         DC    CL4'DIGI'                                                        
         DC    CL4'OUT*'                                                        
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        EDIT DIMENSIONS - UNITS, NUMERATOR AND DENOMINATOR                     
*    R3 HAS UNITS IN HIGH-ORDER BYTES 1-2, NUMERATOR IN BYTE 3,                 
*               AND DENOMINATOR IN BYTE 4                                       
**********************************************************************          
DOFRACT  NTR1                                                                   
         LTR   R3,R3               UNITS, NUM, DENOM. ALL ZEROS ?               
         BZ    DOFRNG              YES - ERROR                                  
         ST    R3,FULL                                                          
         CLI   FULL+2,63           NUMERATOR GT 63 ?                            
         BH    DOFRNG              YES - ERROR                                  
         CLI   FULL+3,64           DENOMINATOR GT 64 ?                          
         BH    DOFRNG              YES - ERROR                                  
         CLI   FULL+2,0            NUMERATOR ZERO ?                             
         BNE   DOFR30              NO                                           
         CLI   FULL+3,0            YES - DENOMINATOR ZERO ?                     
         BNE   DOFRNG              NO - ERROR                                   
         B     EXIT                RETURN                                       
DOFR30   CLI   FULL+3,0            DENOMINATOR ZERO ?                           
         BE    DOFRNG              YES - ERROR                                  
         CLC   FULL+3(1),FULL+2    DENOMINATOR GT NUMERATOR ?                   
         BH    EXIT                YES - OK - RETURN                            
*                                                                               
DOFRNG   MVI   WORK,X'FF'          BAD DIMENSION INDICATOR                      
         B     EXIT                RETURN                                       
*                                                                               
*********************************************************************           
*   RIGHT-JUSTIFY AND ZERO-FILL LEFT-JUSTIFIED FOUR-CHARACTER FIELD             
*          (EX. 3..., 30.., 255. BECOME 0003, 0030, 0255)                       
*      INPUT AND CONVERTED OUTPUT IN WORK(4) AND 2-BYTE BINARY                  
*            VALUE OF CONVERTED FIELD IN WORK+16(2)                             
*                      AND ALSO IN R3                                           
*        IF VALUE OF FIELD IS ZERO, X'FF' OUTPUT TO WORK+20                     
*********************************************************************           
DOUNITS  NTR1                                                                   
         MVI   WORK+20,0           CLEAR                                        
         MVC   WORK+12(4),=4C'0'   ZERO-FILL "WORK" FIELD                       
         LA    R0,4                LUP COUNTER                                  
         LA    R3,WORK+15          END OF "WORK" FIELD                          
         LA    R4,WORK+3           END OF INPUT FIELD                           
DOUN20   CLI   0(R4),C'0'          INPUT NUMERIC ?                              
         BL    DOUN40              NO                                           
         MVC   0(1,R3),0(R4)       YES - MOVE                                   
         BCTR  R3,0                MOVE LEFT 1 (OUTPUT)                         
DOUN40   BCTR  R4,0                MOVE LEFT 1 (INPUT)                          
         BCT   R0,DOUN20           NEXT CHARACTER                               
*                                                                               
         MVC   WORK(4),WORK+12     STORE CONVERTED FIELD                        
         PACK  DUB,WORK(4)         PACK FIELD                                   
         CVB   R3,DUB              CONVERT TO BINARY                            
         STCM  R3,3,WORK+16        STORE RESULT (2 BYTES)                       
         LTR   R3,R3               VALUE ZERO ?                                 
         BNZ   DOUNXIT             NO                                           
         MVI   WORK+20,X'FF'       YES                                          
*                                                                               
DOUNXIT  DS    0H                  RETURN WITH 2-BYTE BINARY VALUE              
         XIT1  REGS=(R3)           IN WORK+16 AND IN R3                         
*                                                                               
         EJECT                                                                  
*                                                                               
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         MVI   P,C' '                                                           
         LA    R5,GKEY                                                          
         LA    R2,40                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(40),0(R5)                                                   
         TR    WORK(40),TRTAB                                                   
         MVC   P+90(40),WORK                                                    
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         MVI   P,C' '                                                           
         LA    R5,PCONREC-4                                                     
         MVC   HALF,36(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4B4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
XMAIN    DCB   DDNAME=XMAIN,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=MAINEOF,                                          X        
               MACRF=GM                                                         
XHEAD    DCB   DDNAME=XHEAD,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00445,                                            X        
               BLKSIZE=04450,                                          X        
               EODAD=HEADEOF,                                          X        
               MACRF=GM                                                         
XTITL    DCB   DDNAME=XTITL,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=TITLEOF,                                          X        
               MACRF=GM                                                         
XCORP    DCB   DDNAME=XCORP,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=CORPEOF,                                          X        
               MACRF=GM                                                         
XPERS    DCB   DDNAME=XPERS,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=PERSEOF,                                          X        
               MACRF=GM                                                         
XSHIP    DCB   DDNAME=XSHIP,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00445,                                            X        
               BLKSIZE=04450,                                          X        
               EODAD=SHIPEOF,                                          X        
               MACRF=GM                                                         
XDIME    DCB   DDNAME=XDIME,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=DIMEEOF,                                          X        
               MACRF=GM                                                         
XBLEE    DCB   DDNAME=XBLEE,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=BLEEEOF,                                          X        
               MACRF=GM                                                         
XMECH    DCB   DDNAME=XMECH,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=MECHEOF,                                          X        
               MACRF=GM                                                         
XSAFE    DCB   DDNAME=XSAFE,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=SAFEEOF,                                          X        
               MACRF=GM                                                         
XPRIN    DCB   DDNAME=XPRIN,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00500,                                            X        
               BLKSIZE=05000,                                          X        
               EODAD=PRINEOF,                                          X        
               MACRF=GM                                                         
XISSU    DCB   DDNAME=XISSU,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00255,                                            X        
               BLKSIZE=02550,                                          X        
               EODAD=ISSUEOF,                                          X        
               MACRF=GM                                                         
XDIGI    DCB   DDNAME=XDIGI,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00780,                                            X        
               BLKSIZE=07800,                                          X        
               EODAD=DIGIEOF,                                          X        
               MACRF=GM                                                         
*                                                                               
OUT      DCB   DDNAME=TAPEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02048,                                            X        
               BLKSIZE=08120,                                          X        
               BUFNO=2,                                                X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
***                                I/O ROUTINES                                 
GETSREC  CSECT                                                                  
         NMOD1 0,GETSREC           NOTE: DO NOT USE R9 IN THIS                  
         USING PPWORKD,RA                CSECT - IT IS NEEDED TO                
         USING PPSDWRKD,R8               ADDRESS PRIOR FIELDS                   
*                                                                               
         CLI   WORK,C'M'                                                        
         BE    GETMAIN                                                          
         CLI   WORK,C'H'                                                        
         BE    GETHEAD                                                          
         CLI   WORK,C'F'                                                        
         BE    GETTITL                                                          
         CLI   WORK,C'C'                                                        
         BE    GETCORP                                                          
         CLI   WORK,C'P'                                                        
         BE    GETPERS                                                          
         CLI   WORK,C'T'                                                        
         BE    GETSHIP                                                          
         CLI   WORK,C'A'                                                        
         BE    GETDIME                                                          
         CLI   WORK,C'B'                                                        
         BE    GETBLEE                                                          
         CLI   WORK,C'E'                                                        
         BE    GETMECH                                                          
         CLI   WORK,C'S'                                                        
         BE    GETSAFE                                                          
         CLI   WORK,C'R'                                                        
         BE    GETPRIN                                                          
         CLI   WORK,C'I'                                                        
         BE    GETISSU                                                          
         CLI   WORK,C'D'                                                        
         BE    GETDIGI                                                          
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
GETMAIN  DS    0H                                                               
         CLI   MAINREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT             (SHOULD NOT HAPPEN HERE)                     
         MVC   LASTMAIN,MAINREC                                                 
GETM10   GET   XMAIN,INPREC                                                     
         MVC   MAINREC,INPREC                                                   
         CLI   MAINREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETM10              YES - SKIP                                   
         AP    MAINCNT,=P'1'                                                    
         CLC   LASTMAIN,MAINREC                                                 
         BL    GETEXIT             OK - NOT DUP OR OUT OF SEQUENCE              
*                              ERROR FLAG (PRINT) AND SKIP MAINREC              
         BE    GETM20                                                           
         MVC   P(15),=C'MAIN   NOT/SEQ*'                                        
         B     GETM30                                                           
GETM20   MVC   P(15),=C'MAIN   *DUPLIC*'                                        
GETM30   AP    MAINSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETM10              NO - SKIP THIS RECORD                        
         OC    MAINREC(80),SPACES                                               
         MVC   P+18(80),MAINREC                                                 
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETM10              SKIP THIS RECORD                             
*                                                                               
GETHEAD  DS    0H                                                               
         CLI   HEADREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTHEAD,HEADREC                                                 
GETH10   GET   XHEAD,INPREC                                                     
         MOVE  (HEADREC,445),INPREC                                             
         CLI   HEADREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETH10              YES - SKIP                                   
         AP    HEADCNT,=P'1'                                                    
         CLC   LASTHEAD,HEADREC                                                 
         BL    GETEXIT             OK - NOT DUP OR OUT OF SEQUENCE              
*                              ERROR FLAG (PRINT) AND SKIP HEADREC              
         BE    GETH20                                                           
         MVC   P(15),=C'HEAD   NOT/SEQ*'                                        
         B     GETH30                                                           
GETH20   MVC   P(15),=C'HEAD   *DUPLIC*'                                        
GETH30   AP    HEADSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETH10              NO - SKIP THIS RECORD                        
         OC    HEADREC(112),SPACES                                              
         MVC   P+18(112),HEADREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETH10              SKIP THIS RECORD                             
*                                                                               
GETTITL  DS    0H                                                               
         MVI   TITLTYP,C' '        CLEAR (B)USINESS OR (C)ONSUMER BYTE          
         CLI   TITLREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTTITL,TITLREC                                                 
GETF10   GET   XTITL,INPREC                                                     
         MVC   TITLREC,INPREC                                                   
         CLI   TITLREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETF10              YES - SKIP                                   
         AP    TITLCNT,=P'1'                                                    
         CLC   LASTTITL,TITLREC                                                 
         BL    GETF40              OK - NOT DUP OR OUT OF SEQUENCE              
*                              ERROR FLAG (PRINT) AND SKIP HEADREC              
         BE    GETF20                                                           
         MVC   P(15),=C'TITL   NOT/SEQ*'                                        
         B     GETF30                                                           
GETF20   MVC   P(15),=C'TITL   *DUPLIC*'                                        
GETF30   AP    TITLSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETF10              NO - SKIP THIS RECORD                        
         OC    TITLREC(112),SPACES                                              
         MVC   P+18(112),TITLREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETF10              SKIP THIS RECORD                             
GETF40   DS    0H                                                               
         LA    R5,TITLREC                                                       
         USING TITLE,R5                                                         
         CLC   TIPID(2),=C'5 '     BUSINESS TYPE PUB ?                          
         BNE   GETF45                                                           
         MVI   TITLTYP,C'B'        SET (B)USINESS FOR PUB KEY                   
         AP    TITLBUS,=P'1'                                                    
         B     GETEXIT             DONE                                         
GETF45   CLC   TIPID(2),=C'18'     CONSUMER TYPE PUB ?                          
         BNE   GETF50                                                           
         MVI   TITLTYP,C'C'        SET (C)ONSUMER FOR PUB KEY                   
         AP    TITLCON,=P'1'                                                    
         B     GETEXIT             DONE                                         
GETF50   MVC   P(15),=C'TITL   NOT 5/18'                                        
         B     GETF30              PRINT AND SKIP TITLREC                       
         DROP  R5                                                               
*                                                                               
GETCORP  DS    0H                                                               
         CLI   CORPREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTCORP,CORPREC                                                 
GETC10   GET   XCORP,INPREC                                                     
         MVC   CORPREC,INPREC                                                   
         CLI   CORPREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETC10              YES - SKIP                                   
         AP    CORPCNT,=P'1'                                                    
         CLC   LASTCORP,CORPREC                                                 
         BL    GETEXIT             OK - NOT DUP OR OUT OF SEQUENCE              
*                              ERROR FLAG (PRINT) AND SKIP CORPREC              
         BE    GETC20                                                           
         MVC   P(15),=C'CORP   NOT/SEQ*'                                        
         B     GETC30                                                           
GETC20   MVC   P(15),=C'CORP   *DUPLIC*'                                        
GETC30   AP    CORPSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETC10              NO - SKIP THIS RECORD                        
         OC    CORPREC(122),SPACES                                              
         MVC   P+18(112),CORPREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETC10              SKIP THIS RECORD                             
*                                                                               
GETPERS  DS    0H                                                               
         CLI   PERSREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTPERS,PERSREC                                                 
GETP10   GET   XPERS,INPREC                                                     
         MVC   PERSREC,INPREC                                                   
         CLI   PERSREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETP10              YES - SKIP                                   
         AP    PERSCNT,=P'1'                                                    
         CLC   LASTPERS,PERSREC                                                 
         BNH   GETP20              NOT OUT OF SEQUENCE                          
*                              ERROR FLAG (PRINT) AND SKIP PERSREC              
         MVC   P(15),=C'PERS   NOT/SEQ*'                                        
         B     GETP30                                                           
GETP20   LA    R5,PERSREC                                                       
         USING PERSON,R5                                                        
         CLI   PESEQ,C'1'          SEQUENCE 1 ? (PERSONNEL)                     
         BE    GETEXIT             YES - OK                                     
         CLI   PESEQ,C'2'          SEQUENCE 2 ? (EXTENSION)                     
         BE    GETEXIT             YES - OK                                     
         DROP  R5                                                               
         MVC   P(15),=C'PERS   NOT 1/2*'                                        
GETP30   AP    PERSSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETP10              NO - SKIP THIS RECORD                        
         OC    PERSREC(112),SPACES                                              
         MVC   P+18(112),PERSREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETP10              SKIP THIS RECORD                             
*                                                                               
GETSHIP  DS    0H                                                               
         CLI   SHIPREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTSHIP,SHIPREC                                                 
GETT10   GET   XSHIP,INPREC                                                     
         MOVE  (SHIPREC,310),INPREC                                             
         CLI   SHIPREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETT10              YES - SKIP                                   
         AP    SHIPCNT,=P'1'                                                    
         CLC   LASTSHIP,SHIPREC                                                 
         BL    GETEXIT             OK - NOT DUP OR OUT OF SEQUENCE              
*                              ERROR FLAG (PRINT) AND SKIP SHIPREC              
         BE    GETT20                                                           
         MVC   P(15),=C'SHIP   NOT/SEQ*'                                        
         B     GETT30                                                           
GETT20   MVC   P(15),=C'SHIP   *DUPLIC*'                                        
GETT30   AP    SHIPSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETT10              NO - SKIP THIS RECORD                        
         OC    SHIPREC(112),SPACES                                              
         MVC   P+18(112),SHIPREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETT10              SKIP THIS RECORD                             
*                                                                               
GETDIME  DS    0H                                                               
         CLI   DIMEREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTDIME,DIMEREC                                                 
GETD10   GET   XDIME,INPREC                                                     
         MVC   DIMEREC,INPREC                                                   
         CLI   DIMEREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETD10              YES - SKIP                                   
         AP    DIMECNT,=P'1'                                                    
         CLC   LASTDIME,DIMEREC                                                 
         BNH   GETEXIT             OK - NOT OUT OF SEQUENCE                     
*                              ERROR FLAG (PRINT) AND SKIP DIMEREC              
         AP    DIMESKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETD10              NO - SKIP THIS RECORD                        
         MVC   P(15),=C'DIME   NOT/SEQ*'                                        
         OC    DIMEREC(112),SPACES                                              
         MVC   P+18(112),DIMEREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETD10              SKIP THIS RECORD                             
*                                                                               
GETBLEE  DS    0H                                                               
         CLI   BLEEREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTBLEE,BLEEREC                                                 
GETB10   GET   XBLEE,INPREC                                                     
         MVC   BLEEREC,INPREC                                                   
         CLI   BLEEREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETB10              YES - SKIP                                   
         AP    BLEECNT,=P'1'                                                    
         CLC   LASTBLEE,BLEEREC                                                 
         BNH   GETEXIT             OK - NOT OUT OF SEQUENCE                     
*                              ERROR FLAG (PRINT) AND SKIP BLEEREC              
         AP    BLEESKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETB10              NO - SKIP THIS RECORD                        
         MVC   P(15),=C'BLEE   NOT/SEQ*'                                        
         OC    BLEEREC(51),SPACES                                               
         MVC   P+18(51),BLEEREC                                                 
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETB10              SKIP THIS RECORD                             
*                                                                               
GETMECH  DS    0H                                                               
         CLI   MECHREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTMECH,MECHREC                                                 
GETE10   GET   XMECH,INPREC                                                     
         MVC   MECHREC,INPREC                                                   
         CLI   MECHREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETE10              YES - SKIP                                   
         AP    MECHCNT,=P'1'                                                    
         CLC   LASTMECH,MECHREC                                                 
         BNH   GETEXIT             OK - NOT OUT OF SEQUENCE                     
*                              ERROR FLAG (PRINT) AND SKIP MECHREC              
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETE10              NO - SKIP THIS RECORD                        
         AP    MECHSKP,=P'1'                                                    
         MVC   P(15),=C'MECH   NOT/SEQ*'                                        
         OC    MECHREC(112),SPACES                                              
         MVC   P+18(112),MECHREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETE10              SKIP THIS RECORD                             
*                                                                               
GETSAFE  DS    0H                                                               
         CLI   SAFEREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTSAFE,SAFEREC                                                 
GETS10   GET   XSAFE,INPREC                                                     
         MVC   SAFEREC,INPREC                                                   
         CLI   SAFEREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETS10              YES - SKIP                                   
         AP    SAFECNT,=P'1'                                                    
         CLC   LASTSAFE,SAFEREC                                                 
         BNH   GETEXIT             OK - NOT OUT OF SEQUENCE                     
*                              ERROR FLAG (PRINT) AND SKIP SAFEREC              
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETS10              NO - SKIP THIS RECORD                        
         AP    SAFESKP,=P'1'                                                    
         MVC   P(15),=C'SAFE   NOT/SEQ*'                                        
         OC    SAFEREC(50),SPACES                                               
         MVC   P+18(50),SAFEREC                                                 
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETS10              SKIP THIS RECORD                             
*                                                                               
GETPRIN  DS    0H                                                               
         CLI   PRINREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTPRIN,PRINREC                                                 
GETR10   GET   XPRIN,INPREC                                                     
         MOVE  (PRINREC,335),INPREC                                             
         CLI   PRINREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETR10              YES - SKIP                                   
         AP    PRINCNT,=P'1'                                                    
         CLC   LASTPRIN,PRINREC                                                 
         BNH   GETEXIT             OK - NOT OUT OF SEQUENCE                     
*                              ERROR FLAG (PRINT) AND SKIP PRINREC              
         AP    PRINSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETR10              NO - SKIP THIS RECORD                        
         MVC   P(15),=C'PRIN   NOT/SEQ*'                                        
         OC    PRINREC(112),SPACES                                              
         MVC   P+18(112),PRINREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETR10              SKIP THIS RECORD                             
*                                                                               
GETISSU  DS    0H                                                               
         CLI   ISSUREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTISSU,ISSUREC                                                 
GETI10   GET   XISSU,INPREC                                                     
         MVC   ISSUREC,INPREC                                                   
         CLI   ISSUREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETI10              YES - SKIP                                   
         AP    ISSUCNT,=P'1'                                                    
         CLC   LASTISSU,ISSUREC                                                 
         BNH   GETEXIT             OK - NOT OUT OF SEQUENCE                     
*                              ERROR FLAG (PRINT) AND SKIP ISSUREC              
         AP    ISSUSKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETI10              NO - SKIP THIS RECORD                        
         MVC   P(15),=C'ISSU   NOT/SEQ*'                                        
         OC    ISSUREC(112),SPACES                                              
         MVC   P+18(112),ISSUREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETI10              SKIP THIS RECORD                             
*                                                                               
GETDIGI  DS    0H                                                               
         CLI   DIGIREC,X'FF'       NO MORE RECORDS ?                            
         BE    GETEXIT                                                          
         MVC   LASTDIGI,DIGIREC                                                 
GETG10   GET   XDIGI,INPREC                                                     
         MOVE  (DIGIREC,780),INPREC                                             
         CLI   DIGIREC,C'0'        FIRST RECORD (LABELS) ?                      
         BL    GETG10              YES - SKIP                                   
         AP    DIGICNT,=P'1'                                                    
         CLC   LASTDIGI,DIGIREC                                                 
         BNH   GETEXIT             OK - NOT OUT OF SEQUENCE                     
*                              ERROR FLAG (PRINT) AND SKIP ISSUREC              
         AP    DIGISKP,=P'1'                                                    
         CLI   QOPT4,C'Y'          PRINT ALL ERROR MESSAGES ?                   
         BNE   GETG10              NO - SKIP THIS RECORD                        
         MVC   P(15),=C'DIGI   NOT/SEQ*'                                        
         OC    DIGIREC(112),SPACES                                              
         MVC   P+18(112),DIGIREC                                                
         BAS   RE,GRPRT            PRINT THIS RECORD                            
         B     GETG10              SKIP THIS RECORD                             
*                                                                               
         DS    0F                                                               
GRPRT    NTR1                                                                   
         CLI   TESTSW,1                                                         
         BNE   GRPRTX                                                           
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
GRPRTX   MVC   PSECOND,SPACES                                                   
         XIT1                                                                   
*                                                                               
GETEXIT  DS    0H                                                               
         XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
*                                  END-OF-FILE ROUTINES                         
*                                                                               
MAINEOF  MVI   MAINREC,X'FF'       NO MORE MAIN RECORDS                         
         MVI   MODE,RUNLAST                                                     
         B     GETEXIT                                                          
*                                                                               
HEADEOF  MVI   HEADREC,X'FF'       NO MORE HEAD RECORDS                         
         B     GETEXIT                                                          
*                                                                               
CORPEOF  MVI   CORPREC,X'FF'       NO MORE CORP RECORDS                         
         B     GETEXIT                                                          
*                                                                               
PERSEOF  MVI   PERSREC,X'FF'       NO MORE PERS RECORDS                         
         B     GETEXIT                                                          
*                                                                               
SHIPEOF  MVI   SHIPREC,X'FF'       NO MORE SHIP RECORDS                         
         B     GETEXIT                                                          
*                                                                               
DIMEEOF  MVI   DIMEREC,X'FF'       NO MORE DIME RECORDS                         
         B     GETEXIT                                                          
*                                                                               
BLEEEOF  MVI   BLEEREC,X'FF'       NO MORE BLEE RECORDS                         
         B     GETEXIT                                                          
*                                                                               
MECHEOF  MVI   MECHREC,X'FF'       NO MORE MECH RECORDS                         
         B     GETEXIT                                                          
*                                                                               
SAFEEOF  MVI   SAFEREC,X'FF'       NO MORE SAFE RECORDS                         
         B     GETEXIT                                                          
*                                                                               
PRINEOF  MVI   PRINREC,X'FF'       NO MORE PRIN RECORDS                         
         B     GETEXIT                                                          
*                                                                               
ISSUEOF  MVI   ISSUREC,X'FF'       NO MORE ISSU RECORDS                         
         B     GETEXIT                                                          
*                                                                               
DIGIEOF  MVI   DIGIREC,X'FF'       NO MORE DIGI RECORDS                         
         B     GETEXIT                                                          
*                                                                               
TITLEOF  MVI   TITLREC,X'FF'       NO MORE TITL RECORDS                         
         B     GETEXIT                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
PPSDWRKD DSECT                                                                  
ELCODE   DS    X                                                                
ELEM     DS    XL255                                                            
DATADISP DS    H                                                                
SYSFIL   DS    CL8                                                              
GKEY     DS    CL40                                                             
GKEYSAVE DS    CL40                                                             
PUBNMSAV DS    CL50                PUB NAME SAVED                               
TITLTYP  DS    C                   (B)USINESS OR (C)ONSUMER PUB TYPE            
TESTSW   DS    X                                                                
*                                                                               
LASTMAIN DS    CL9                                                              
LASTHEAD DS    CL9                                                              
LASTTITL DS    CL9                                                              
LASTCORP DS    CL9                                                              
LASTPERS DS    CL9                                                              
LASTSHIP DS    CL9                                                              
LASTDIME DS    CL9                                                              
LASTBLEE DS    CL9                                                              
LASTMECH DS    CL9                                                              
LASTSAFE DS    CL9                                                              
LASTPRIN DS    CL9                                                              
LASTISSU DS    CL9                                                              
LASTDIGI DS    CL9                                                              
*                                                                               
COUNTS   DS    0C                                                               
MAINCNT  DS    PL4                                                              
MAINSKP  DS    PL4                                                              
HEADCNT  DS    PL4                                                              
HEADSKP  DS    PL4                                                              
TITLCNT  DS    PL4                                                              
TITLSKP  DS    PL4                                                              
CORPCNT  DS    PL4                                                              
CORPSKP  DS    PL4                                                              
PERSCNT  DS    PL4                                                              
PERSSKP  DS    PL4                                                              
SHIPCNT  DS    PL4                                                              
SHIPSKP  DS    PL4                                                              
DIMECNT  DS    PL4                                                              
DIMESKP  DS    PL4                                                              
BLEECNT  DS    PL4                                                              
BLEESKP  DS    PL4                                                              
MECHCNT  DS    PL4                                                              
MECHSKP  DS    PL4                                                              
SAFECNT  DS    PL4                                                              
SAFESKP  DS    PL4                                                              
PRINCNT  DS    PL4                                                              
PRINSKP  DS    PL4                                                              
ISSUCNT  DS    PL4                                                              
ISSUSKP  DS    PL4                                                              
DIGICNT  DS    PL4                                                              
DIGISKP  DS    PL4                                                              
OUTCNT   DS    PL4                                                              
WRTCNT   DS    PL4                                                              
DUMPCNT  DS    PL4                                                              
ISSUNUM  DS    PL4                                                              
ISSULNG  DS    PL4                                                              
ISSUMNY  DS    PL4                                                              
TITLBUS  DS    PL4                                                              
TITLCON  DS    PL4                                                              
*                                                                               
*                                                                               
INPREC   DS    CL780      *****  MUST BE MAXIMUM RECORD SIZE  *****             
MAINREC  DS    CL100               WAS 255                                      
HEADREC  DS    CL445                                                            
TITLREC  DS    CL187               WAS 255                                      
CORPREC  DS    CL125               WAS 255                                      
PERSREC  DS    CL200               WAS 255                                      
SHIPREC  DS    CL310               WAS 445                                      
DIMEREC  DS    CL115               WAS 255                                      
BLEEREC  DS    CL60                WAS 255                                      
MECHREC  DS    CL160               WAS 255                                      
SAFEREC  DS    CL60                WAS 255                                      
PRINREC  DS    CL335               WAS 500                                      
ISSUREC  DS    CL165               WAS 255                                      
DIGIREC  DS    CL780                                                            
*                                                                               
       ++INCLUDE CTGENPUBS                                                      
       ++INCLUDE SRDSPROD                                                       
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SRDSPISSU 10/22/98'                                      
         END                                                                    
