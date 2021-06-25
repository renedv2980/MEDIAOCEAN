*          DATA SET DEARBSKED  AT LEVEL 047 AS OF 01/17/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEARSKDB                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE KHDUMMY                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
         EJECT                                                                  
         MACRO                                                                  
         RELDT &BOOK,&MARKET,&RELDATE                                           
         AIF   (T'&MARKET EQ 'N').R10                                           
         MNOTE 4,'INVALID MARKET NUMBER'                                        
         MEXIT                                                                  
.R10     ANOP                                                                   
         DC    CL6'&BOOK',C' ',CL3'&MARKET',C' ',CL10'&RELDATE',CL11' '         
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
         TITLE 'NIELSEN RADIO (AKA ARBITRON) EMBARGO RELEASE CHECK'             
DEARSKED CSECT                                                                  
***********************************************************************         
*                                                                               
* CONTROL CARDS:                                                                
*                                                                               
*  TYPE=PPM/DIARY    (SO WE KNOW WHICH TABLE TO CHECK)                          
*  OVERRIDE=NO/YES   (THE DEFAULT IS OVERRIDE=NO. USE OVERRIDE=YES IF           
*                      WE WANT TO IGNORE THE TABLE.)                            
*  TODAY=<DATE>      (FOR DEBUGGING ONLY. OVERRIDES TODAY'S DATE.)              
*                                                                               
* RETURN CODES:                                                                 
*                                                                               
*   0: NO ERRORS OR WARNINGS                                                    
*   4: UNEXPECTED MARKET FOUND, OR                                              
*       ERROR WAS FOUND, BUT OVERRIDE=YES WAS SPECIFIED                         
*  16: ERROR WAS FOUND, AND OVERRIDE=NO                                         
*                                                                               
* ASSUMPTIONS:                                                                  
*                                                                               
* 1. THE JCL SHOULD BE SET UP SO THAT A WARNING E-MAIL GOES OUT IF              
*     RC > 0. IN ADDITION, AN ABEND SHOULD OCCUR (TO KILL THE NET) IF           
*     RC > 4.                                                                   
* 2. TABLE ENTRIES ARE PRESENT FOR:                                             
*     A. PPM BOOKS.                                                             
*     B. DIARY SUMMARY BOOKS.                                                   
* 3. THE UNZIPPED PATHNAMES ARE PRESUMED TO BE OF A PREDICTABLE FORMAT.         
*     THE EXTENSION MUST BE THE MARKET NUMBER. THE FIRST CHARACTER OF           
*     THE SURVEY IS FOUND BY SCANNING FORWARD FOR THE FIRST DIGIT AFTER         
*     THE FORWARD SLASH, THEN TAKING 6 CHARACTERS STARTING WITH THAT            
*     DIGIT.                                                                    
*      EXAMPLES:                                                                
*       PPM:     ...201103%.###                                                 
*       SUMMARY: ...132011%.###                                                 
* 4. THE ETHNIC SURVEYS ARE ASSUMED TO HAVE THE SAME RELEASE DATE AS            
*     THEIR CORRESPONDING STANDARD SURVEYS.                                     
*                                                                               
* FOR DIARY MARKETS, IT IS CRITICAL TO *REMOVE* THE TABLE ENTRIES FOR           
* A SURVEY ONCE IT HAS CONCLUDED. THAT'S BECAUSE THE DIARY MARKET TABLE         
* IS KEYED BY SURVEY *YEAR* ONLY. IF WE LEAVE AN OLD SURVEY IN THE              
* TABLE, THEN WHEN THE NEXT SURVEY COMES IN FOR THAT YEAR, ALL OF THE           
* MARKETS WILL APPEAR AS THOUGH THEIR RELEASE DATES HAVE ALREADY BEEN           
* REACHED (WHICH DEFEATS THE ENTIRE PURPOSE OF THIS PROGRAM!). NOTE,            
* HOWEVER, THAT THIS ALSO MEANS THAT IF AN OLD SURVEY IS *REISSUED* BY          
* NIELSEN ONCE IT HAS BEEN REMOVED FROM THE TABLE, WE'LL KICK IT OUT            
* BECAUSE WE'LL THINK ITS RELEASE DATE HASN'T YET BEEN REACHED. IN THAT         
* CASE, USE OVERRIDE=YES, WHICH WILL LET THE MARKET THROUGH.                    
*                                                                               
***********************************************************************         
         PRINT NOGEN                                                            
         NBASE 0,DEARSKED,=V(REGSAVE)                                           
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE,=CL60'NIELSEN RADIO RELEASE DATE DOUBLE-CHECK'             
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
* READ CONTROL CARDS                                                            
*                                                                               
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
READCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         EOF ON PARAMETER CARDS?                      
         BE    GETTODAY            YES                                          
*                                                                               
         MVC   P(80),CARD          PRINT CONTROL CARD                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    READCARD            YES                                          
*                                                                               
         CLC   =C'TYPE=',CARD      TYPE= CARD ?                                 
         BNE   OVERRCRD            NO                                           
         CLC   =C'PPM ',CARD+5     DEFAULT IS PPM                               
         BE    READCARD                                                         
         CLC   =C'DIARY ',CARD+5   OTHERWISE MUST BE DIARY                      
         BE    *+6                                                              
         DC    H'0'                INVALID TYPE= VALUE                          
         MVI   SURVTYPE,C'D'       SURVEY TYPE = DIARY                          
         B     READCARD            READ NEXT PARAMETER CARD                     
*                                                                               
OVERRCRD DS    0H                                                               
         CLC   =C'OVERRIDE=',CARD  OVERRIDE= CARD ?                             
         BNE   TODAYCRD            NO                                           
         CLC   =C'NO ',CARD+9      DEFAULT IS OVERRIDE=NO                       
         BE    READCARD                                                         
         CLC   =C'YES ',CARD+9     OVERRIDE=YES ?                               
         BE    *+6                 YES                                          
         DC    H'0'                INVALID OVERRIDE= VALUE                      
         MVI   OVERRIDE,C'Y'       SET OVERRIDE MODE                            
         B     READCARD            READ NEXT PARAMETER CARD                     
*                                                                               
TODAYCRD DS    0H                                                               
         CLC   =C'TODAY=',CARD     TODAY= CARD?                                 
         BE    *+6                                                              
         DC    H'0'                INVALID CONTROL CARD                         
         GOTO1 =V(DATVAL),DMCB,(0,CARD+6),DUB  VALIDATE FOR Y/M/D               
         OC    DMCB(4),DMCB        VALID DATE IN PARAMETER CARD?                
         BNZ   *+6                                                              
         DC    H'0'                INVALID DATE                                 
         GOTO1 =V(DATCON),DMCB,DUB,(23,TODAY)  CONVERT TO YYYY-MM-DD            
         B     READCARD            READ NEXT PARAMETER CARD                     
*                                                                               
GETTODAY DS    0H                                                               
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* DERIVE THE CURRENT DATE/TIME                                                  
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACK4         R0 = DDS TIME ADJUSTMENT (6AM)               
         ST    R1,TIME             CURRENT DDS TIME                             
         AP    TIME,PACK4          ADD DDS TIME ADJUSTMENT                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(0,DUB)                                    
         CP    TIME,=P'240000'     PAST MIDNIGHT?                               
         BL    AROUND              NO                                           
         SP    TIME,=P'240000'     YES: BACK TIME UP BY 24 HOURS                
         BNM   *+6                 (TIME HAD BETTER NOT BE NEGATIVE!)           
         DC    H'0'                SEVERE LOGIC ERROR                           
         GOTO1 =V(ADDAY),DMCB,DUB,DUB,F'1'  BUMP UP THE DATE BY 1               
*                                                                               
AROUND   DS    0H                                                               
         CLC   TODAY,SPACES        OVERRIDE TODAY DATE WAS PROVIDED?            
         BNE   CHKTODAY            YES: USE IT                                  
         GOTO1 =V(DATCON),DMCB,DUB,(23,TODAY)  CONVERT TO YYYY-MM-DD            
*                                                                               
* CHECK AND PRINT CURRENT DATE/TIME                                             
*                                                                               
CHKTODAY DS    0H                                                               
         MVC   P(51),=C'IT IS NOW YYYY-MM-DD AT HH:MM EASTERN STANDARD +        
               TIME'               ASSUME IT'S STANDARD TIME IN U.S.            
         MVC   P+10(10),TODAY      FORMAT DATE AND TIME                         
         UNPK  DUB,TIME                                                         
         OI    DUB+7,X'F0'                                                      
         MVC   P+24(2),DUB+2                                                    
         MVC   P+27(2),DUB+4                                                    
*                                                                               
* DAYLIGHT SAVING TIME BEGINS ON THE 2ND SUNDAY OF MARCH, AND ENDS ON           
* THE 1ST SUNDAY OF NOVEMBER.                                                   
*                                                                               
         GOTO1 =V(DATCON),DMCB,(10,TODAY),(0,DUB)  CONVERT TO YYMMDD            
         MVC   WORK(2),DUB         EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'0307'  START LOOKING ON MARCH 7                     
*                                                                               
DAYLITE5 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLC   WORK+6(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                BAD RETURN FROM GETDAY ?!?                   
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE5            NO                                           
*                                                                               
         CLC   DUB,WORK                                                         
         BL    PRNTDATE            TODAY IS PRIOR TO START OF DST               
*                                                                               
         MVC   WORK(2),DUB         EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'1031'  START LOOKING AT OCTOBER 31                  
*                                                                               
DAYLITE7 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLC   WORK+6(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                BAD RETURN FROM GETDAY ?!?                   
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE7            NO                                           
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'-1' SATURDAY IS END OF DST            
*                                                                               
         CLC   DUB,WORK                                                         
         BH    PRNTDATE            TODAY IS AFTER THE END OF DST                
         MVC   P+38(8),=C'DAYLIGHT'  MODIFY TRACE MESSAGE FOR DST               
*                                                                               
PRNTDATE DS    0H                                                               
         GOTO1 =V(PRINTER)         PRINT CURRENT DATE/TIME                      
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=C'CHECKING MARKET RELEASE DATES:'                         
         GOTO1 =V(PRINTER)                                                      
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(LOADER),DMCB,=CL8'T00AD2',0     LOAD DEDEMTABOF               
         ICM   RF,15,DMCB+4        DEMTABOF LOADED OKAY?                        
         BNZ   *+6                 YES                                          
         DC    H'0'                NO                                           
*                                                                               
         GOTO1 (RF),DMCB,MRKTNAMT  GET A(MARKET NAME TABLE)                     
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MKTNAMLN,6(R1)      L'MKTNAMS TABLE ENTRY                        
*                                                                               
         CLC   =C'AR',0(RE)        FIND NIELSEN RADIO TABLE                     
         BE    *+20                GOT IT                                       
         ICM   RE,7,2(RE)          TRY NEXT ONE                                 
         OC    0(2,RE),0(RE)       EOT?                                         
         BNZ   *-20                NO                                           
         DC    H'0'                YES: WHERE IS THE TABLE?                     
         LA    RE,5(RE)            BYPASS HEADER                                
         ST    RE,AMKTNAMS         SAVE A(AR MARKET NAME TABLE)                 
*                                                                               
         GOTO1 =V(LOADER),DMCB,=CL8'T00AD1',0     LOAD DEDEMTABS                
         ICM   RF,15,DMCB+4        DEMTABS LOADED OKAY?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO                                           
*                                                                               
         GOTO1 (RF),DMCB,RAD_NMKT  GET A(RADIO ETHNIC MARKET NO. TABLE)         
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,ARADETH          SAVE A(ETHNIC MARKET NUMBER TABLE)           
         MVC   RADETHLN,6(R1)      L'RADETH TABLE ENTRY                         
*                                                                               
         OPEN  NFSFILES            CONTAINS PATHNAMES IN MOUNTED FOLDER         
*                                                                               
NEXTFILE DS    0H                                                               
*                                                                               
* PARSE THE PATHNAME TO DERIVE THE SURVEY AND MARKET                            
*                                                                               
K        USING MKTLISTD,KEY                                                     
*                                                                               
         MVI   IOAREA,C' '         CLEAR READ BUFFER                            
         MVC   IOAREA+1(L'IOAREA-1),IOAREA                                      
         GET   NFSFILES,IORDW      GET A PATHNAME                               
         MVC   P(80),IOAREA        PRINT THE PATHNAME                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,IOAREA+L'IOAREA-1  POINT TO END OF BUFFER                     
*                                                                               
         CLI   0(R2),C' '                                                       
         BNE   *+10                                                             
         BCTR  R2,0                                                             
         B     *-10                BACK UP TO LAST CHAR. IN PATHNAME            
*                                                                               
         SHI   R2,2                R2 = A(3-BYTE EXTENSION)                     
         MVC   K.MARKET,0(R2)      EXTENSION IS MARKET NUMBER                   
*                                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C'/'                                                       
         BNE   *-6                 BACK UP TO LAST '/' CHAR IN PATHNAME         
*                                                                               
         LA    R2,1(R2)            BUMP PAST '/'                                
         CLI   0(R2),C'0'          FIND START OF SURVEY                         
         BL    *-8                 SURVEY STARTS WITH A DIGIT                   
         MVC   K.SURVEY,0(R2)      SAVE SURVEY                                  
*                                                                               
         PACK  DUB,K.MARKET        CONVERT EBCDIC MARKET# TO BINARY             
         CVB   R0,DUB                                                           
         L     R4,AMKTNAMS         A(NIELSEN RADIO MARKET NAME TABLE)           
         USING MKNTABD,R4                                                       
NXTMKT   OC    0(2,R4),0(R4)       EOT?                                         
         BNZ   *+6                                                              
         DC    H'0'                YES: UNKNOWN MARKET ?!?                      
         CH    R0,MKNNUM           MATCH ON MARKET NUMBER?                      
         BE    *+12                                                             
         AH    R4,MKTNAMLN         NO: BUMP TO NEXT                             
         B     NXTMKT                                                           
*                                                                               
         MVC   P(14),=C'  MARKET NNN: '                                         
         MVC   P+9(L'K.MARKET),K.MARKET                                         
         MVC   P+14(L'MKNNAME),MKNNAME                                          
         CLI   MKNBTYP,C' '        DEFAULT TYPE?                                
         BE    *+18                YES                                          
         MVI   P+64,C'('                                                        
         MVC   P+65(L'MKNBTYP),MKNBTYP                                          
         MVI   P+66,C')'                                                        
         GOTO1 =V(SQUASHER),DMCB,P+2,L'P-2                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   MKNBTYP,MKNCSARQ    CSAR MARKET?                                 
         BE    MARKETOK            THESE ARE NOT EMBARGOED                      
*                                                                               
         CLI   MKNBTYP,MKNHISQ     BOOKTYPE H (HISPANIC)?                       
         BE    *+12                                                             
         CLI   MKNBTYP,MKNBLKQ     BOOKTYPE B (BLACK)?                          
         BNE   CHKMRKT                                                          
*                                                                               
         L     R2,ARADETH          ETHNIC MARKET TABLE                          
         USING RADNMKTD,R2                                                      
CHKETHMK CLI   0(R2),X'FF'         SCAN FOR THE ETHNIC MARKET                   
         BNE   CHKETHM5            EOT NOT REACHED YET                          
*                                                                               
         MVC   P(73),=C'  *** WARNING *** ETHNIC MARKET IS NOT LINKED T+        
               O A GENERAL SURVEY MARKET.'                                      
         GOTO1 =V(PRINTER)                                                      
         MVI   ERRFLAG,C'Y'        *** MISSING RAD_NMKT TABLE ENTRY ***         
         B     NEXT                THERE'S NO RELEASE DATE TO CHECK             
*                                                                               
CHKETHM5 DS    0H                                                               
         CH    R0,RADETHMK         MATCH ON ETHNIC MARKET NUMBER?               
         BNE   *+14                                                             
         CLC   MKNBTYP,RADBKTYP    MATCH ON BOOKTYPE?                           
         BE    *+12                                                             
         AH    R2,RADETHLN         NO: BUMP TO NEXT                             
         B     CHKETHMK                                                         
*                                                                               
         LH    R0,RADSNDMK         USE STANDARD MARKET NUMBER                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  K.MARKET,DUB                                                     
*                                                                               
         MVC   P(63),=C'  ETHNIC MARKET. STANDARD MARKET XXX RELEASE DA+        
               TE WILL BE USED.'                                                
         MVC   P+33(L'K.MARKET),K.MARKET                                        
         GOTO1 =V(PRINTER)                                                      
         DROP  R2                                                               
*                                                                               
CHKMRKT  DS    0H                                                               
*                                                                               
* LOOKUP THE SURVEY/MARKET IN THE MAIN TABLE                                    
*                                                                               
         LARL  R3,MARKET_TABLE     HARD-CODED MARKET TABLE                      
         USING MKTLISTD,R3                                                      
NXTENTRY DS    0H                                                               
         CLI   SURVTYPE,C'P'       ARE WE CHECKING PPM?                         
         BNE   *+18                NO: IT'S DIARY                               
         CLC   K.SURVEY,SURVEY     MATCH ON PPM SURVEY?                         
         BNE   BUMP                NO: TRY NEXT                                 
         B     *+14                YES: CHECK MARKET                            
         CLC   K.DIARY_YR,DIARY_YR   DIARY MUST MATCH ONLY ON YEAR              
         BNE   BUMP                NO MATCH: TRY NEXT                           
*                                                                               
         CLC   K.MARKET,MARKET     BOTH TYPES MUST MATCH ON MARKET              
         BE    CHKDATE             GOT IT                                       
         DROP  K                                                                
*                                                                               
BUMP     DS    0H                                                               
         LA    R3,MKTTABLQ(R3)     BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'                                                      
         BNE   NXTENTRY                                                         
*                                                                               
         MVC   P(75),=C'  *** WARNING *** MISSING TABLE ENTRY. SURVEY/M+        
               ARKET COMBINATION NOT FOUND.'                                    
         GOTO1 =V(PRINTER)                                                      
         MVI   ERRFLAG,C'Y'        *** MISSING TABLE ENTRY ***                  
         B     NEXT                                                             
*                                                                               
CHKDATE  DS    0H                                                               
         MVC   P(28),=C'  RELEASE DATE IS YYYY-MM-DD'                           
         MVC   P+18(L'RELDATE),RELDATE                                          
         GOTO1 =V(PRINTER)                                                      
         DROP  R4                                                               
*                                                                               
         CLC   TODAY,RELDATE       COMPARE TODAY'S DATE TO RELEASE DATE         
         BH    MARKETOK            IT'S PAST THE RELEASE DATE: OKAY             
         BE    CHKTIME             SAME DATE: CHECK THE TIME ALSO               
*                                                                               
         MVC   P(44),=C'  *** ERROR *** RELEASE DATE NOT YET REACHED'           
         GOTO1 =V(PRINTER)                                                      
         MVI   ERRFLAG,C'Y'        *** RELEASE DATE NOT YET REACHED ***         
         B     NEXT                                                             
*                                                                               
CHKTIME  DS    0H                                                               
*                                                                               
* UNLESS WE'RE LOADING ANCHORAGE OR HONOLULU, WE CAN ASSUME THAT IT IS          
* SAFE TO LOAD ALL MARKETS BY 12 NOON PACIFIC TIME, WHICH IS 3PM                
* NEW YORK TIME. THIS IS TRUE REGARDLESS OF WHETHER IT IS DAYLIGHT              
* SAVING TIME OR NOT.                                                           
* - FOR ANCHORAGE, WE MUST WAIT UNTIL THEIR LOCAL NOON, WHICH IS ALWAYS         
*   4PM NEW YORK TIME.                                                          
* - FOR HONOLULU, THE RELEASE TIME IS EITHER 10AM OR 11AM THEIR LOCAL           
*   TIME. THIS DIFFERENCE IS PROBABLY DUE TO THE FACT THAT HAWAII NEVER         
*   GOES ON DAYLIGHT SAVING TIME. JUST TO BE SAFE, WE ALWAYS WAIT UNTIL         
*   4PM NEW YORK TIME.                                                          
*                                                                               
* 150000 = 15:00:00 (MILITARY) = 3PM                                            
* 160000 = 16:00:00 (MILITARY) = 4PM                                            
         ZAP   PACK4,=P'150000'    DEFAULT IS 3PM RELEASE TIME                  
         CLC   MARKET,=C'315'      ANCHORAGE RELEASES AT LOCAL NOON...          
         BE    *+14                ...WHICH IS ALWAYS 4PM NY TIME               
         CLC   MARKET,=C'099'      HONOLULU RELEASES AT 10AM OR 11AM            
         BNE   *+10                                                             
         ZAP   PACK4,=P'160000'    4PM NEW YORK TIME                            
*                                                                               
* GCOR SAYS THAT WE CANNOT RELY ON THE ACCURACY OF THE MACHINE CLOCK.           
* SO WE ADD 15 MINUTES JUST TO MAKE DEAD SURE THAT THE EMBARGO IS               
* LIFTED.                                                                       
*                                                                               
         AP    PACK4,=P'1500'      ADD 15 MINUTES FOR BAD MACHINE CLOCK         
         CP    TIME,PACK4          HAVE WE REACHED THE RELEASE TIME?            
         BNL   MARKETOK            YES: WE'RE GOOD                              
*                                                                               
         MVC   P(44),=C'  *** ERROR *** RELEASE TIME NOT YET REACHED'           
         GOTO1 =V(PRINTER)                                                      
         MVI   ERRFLAG,C'Y'        *** RELEASE TIME NOT YET REACHED ***         
         B     NEXT                                                             
*                                                                               
MARKETOK DS    0H                                                               
         MVC   P(17),=C'  OKAY TO RELEASE'                                      
         GOTO1 =V(PRINTER)                                                      
         DROP  R3                                                               
*                                                                               
NEXT     DS    0H                                                               
         B     NEXTFILE            GET NEXT PATHNAME                            
*                                                                               
CLOSNAMS DS    0H                                                               
         CLOSE NFSFILES                                                         
*                                                                               
         CLI   ERRFLAG,C'Y'        ANY ERRORS?                                  
         BNE   XBASE               NO: LEAVE RC AS IT IS                        
         MVC   RETCODE,=F'16'      YES: SET RC=16                               
         CLI   OVERRIDE,C'Y'       BUT IF OVERRIDE=YES...                       
         BNE   *+10                                                             
         MVC   RETCODE,=F'4'       ...SET RC=4 (WARNING)                        
*                                                                               
XBASE    DS    0H                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL24                                                             
RETCODE  DC    F'0'                RETURN CODE                                  
AMKTNAMS DS    A                   A(NIELSEN RADIO MARKET NAME TABLE)           
ARADETH  DS    A                   A(ARB. RADIO ETHNIC MARKET TABLE)            
MKTNAMLN DS    H                   L'MKTNAMS TABLE ENTRY                        
RADETHLN DS    H                   L'RADETH TABLE ENTRY                         
TIME     DS    PL4                 0HHMMSS+                                     
PACK4    DS    PL4                 0HHMMSS+                                     
CARD     DS    CL80                                                             
TODAY    DC    CL10' '             YYYY-MM-DD                                   
SURVTYPE DC    C'P'                'P' = PPM, 'D' = DIARY                       
OVERRIDE DC    C'N'                'Y' = OVERRIDE MODE                          
ERRFLAG  DC    C'N'                'Y' = ERROR(S) FOUND                         
*                                                                               
IORDW    DS    F                   READ BUFFER FOR PATHNAME                     
IOAREA   DS    CL(256-4)                                                        
*                                                                               
KEY      DS    CL(MKTKEYLQ)        SURVEY/MARKET                                
*                                                                               
DUMPLIST DS    0F                  FOR STXITER                                  
         DC    A(DEARSKED)                                                      
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=GM,LRECL=256,RECFM=VB,   +        
               EODAD=CLOSNAMS                                                   
*                                                                               
         EJECT                                                                  
MARKET_TABLE DS 0D                                                              
*                                                                               
* THIS TABLE IS SORTED BY RELEASE DATE FOR EASE OF READING. THE CODE            
* DOES NOT RELY ON THIS SORT ORDER.                                             
*                                                                               
* PPM 2012 MARKETS                                                              
*                                                                               
 RELDT 201201,047,2012-02-20  ATLANTA                                           
 RELDT 201201,005,2012-02-20  CHICAGO                                           
 RELDT 201201,024,2012-02-20  DALLAS-FT. WORTH                                  
 RELDT 201201,033,2012-02-20  HOUSTON-GALVESTON                                 
 RELDT 201201,003,2012-02-20  LOS ANGELES                                       
 RELDT 201201,413,2012-02-20  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201201,321,2012-02-20  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201201,001,2012-02-20  NEW YORK                                          
 RELDT 201201,007,2012-02-20  PHILADELPHIA                                      
 RELDT 201201,379,2012-02-20  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201201,009,2012-02-20  SAN FRANCISCO                                     
 RELDT 201201,215,2012-02-20  SAN JOSE                                          
*                                                                               
 RELDT 201201,021,2012-02-21  BALTIMORE                                         
 RELDT 201201,013,2012-02-21  BOSTON                                            
 RELDT 201201,035,2012-02-21  DENVER-BOULDER                                    
 RELDT 201201,011,2012-02-21  DETROIT                                           
 RELDT 201201,429,2012-02-21  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201201,027,2012-02-21  MINNEAPOLIS-ST. PAUL                              
 RELDT 201201,057,2012-02-21  PHOENIX                                           
 RELDT 201201,063,2012-02-21  SAN DIEGO                                         
 RELDT 201201,039,2012-02-21  SEATTLE-TACOMA                                    
 RELDT 201201,017,2012-02-21  ST. LOUIS                                         
 RELDT 201201,087,2012-02-21  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201201,015,2012-02-21  WASHINGTON, DC                                    
*                                                                               
 RELDT 201201,093,2012-02-22  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201201,031,2012-02-22  CINCINNATI                                        
 RELDT 201201,019,2012-02-22  CLEVELAND                                         
 RELDT 201201,045,2012-02-22  COLUMBUS, OH                                      
 RELDT 201201,041,2012-02-22  KANSAS CITY                                       
 RELDT 201201,257,2012-02-22  LAS VEGAS                                         
 RELDT 201201,131,2012-02-22  ORLANDO                                           
 RELDT 201201,023,2012-02-22  PITTSBURGH, PA                                    
 RELDT 201201,051,2012-02-22  PORTLAND, OR                                      
 RELDT 201201,065,2012-02-22  SACRAMENTO                                        
 RELDT 201201,101,2012-02-22  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201201,059,2012-02-22  SAN ANTONIO                                       
*                                                                               
 RELDT 201201,135,2012-02-23  AUSTIN                                            
 RELDT 201201,166,2012-02-23  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201201,061,2012-02-23  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201201,049,2012-02-23  INDIANAPOLIS                                      
 RELDT 201201,107,2012-02-23  JACKSONVILLE                                      
 RELDT 201201,075,2012-02-23  MEMPHIS                                           
 RELDT 201201,043,2012-02-23  MILWAUKEE-RACINE                                  
 RELDT 201201,073,2012-02-23  NASHVILLE                                         
 RELDT 201201,109,2012-02-23  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201201,077,2012-02-23  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201201,115,2012-02-23  RALEIGH-DURHAM                                    
 RELDT 201201,299,2012-02-23  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201202,047,2012-03-19  ATLANTA                                           
 RELDT 201202,005,2012-03-19  CHICAGO                                           
 RELDT 201202,024,2012-03-19  DALLAS-FT. WORTH                                  
 RELDT 201202,033,2012-03-19  HOUSTON-GALVESTON                                 
 RELDT 201202,003,2012-03-19  LOS ANGELES                                       
 RELDT 201202,413,2012-03-19  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201202,321,2012-03-19  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201202,001,2012-03-19  NEW YORK                                          
 RELDT 201202,007,2012-03-19  PHILADELPHIA                                      
 RELDT 201202,379,2012-03-19  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201202,009,2012-03-19  SAN FRANCISCO                                     
 RELDT 201202,215,2012-03-19  SAN JOSE                                          
*                                                                               
 RELDT 201202,021,2012-03-20  BALTIMORE                                         
 RELDT 201202,013,2012-03-20  BOSTON                                            
 RELDT 201202,035,2012-03-20  DENVER-BOULDER                                    
 RELDT 201202,011,2012-03-20  DETROIT                                           
 RELDT 201202,429,2012-03-20  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201202,027,2012-03-20  MINNEAPOLIS-ST. PAUL                              
 RELDT 201202,057,2012-03-20  PHOENIX                                           
 RELDT 201202,063,2012-03-20  SAN DIEGO                                         
 RELDT 201202,039,2012-03-20  SEATTLE-TACOMA                                    
 RELDT 201202,017,2012-03-20  ST. LOUIS                                         
 RELDT 201202,087,2012-03-20  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201202,015,2012-03-20  WASHINGTON, DC                                    
*                                                                               
 RELDT 201202,093,2012-03-21  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201202,031,2012-03-21  CINCINNATI                                        
 RELDT 201202,019,2012-03-21  CLEVELAND                                         
 RELDT 201202,045,2012-03-21  COLUMBUS, OH                                      
 RELDT 201202,041,2012-03-21  KANSAS CITY                                       
 RELDT 201202,257,2012-03-21  LAS VEGAS                                         
 RELDT 201202,131,2012-03-21  ORLANDO                                           
 RELDT 201202,023,2012-03-21  PITTSBURGH, PA                                    
 RELDT 201202,051,2012-03-21  PORTLAND, OR                                      
 RELDT 201202,065,2012-03-21  SACRAMENTO                                        
 RELDT 201202,101,2012-03-21  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201202,059,2012-03-21  SAN ANTONIO                                       
*                                                                               
 RELDT 201202,135,2012-03-22  AUSTIN                                            
 RELDT 201202,166,2012-03-22  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201202,061,2012-03-22  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201202,049,2012-03-22  INDIANAPOLIS                                      
 RELDT 201202,107,2012-03-22  JACKSONVILLE                                      
 RELDT 201202,075,2012-03-22  MEMPHIS                                           
 RELDT 201202,043,2012-03-22  MILWAUKEE-RACINE                                  
 RELDT 201202,073,2012-03-22  NASHVILLE                                         
 RELDT 201202,109,2012-03-22  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201202,077,2012-03-22  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201202,115,2012-03-22  RALEIGH-DURHAM                                    
 RELDT 201202,299,2012-03-22  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201203,047,2012-04-16  ATLANTA                                           
 RELDT 201203,005,2012-04-16  CHICAGO                                           
 RELDT 201203,024,2012-04-16  DALLAS-FT. WORTH                                  
 RELDT 201203,033,2012-04-16  HOUSTON-GALVESTON                                 
 RELDT 201203,003,2012-04-16  LOS ANGELES                                       
 RELDT 201203,413,2012-04-16  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201203,321,2012-04-16  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201203,001,2012-04-16  NEW YORK                                          
 RELDT 201203,007,2012-04-16  PHILADELPHIA                                      
 RELDT 201203,379,2012-04-16  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201203,009,2012-04-16  SAN FRANCISCO                                     
 RELDT 201203,215,2012-04-16  SAN JOSE                                          
*                                                                               
 RELDT 201203,021,2012-04-17  BALTIMORE                                         
 RELDT 201203,013,2012-04-17  BOSTON                                            
 RELDT 201203,035,2012-04-17  DENVER-BOULDER                                    
 RELDT 201203,011,2012-04-17  DETROIT                                           
 RELDT 201203,429,2012-04-17  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201203,027,2012-04-17  MINNEAPOLIS-ST. PAUL                              
 RELDT 201203,057,2012-04-17  PHOENIX                                           
 RELDT 201203,063,2012-04-17  SAN DIEGO                                         
 RELDT 201203,039,2012-04-17  SEATTLE-TACOMA                                    
 RELDT 201203,017,2012-04-17  ST. LOUIS                                         
 RELDT 201203,087,2012-04-17  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201203,015,2012-04-17  WASHINGTON, DC                                    
*                                                                               
 RELDT 201203,093,2012-04-18  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201203,031,2012-04-18  CINCINNATI                                        
 RELDT 201203,019,2012-04-18  CLEVELAND                                         
 RELDT 201203,045,2012-04-18  COLUMBUS, OH                                      
 RELDT 201203,041,2012-04-18  KANSAS CITY                                       
 RELDT 201203,257,2012-04-18  LAS VEGAS                                         
 RELDT 201203,131,2012-04-18  ORLANDO                                           
 RELDT 201203,023,2012-04-18  PITTSBURGH, PA                                    
 RELDT 201203,051,2012-04-18  PORTLAND, OR                                      
 RELDT 201203,065,2012-04-18  SACRAMENTO                                        
 RELDT 201203,101,2012-04-18  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201203,059,2012-04-18  SAN ANTONIO                                       
*                                                                               
 RELDT 201203,135,2012-04-19  AUSTIN                                            
 RELDT 201203,166,2012-04-19  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201203,061,2012-04-19  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201203,049,2012-04-19  INDIANAPOLIS                                      
 RELDT 201203,107,2012-04-19  JACKSONVILLE                                      
 RELDT 201203,075,2012-04-19  MEMPHIS                                           
 RELDT 201203,043,2012-04-19  MILWAUKEE-RACINE                                  
 RELDT 201203,073,2012-04-19  NASHVILLE                                         
 RELDT 201203,109,2012-04-19  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201203,077,2012-04-19  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201203,115,2012-04-19  RALEIGH-DURHAM                                    
 RELDT 201203,299,2012-04-19  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201204,047,2012-05-14  ATLANTA                                           
 RELDT 201204,005,2012-05-14  CHICAGO                                           
 RELDT 201204,024,2012-05-14  DALLAS-FT. WORTH                                  
 RELDT 201204,033,2012-05-14  HOUSTON-GALVESTON                                 
 RELDT 201204,003,2012-05-14  LOS ANGELES                                       
 RELDT 201204,413,2012-05-14  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201204,321,2012-05-14  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201204,001,2012-05-14  NEW YORK                                          
 RELDT 201204,007,2012-05-14  PHILADELPHIA                                      
 RELDT 201204,379,2012-05-14  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201204,009,2012-05-14  SAN FRANCISCO                                     
 RELDT 201204,215,2012-05-14  SAN JOSE                                          
*                                                                               
 RELDT 201204,021,2012-05-15  BALTIMORE                                         
 RELDT 201204,013,2012-05-15  BOSTON                                            
 RELDT 201204,035,2012-05-15  DENVER-BOULDER                                    
 RELDT 201204,011,2012-05-15  DETROIT                                           
 RELDT 201204,429,2012-05-15  MIAMI-FT. LAUDERDALE-HOLLYWOOD                    
 RELDT 201204,027,2012-05-15  MINNEAPOLIS-ST. PAUL                              
 RELDT 201204,057,2012-05-15  PHOENIX                                           
 RELDT 201204,063,2012-05-15  SAN DIEGO                                         
 RELDT 201204,039,2012-05-15  SEATTLE-TACOMA                                    
 RELDT 201204,017,2012-05-15  ST. LOUIS                                         
 RELDT 201204,087,2012-05-15  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201204,015,2012-05-15  WASHINGTON, DC                                    
*                                                                               
 RELDT 201204,093,2012-05-16  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201204,031,2012-05-16  CINCINNATI                                        
 RELDT 201204,019,2012-05-16  CLEVELAND                                         
 RELDT 201204,045,2012-05-16  COLUMBUS, OH                                      
 RELDT 201204,041,2012-05-16  KANSAS CITY                                       
 RELDT 201204,257,2012-05-16  LAS VEGAS                                         
 RELDT 201204,131,2012-05-16  ORLANDO                                           
 RELDT 201204,023,2012-05-16  PITTSBURGH, PA                                    
 RELDT 201204,051,2012-05-16  PORTLAND, OR                                      
 RELDT 201204,065,2012-05-16  SACRAMENTO                                        
 RELDT 201204,101,2012-05-16  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201204,059,2012-05-16  SAN ANTONIO                                       
*                                                                               
 RELDT 201204,135,2012-05-17  AUSTIN                                            
 RELDT 201204,166,2012-05-17  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201204,061,2012-05-17  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201204,049,2012-05-17  INDIANAPOLIS                                      
 RELDT 201204,107,2012-05-17  JACKSONVILLE                                      
 RELDT 201204,075,2012-05-17  MEMPHIS                                           
 RELDT 201204,043,2012-05-17  MILWAUKEE-RACINE                                  
 RELDT 201204,073,2012-05-17  NASHVILLE                                         
 RELDT 201204,109,2012-05-17  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201204,077,2012-05-17  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201204,115,2012-05-17  RALEIGH-DURHAM                                    
 RELDT 201204,299,2012-05-17  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201205,047,2012-06-11  ATLANTA                                           
 RELDT 201205,005,2012-06-11  CHICAGO                                           
 RELDT 201205,024,2012-06-11  DALLAS-FT. WORTH                                  
 RELDT 201205,033,2012-06-11  HOUSTON-GALVESTON                                 
 RELDT 201205,003,2012-06-11  LOS ANGELES                                       
 RELDT 201205,413,2012-06-11  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201205,321,2012-06-11  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201205,001,2012-06-11  NEW YORK                                          
 RELDT 201205,007,2012-06-11  PHILADELPHIA                                      
 RELDT 201205,379,2012-06-11  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201205,009,2012-06-11  SAN FRANCISCO                                     
 RELDT 201205,215,2012-06-11  SAN JOSE                                          
*                                                                               
 RELDT 201205,021,2012-06-12  BALTIMORE                                         
 RELDT 201205,013,2012-06-12  BOSTON                                            
 RELDT 201205,035,2012-06-12  DENVER-BOULDER                                    
 RELDT 201205,011,2012-06-12  DETROIT                                           
 RELDT 201205,429,2012-06-12  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201205,027,2012-06-12  MINNEAPOLIS-ST. PAUL                              
 RELDT 201205,057,2012-06-12  PHOENIX                                           
 RELDT 201205,063,2012-06-12  SAN DIEGO                                         
 RELDT 201205,039,2012-06-12  SEATTLE-TACOMA                                    
 RELDT 201205,017,2012-06-12  ST. LOUIS                                         
 RELDT 201205,087,2012-06-12  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201205,015,2012-06-12  WASHINGTON, DC                                    
*                                                                               
 RELDT 201205,093,2012-06-13  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201205,031,2012-06-13  CINCINNATI                                        
 RELDT 201205,019,2012-06-13  CLEVELAND                                         
 RELDT 201205,045,2012-06-13  COLUMBUS, OH                                      
 RELDT 201205,041,2012-06-13  KANSAS CITY                                       
 RELDT 201205,257,2012-06-13  LAS VEGAS                                         
 RELDT 201205,131,2012-06-13  ORLANDO                                           
 RELDT 201205,023,2012-06-13  PITTSBURGH, PA                                    
 RELDT 201205,051,2012-06-13  PORTLAND, OR                                      
 RELDT 201205,065,2012-06-13  SACRAMENTO                                        
 RELDT 201205,101,2012-06-13  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201205,059,2012-06-13  SAN ANTONIO                                       
*                                                                               
 RELDT 201205,135,2012-06-14  AUSTIN                                            
 RELDT 201205,166,2012-06-14  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201205,061,2012-06-14  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201205,049,2012-06-14  INDIANAPOLIS                                      
 RELDT 201205,107,2012-06-14  JACKSONVILLE                                      
 RELDT 201205,075,2012-06-14  MEMPHIS                                           
 RELDT 201205,043,2012-06-14  MILWAUKEE-RACINE                                  
 RELDT 201205,073,2012-06-14  NASHVILLE                                         
 RELDT 201205,109,2012-06-14  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201205,077,2012-06-14  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201205,115,2012-06-14  RALEIGH-DURHAM                                    
 RELDT 201205,299,2012-06-14  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201206,047,2012-07-10  ATLANTA                                           
 RELDT 201206,005,2012-07-10  CHICAGO                                           
 RELDT 201206,024,2012-07-10  DALLAS-FT. WORTH                                  
 RELDT 201206,033,2012-07-10  HOUSTON-GALVESTON                                 
 RELDT 201206,003,2012-07-10  LOS ANGELES                                       
 RELDT 201206,413,2012-07-10  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201206,321,2012-07-10  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201206,001,2012-07-10  NEW YORK                                          
 RELDT 201206,007,2012-07-10  PHILADELPHIA                                      
 RELDT 201206,379,2012-07-10  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201206,009,2012-07-10  SAN FRANCISCO                                     
 RELDT 201206,215,2012-07-10  SAN JOSE                                          
*                                                                               
 RELDT 201206,021,2012-07-11  BALTIMORE                                         
 RELDT 201206,013,2012-07-11  BOSTON                                            
 RELDT 201206,035,2012-07-11  DENVER-BOULDER                                    
 RELDT 201206,011,2012-07-11  DETROIT                                           
 RELDT 201206,429,2012-07-11  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201206,027,2012-07-11  MINNEAPOLIS-ST. PAUL                              
 RELDT 201206,057,2012-07-11  PHOENIX                                           
 RELDT 201206,063,2012-07-11  SAN DIEGO                                         
 RELDT 201206,039,2012-07-11  SEATTLE-TACOMA                                    
 RELDT 201206,017,2012-07-11  ST. LOUIS                                         
 RELDT 201206,087,2012-07-11  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201206,015,2012-07-11  WASHINGTON, DC                                    
*                                                                               
 RELDT 201206,093,2012-07-12  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201206,031,2012-07-12  CINCINNATI                                        
 RELDT 201206,019,2012-07-12  CLEVELAND                                         
 RELDT 201206,045,2012-07-12  COLUMBUS, OH                                      
 RELDT 201206,041,2012-07-12  KANSAS CITY                                       
 RELDT 201206,257,2012-07-12  LAS VEGAS                                         
 RELDT 201206,131,2012-07-12  ORLANDO                                           
 RELDT 201206,023,2012-07-12  PITTSBURGH, PA                                    
 RELDT 201206,051,2012-07-12  PORTLAND, OR                                      
 RELDT 201206,065,2012-07-12  SACRAMENTO                                        
 RELDT 201206,101,2012-07-12  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201206,059,2012-07-12  SAN ANTONIO                                       
*                                                                               
 RELDT 201206,135,2012-07-13  AUSTIN                                            
 RELDT 201206,166,2012-07-13  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201206,061,2012-07-13  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201206,049,2012-07-13  INDIANAPOLIS                                      
 RELDT 201206,107,2012-07-13  JACKSONVILLE                                      
 RELDT 201206,075,2012-07-13  MEMPHIS                                           
 RELDT 201206,043,2012-07-13  MILWAUKEE-RACINE                                  
 RELDT 201206,073,2012-07-13  NASHVILLE                                         
 RELDT 201206,109,2012-07-13  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201206,077,2012-07-13  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201206,115,2012-07-13  RALEIGH-DURHAM                                    
 RELDT 201206,299,2012-07-13  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201207,047,2012-08-06  ATLANTA                                           
 RELDT 201207,005,2012-08-06  CHICAGO                                           
 RELDT 201207,024,2012-08-06  DALLAS-FT. WORTH                                  
 RELDT 201207,033,2012-08-06  HOUSTON-GALVESTON                                 
 RELDT 201207,003,2012-08-06  LOS ANGELES                                       
 RELDT 201207,413,2012-08-06  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201207,321,2012-08-06  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201207,001,2012-08-06  NEW YORK                                          
 RELDT 201207,007,2012-08-06  PHILADELPHIA                                      
 RELDT 201207,379,2012-08-06  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201207,009,2012-08-06  SAN FRANCISCO                                     
 RELDT 201207,215,2012-08-06  SAN JOSE                                          
*                                                                               
 RELDT 201207,021,2012-08-07  BALTIMORE                                         
 RELDT 201207,013,2012-08-07  BOSTON                                            
 RELDT 201207,035,2012-08-07  DENVER-BOULDER                                    
 RELDT 201207,011,2012-08-07  DETROIT                                           
 RELDT 201207,429,2012-08-07  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201207,027,2012-08-07  MINNEAPOLIS-ST. PAUL                              
 RELDT 201207,057,2012-08-07  PHOENIX                                           
 RELDT 201207,063,2012-08-07  SAN DIEGO                                         
 RELDT 201207,039,2012-08-07  SEATTLE-TACOMA                                    
 RELDT 201207,017,2012-08-07  ST. LOUIS                                         
 RELDT 201207,087,2012-08-07  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201207,015,2012-08-07  WASHINGTON, DC                                    
*                                                                               
 RELDT 201207,093,2012-08-08  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201207,031,2012-08-08  CINCINNATI                                        
 RELDT 201207,019,2012-08-08  CLEVELAND                                         
 RELDT 201207,045,2012-08-08  COLUMBUS, OH                                      
 RELDT 201207,041,2012-08-08  KANSAS CITY                                       
 RELDT 201207,257,2012-08-08  LAS VEGAS                                         
 RELDT 201207,131,2012-08-08  ORLANDO                                           
 RELDT 201207,023,2012-08-08  PITTSBURGH, PA                                    
 RELDT 201207,051,2012-08-08  PORTLAND, OR                                      
 RELDT 201207,065,2012-08-08  SACRAMENTO                                        
 RELDT 201207,101,2012-08-08  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201207,059,2012-08-08  SAN ANTONIO                                       
*                                                                               
 RELDT 201207,135,2012-08-09  AUSTIN                                            
 RELDT 201207,166,2012-08-09  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201207,061,2012-08-09  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201207,049,2012-08-09  INDIANAPOLIS                                      
 RELDT 201207,107,2012-08-09  JACKSONVILLE                                      
 RELDT 201207,075,2012-08-09  MEMPHIS                                           
 RELDT 201207,043,2012-08-09  MILWAUKEE-RACINE                                  
 RELDT 201207,073,2012-08-09  NASHVILLE                                         
 RELDT 201207,109,2012-08-09  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201207,077,2012-08-09  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201207,115,2012-08-09  RALEIGH-DURHAM                                    
 RELDT 201207,299,2012-08-09  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201208,047,2012-09-04  ATLANTA                                           
 RELDT 201208,005,2012-09-04  CHICAGO                                           
 RELDT 201208,024,2012-09-04  DALLAS-FT. WORTH                                  
 RELDT 201208,033,2012-09-04  HOUSTON-GALVESTON                                 
 RELDT 201208,003,2012-09-04  LOS ANGELES                                       
 RELDT 201208,413,2012-09-04  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201208,321,2012-09-04  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201208,001,2012-09-04  NEW YORK                                          
 RELDT 201208,007,2012-09-04  PHILADELPHIA                                      
 RELDT 201208,379,2012-09-04  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201208,009,2012-09-04  SAN FRANCISCO                                     
 RELDT 201208,215,2012-09-04  SAN JOSE                                          
*                                                                               
 RELDT 201208,021,2012-09-05  BALTIMORE                                         
 RELDT 201208,013,2012-09-05  BOSTON                                            
 RELDT 201208,035,2012-09-05  DENVER-BOULDER                                    
 RELDT 201208,011,2012-09-05  DETROIT                                           
 RELDT 201208,429,2012-09-05  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201208,027,2012-09-05  MINNEAPOLIS-ST. PAUL                              
 RELDT 201208,057,2012-09-05  PHOENIX                                           
 RELDT 201208,063,2012-09-05  SAN DIEGO                                         
 RELDT 201208,039,2012-09-05  SEATTLE-TACOMA                                    
 RELDT 201208,017,2012-09-05  ST. LOUIS                                         
 RELDT 201208,087,2012-09-05  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201208,015,2012-09-05  WASHINGTON, DC                                    
*                                                                               
 RELDT 201208,093,2012-09-06  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201208,031,2012-09-06  CINCINNATI                                        
 RELDT 201208,019,2012-09-06  CLEVELAND                                         
 RELDT 201208,045,2012-09-06  COLUMBUS, OH                                      
 RELDT 201208,041,2012-09-06  KANSAS CITY                                       
 RELDT 201208,257,2012-09-06  LAS VEGAS                                         
 RELDT 201208,131,2012-09-06  ORLANDO                                           
 RELDT 201208,023,2012-09-06  PITTSBURGH, PA                                    
 RELDT 201208,051,2012-09-06  PORTLAND, OR                                      
 RELDT 201208,065,2012-09-06  SACRAMENTO                                        
 RELDT 201208,101,2012-09-06  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201208,059,2012-09-06  SAN ANTONIO                                       
*                                                                               
 RELDT 201208,135,2012-09-07  AUSTIN                                            
 RELDT 201208,166,2012-09-07  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201208,061,2012-09-07  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201208,049,2012-09-07  INDIANAPOLIS                                      
 RELDT 201208,107,2012-09-07  JACKSONVILLE                                      
 RELDT 201208,075,2012-09-07  MEMPHIS                                           
 RELDT 201208,043,2012-09-07  MILWAUKEE-RACINE                                  
 RELDT 201208,073,2012-09-07  NASHVILLE                                         
 RELDT 201208,109,2012-09-07  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201208,077,2012-09-07  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201208,115,2012-09-07  RALEIGH-DURHAM                                    
 RELDT 201208,299,2012-09-07  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201209,047,2012-10-01  ATLANTA                                           
 RELDT 201209,005,2012-10-01  CHICAGO                                           
 RELDT 201209,024,2012-10-01  DALLAS-FT. WORTH                                  
 RELDT 201209,033,2012-10-01  HOUSTON-GALVESTON                                 
 RELDT 201209,003,2012-10-01  LOS ANGELES                                       
 RELDT 201209,413,2012-10-01  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201209,321,2012-10-01  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201209,001,2012-10-01  NEW YORK                                          
 RELDT 201209,007,2012-10-01  PHILADELPHIA                                      
 RELDT 201209,379,2012-10-01  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201209,009,2012-10-01  SAN FRANCISCO                                     
 RELDT 201209,215,2012-10-01  SAN JOSE                                          
*                                                                               
 RELDT 201209,021,2012-10-02  BALTIMORE                                         
 RELDT 201209,013,2012-10-02  BOSTON                                            
 RELDT 201209,035,2012-10-02  DENVER-BOULDER                                    
 RELDT 201209,011,2012-10-02  DETROIT                                           
 RELDT 201209,429,2012-10-02  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201209,027,2012-10-02  MINNEAPOLIS-ST. PAUL                              
 RELDT 201209,057,2012-10-02  PHOENIX                                           
 RELDT 201209,063,2012-10-02  SAN DIEGO                                         
 RELDT 201209,039,2012-10-02  SEATTLE-TACOMA                                    
 RELDT 201209,017,2012-10-02  ST. LOUIS                                         
 RELDT 201209,087,2012-10-02  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201209,015,2012-10-02  WASHINGTON, DC                                    
*                                                                               
 RELDT 201209,093,2012-10-03  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201209,031,2012-10-03  CINCINNATI                                        
 RELDT 201209,019,2012-10-03  CLEVELAND                                         
 RELDT 201209,045,2012-10-03  COLUMBUS, OH                                      
 RELDT 201209,041,2012-10-03  KANSAS CITY                                       
 RELDT 201209,257,2012-10-03  LAS VEGAS                                         
 RELDT 201209,131,2012-10-03  ORLANDO                                           
 RELDT 201209,023,2012-10-03  PITTSBURGH, PA                                    
 RELDT 201209,051,2012-10-03  PORTLAND, OR                                      
 RELDT 201209,065,2012-10-03  SACRAMENTO                                        
 RELDT 201209,101,2012-10-03  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201209,059,2012-10-03  SAN ANTONIO                                       
*                                                                               
 RELDT 201209,135,2012-10-04  AUSTIN                                            
 RELDT 201209,166,2012-10-04  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201209,061,2012-10-04  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201209,049,2012-10-04  INDIANAPOLIS                                      
 RELDT 201209,107,2012-10-04  JACKSONVILLE                                      
 RELDT 201209,075,2012-10-04  MEMPHIS                                           
 RELDT 201209,043,2012-10-04  MILWAUKEE-RACINE                                  
 RELDT 201209,073,2012-10-04  NASHVILLE                                         
 RELDT 201209,109,2012-10-04  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201209,077,2012-10-04  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201209,115,2012-10-04  RALEIGH-DURHAM                                    
 RELDT 201209,299,2012-10-04  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201210,047,2012-10-29  ATLANTA                                           
 RELDT 201210,005,2012-10-29  CHICAGO                                           
 RELDT 201210,024,2012-10-29  DALLAS-FT. WORTH                                  
 RELDT 201210,033,2012-10-29  HOUSTON-GALVESTON                                 
 RELDT 201210,003,2012-10-29  LOS ANGELES                                       
 RELDT 201210,413,2012-10-29  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201210,321,2012-10-29  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201210,001,2012-10-29  NEW YORK                                          
 RELDT 201210,007,2012-10-29  PHILADELPHIA                                      
 RELDT 201210,379,2012-10-29  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201210,009,2012-10-29  SAN FRANCISCO                                     
 RELDT 201210,215,2012-10-29  SAN JOSE                                          
*                                                                               
 RELDT 201210,021,2012-10-30  BALTIMORE                                         
 RELDT 201210,013,2012-10-30  BOSTON                                            
 RELDT 201210,035,2012-10-30  DENVER-BOULDER                                    
 RELDT 201210,011,2012-10-30  DETROIT                                           
 RELDT 201210,429,2012-10-30  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201210,027,2012-10-30  MINNEAPOLIS-ST. PAUL                              
 RELDT 201210,057,2012-10-30  PHOENIX                                           
 RELDT 201210,063,2012-10-30  SAN DIEGO                                         
 RELDT 201210,039,2012-10-30  SEATTLE-TACOMA                                    
 RELDT 201210,017,2012-10-30  ST. LOUIS                                         
 RELDT 201210,087,2012-10-30  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201210,015,2012-10-30  WASHINGTON, DC                                    
*                                                                               
 RELDT 201210,093,2012-10-31  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201210,031,2012-10-31  CINCINNATI                                        
 RELDT 201210,019,2012-10-31  CLEVELAND                                         
 RELDT 201210,045,2012-10-31  COLUMBUS, OH                                      
 RELDT 201210,041,2012-10-31  KANSAS CITY                                       
 RELDT 201210,257,2012-10-31  LAS VEGAS                                         
 RELDT 201210,131,2012-10-31  ORLANDO                                           
 RELDT 201210,023,2012-10-31  PITTSBURGH, PA                                    
 RELDT 201210,051,2012-10-31  PORTLAND, OR                                      
 RELDT 201210,065,2012-10-31  SACRAMENTO                                        
 RELDT 201210,101,2012-10-31  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201210,059,2012-10-31  SAN ANTONIO                                       
*                                                                               
 RELDT 201210,135,2012-11-01  AUSTIN                                            
 RELDT 201210,166,2012-11-01  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201210,061,2012-11-01  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201210,049,2012-11-01  INDIANAPOLIS                                      
 RELDT 201210,107,2012-11-01  JACKSONVILLE                                      
 RELDT 201210,075,2012-11-01  MEMPHIS                                           
 RELDT 201210,043,2012-11-01  MILWAUKEE-RACINE                                  
 RELDT 201210,073,2012-11-01  NASHVILLE                                         
 RELDT 201210,109,2012-11-01  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201210,077,2012-11-01  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201210,115,2012-11-01  RALEIGH-DURHAM                                    
 RELDT 201210,299,2012-11-01  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201211,047,2012-11-27  ATLANTA                                           
 RELDT 201211,005,2012-11-27  CHICAGO                                           
 RELDT 201211,024,2012-11-27  DALLAS-FT. WORTH                                  
 RELDT 201211,033,2012-11-27  HOUSTON-GALVESTON                                 
 RELDT 201211,003,2012-11-27  LOS ANGELES                                       
 RELDT 201211,413,2012-11-27  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201211,321,2012-11-27  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201211,001,2012-11-27  NEW YORK                                          
 RELDT 201211,007,2012-11-27  PHILADELPHIA                                      
 RELDT 201211,379,2012-11-27  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201211,009,2012-11-27  SAN FRANCISCO                                     
 RELDT 201211,215,2012-11-27  SAN JOSE                                          
*                                                                               
 RELDT 201211,021,2012-11-28  BALTIMORE                                         
 RELDT 201211,013,2012-11-28  BOSTON                                            
 RELDT 201211,035,2012-11-28  DENVER-BOULDER                                    
 RELDT 201211,011,2012-11-28  DETROIT                                           
 RELDT 201211,429,2012-11-28  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201211,027,2012-11-28  MINNEAPOLIS-ST. PAUL                              
 RELDT 201211,057,2012-11-28  PHOENIX                                           
 RELDT 201211,063,2012-11-28  SAN DIEGO                                         
 RELDT 201211,039,2012-11-28  SEATTLE-TACOMA                                    
 RELDT 201211,017,2012-11-28  ST. LOUIS                                         
 RELDT 201211,087,2012-11-28  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201211,015,2012-11-28  WASHINGTON, DC                                    
*                                                                               
 RELDT 201211,093,2012-11-29  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201211,031,2012-11-29  CINCINNATI                                        
 RELDT 201211,019,2012-11-29  CLEVELAND                                         
 RELDT 201211,045,2012-11-29  COLUMBUS, OH                                      
 RELDT 201211,041,2012-11-29  KANSAS CITY                                       
 RELDT 201211,257,2012-11-29  LAS VEGAS                                         
 RELDT 201211,131,2012-11-29  ORLANDO                                           
 RELDT 201211,023,2012-11-29  PITTSBURGH, PA                                    
 RELDT 201211,051,2012-11-29  PORTLAND, OR                                      
 RELDT 201211,065,2012-11-29  SACRAMENTO                                        
 RELDT 201211,101,2012-11-29  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201211,059,2012-11-29  SAN ANTONIO                                       
*                                                                               
 RELDT 201211,135,2012-11-30  AUSTIN                                            
 RELDT 201211,166,2012-11-30  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201211,061,2012-11-30  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201211,049,2012-11-30  INDIANAPOLIS                                      
 RELDT 201211,107,2012-11-30  JACKSONVILLE                                      
 RELDT 201211,075,2012-11-30  MEMPHIS                                           
 RELDT 201211,043,2012-11-30  MILWAUKEE-RACINE                                  
 RELDT 201211,073,2012-11-30  NASHVILLE                                         
 RELDT 201211,109,2012-11-30  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201211,077,2012-11-30  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201211,115,2012-11-30  RALEIGH-DURHAM                                    
 RELDT 201211,299,2012-11-30  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201212,047,2012-12-26  ATLANTA                                           
 RELDT 201212,005,2012-12-26  CHICAGO                                           
 RELDT 201212,024,2012-12-26  DALLAS-FT. WORTH                                  
 RELDT 201212,033,2012-12-26  HOUSTON-GALVESTON                                 
 RELDT 201212,003,2012-12-26  LOS ANGELES                                       
 RELDT 201212,413,2012-12-26  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201212,321,2012-12-26  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201212,001,2012-12-26  NEW YORK                                          
 RELDT 201212,007,2012-12-26  PHILADELPHIA                                      
 RELDT 201212,379,2012-12-26  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201212,009,2012-12-26  SAN FRANCISCO                                     
 RELDT 201212,215,2012-12-26  SAN JOSE                                          
*                                                                               
 RELDT 201212,021,2012-12-27  BALTIMORE                                         
 RELDT 201212,013,2012-12-27  BOSTON                                            
 RELDT 201212,035,2012-12-27  DENVER-BOULDER                                    
 RELDT 201212,011,2012-12-27  DETROIT                                           
 RELDT 201212,429,2012-12-27  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201212,027,2012-12-27  MINNEAPOLIS-ST. PAUL                              
 RELDT 201212,057,2012-12-27  PHOENIX                                           
 RELDT 201212,063,2012-12-27  SAN DIEGO                                         
 RELDT 201212,039,2012-12-27  SEATTLE-TACOMA                                    
 RELDT 201212,017,2012-12-27  ST. LOUIS                                         
 RELDT 201212,087,2012-12-27  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201212,015,2012-12-27  WASHINGTON, DC                                    
*                                                                               
 RELDT 201212,093,2012-12-28  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201212,031,2012-12-28  CINCINNATI                                        
 RELDT 201212,019,2012-12-28  CLEVELAND                                         
 RELDT 201212,045,2012-12-28  COLUMBUS, OH                                      
 RELDT 201212,041,2012-12-28  KANSAS CITY                                       
 RELDT 201212,257,2012-12-28  LAS VEGAS                                         
 RELDT 201212,131,2012-12-28  ORLANDO                                           
 RELDT 201212,023,2012-12-28  PITTSBURGH, PA                                    
 RELDT 201212,051,2012-12-28  PORTLAND, OR                                      
 RELDT 201212,065,2012-12-28  SACRAMENTO                                        
 RELDT 201212,101,2012-12-28  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201212,059,2012-12-28  SAN ANTONIO                                       
*                                                                               
 RELDT 201212,135,2012-12-28  AUSTIN                                            
 RELDT 201212,166,2012-12-28  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201212,061,2012-12-28  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201212,049,2012-12-28  INDIANAPOLIS                                      
 RELDT 201212,107,2012-12-28  JACKSONVILLE                                      
 RELDT 201212,075,2012-12-28  MEMPHIS                                           
 RELDT 201212,043,2012-12-28  MILWAUKEE-RACINE                                  
 RELDT 201212,073,2012-12-28  NASHVILLE                                         
 RELDT 201212,109,2012-12-28  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201212,077,2012-12-28  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201212,115,2012-12-28  RALEIGH-DURHAM                                    
 RELDT 201212,299,2012-12-28  WEST PALM BEACH-BOCA RATON                        
*                                                                               
* HOLIDAY BOOK                                                                  
 RELDT 201223,047,2013-01-22  ATLANTA                                           
 RELDT 201223,005,2013-01-22  CHICAGO                                           
 RELDT 201223,024,2013-01-22  DALLAS-FT. WORTH                                  
 RELDT 201223,033,2013-01-22  HOUSTON-GALVESTON                                 
 RELDT 201223,003,2013-01-22  LOS ANGELES                                       
 RELDT 201223,413,2013-01-22  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201223,321,2013-01-22  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201223,001,2013-01-22  NEW YORK                                          
 RELDT 201223,007,2013-01-22  PHILADELPHIA                                      
 RELDT 201223,379,2013-01-22  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201223,009,2013-01-22  SAN FRANCISCO                                     
 RELDT 201223,215,2013-01-22  SAN JOSE                                          
*                                                                               
 RELDT 201223,021,2013-01-23  BALTIMORE                                         
 RELDT 201223,013,2013-01-23  BOSTON                                            
 RELDT 201223,035,2013-01-23  DENVER-BOULDER                                    
 RELDT 201223,011,2013-01-23  DETROIT                                           
 RELDT 201223,429,2013-01-23  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201223,027,2013-01-23  MINNEAPOLIS-ST. PAUL                              
 RELDT 201223,057,2013-01-23  PHOENIX                                           
 RELDT 201223,063,2013-01-23  SAN DIEGO                                         
 RELDT 201223,039,2013-01-23  SEATTLE-TACOMA                                    
 RELDT 201223,017,2013-01-23  ST. LOUIS                                         
 RELDT 201223,087,2013-01-23  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201223,015,2013-01-23  WASHINGTON, DC                                    
*                                                                               
 RELDT 201223,093,2013-01-24  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201223,031,2013-01-24  CINCINNATI                                        
 RELDT 201223,019,2013-01-24  CLEVELAND                                         
 RELDT 201223,045,2013-01-24  COLUMBUS, OH                                      
 RELDT 201223,041,2013-01-24  KANSAS CITY                                       
 RELDT 201223,257,2013-01-24  LAS VEGAS                                         
 RELDT 201223,131,2013-01-24  ORLANDO                                           
 RELDT 201223,023,2013-01-24  PITTSBURGH, PA                                    
 RELDT 201223,051,2013-01-24  PORTLAND, OR                                      
 RELDT 201223,065,2013-01-24  SACRAMENTO                                        
 RELDT 201223,101,2013-01-24  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201223,059,2013-01-24  SAN ANTONIO                                       
*                                                                               
 RELDT 201223,135,2013-01-25  AUSTIN                                            
 RELDT 201223,166,2013-01-25  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201223,061,2013-01-25  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201223,049,2013-01-25  INDIANAPOLIS                                      
 RELDT 201223,107,2013-01-25  JACKSONVILLE                                      
 RELDT 201223,075,2013-01-25  MEMPHIS                                           
 RELDT 201223,043,2013-01-25  MILWAUKEE-RACINE                                  
 RELDT 201223,073,2013-01-25  NASHVILLE                                         
 RELDT 201223,109,2013-01-25  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201223,077,2013-01-25  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201223,115,2013-01-25  RALEIGH-DURHAM                                    
 RELDT 201223,299,2013-01-25  WEST PALM BEACH-BOCA RATON                        
*                                                                               
* PPM 2013 MARKETS                                                              
*                                                                               
 RELDT 201301,047,2013-02-18  ATLANTA                                           
 RELDT 201301,005,2013-02-18  CHICAGO                                           
 RELDT 201301,024,2013-02-18  DALLAS-FT. WORTH                                  
 RELDT 201301,033,2013-02-18  HOUSTON-GALVESTON                                 
 RELDT 201301,003,2013-02-18  LOS ANGELES                                       
 RELDT 201301,413,2013-02-18  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201301,321,2013-02-18  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201301,001,2013-02-18  NEW YORK                                          
 RELDT 201301,007,2013-02-18  PHILADELPHIA                                      
 RELDT 201301,379,2013-02-18  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201301,009,2013-02-18  SAN FRANCISCO                                     
 RELDT 201301,215,2013-02-18  SAN JOSE                                          
*                                                                               
 RELDT 201301,021,2013-02-19  BALTIMORE                                         
 RELDT 201301,013,2013-02-19  BOSTON                                            
 RELDT 201301,035,2013-02-19  DENVER-BOULDER                                    
 RELDT 201301,011,2013-02-19  DETROIT                                           
 RELDT 201301,429,2013-02-19  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201301,027,2013-02-19  MINNEAPOLIS-ST. PAUL                              
 RELDT 201301,057,2013-02-19  PHOENIX                                           
 RELDT 201301,063,2013-02-19  SAN DIEGO                                         
 RELDT 201301,039,2013-02-19  SEATTLE-TACOMA                                    
 RELDT 201301,017,2013-02-19  ST. LOUIS                                         
 RELDT 201301,087,2013-02-19  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201301,015,2013-02-19  WASHINGTON, DC                                    
*                                                                               
 RELDT 201301,093,2013-02-20  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201301,031,2013-02-20  CINCINNATI                                        
 RELDT 201301,019,2013-02-20  CLEVELAND                                         
 RELDT 201301,045,2013-02-20  COLUMBUS, OH                                      
 RELDT 201301,041,2013-02-20  KANSAS CITY                                       
 RELDT 201301,257,2013-02-20  LAS VEGAS                                         
 RELDT 201301,131,2013-02-20  ORLANDO                                           
 RELDT 201301,023,2013-02-20  PITTSBURGH, PA                                    
 RELDT 201301,051,2013-02-20  PORTLAND, OR                                      
 RELDT 201301,065,2013-02-20  SACRAMENTO                                        
 RELDT 201301,101,2013-02-20  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201301,059,2013-02-20  SAN ANTONIO                                       
*                                                                               
 RELDT 201301,135,2013-02-21  AUSTIN                                            
 RELDT 201301,166,2013-02-21  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201301,061,2013-02-21  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201301,049,2013-02-21  INDIANAPOLIS                                      
 RELDT 201301,107,2013-02-21  JACKSONVILLE                                      
 RELDT 201301,075,2013-02-21  MEMPHIS                                           
 RELDT 201301,043,2013-02-21  MILWAUKEE-RACINE                                  
 RELDT 201301,073,2013-02-21  NASHVILLE                                         
 RELDT 201301,109,2013-02-21  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201301,077,2013-02-21  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201301,115,2013-02-21  RALEIGH-DURHAM                                    
 RELDT 201301,299,2013-02-21  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201302,047,2013-03-18  ATLANTA                                           
 RELDT 201302,005,2013-03-18  CHICAGO                                           
 RELDT 201302,024,2013-03-18  DALLAS-FT. WORTH                                  
 RELDT 201302,033,2013-03-18  HOUSTON-GALVESTON                                 
 RELDT 201302,003,2013-03-18  LOS ANGELES                                       
 RELDT 201302,413,2013-03-18  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201302,321,2013-03-18  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201302,001,2013-03-18  NEW YORK                                          
 RELDT 201302,007,2013-03-18  PHILADELPHIA                                      
 RELDT 201302,379,2013-03-18  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201302,009,2013-03-18  SAN FRANCISCO                                     
 RELDT 201302,215,2013-03-18  SAN JOSE                                          
*                                                                               
 RELDT 201302,021,2013-03-19  BALTIMORE                                         
 RELDT 201302,013,2013-03-19  BOSTON                                            
 RELDT 201302,035,2013-03-19  DENVER-BOULDER                                    
 RELDT 201302,011,2013-03-19  DETROIT                                           
 RELDT 201302,429,2013-03-19  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201302,027,2013-03-19  MINNEAPOLIS-ST. PAUL                              
 RELDT 201302,057,2013-03-19  PHOENIX                                           
 RELDT 201302,063,2013-03-19  SAN DIEGO                                         
 RELDT 201302,039,2013-03-19  SEATTLE-TACOMA                                    
 RELDT 201302,017,2013-03-19  ST. LOUIS                                         
 RELDT 201302,087,2013-03-19  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201302,015,2013-03-19  WASHINGTON, DC                                    
*                                                                               
 RELDT 201302,093,2013-03-20  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201302,031,2013-03-20  CINCINNATI                                        
 RELDT 201302,019,2013-03-20  CLEVELAND                                         
 RELDT 201302,045,2013-03-20  COLUMBUS, OH                                      
 RELDT 201302,041,2013-03-20  KANSAS CITY                                       
 RELDT 201302,257,2013-03-20  LAS VEGAS                                         
 RELDT 201302,131,2013-03-20  ORLANDO                                           
 RELDT 201302,023,2013-03-20  PITTSBURGH, PA                                    
 RELDT 201302,051,2013-03-20  PORTLAND, OR                                      
 RELDT 201302,065,2013-03-20  SACRAMENTO                                        
 RELDT 201302,101,2013-03-20  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201302,059,2013-03-20  SAN ANTONIO                                       
*                                                                               
 RELDT 201302,135,2013-03-21  AUSTIN                                            
 RELDT 201302,166,2013-03-21  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201302,061,2013-03-21  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201302,049,2013-03-21  INDIANAPOLIS                                      
 RELDT 201302,107,2013-03-21  JACKSONVILLE                                      
 RELDT 201302,075,2013-03-21  MEMPHIS                                           
 RELDT 201302,043,2013-03-21  MILWAUKEE-RACINE                                  
 RELDT 201302,073,2013-03-21  NASHVILLE                                         
 RELDT 201302,109,2013-03-21  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201302,077,2013-03-21  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201302,115,2013-03-21  RALEIGH-DURHAM                                    
 RELDT 201302,299,2013-03-21  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201303,047,2013-04-15  ATLANTA                                           
 RELDT 201303,005,2013-04-15  CHICAGO                                           
 RELDT 201303,024,2013-04-15  DALLAS-FT. WORTH                                  
 RELDT 201303,033,2013-04-15  HOUSTON-GALVESTON                                 
 RELDT 201303,003,2013-04-15  LOS ANGELES                                       
 RELDT 201303,413,2013-04-15  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201303,321,2013-04-15  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201303,001,2013-04-15  NEW YORK                                          
 RELDT 201303,007,2013-04-15  PHILADELPHIA                                      
 RELDT 201303,379,2013-04-15  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201303,009,2013-04-15  SAN FRANCISCO                                     
 RELDT 201303,215,2013-04-15  SAN JOSE                                          
*                                                                               
 RELDT 201303,021,2013-04-16  BALTIMORE                                         
 RELDT 201303,013,2013-04-16  BOSTON                                            
 RELDT 201303,035,2013-04-16  DENVER-BOULDER                                    
 RELDT 201303,011,2013-04-16  DETROIT                                           
 RELDT 201303,429,2013-04-16  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201303,027,2013-04-16  MINNEAPOLIS-ST. PAUL                              
 RELDT 201303,057,2013-04-16  PHOENIX                                           
 RELDT 201303,063,2013-04-16  SAN DIEGO                                         
 RELDT 201303,039,2013-04-16  SEATTLE-TACOMA                                    
 RELDT 201303,017,2013-04-16  ST. LOUIS                                         
 RELDT 201303,087,2013-04-16  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201303,015,2013-04-16  WASHINGTON, DC                                    
*                                                                               
 RELDT 201303,093,2013-04-17  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201303,031,2013-04-17  CINCINNATI                                        
 RELDT 201303,019,2013-04-17  CLEVELAND                                         
 RELDT 201303,045,2013-04-17  COLUMBUS, OH                                      
 RELDT 201303,041,2013-04-17  KANSAS CITY                                       
 RELDT 201303,257,2013-04-17  LAS VEGAS                                         
 RELDT 201303,131,2013-04-17  ORLANDO                                           
 RELDT 201303,023,2013-04-17  PITTSBURGH, PA                                    
 RELDT 201303,051,2013-04-17  PORTLAND, OR                                      
 RELDT 201303,065,2013-04-17  SACRAMENTO                                        
 RELDT 201303,101,2013-04-17  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201303,059,2013-04-17  SAN ANTONIO                                       
*                                                                               
 RELDT 201303,135,2013-04-18  AUSTIN                                            
 RELDT 201303,166,2013-04-18  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201303,061,2013-04-18  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201303,049,2013-04-18  INDIANAPOLIS                                      
 RELDT 201303,107,2013-04-18  JACKSONVILLE                                      
 RELDT 201303,075,2013-04-18  MEMPHIS                                           
 RELDT 201303,043,2013-04-18  MILWAUKEE-RACINE                                  
 RELDT 201303,073,2013-04-18  NASHVILLE                                         
 RELDT 201303,109,2013-04-18  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201303,077,2013-04-18  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201303,115,2013-04-18  RALEIGH-DURHAM                                    
 RELDT 201303,299,2013-04-18  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201304,047,2013-05-13  ATLANTA                                           
 RELDT 201304,005,2013-05-13  CHICAGO                                           
 RELDT 201304,024,2013-05-13  DALLAS-FT. WORTH                                  
 RELDT 201304,033,2013-05-13  HOUSTON-GALVESTON                                 
 RELDT 201304,003,2013-05-13  LOS ANGELES                                       
 RELDT 201304,413,2013-05-13  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201304,321,2013-05-13  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201304,001,2013-05-13  NEW YORK                                          
 RELDT 201304,007,2013-05-13  PHILADELPHIA                                      
 RELDT 201304,379,2013-05-13  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201304,009,2013-05-13  SAN FRANCISCO                                     
 RELDT 201304,215,2013-05-13  SAN JOSE                                          
*                                                                               
 RELDT 201304,021,2013-05-14  BALTIMORE                                         
 RELDT 201304,013,2013-05-14  BOSTON                                            
 RELDT 201304,035,2013-05-14  DENVER-BOULDER                                    
 RELDT 201304,011,2013-05-14  DETROIT                                           
 RELDT 201304,429,2013-05-14  MIAMI-FT. LAUDERDALE-HOLLYWOOD                    
 RELDT 201304,027,2013-05-14  MINNEAPOLIS-ST. PAUL                              
 RELDT 201304,057,2013-05-14  PHOENIX                                           
 RELDT 201304,063,2013-05-14  SAN DIEGO                                         
 RELDT 201304,039,2013-05-14  SEATTLE-TACOMA                                    
 RELDT 201304,017,2013-05-14  ST. LOUIS                                         
 RELDT 201304,087,2013-05-14  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201304,015,2013-05-14  WASHINGTON, DC                                    
*                                                                               
 RELDT 201304,093,2013-05-15  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201304,031,2013-05-15  CINCINNATI                                        
 RELDT 201304,019,2013-05-15  CLEVELAND                                         
 RELDT 201304,045,2013-05-15  COLUMBUS, OH                                      
 RELDT 201304,041,2013-05-15  KANSAS CITY                                       
 RELDT 201304,257,2013-05-15  LAS VEGAS                                         
 RELDT 201304,131,2013-05-15  ORLANDO                                           
 RELDT 201304,023,2013-05-15  PITTSBURGH, PA                                    
 RELDT 201304,051,2013-05-15  PORTLAND, OR                                      
 RELDT 201304,065,2013-05-15  SACRAMENTO                                        
 RELDT 201304,101,2013-05-15  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201304,059,2013-05-15  SAN ANTONIO                                       
*                                                                               
 RELDT 201304,135,2013-05-16  AUSTIN                                            
 RELDT 201304,166,2013-05-16  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201304,061,2013-05-16  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201304,049,2013-05-16  INDIANAPOLIS                                      
 RELDT 201304,107,2013-05-16  JACKSONVILLE                                      
 RELDT 201304,075,2013-05-16  MEMPHIS                                           
 RELDT 201304,043,2013-05-16  MILWAUKEE-RACINE                                  
 RELDT 201304,073,2013-05-16  NASHVILLE                                         
 RELDT 201304,109,2013-05-16  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201304,077,2013-05-16  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201304,115,2013-05-16  RALEIGH-DURHAM                                    
 RELDT 201304,299,2013-05-16  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201305,047,2013-06-10  ATLANTA                                           
 RELDT 201305,005,2013-06-10  CHICAGO                                           
 RELDT 201305,024,2013-06-10  DALLAS-FT. WORTH                                  
 RELDT 201305,033,2013-06-10  HOUSTON-GALVESTON                                 
 RELDT 201305,003,2013-06-10  LOS ANGELES                                       
 RELDT 201305,413,2013-06-10  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201305,321,2013-06-10  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201305,001,2013-06-10  NEW YORK                                          
 RELDT 201305,007,2013-06-10  PHILADELPHIA                                      
 RELDT 201305,379,2013-06-10  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201305,009,2013-06-10  SAN FRANCISCO                                     
 RELDT 201305,215,2013-06-10  SAN JOSE                                          
*                                                                               
 RELDT 201305,021,2013-06-11  BALTIMORE                                         
 RELDT 201305,013,2013-06-11  BOSTON                                            
 RELDT 201305,035,2013-06-11  DENVER-BOULDER                                    
 RELDT 201305,011,2013-06-11  DETROIT                                           
 RELDT 201305,429,2013-06-11  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201305,027,2013-06-11  MINNEAPOLIS-ST. PAUL                              
 RELDT 201305,057,2013-06-11  PHOENIX                                           
 RELDT 201305,063,2013-06-11  SAN DIEGO                                         
 RELDT 201305,039,2013-06-11  SEATTLE-TACOMA                                    
 RELDT 201305,017,2013-06-11  ST. LOUIS                                         
 RELDT 201305,087,2013-06-11  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201305,015,2013-06-11  WASHINGTON, DC                                    
*                                                                               
 RELDT 201305,093,2013-06-12  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201305,031,2013-06-12  CINCINNATI                                        
 RELDT 201305,019,2013-06-12  CLEVELAND                                         
 RELDT 201305,045,2013-06-12  COLUMBUS, OH                                      
 RELDT 201305,041,2013-06-12  KANSAS CITY                                       
 RELDT 201305,257,2013-06-12  LAS VEGAS                                         
 RELDT 201305,131,2013-06-12  ORLANDO                                           
 RELDT 201305,023,2013-06-12  PITTSBURGH, PA                                    
 RELDT 201305,051,2013-06-12  PORTLAND, OR                                      
 RELDT 201305,065,2013-06-12  SACRAMENTO                                        
 RELDT 201305,101,2013-06-12  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201305,059,2013-06-12  SAN ANTONIO                                       
*                                                                               
 RELDT 201305,135,2013-06-13  AUSTIN                                            
 RELDT 201305,166,2013-06-13  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201305,061,2013-06-13  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201305,049,2013-06-13  INDIANAPOLIS                                      
 RELDT 201305,107,2013-06-13  JACKSONVILLE                                      
 RELDT 201305,075,2013-06-13  MEMPHIS                                           
 RELDT 201305,043,2013-06-13  MILWAUKEE-RACINE                                  
 RELDT 201305,073,2013-06-13  NASHVILLE                                         
 RELDT 201305,109,2013-06-13  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201305,077,2013-06-13  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201305,115,2013-06-13  RALEIGH-DURHAM                                    
 RELDT 201305,299,2013-06-13  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201306,047,2013-07-09  ATLANTA                                           
 RELDT 201306,005,2013-07-09  CHICAGO                                           
 RELDT 201306,024,2013-07-09  DALLAS-FT. WORTH                                  
 RELDT 201306,033,2013-07-09  HOUSTON-GALVESTON                                 
 RELDT 201306,003,2013-07-09  LOS ANGELES                                       
 RELDT 201306,413,2013-07-09  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201306,321,2013-07-09  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201306,001,2013-07-09  NEW YORK                                          
 RELDT 201306,007,2013-07-09  PHILADELPHIA                                      
 RELDT 201306,379,2013-07-09  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201306,009,2013-07-09  SAN FRANCISCO                                     
 RELDT 201306,215,2013-07-09  SAN JOSE                                          
*                                                                               
 RELDT 201306,021,2013-07-10  BALTIMORE                                         
 RELDT 201306,013,2013-07-10  BOSTON                                            
 RELDT 201306,035,2013-07-10  DENVER-BOULDER                                    
 RELDT 201306,011,2013-07-10  DETROIT                                           
 RELDT 201306,429,2013-07-10  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201306,027,2013-07-10  MINNEAPOLIS-ST. PAUL                              
 RELDT 201306,057,2013-07-10  PHOENIX                                           
 RELDT 201306,063,2013-07-10  SAN DIEGO                                         
 RELDT 201306,039,2013-07-10  SEATTLE-TACOMA                                    
 RELDT 201306,017,2013-07-10  ST. LOUIS                                         
 RELDT 201306,087,2013-07-10  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201306,015,2013-07-10  WASHINGTON, DC                                    
*                                                                               
 RELDT 201306,093,2013-07-11  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201306,031,2013-07-11  CINCINNATI                                        
 RELDT 201306,019,2013-07-11  CLEVELAND                                         
 RELDT 201306,045,2013-07-11  COLUMBUS, OH                                      
 RELDT 201306,041,2013-07-11  KANSAS CITY                                       
 RELDT 201306,257,2013-07-11  LAS VEGAS                                         
 RELDT 201306,131,2013-07-11  ORLANDO                                           
 RELDT 201306,023,2013-07-11  PITTSBURGH, PA                                    
 RELDT 201306,051,2013-07-11  PORTLAND, OR                                      
 RELDT 201306,065,2013-07-11  SACRAMENTO                                        
 RELDT 201306,101,2013-07-11  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201306,059,2013-07-11  SAN ANTONIO                                       
*                                                                               
 RELDT 201306,135,2013-07-12  AUSTIN                                            
 RELDT 201306,166,2013-07-12  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201306,061,2013-07-12  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201306,049,2013-07-12  INDIANAPOLIS                                      
 RELDT 201306,107,2013-07-12  JACKSONVILLE                                      
 RELDT 201306,075,2013-07-12  MEMPHIS                                           
 RELDT 201306,043,2013-07-12  MILWAUKEE-RACINE                                  
 RELDT 201306,073,2013-07-12  NASHVILLE                                         
 RELDT 201306,109,2013-07-12  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201306,077,2013-07-12  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201306,115,2013-07-12  RALEIGH-DURHAM                                    
 RELDT 201306,299,2013-07-12  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201307,047,2013-08-05  ATLANTA                                           
 RELDT 201307,005,2013-08-05  CHICAGO                                           
 RELDT 201307,024,2013-08-05  DALLAS-FT. WORTH                                  
 RELDT 201307,033,2013-08-05  HOUSTON-GALVESTON                                 
 RELDT 201307,003,2013-08-05  LOS ANGELES                                       
 RELDT 201307,413,2013-08-05  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201307,321,2013-08-05  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201307,001,2013-08-05  NEW YORK                                          
 RELDT 201307,007,2013-08-05  PHILADELPHIA                                      
 RELDT 201307,379,2013-08-05  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201307,009,2013-08-05  SAN FRANCISCO                                     
 RELDT 201307,215,2013-08-05  SAN JOSE                                          
*                                                                               
 RELDT 201307,021,2013-08-06  BALTIMORE                                         
 RELDT 201307,013,2013-08-06  BOSTON                                            
 RELDT 201307,035,2013-08-06  DENVER-BOULDER                                    
 RELDT 201307,011,2013-08-06  DETROIT                                           
 RELDT 201307,429,2013-08-06  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201307,027,2013-08-06  MINNEAPOLIS-ST. PAUL                              
 RELDT 201307,057,2013-08-06  PHOENIX                                           
 RELDT 201307,063,2013-08-06  SAN DIEGO                                         
 RELDT 201307,039,2013-08-06  SEATTLE-TACOMA                                    
 RELDT 201307,017,2013-08-06  ST. LOUIS                                         
 RELDT 201307,087,2013-08-06  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201307,015,2013-08-06  WASHINGTON, DC                                    
*                                                                               
 RELDT 201307,093,2013-08-07  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201307,031,2013-08-07  CINCINNATI                                        
 RELDT 201307,019,2013-08-07  CLEVELAND                                         
 RELDT 201307,045,2013-08-07  COLUMBUS, OH                                      
 RELDT 201307,041,2013-08-07  KANSAS CITY                                       
 RELDT 201307,257,2013-08-07  LAS VEGAS                                         
 RELDT 201307,131,2013-08-07  ORLANDO                                           
 RELDT 201307,023,2013-08-07  PITTSBURGH, PA                                    
 RELDT 201307,051,2013-08-07  PORTLAND, OR                                      
 RELDT 201307,065,2013-08-07  SACRAMENTO                                        
 RELDT 201307,101,2013-08-07  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201307,059,2013-08-07  SAN ANTONIO                                       
*                                                                               
 RELDT 201307,135,2013-08-08  AUSTIN                                            
 RELDT 201307,166,2013-08-08  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201307,061,2013-08-08  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201307,049,2013-08-08  INDIANAPOLIS                                      
 RELDT 201307,107,2013-08-08  JACKSONVILLE                                      
 RELDT 201307,075,2013-08-08  MEMPHIS                                           
 RELDT 201307,043,2013-08-08  MILWAUKEE-RACINE                                  
 RELDT 201307,073,2013-08-08  NASHVILLE                                         
 RELDT 201307,109,2013-08-08  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201307,077,2013-08-08  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201307,115,2013-08-08  RALEIGH-DURHAM                                    
 RELDT 201307,299,2013-08-08  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201308,047,2013-09-03  ATLANTA                                           
 RELDT 201308,005,2013-09-03  CHICAGO                                           
 RELDT 201308,024,2013-09-03  DALLAS-FT. WORTH                                  
 RELDT 201308,033,2013-09-03  HOUSTON-GALVESTON                                 
 RELDT 201308,003,2013-09-03  LOS ANGELES                                       
 RELDT 201308,413,2013-09-03  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201308,321,2013-09-03  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201308,001,2013-09-03  NEW YORK                                          
 RELDT 201308,007,2013-09-03  PHILADELPHIA                                      
 RELDT 201308,379,2013-09-03  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201308,009,2013-09-03  SAN FRANCISCO                                     
 RELDT 201308,215,2013-09-03  SAN JOSE                                          
*                                                                               
 RELDT 201308,021,2013-09-04  BALTIMORE                                         
 RELDT 201308,013,2013-09-04  BOSTON                                            
 RELDT 201308,035,2013-09-04  DENVER-BOULDER                                    
 RELDT 201308,011,2013-09-04  DETROIT                                           
 RELDT 201308,429,2013-09-04  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201308,027,2013-09-04  MINNEAPOLIS-ST. PAUL                              
 RELDT 201308,057,2013-09-04  PHOENIX                                           
 RELDT 201308,063,2013-09-04  SAN DIEGO                                         
 RELDT 201308,039,2013-09-04  SEATTLE-TACOMA                                    
 RELDT 201308,017,2013-09-04  ST. LOUIS                                         
 RELDT 201308,087,2013-09-04  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201308,015,2013-09-04  WASHINGTON, DC                                    
*                                                                               
 RELDT 201308,093,2013-09-06  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201308,031,2013-09-06  CINCINNATI                                        
 RELDT 201308,019,2013-09-06  CLEVELAND                                         
 RELDT 201308,045,2013-09-06  COLUMBUS, OH                                      
 RELDT 201308,041,2013-09-06  KANSAS CITY                                       
 RELDT 201308,257,2013-09-06  LAS VEGAS                                         
 RELDT 201308,131,2013-09-06  ORLANDO                                           
 RELDT 201308,023,2013-09-06  PITTSBURGH, PA                                    
 RELDT 201308,051,2013-09-06  PORTLAND, OR                                      
 RELDT 201308,065,2013-09-06  SACRAMENTO                                        
 RELDT 201308,101,2013-09-06  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201308,059,2013-09-06  SAN ANTONIO                                       
*                                                                               
 RELDT 201308,135,2013-09-09  AUSTIN                                            
 RELDT 201308,166,2013-09-09  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201308,061,2013-09-09  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201308,049,2013-09-09  INDIANAPOLIS                                      
 RELDT 201308,107,2013-09-09  JACKSONVILLE                                      
 RELDT 201308,075,2013-09-09  MEMPHIS                                           
 RELDT 201308,043,2013-09-09  MILWAUKEE-RACINE                                  
 RELDT 201308,073,2013-09-09  NASHVILLE                                         
 RELDT 201308,109,2013-09-09  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201308,077,2013-09-09  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201308,115,2013-09-09  RALEIGH-DURHAM                                    
 RELDT 201308,299,2013-09-09  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201309,047,2013-09-30  ATLANTA                                           
 RELDT 201309,005,2013-09-30  CHICAGO                                           
 RELDT 201309,024,2013-09-30  DALLAS-FT. WORTH                                  
 RELDT 201309,033,2013-09-30  HOUSTON-GALVESTON                                 
 RELDT 201309,003,2013-09-30  LOS ANGELES                                       
 RELDT 201309,413,2013-09-30  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201309,321,2013-09-30  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201309,001,2013-09-30  NEW YORK                                          
 RELDT 201309,007,2013-09-30  PHILADELPHIA                                      
 RELDT 201309,379,2013-09-30  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201309,009,2013-09-30  SAN FRANCISCO                                     
 RELDT 201309,215,2013-09-30  SAN JOSE                                          
*                                                                               
 RELDT 201309,021,2013-10-01  BALTIMORE                                         
 RELDT 201309,013,2013-10-01  BOSTON                                            
 RELDT 201309,035,2013-10-01  DENVER-BOULDER                                    
 RELDT 201309,011,2013-10-01  DETROIT                                           
 RELDT 201309,429,2013-10-01  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201309,027,2013-10-01  MINNEAPOLIS-ST. PAUL                              
 RELDT 201309,057,2013-10-01  PHOENIX                                           
 RELDT 201309,063,2013-10-01  SAN DIEGO                                         
 RELDT 201309,039,2013-10-01  SEATTLE-TACOMA                                    
 RELDT 201309,017,2013-10-01  ST. LOUIS                                         
 RELDT 201309,087,2013-10-01  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201309,015,2013-10-01  WASHINGTON, DC                                    
*                                                                               
 RELDT 201309,093,2013-10-02  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201309,031,2013-10-02  CINCINNATI                                        
 RELDT 201309,019,2013-10-02  CLEVELAND                                         
 RELDT 201309,045,2013-10-02  COLUMBUS, OH                                      
 RELDT 201309,041,2013-10-02  KANSAS CITY                                       
 RELDT 201309,257,2013-10-02  LAS VEGAS                                         
 RELDT 201309,131,2013-10-02  ORLANDO                                           
 RELDT 201309,023,2013-10-02  PITTSBURGH, PA                                    
 RELDT 201309,051,2013-10-02  PORTLAND, OR                                      
 RELDT 201309,065,2013-10-02  SACRAMENTO                                        
 RELDT 201309,101,2013-10-02  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201309,059,2013-10-02  SAN ANTONIO                                       
*                                                                               
 RELDT 201309,135,2013-10-03  AUSTIN                                            
 RELDT 201309,166,2013-10-03  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201309,061,2013-10-03  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201309,049,2013-10-03  INDIANAPOLIS                                      
 RELDT 201309,107,2013-10-03  JACKSONVILLE                                      
 RELDT 201309,075,2013-10-03  MEMPHIS                                           
 RELDT 201309,043,2013-10-03  MILWAUKEE-RACINE                                  
 RELDT 201309,073,2013-10-03  NASHVILLE                                         
 RELDT 201309,109,2013-10-03  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201309,077,2013-10-03  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201309,115,2013-10-03  RALEIGH-DURHAM                                    
 RELDT 201309,299,2013-10-03  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201310,047,2013-10-28  ATLANTA                                           
 RELDT 201310,005,2013-10-28  CHICAGO                                           
 RELDT 201310,024,2013-10-28  DALLAS-FT. WORTH                                  
 RELDT 201310,033,2013-10-28  HOUSTON-GALVESTON                                 
 RELDT 201310,003,2013-10-28  LOS ANGELES                                       
 RELDT 201310,413,2013-10-28  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201310,321,2013-10-28  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201310,001,2013-10-28  NEW YORK                                          
 RELDT 201310,007,2013-10-28  PHILADELPHIA                                      
 RELDT 201310,379,2013-10-28  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201310,009,2013-10-28  SAN FRANCISCO                                     
 RELDT 201310,215,2013-10-28  SAN JOSE                                          
*                                                                               
 RELDT 201310,021,2013-10-29  BALTIMORE                                         
 RELDT 201310,013,2013-10-29  BOSTON                                            
 RELDT 201310,035,2013-10-29  DENVER-BOULDER                                    
 RELDT 201310,011,2013-10-29  DETROIT                                           
 RELDT 201310,429,2013-10-29  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201310,027,2013-10-29  MINNEAPOLIS-ST. PAUL                              
 RELDT 201310,057,2013-10-29  PHOENIX                                           
 RELDT 201310,063,2013-10-29  SAN DIEGO                                         
 RELDT 201310,039,2013-10-29  SEATTLE-TACOMA                                    
 RELDT 201310,017,2013-10-29  ST. LOUIS                                         
 RELDT 201310,087,2013-10-29  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201310,015,2013-10-29  WASHINGTON, DC                                    
*                                                                               
 RELDT 201310,093,2013-10-30  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201310,031,2013-10-30  CINCINNATI                                        
 RELDT 201310,019,2013-10-30  CLEVELAND                                         
 RELDT 201310,045,2013-10-30  COLUMBUS, OH                                      
 RELDT 201310,041,2013-10-30  KANSAS CITY                                       
 RELDT 201310,257,2013-10-30  LAS VEGAS                                         
 RELDT 201310,131,2013-10-30  ORLANDO                                           
 RELDT 201310,023,2013-10-30  PITTSBURGH, PA                                    
 RELDT 201310,051,2013-10-30  PORTLAND, OR                                      
 RELDT 201310,065,2013-10-30  SACRAMENTO                                        
 RELDT 201310,101,2013-10-30  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201310,059,2013-10-30  SAN ANTONIO                                       
*                                                                               
 RELDT 201310,135,2013-10-31  AUSTIN                                            
 RELDT 201310,166,2013-10-31  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201310,061,2013-10-31  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201310,049,2013-10-31  INDIANAPOLIS                                      
 RELDT 201310,107,2013-10-31  JACKSONVILLE                                      
 RELDT 201310,075,2013-10-31  MEMPHIS                                           
 RELDT 201310,043,2013-10-31  MILWAUKEE-RACINE                                  
 RELDT 201310,073,2013-10-31  NASHVILLE                                         
 RELDT 201310,109,2013-10-31  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201310,077,2013-10-31  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201310,115,2013-10-31  RALEIGH-DURHAM                                    
 RELDT 201310,299,2013-10-31  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201311,047,2013-11-25  ATLANTA                                           
 RELDT 201311,005,2013-11-25  CHICAGO                                           
 RELDT 201311,024,2013-11-25  DALLAS-FT. WORTH                                  
 RELDT 201311,033,2013-11-25  HOUSTON-GALVESTON                                 
 RELDT 201311,003,2013-11-25  LOS ANGELES                                       
 RELDT 201311,413,2013-11-25  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201311,321,2013-11-25  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201311,001,2013-11-25  NEW YORK                                          
 RELDT 201311,007,2013-11-25  PHILADELPHIA                                      
 RELDT 201311,379,2013-11-25  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201311,009,2013-11-25  SAN FRANCISCO                                     
 RELDT 201311,215,2013-11-25  SAN JOSE                                          
*                                                                               
 RELDT 201311,021,2013-11-26  BALTIMORE                                         
 RELDT 201311,013,2013-11-26  BOSTON                                            
 RELDT 201311,035,2013-11-26  DENVER-BOULDER                                    
 RELDT 201311,011,2013-11-26  DETROIT                                           
 RELDT 201311,429,2013-11-26  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201311,027,2013-11-26  MINNEAPOLIS-ST. PAUL                              
 RELDT 201311,057,2013-11-26  PHOENIX                                           
 RELDT 201311,063,2013-11-26  SAN DIEGO                                         
 RELDT 201311,039,2013-11-26  SEATTLE-TACOMA                                    
 RELDT 201311,017,2013-11-26  ST. LOUIS                                         
 RELDT 201311,087,2013-11-26  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201311,015,2013-11-26  WASHINGTON, DC                                    
*                                                                               
 RELDT 201311,093,2013-11-27  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201311,031,2013-11-27  CINCINNATI                                        
 RELDT 201311,019,2013-11-27  CLEVELAND                                         
 RELDT 201311,045,2013-11-27  COLUMBUS, OH                                      
 RELDT 201311,041,2013-11-27  KANSAS CITY                                       
 RELDT 201311,257,2013-11-27  LAS VEGAS                                         
 RELDT 201311,131,2013-11-27  ORLANDO                                           
 RELDT 201311,023,2013-11-27  PITTSBURGH, PA                                    
 RELDT 201311,051,2013-11-27  PORTLAND, OR                                      
 RELDT 201311,065,2013-11-27  SACRAMENTO                                        
 RELDT 201311,101,2013-11-27  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201311,059,2013-11-27  SAN ANTONIO                                       
*                                                                               
 RELDT 201311,135,2013-12-02  AUSTIN                                            
 RELDT 201311,166,2013-12-02  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201311,061,2013-12-02  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201311,049,2013-12-02  INDIANAPOLIS                                      
 RELDT 201311,107,2013-12-02  JACKSONVILLE                                      
 RELDT 201311,075,2013-12-02  MEMPHIS                                           
 RELDT 201311,043,2013-12-02  MILWAUKEE-RACINE                                  
 RELDT 201311,073,2013-12-02  NASHVILLE                                         
 RELDT 201311,109,2013-12-02  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201311,077,2013-12-02  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201311,115,2013-12-02  RALEIGH-DURHAM                                    
 RELDT 201311,299,2013-12-02  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201312,047,2013-12-23  ATLANTA                                           
 RELDT 201312,005,2013-12-23  CHICAGO                                           
 RELDT 201312,024,2013-12-23  DALLAS-FT. WORTH                                  
 RELDT 201312,033,2013-12-23  HOUSTON-GALVESTON                                 
 RELDT 201312,003,2013-12-23  LOS ANGELES                                       
 RELDT 201312,413,2013-12-23  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201312,321,2013-12-23  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201312,001,2013-12-23  NEW YORK                                          
 RELDT 201312,007,2013-12-23  PHILADELPHIA                                      
 RELDT 201312,379,2013-12-23  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201312,009,2013-12-23  SAN FRANCISCO                                     
 RELDT 201312,215,2013-12-23  SAN JOSE                                          
*                                                                               
 RELDT 201312,021,2013-12-26  BALTIMORE                                         
 RELDT 201312,013,2013-12-26  BOSTON                                            
 RELDT 201312,035,2013-12-26  DENVER-BOULDER                                    
 RELDT 201312,011,2013-12-26  DETROIT                                           
 RELDT 201312,429,2013-12-26  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201312,027,2013-12-26  MINNEAPOLIS-ST. PAUL                              
 RELDT 201312,057,2013-12-26  PHOENIX                                           
 RELDT 201312,063,2013-12-26  SAN DIEGO                                         
 RELDT 201312,039,2013-12-26  SEATTLE-TACOMA                                    
 RELDT 201312,017,2013-12-26  ST. LOUIS                                         
 RELDT 201312,087,2013-12-26  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201312,015,2013-12-26  WASHINGTON, DC                                    
*                                                                               
 RELDT 201312,093,2013-12-27  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201312,031,2013-12-27  CINCINNATI                                        
 RELDT 201312,019,2013-12-27  CLEVELAND                                         
 RELDT 201312,045,2013-12-27  COLUMBUS, OH                                      
 RELDT 201312,041,2013-12-27  KANSAS CITY                                       
 RELDT 201312,257,2013-12-27  LAS VEGAS                                         
 RELDT 201312,131,2013-12-27  ORLANDO                                           
 RELDT 201312,023,2013-12-27  PITTSBURGH, PA                                    
 RELDT 201312,051,2013-12-27  PORTLAND, OR                                      
 RELDT 201312,065,2013-12-27  SACRAMENTO                                        
 RELDT 201312,101,2013-12-27  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201312,059,2013-12-27  SAN ANTONIO                                       
*                                                                               
 RELDT 201312,135,2013-12-30  AUSTIN                                            
 RELDT 201312,166,2013-12-30  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201312,061,2013-12-30  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201312,049,2013-12-30  INDIANAPOLIS                                      
 RELDT 201312,107,2013-12-30  JACKSONVILLE                                      
 RELDT 201312,075,2013-12-30  MEMPHIS                                           
 RELDT 201312,043,2013-12-30  MILWAUKEE-RACINE                                  
 RELDT 201312,073,2013-12-30  NASHVILLE                                         
 RELDT 201312,109,2013-12-30  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201312,077,2013-12-30  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201312,115,2013-12-30  RALEIGH-DURHAM                                    
 RELDT 201312,299,2013-12-30  WEST PALM BEACH-BOCA RATON                        
*                                                                               
* HOLIDAY BOOK                                                                  
 RELDT 201323,047,2014-01-21  ATLANTA                                           
 RELDT 201323,005,2014-01-21  CHICAGO                                           
 RELDT 201323,024,2014-01-21  DALLAS-FT. WORTH                                  
 RELDT 201323,033,2014-01-21  HOUSTON-GALVESTON                                 
 RELDT 201323,003,2014-01-21  LOS ANGELES                                       
 RELDT 201323,413,2014-01-21  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201323,321,2014-01-21  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201323,001,2014-01-21  NEW YORK                                          
 RELDT 201323,007,2014-01-21  PHILADELPHIA                                      
 RELDT 201323,379,2014-01-21  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201323,009,2014-01-21  SAN FRANCISCO                                     
 RELDT 201323,215,2014-01-21  SAN JOSE                                          
*                                                                               
 RELDT 201323,021,2014-01-22  BALTIMORE                                         
 RELDT 201323,013,2014-01-22  BOSTON                                            
 RELDT 201323,035,2014-01-22  DENVER-BOULDER                                    
 RELDT 201323,011,2014-01-22  DETROIT                                           
 RELDT 201323,429,2014-01-22  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201323,027,2014-01-22  MINNEAPOLIS-ST. PAUL                              
 RELDT 201323,057,2014-01-22  PHOENIX                                           
 RELDT 201323,063,2014-01-22  SAN DIEGO                                         
 RELDT 201323,039,2014-01-22  SEATTLE-TACOMA                                    
 RELDT 201323,017,2014-01-22  ST. LOUIS                                         
 RELDT 201323,087,2014-01-22  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201323,015,2014-01-22  WASHINGTON, DC                                    
*                                                                               
 RELDT 201323,093,2014-01-23  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201323,031,2014-01-23  CINCINNATI                                        
 RELDT 201323,019,2014-01-23  CLEVELAND                                         
 RELDT 201323,045,2014-01-23  COLUMBUS, OH                                      
 RELDT 201323,041,2014-01-23  KANSAS CITY                                       
 RELDT 201323,257,2014-01-23  LAS VEGAS                                         
 RELDT 201323,131,2014-01-23  ORLANDO                                           
 RELDT 201323,023,2014-01-23  PITTSBURGH, PA                                    
 RELDT 201323,051,2014-01-23  PORTLAND, OR                                      
 RELDT 201323,065,2014-01-23  SACRAMENTO                                        
 RELDT 201323,101,2014-01-23  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201323,059,2014-01-23  SAN ANTONIO                                       
*                                                                               
 RELDT 201323,135,2014-01-24  AUSTIN                                            
 RELDT 201323,166,2014-01-24  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201323,061,2014-01-24  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201323,049,2014-01-24  INDIANAPOLIS                                      
 RELDT 201323,107,2014-01-24  JACKSONVILLE                                      
 RELDT 201323,075,2014-01-24  MEMPHIS                                           
 RELDT 201323,043,2014-01-24  MILWAUKEE-RACINE                                  
 RELDT 201323,073,2014-01-24  NASHVILLE                                         
 RELDT 201323,109,2014-01-24  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201323,077,2014-01-24  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201323,115,2014-01-24  RALEIGH-DURHAM                                    
 RELDT 201323,299,2014-01-24  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
* PPM 2014 MARKETS                                                              
*                                                                               
 RELDT 201401,047,2014-02-17  ATLANTA                                           
 RELDT 201401,005,2014-02-17  CHICAGO                                           
 RELDT 201401,024,2014-02-17  DALLAS-FT. WORTH                                  
 RELDT 201401,033,2014-02-17  HOUSTON-GALVESTON                                 
 RELDT 201401,003,2014-02-17  LOS ANGELES                                       
 RELDT 201401,413,2014-02-17  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201401,321,2014-02-17  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201401,001,2014-02-17  NEW YORK                                          
 RELDT 201401,007,2014-02-17  PHILADELPHIA                                      
 RELDT 201401,379,2014-02-17  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201401,009,2014-02-17  SAN FRANCISCO                                     
 RELDT 201401,215,2014-02-17  SAN JOSE                                          
*                                                                               
 RELDT 201401,021,2014-02-18  BALTIMORE                                         
 RELDT 201401,013,2014-02-18  BOSTON                                            
 RELDT 201401,035,2014-02-18  DENVER-BOULDER                                    
 RELDT 201401,011,2014-02-18  DETROIT                                           
 RELDT 201401,429,2014-02-18  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201401,027,2014-02-18  MINNEAPOLIS-ST. PAUL                              
 RELDT 201401,057,2014-02-18  PHOENIX                                           
 RELDT 201401,063,2014-02-18  SAN DIEGO                                         
 RELDT 201401,039,2014-02-18  SEATTLE-TACOMA                                    
 RELDT 201401,017,2014-02-18  ST. LOUIS                                         
 RELDT 201401,087,2014-02-18  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201401,015,2014-02-18  WASHINGTON, DC                                    
*                                                                               
 RELDT 201401,093,2014-02-19  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201401,031,2014-02-19  CINCINNATI                                        
 RELDT 201401,019,2014-02-19  CLEVELAND                                         
 RELDT 201401,045,2014-02-19  COLUMBUS, OH                                      
 RELDT 201401,041,2014-02-19  KANSAS CITY                                       
 RELDT 201401,257,2014-02-19  LAS VEGAS                                         
 RELDT 201401,131,2014-02-19  ORLANDO                                           
 RELDT 201401,023,2014-02-19  PITTSBURGH, PA                                    
 RELDT 201401,051,2014-02-19  PORTLAND, OR                                      
 RELDT 201401,065,2014-02-19  SACRAMENTO                                        
 RELDT 201401,101,2014-02-19  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201401,059,2014-02-19  SAN ANTONIO                                       
*                                                                               
 RELDT 201401,135,2014-02-20  AUSTIN                                            
 RELDT 201401,166,2014-02-20  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201401,061,2014-02-20  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201401,049,2014-02-20  INDIANAPOLIS                                      
 RELDT 201401,107,2014-02-20  JACKSONVILLE                                      
 RELDT 201401,075,2014-02-20  MEMPHIS                                           
 RELDT 201401,043,2014-02-20  MILWAUKEE-RACINE                                  
 RELDT 201401,073,2014-02-20  NASHVILLE                                         
 RELDT 201401,109,2014-02-20  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201401,077,2014-02-20  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201401,115,2014-02-20  RALEIGH-DURHAM                                    
 RELDT 201401,299,2014-02-20  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201402,047,2014-03-17  ATLANTA                                           
 RELDT 201402,005,2014-03-17  CHICAGO                                           
 RELDT 201402,024,2014-03-17  DALLAS-FT. WORTH                                  
 RELDT 201402,033,2014-03-17  HOUSTON-GALVESTON                                 
 RELDT 201402,003,2014-03-17  LOS ANGELES                                       
 RELDT 201402,413,2014-03-17  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201402,321,2014-03-17  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201402,001,2014-03-17  NEW YORK                                          
 RELDT 201402,007,2014-03-17  PHILADELPHIA                                      
 RELDT 201402,379,2014-03-17  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201402,009,2014-03-17  SAN FRANCISCO                                     
 RELDT 201402,215,2014-03-17  SAN JOSE                                          
*                                                                               
 RELDT 201402,021,2014-03-18  BALTIMORE                                         
 RELDT 201402,013,2014-03-18  BOSTON                                            
 RELDT 201402,035,2014-03-18  DENVER-BOULDER                                    
 RELDT 201402,011,2014-03-18  DETROIT                                           
 RELDT 201402,429,2014-03-18  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201402,027,2014-03-18  MINNEAPOLIS-ST. PAUL                              
 RELDT 201402,057,2014-03-18  PHOENIX                                           
 RELDT 201402,063,2014-03-18  SAN DIEGO                                         
 RELDT 201402,039,2014-03-18  SEATTLE-TACOMA                                    
 RELDT 201402,017,2014-03-18  ST. LOUIS                                         
 RELDT 201402,087,2014-03-18  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201402,015,2014-03-18  WASHINGTON, DC                                    
*                                                                               
 RELDT 201402,093,2014-03-19  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201402,031,2014-03-19  CINCINNATI                                        
 RELDT 201402,019,2014-03-19  CLEVELAND                                         
 RELDT 201402,045,2014-03-19  COLUMBUS, OH                                      
 RELDT 201402,041,2014-03-19  KANSAS CITY                                       
 RELDT 201402,257,2014-03-19  LAS VEGAS                                         
 RELDT 201402,131,2014-03-19  ORLANDO                                           
 RELDT 201402,023,2014-03-19  PITTSBURGH, PA                                    
 RELDT 201402,051,2014-03-19  PORTLAND, OR                                      
 RELDT 201402,065,2014-03-19  SACRAMENTO                                        
 RELDT 201402,101,2014-03-19  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201402,059,2014-03-19  SAN ANTONIO                                       
*                                                                               
 RELDT 201402,135,2014-03-20  AUSTIN                                            
 RELDT 201402,166,2014-03-20  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201402,061,2014-03-20  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201402,049,2014-03-20  INDIANAPOLIS                                      
 RELDT 201402,107,2014-03-20  JACKSONVILLE                                      
 RELDT 201402,075,2014-03-20  MEMPHIS                                           
 RELDT 201402,043,2014-03-20  MILWAUKEE-RACINE                                  
 RELDT 201402,073,2014-03-20  NASHVILLE                                         
 RELDT 201402,109,2014-03-20  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201402,077,2014-03-20  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201402,115,2014-03-20  RALEIGH-DURHAM                                    
 RELDT 201402,299,2014-03-20  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201403,047,2014-04-14  ATLANTA                                           
 RELDT 201403,005,2014-04-14  CHICAGO                                           
 RELDT 201403,024,2014-04-14  DALLAS-FT. WORTH                                  
 RELDT 201403,033,2014-04-14  HOUSTON-GALVESTON                                 
 RELDT 201403,003,2014-04-14  LOS ANGELES                                       
 RELDT 201403,413,2014-04-14  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201403,321,2014-04-14  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201403,001,2014-04-14  NEW YORK                                          
 RELDT 201403,007,2014-04-14  PHILADELPHIA                                      
 RELDT 201403,379,2014-04-14  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201403,009,2014-04-14  SAN FRANCISCO                                     
 RELDT 201403,215,2014-04-14  SAN JOSE                                          
*                                                                               
 RELDT 201403,021,2014-04-16  BALTIMORE                                         
 RELDT 201403,013,2014-04-16  BOSTON                                            
 RELDT 201403,035,2014-04-16  DENVER-BOULDER                                    
 RELDT 201403,011,2014-04-16  DETROIT                                           
 RELDT 201403,429,2014-04-16  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201403,027,2014-04-16  MINNEAPOLIS-ST. PAUL                              
 RELDT 201403,057,2014-04-16  PHOENIX                                           
 RELDT 201403,063,2014-04-16  SAN DIEGO                                         
 RELDT 201403,039,2014-04-16  SEATTLE-TACOMA                                    
 RELDT 201403,017,2014-04-16  ST. LOUIS                                         
 RELDT 201403,087,2014-04-16  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201403,015,2014-04-16  WASHINGTON, DC                                    
*                                                                               
 RELDT 201403,093,2014-04-17  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201403,031,2014-04-17  CINCINNATI                                        
 RELDT 201403,019,2014-04-17  CLEVELAND                                         
 RELDT 201403,045,2014-04-17  COLUMBUS, OH                                      
 RELDT 201403,041,2014-04-17  KANSAS CITY                                       
 RELDT 201403,257,2014-04-17  LAS VEGAS                                         
 RELDT 201403,131,2014-04-17  ORLANDO                                           
 RELDT 201403,023,2014-04-17  PITTSBURGH, PA                                    
 RELDT 201403,051,2014-04-17  PORTLAND, OR                                      
 RELDT 201403,065,2014-04-17  SACRAMENTO                                        
 RELDT 201403,101,2014-04-17  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201403,059,2014-04-17  SAN ANTONIO                                       
*                                                                               
 RELDT 201403,135,2014-04-21  AUSTIN                                            
 RELDT 201403,166,2014-04-21  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201403,061,2014-04-21  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201403,049,2014-04-21  INDIANAPOLIS                                      
 RELDT 201403,107,2014-04-21  JACKSONVILLE                                      
 RELDT 201403,075,2014-04-21  MEMPHIS                                           
 RELDT 201403,043,2014-04-21  MILWAUKEE-RACINE                                  
 RELDT 201403,073,2014-04-21  NASHVILLE                                         
 RELDT 201403,109,2014-04-21  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201403,077,2014-04-21  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201403,115,2014-04-21  RALEIGH-DURHAM                                    
 RELDT 201403,299,2014-04-21  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201404,047,2014-05-12  ATLANTA                                           
 RELDT 201404,005,2014-05-12  CHICAGO                                           
 RELDT 201404,024,2014-05-12  DALLAS-FT. WORTH                                  
 RELDT 201404,033,2014-05-12  HOUSTON-GALVESTON                                 
 RELDT 201404,003,2014-05-12  LOS ANGELES                                       
 RELDT 201404,413,2014-05-12  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201404,321,2014-05-12  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201404,001,2014-05-12  NEW YORK                                          
 RELDT 201404,007,2014-05-12  PHILADELPHIA                                      
 RELDT 201404,379,2014-05-12  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201404,009,2014-05-12  SAN FRANCISCO                                     
 RELDT 201404,215,2014-05-12  SAN JOSE                                          
*                                                                               
 RELDT 201404,021,2014-05-13  BALTIMORE                                         
 RELDT 201404,013,2014-05-13  BOSTON                                            
 RELDT 201404,035,2014-05-13  DENVER-BOULDER                                    
 RELDT 201404,011,2014-05-13  DETROIT                                           
 RELDT 201404,429,2014-05-13  MIAMI-FT. LAUDERDALE-HOLLYWOOD                    
 RELDT 201404,027,2014-05-13  MINNEAPOLIS-ST. PAUL                              
 RELDT 201404,057,2014-05-13  PHOENIX                                           
 RELDT 201404,063,2014-05-13  SAN DIEGO                                         
 RELDT 201404,039,2014-05-13  SEATTLE-TACOMA                                    
 RELDT 201404,017,2014-05-13  ST. LOUIS                                         
 RELDT 201404,087,2014-05-13  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201404,015,2014-05-13  WASHINGTON, DC                                    
*                                                                               
 RELDT 201404,093,2014-05-14  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201404,031,2014-05-14  CINCINNATI                                        
 RELDT 201404,019,2014-05-14  CLEVELAND                                         
 RELDT 201404,045,2014-05-14  COLUMBUS, OH                                      
 RELDT 201404,041,2014-05-14  KANSAS CITY                                       
 RELDT 201404,257,2014-05-14  LAS VEGAS                                         
 RELDT 201404,131,2014-05-14  ORLANDO                                           
 RELDT 201404,023,2014-05-14  PITTSBURGH, PA                                    
 RELDT 201404,051,2014-05-14  PORTLAND, OR                                      
 RELDT 201404,065,2014-05-14  SACRAMENTO                                        
 RELDT 201404,101,2014-05-14  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201404,059,2014-05-14  SAN ANTONIO                                       
*                                                                               
 RELDT 201404,135,2014-05-15  AUSTIN                                            
 RELDT 201404,166,2014-05-15  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201404,061,2014-05-15  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201404,049,2014-05-15  INDIANAPOLIS                                      
 RELDT 201404,107,2014-05-15  JACKSONVILLE                                      
 RELDT 201404,075,2014-05-15  MEMPHIS                                           
 RELDT 201404,043,2014-05-15  MILWAUKEE-RACINE                                  
 RELDT 201404,073,2014-05-15  NASHVILLE                                         
 RELDT 201404,109,2014-05-15  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201404,077,2014-05-15  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201404,115,2014-05-15  RALEIGH-DURHAM                                    
 RELDT 201404,299,2014-05-15  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201405,047,2014-06-09  ATLANTA                                           
 RELDT 201405,005,2014-06-09  CHICAGO                                           
 RELDT 201405,024,2014-06-09  DALLAS-FT. WORTH                                  
 RELDT 201405,033,2014-06-09  HOUSTON-GALVESTON                                 
 RELDT 201405,003,2014-06-09  LOS ANGELES                                       
 RELDT 201405,413,2014-06-09  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201405,321,2014-06-09  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201405,001,2014-06-09  NEW YORK                                          
 RELDT 201405,007,2014-06-09  PHILADELPHIA                                      
 RELDT 201405,379,2014-06-09  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201405,009,2014-06-09  SAN FRANCISCO                                     
 RELDT 201405,215,2014-06-09  SAN JOSE                                          
*                                                                               
 RELDT 201405,021,2014-06-10  BALTIMORE                                         
 RELDT 201405,013,2014-06-10  BOSTON                                            
 RELDT 201405,035,2014-06-10  DENVER-BOULDER                                    
 RELDT 201405,011,2014-06-10  DETROIT                                           
 RELDT 201405,429,2014-06-10  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201405,027,2014-06-10  MINNEAPOLIS-ST. PAUL                              
 RELDT 201405,057,2014-06-10  PHOENIX                                           
 RELDT 201405,063,2014-06-10  SAN DIEGO                                         
 RELDT 201405,039,2014-06-10  SEATTLE-TACOMA                                    
 RELDT 201405,017,2014-06-10  ST. LOUIS                                         
 RELDT 201405,087,2014-06-10  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201405,015,2014-06-10  WASHINGTON, DC                                    
*                                                                               
 RELDT 201405,093,2014-06-11  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201405,031,2014-06-11  CINCINNATI                                        
 RELDT 201405,019,2014-06-11  CLEVELAND                                         
 RELDT 201405,045,2014-06-11  COLUMBUS, OH                                      
 RELDT 201405,041,2014-06-11  KANSAS CITY                                       
 RELDT 201405,257,2014-06-11  LAS VEGAS                                         
 RELDT 201405,131,2014-06-11  ORLANDO                                           
 RELDT 201405,023,2014-06-11  PITTSBURGH, PA                                    
 RELDT 201405,051,2014-06-11  PORTLAND, OR                                      
 RELDT 201405,065,2014-06-11  SACRAMENTO                                        
 RELDT 201405,101,2014-06-11  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201405,059,2014-06-11  SAN ANTONIO                                       
*                                                                               
 RELDT 201405,135,2014-06-12  AUSTIN                                            
 RELDT 201405,166,2014-06-12  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201405,061,2014-06-12  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201405,049,2014-06-12  INDIANAPOLIS                                      
 RELDT 201405,107,2014-06-12  JACKSONVILLE                                      
 RELDT 201405,075,2014-06-12  MEMPHIS                                           
 RELDT 201405,043,2014-06-12  MILWAUKEE-RACINE                                  
 RELDT 201405,073,2014-06-12  NASHVILLE                                         
 RELDT 201405,109,2014-06-12  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201405,077,2014-06-12  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201405,115,2014-06-12  RALEIGH-DURHAM                                    
 RELDT 201405,299,2014-06-12  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201406,047,2014-07-08  ATLANTA                                           
 RELDT 201406,005,2014-07-08  CHICAGO                                           
 RELDT 201406,024,2014-07-08  DALLAS-FT. WORTH                                  
 RELDT 201406,033,2014-07-08  HOUSTON-GALVESTON                                 
 RELDT 201406,003,2014-07-08  LOS ANGELES                                       
 RELDT 201406,413,2014-07-08  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201406,321,2014-07-08  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201406,001,2014-07-08  NEW YORK                                          
 RELDT 201406,007,2014-07-08  PHILADELPHIA                                      
 RELDT 201406,379,2014-07-08  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201406,009,2014-07-08  SAN FRANCISCO                                     
 RELDT 201406,215,2014-07-08  SAN JOSE                                          
*                                                                               
 RELDT 201406,021,2014-07-09  BALTIMORE                                         
 RELDT 201406,013,2014-07-09  BOSTON                                            
 RELDT 201406,035,2014-07-09  DENVER-BOULDER                                    
 RELDT 201406,011,2014-07-09  DETROIT                                           
 RELDT 201406,429,2014-07-09  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201406,027,2014-07-09  MINNEAPOLIS-ST. PAUL                              
 RELDT 201406,057,2014-07-09  PHOENIX                                           
 RELDT 201406,063,2014-07-09  SAN DIEGO                                         
 RELDT 201406,039,2014-07-09  SEATTLE-TACOMA                                    
 RELDT 201406,017,2014-07-09  ST. LOUIS                                         
 RELDT 201406,087,2014-07-09  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201406,015,2014-07-09  WASHINGTON, DC                                    
*                                                                               
 RELDT 201406,093,2014-07-10  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201406,031,2014-07-10  CINCINNATI                                        
 RELDT 201406,019,2014-07-10  CLEVELAND                                         
 RELDT 201406,045,2014-07-10  COLUMBUS, OH                                      
 RELDT 201406,041,2014-07-10  KANSAS CITY                                       
 RELDT 201406,257,2014-07-10  LAS VEGAS                                         
 RELDT 201406,131,2014-07-10  ORLANDO                                           
 RELDT 201406,023,2014-07-10  PITTSBURGH, PA                                    
 RELDT 201406,051,2014-07-10  PORTLAND, OR                                      
 RELDT 201406,065,2014-07-10  SACRAMENTO                                        
 RELDT 201406,101,2014-07-10  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201406,059,2014-07-10  SAN ANTONIO                                       
*                                                                               
 RELDT 201406,135,2014-07-11  AUSTIN                                            
 RELDT 201406,166,2014-07-11  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201406,061,2014-07-11  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201406,049,2014-07-11  INDIANAPOLIS                                      
 RELDT 201406,107,2014-07-11  JACKSONVILLE                                      
 RELDT 201406,075,2014-07-11  MEMPHIS                                           
 RELDT 201406,043,2014-07-11  MILWAUKEE-RACINE                                  
 RELDT 201406,073,2014-07-11  NASHVILLE                                         
 RELDT 201406,109,2014-07-11  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201406,077,2014-07-11  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201406,115,2014-07-11  RALEIGH-DURHAM                                    
 RELDT 201406,299,2014-07-11  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201407,047,2014-08-04  ATLANTA                                           
 RELDT 201407,005,2014-08-04  CHICAGO                                           
 RELDT 201407,024,2014-08-04  DALLAS-FT. WORTH                                  
 RELDT 201407,033,2014-08-04  HOUSTON-GALVESTON                                 
 RELDT 201407,003,2014-08-04  LOS ANGELES                                       
 RELDT 201407,413,2014-08-04  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201407,321,2014-08-04  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201407,001,2014-08-04  NEW YORK                                          
 RELDT 201407,007,2014-08-04  PHILADELPHIA                                      
 RELDT 201407,379,2014-08-04  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201407,009,2014-08-04  SAN FRANCISCO                                     
 RELDT 201407,215,2014-08-04  SAN JOSE                                          
*                                                                               
 RELDT 201407,021,2014-08-05  BALTIMORE                                         
 RELDT 201407,013,2014-08-05  BOSTON                                            
 RELDT 201407,035,2014-08-05  DENVER-BOULDER                                    
 RELDT 201407,011,2014-08-05  DETROIT                                           
 RELDT 201407,429,2014-08-05  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201407,027,2014-08-05  MINNEAPOLIS-ST. PAUL                              
 RELDT 201407,057,2014-08-05  PHOENIX                                           
 RELDT 201407,063,2014-08-05  SAN DIEGO                                         
 RELDT 201407,039,2014-08-05  SEATTLE-TACOMA                                    
 RELDT 201407,017,2014-08-05  ST. LOUIS                                         
 RELDT 201407,087,2014-08-05  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201407,015,2014-08-05  WASHINGTON, DC                                    
*                                                                               
 RELDT 201407,093,2014-08-06  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201407,031,2014-08-06  CINCINNATI                                        
 RELDT 201407,019,2014-08-06  CLEVELAND                                         
 RELDT 201407,045,2014-08-06  COLUMBUS, OH                                      
 RELDT 201407,041,2014-08-06  KANSAS CITY                                       
 RELDT 201407,257,2014-08-06  LAS VEGAS                                         
 RELDT 201407,131,2014-08-06  ORLANDO                                           
 RELDT 201407,023,2014-08-06  PITTSBURGH, PA                                    
 RELDT 201407,051,2014-08-06  PORTLAND, OR                                      
 RELDT 201407,065,2014-08-06  SACRAMENTO                                        
 RELDT 201407,101,2014-08-06  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201407,059,2014-08-06  SAN ANTONIO                                       
*                                                                               
 RELDT 201407,135,2014-08-07  AUSTIN                                            
 RELDT 201407,166,2014-08-07  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201407,061,2014-08-07  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201407,049,2014-08-07  INDIANAPOLIS                                      
 RELDT 201407,107,2014-08-07  JACKSONVILLE                                      
 RELDT 201407,075,2014-08-07  MEMPHIS                                           
 RELDT 201407,043,2014-08-07  MILWAUKEE-RACINE                                  
 RELDT 201407,073,2014-08-07  NASHVILLE                                         
 RELDT 201407,109,2014-08-07  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201407,077,2014-08-07  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201407,115,2014-08-07  RALEIGH-DURHAM                                    
 RELDT 201407,299,2014-08-07  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201408,047,2014-09-02  ATLANTA                                           
 RELDT 201408,005,2014-09-02  CHICAGO                                           
 RELDT 201408,024,2014-09-02  DALLAS-FT. WORTH                                  
 RELDT 201408,033,2014-09-02  HOUSTON-GALVESTON                                 
 RELDT 201408,003,2014-09-02  LOS ANGELES                                       
 RELDT 201408,413,2014-09-02  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201408,321,2014-09-02  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201408,001,2014-09-02  NEW YORK                                          
 RELDT 201408,007,2014-09-02  PHILADELPHIA                                      
 RELDT 201408,379,2014-09-02  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201408,009,2014-09-02  SAN FRANCISCO                                     
 RELDT 201408,215,2014-09-02  SAN JOSE                                          
*                                                                               
 RELDT 201408,021,2014-09-03  BALTIMORE                                         
 RELDT 201408,013,2014-09-03  BOSTON                                            
 RELDT 201408,035,2014-09-03  DENVER-BOULDER                                    
 RELDT 201408,011,2014-09-03  DETROIT                                           
 RELDT 201408,429,2014-09-03  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201408,027,2014-09-03  MINNEAPOLIS-ST. PAUL                              
 RELDT 201408,057,2014-09-03  PHOENIX                                           
 RELDT 201408,063,2014-09-03  SAN DIEGO                                         
 RELDT 201408,039,2014-09-03  SEATTLE-TACOMA                                    
 RELDT 201408,017,2014-09-03  ST. LOUIS                                         
 RELDT 201408,087,2014-09-03  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201408,015,2014-09-03  WASHINGTON, DC                                    
*                                                                               
 RELDT 201408,093,2014-09-04  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201408,031,2014-09-04  CINCINNATI                                        
 RELDT 201408,019,2014-09-04  CLEVELAND                                         
 RELDT 201408,045,2014-09-04  COLUMBUS, OH                                      
 RELDT 201408,041,2014-09-04  KANSAS CITY                                       
 RELDT 201408,257,2014-09-04  LAS VEGAS                                         
 RELDT 201408,131,2014-09-04  ORLANDO                                           
 RELDT 201408,023,2014-09-04  PITTSBURGH, PA                                    
 RELDT 201408,051,2014-09-04  PORTLAND, OR                                      
 RELDT 201408,065,2014-09-04  SACRAMENTO                                        
 RELDT 201408,101,2014-09-04  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201408,059,2014-09-04  SAN ANTONIO                                       
*                                                                               
 RELDT 201408,135,2014-09-05  AUSTIN                                            
 RELDT 201408,166,2014-09-05  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201408,061,2014-09-05  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201408,049,2014-09-05  INDIANAPOLIS                                      
 RELDT 201408,107,2014-09-05  JACKSONVILLE                                      
 RELDT 201408,075,2014-09-05  MEMPHIS                                           
 RELDT 201408,043,2014-09-05  MILWAUKEE-RACINE                                  
 RELDT 201408,073,2014-09-05  NASHVILLE                                         
 RELDT 201408,109,2014-09-05  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201408,077,2014-09-05  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201408,115,2014-09-05  RALEIGH-DURHAM                                    
 RELDT 201408,299,2014-09-05  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201409,047,2014-09-29  ATLANTA                                           
 RELDT 201409,005,2014-09-29  CHICAGO                                           
 RELDT 201409,024,2014-09-29  DALLAS-FT. WORTH                                  
 RELDT 201409,033,2014-09-29  HOUSTON-GALVESTON                                 
 RELDT 201409,003,2014-09-29  LOS ANGELES                                       
 RELDT 201409,413,2014-09-29  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201409,321,2014-09-29  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201409,001,2014-09-29  NEW YORK                                          
 RELDT 201409,007,2014-09-29  PHILADELPHIA                                      
 RELDT 201409,379,2014-09-29  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201409,009,2014-09-29  SAN FRANCISCO                                     
 RELDT 201409,215,2014-09-29  SAN JOSE                                          
*                                                                               
 RELDT 201409,021,2014-09-30  BALTIMORE                                         
 RELDT 201409,013,2014-09-30  BOSTON                                            
 RELDT 201409,035,2014-09-30  DENVER-BOULDER                                    
 RELDT 201409,011,2014-09-30  DETROIT                                           
 RELDT 201409,429,2014-09-30  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201409,027,2014-09-30  MINNEAPOLIS-ST. PAUL                              
 RELDT 201409,057,2014-09-30  PHOENIX                                           
 RELDT 201409,063,2014-09-30  SAN DIEGO                                         
 RELDT 201409,039,2014-09-30  SEATTLE-TACOMA                                    
 RELDT 201409,017,2014-09-30  ST. LOUIS                                         
 RELDT 201409,087,2014-09-30  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201409,015,2014-09-30  WASHINGTON, DC                                    
*                                                                               
 RELDT 201409,093,2014-10-01  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201409,031,2014-10-01  CINCINNATI                                        
 RELDT 201409,019,2014-10-01  CLEVELAND                                         
 RELDT 201409,045,2014-10-01  COLUMBUS, OH                                      
 RELDT 201409,041,2014-10-01  KANSAS CITY                                       
 RELDT 201409,257,2014-10-01  LAS VEGAS                                         
 RELDT 201409,131,2014-10-01  ORLANDO                                           
 RELDT 201409,023,2014-10-01  PITTSBURGH, PA                                    
 RELDT 201409,051,2014-10-01  PORTLAND, OR                                      
 RELDT 201409,065,2014-10-01  SACRAMENTO                                        
 RELDT 201409,101,2014-10-01  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201409,059,2014-10-01  SAN ANTONIO                                       
*                                                                               
 RELDT 201409,135,2014-10-02  AUSTIN                                            
 RELDT 201409,166,2014-10-02  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201409,061,2014-10-02  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201409,049,2014-10-02  INDIANAPOLIS                                      
 RELDT 201409,107,2014-10-02  JACKSONVILLE                                      
 RELDT 201409,075,2014-10-02  MEMPHIS                                           
 RELDT 201409,043,2014-10-02  MILWAUKEE-RACINE                                  
 RELDT 201409,073,2014-10-02  NASHVILLE                                         
 RELDT 201409,109,2014-10-02  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201409,077,2014-10-02  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201409,115,2014-10-02  RALEIGH-DURHAM                                    
 RELDT 201409,299,2014-10-02  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201410,047,2014-10-27  ATLANTA                                           
 RELDT 201410,005,2014-10-27  CHICAGO                                           
 RELDT 201410,024,2014-10-27  DALLAS-FT. WORTH                                  
 RELDT 201410,033,2014-10-27  HOUSTON-GALVESTON                                 
 RELDT 201410,003,2014-10-27  LOS ANGELES                                       
 RELDT 201410,413,2014-10-27  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201410,321,2014-10-27  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201410,001,2014-10-27  NEW YORK                                          
 RELDT 201410,007,2014-10-27  PHILADELPHIA                                      
 RELDT 201410,379,2014-10-27  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201410,009,2014-10-27  SAN FRANCISCO                                     
 RELDT 201410,215,2014-10-27  SAN JOSE                                          
*                                                                               
 RELDT 201410,021,2014-10-28  BALTIMORE                                         
 RELDT 201410,013,2014-10-28  BOSTON                                            
 RELDT 201410,035,2014-10-28  DENVER-BOULDER                                    
 RELDT 201410,011,2014-10-28  DETROIT                                           
 RELDT 201410,429,2014-10-28  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201410,027,2014-10-28  MINNEAPOLIS-ST. PAUL                              
 RELDT 201410,057,2014-10-28  PHOENIX                                           
 RELDT 201410,063,2014-10-28  SAN DIEGO                                         
 RELDT 201410,039,2014-10-28  SEATTLE-TACOMA                                    
 RELDT 201410,017,2014-10-28  ST. LOUIS                                         
 RELDT 201410,087,2014-10-28  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201410,015,2014-10-28  WASHINGTON, DC                                    
*                                                                               
 RELDT 201410,093,2014-10-29  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201410,031,2014-10-29  CINCINNATI                                        
 RELDT 201410,019,2014-10-29  CLEVELAND                                         
 RELDT 201410,045,2014-10-29  COLUMBUS, OH                                      
 RELDT 201410,041,2014-10-29  KANSAS CITY                                       
 RELDT 201410,257,2014-10-29  LAS VEGAS                                         
 RELDT 201410,131,2014-10-29  ORLANDO                                           
 RELDT 201410,023,2014-10-29  PITTSBURGH, PA                                    
 RELDT 201410,051,2014-10-29  PORTLAND, OR                                      
 RELDT 201410,065,2014-10-29  SACRAMENTO                                        
 RELDT 201410,101,2014-10-29  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201410,059,2014-10-29  SAN ANTONIO                                       
*                                                                               
 RELDT 201410,135,2014-10-30  AUSTIN                                            
 RELDT 201410,166,2014-10-30  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201410,061,2014-10-30  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201410,049,2014-10-30  INDIANAPOLIS                                      
 RELDT 201410,107,2014-10-30  JACKSONVILLE                                      
 RELDT 201410,075,2014-10-30  MEMPHIS                                           
 RELDT 201410,043,2014-10-30  MILWAUKEE-RACINE                                  
 RELDT 201410,073,2014-10-30  NASHVILLE                                         
 RELDT 201410,109,2014-10-30  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201410,077,2014-10-30  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201410,115,2014-10-30  RALEIGH-DURHAM                                    
 RELDT 201410,299,2014-10-30  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201411,047,2014-11-24  ATLANTA                                           
 RELDT 201411,005,2014-11-24  CHICAGO                                           
 RELDT 201411,024,2014-11-24  DALLAS-FT. WORTH                                  
 RELDT 201411,033,2014-11-24  HOUSTON-GALVESTON                                 
 RELDT 201411,003,2014-11-24  LOS ANGELES                                       
 RELDT 201411,413,2014-11-24  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201411,321,2014-11-24  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201411,001,2014-11-24  NEW YORK                                          
 RELDT 201411,007,2014-11-24  PHILADELPHIA                                      
 RELDT 201411,379,2014-11-24  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201411,009,2014-11-24  SAN FRANCISCO                                     
 RELDT 201411,215,2014-11-24  SAN JOSE                                          
*                                                                               
 RELDT 201411,021,2014-11-25  BALTIMORE                                         
 RELDT 201411,013,2014-11-25  BOSTON                                            
 RELDT 201411,035,2014-11-25  DENVER-BOULDER                                    
 RELDT 201411,011,2014-11-25  DETROIT                                           
 RELDT 201411,429,2014-11-25  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201411,027,2014-11-25  MINNEAPOLIS-ST. PAUL                              
 RELDT 201411,057,2014-11-25  PHOENIX                                           
 RELDT 201411,063,2014-11-25  SAN DIEGO                                         
 RELDT 201411,039,2014-11-25  SEATTLE-TACOMA                                    
 RELDT 201411,017,2014-11-25  ST. LOUIS                                         
 RELDT 201411,087,2014-11-25  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201411,015,2014-11-25  WASHINGTON, DC                                    
*                                                                               
 RELDT 201411,093,2014-11-26  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201411,031,2014-11-26  CINCINNATI                                        
 RELDT 201411,019,2014-11-26  CLEVELAND                                         
 RELDT 201411,045,2014-11-26  COLUMBUS, OH                                      
 RELDT 201411,041,2014-11-26  KANSAS CITY                                       
 RELDT 201411,257,2014-11-26  LAS VEGAS                                         
 RELDT 201411,131,2014-11-26  ORLANDO                                           
 RELDT 201411,023,2014-11-26  PITTSBURGH, PA                                    
 RELDT 201411,051,2014-11-26  PORTLAND, OR                                      
 RELDT 201411,065,2014-11-26  SACRAMENTO                                        
 RELDT 201411,101,2014-11-26  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201411,059,2014-11-26  SAN ANTONIO                                       
*                                                                               
 RELDT 201411,135,2014-12-01  AUSTIN                                            
 RELDT 201411,166,2014-12-01  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201411,061,2014-12-01  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201411,049,2014-12-01  INDIANAPOLIS                                      
 RELDT 201411,107,2014-12-01  JACKSONVILLE                                      
 RELDT 201411,075,2014-12-01  MEMPHIS                                           
 RELDT 201411,043,2014-12-01  MILWAUKEE-RACINE                                  
 RELDT 201411,073,2014-12-01  NASHVILLE                                         
 RELDT 201411,109,2014-12-01  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201411,077,2014-12-01  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201411,115,2014-12-01  RALEIGH-DURHAM                                    
 RELDT 201411,299,2014-12-01  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201412,047,2014-12-22  ATLANTA                                           
 RELDT 201412,005,2014-12-22  CHICAGO                                           
 RELDT 201412,024,2014-12-22  DALLAS-FT. WORTH                                  
 RELDT 201412,033,2014-12-22  HOUSTON-GALVESTON                                 
 RELDT 201412,003,2014-12-22  LOS ANGELES                                       
 RELDT 201412,413,2014-12-22  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201412,321,2014-12-22  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201412,001,2014-12-22  NEW YORK                                          
 RELDT 201412,007,2014-12-22  PHILADELPHIA                                      
 RELDT 201412,379,2014-12-22  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201412,009,2014-12-22  SAN FRANCISCO                                     
 RELDT 201412,215,2014-12-22  SAN JOSE                                          
*                                                                               
 RELDT 201412,021,2014-12-23  BALTIMORE                                         
 RELDT 201412,013,2014-12-23  BOSTON                                            
 RELDT 201412,035,2014-12-23  DENVER-BOULDER                                    
 RELDT 201412,011,2014-12-23  DETROIT                                           
 RELDT 201412,429,2014-12-23  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201412,027,2014-12-23  MINNEAPOLIS-ST. PAUL                              
 RELDT 201412,057,2014-12-23  PHOENIX                                           
 RELDT 201412,063,2014-12-23  SAN DIEGO                                         
 RELDT 201412,039,2014-12-23  SEATTLE-TACOMA                                    
 RELDT 201412,017,2014-12-23  ST. LOUIS                                         
 RELDT 201412,087,2014-12-23  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201412,015,2014-12-23  WASHINGTON, DC                                    
*                                                                               
 RELDT 201412,093,2014-12-26  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201412,031,2014-12-26  CINCINNATI                                        
 RELDT 201412,019,2014-12-26  CLEVELAND                                         
 RELDT 201412,045,2014-12-26  COLUMBUS, OH                                      
 RELDT 201412,041,2014-12-26  KANSAS CITY                                       
 RELDT 201412,257,2014-12-26  LAS VEGAS                                         
 RELDT 201412,131,2014-12-26  ORLANDO                                           
 RELDT 201412,023,2014-12-26  PITTSBURGH, PA                                    
 RELDT 201412,051,2014-12-26  PORTLAND, OR                                      
 RELDT 201412,065,2014-12-26  SACRAMENTO                                        
 RELDT 201412,101,2014-12-26  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201412,059,2014-12-26  SAN ANTONIO                                       
*                                                                               
 RELDT 201412,135,2014-12-29  AUSTIN                                            
 RELDT 201412,166,2014-12-29  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201412,061,2014-12-29  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201412,049,2014-12-29  INDIANAPOLIS                                      
 RELDT 201412,107,2014-12-29  JACKSONVILLE                                      
 RELDT 201412,075,2014-12-29  MEMPHIS                                           
 RELDT 201412,043,2014-12-29  MILWAUKEE-RACINE                                  
 RELDT 201412,073,2014-12-29  NASHVILLE                                         
 RELDT 201412,109,2014-12-29  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201412,077,2014-12-29  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201412,115,2014-12-29  RALEIGH-DURHAM                                    
 RELDT 201412,299,2014-12-29  WEST PALM BEACH-BOCA RATON                        
*                                                                               
* HOLIDAY BOOK                                                                  
*                                                                               
 RELDT 201423,047,2015-01-20  ATLANTA                                           
 RELDT 201423,005,2015-01-20  CHICAGO                                           
 RELDT 201423,024,2015-01-20  DALLAS-FT. WORTH                                  
 RELDT 201423,033,2015-01-20  HOUSTON-GALVESTON                                 
 RELDT 201423,003,2015-01-20  LOS ANGELES                                       
 RELDT 201423,413,2015-01-20  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201423,321,2015-01-20  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201423,001,2015-01-20  NEW YORK                                          
 RELDT 201423,007,2015-01-20  PHILADELPHIA                                      
 RELDT 201423,379,2015-01-20  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201423,009,2015-01-20  SAN FRANCISCO                                     
 RELDT 201423,215,2015-01-20  SAN JOSE                                          
*                                                                               
 RELDT 201423,021,2015-01-21  BALTIMORE                                         
 RELDT 201423,013,2015-01-21  BOSTON                                            
 RELDT 201423,035,2015-01-21  DENVER-BOULDER                                    
 RELDT 201423,011,2015-01-21  DETROIT                                           
 RELDT 201423,429,2015-01-21  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201423,027,2015-01-21  MINNEAPOLIS-ST. PAUL                              
 RELDT 201423,057,2015-01-21  PHOENIX                                           
 RELDT 201423,063,2015-01-21  SAN DIEGO                                         
 RELDT 201423,039,2015-01-21  SEATTLE-TACOMA                                    
 RELDT 201423,017,2015-01-21  ST. LOUIS                                         
 RELDT 201423,087,2015-01-21  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201423,015,2015-01-21  WASHINGTON, DC                                    
*                                                                               
 RELDT 201423,093,2015-01-22  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201423,031,2015-01-22  CINCINNATI                                        
 RELDT 201423,019,2015-01-22  CLEVELAND                                         
 RELDT 201423,045,2015-01-22  COLUMBUS, OH                                      
 RELDT 201423,041,2015-01-22  KANSAS CITY                                       
 RELDT 201423,257,2015-01-22  LAS VEGAS                                         
 RELDT 201423,131,2015-01-22  ORLANDO                                           
 RELDT 201423,023,2015-01-22  PITTSBURGH, PA                                    
 RELDT 201423,051,2015-01-22  PORTLAND, OR                                      
 RELDT 201423,065,2015-01-22  SACRAMENTO                                        
 RELDT 201423,101,2015-01-22  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201423,059,2015-01-22  SAN ANTONIO                                       
*                                                                               
 RELDT 201423,135,2015-01-23  AUSTIN                                            
 RELDT 201423,166,2015-01-23  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201423,061,2015-01-23  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201423,049,2015-01-23  INDIANAPOLIS                                      
 RELDT 201423,107,2015-01-23  JACKSONVILLE                                      
 RELDT 201423,075,2015-01-23  MEMPHIS                                           
 RELDT 201423,043,2015-01-23  MILWAUKEE-RACINE                                  
 RELDT 201423,073,2015-01-23  NASHVILLE                                         
 RELDT 201423,109,2015-01-23  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201423,077,2015-01-23  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201423,115,2015-01-23  RALEIGH-DURHAM                                    
 RELDT 201423,299,2015-01-23  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
* PPM 2015 MARKETS                                                              
*                                                                               
 RELDT 201501,047,2015-02-17  ATLANTA                                           
 RELDT 201501,005,2015-02-17  CHICAGO                                           
 RELDT 201501,024,2015-02-17  DALLAS-FT. WORTH                                  
 RELDT 201501,033,2015-02-17  HOUSTON-GALVESTON                                 
 RELDT 201501,003,2015-02-17  LOS ANGELES                                       
 RELDT 201501,413,2015-02-17  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201501,321,2015-02-17  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201501,001,2015-02-17  NEW YORK                                          
 RELDT 201501,007,2015-02-17  PHILADELPHIA                                      
 RELDT 201501,379,2015-02-17  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201501,009,2015-02-17  SAN FRANCISCO                                     
 RELDT 201501,215,2015-02-17  SAN JOSE                                          
*                                                                               
 RELDT 201501,021,2015-02-18  BALTIMORE                                         
 RELDT 201501,013,2015-02-18  BOSTON                                            
 RELDT 201501,035,2015-02-18  DENVER-BOULDER                                    
 RELDT 201501,011,2015-02-18  DETROIT                                           
 RELDT 201501,429,2015-02-18  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201501,027,2015-02-18  MINNEAPOLIS-ST. PAUL                              
 RELDT 201501,057,2015-02-18  PHOENIX                                           
 RELDT 201501,063,2015-02-18  SAN DIEGO                                         
 RELDT 201501,039,2015-02-18  SEATTLE-TACOMA                                    
 RELDT 201501,017,2015-02-18  ST. LOUIS                                         
 RELDT 201501,087,2015-02-18  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201501,015,2015-02-18  WASHINGTON, DC                                    
*                                                                               
 RELDT 201501,093,2015-02-19  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201501,031,2015-02-19  CINCINNATI                                        
 RELDT 201501,019,2015-02-19  CLEVELAND                                         
 RELDT 201501,045,2015-02-19  COLUMBUS, OH                                      
 RELDT 201501,041,2015-02-19  KANSAS CITY                                       
 RELDT 201501,257,2015-02-19  LAS VEGAS                                         
 RELDT 201501,131,2015-02-19  ORLANDO                                           
 RELDT 201501,023,2015-02-19  PITTSBURGH, PA                                    
 RELDT 201501,051,2015-02-19  PORTLAND, OR                                      
 RELDT 201501,065,2015-02-19  SACRAMENTO                                        
 RELDT 201501,101,2015-02-19  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201501,059,2015-02-19  SAN ANTONIO                                       
*                                                                               
 RELDT 201501,135,2015-02-20  AUSTIN                                            
 RELDT 201501,166,2015-02-20  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201501,061,2015-02-20  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201501,049,2015-02-20  INDIANAPOLIS                                      
 RELDT 201501,107,2015-02-20  JACKSONVILLE                                      
 RELDT 201501,075,2015-02-20  MEMPHIS                                           
 RELDT 201501,043,2015-02-20  MILWAUKEE-RACINE                                  
 RELDT 201501,073,2015-02-20  NASHVILLE                                         
 RELDT 201501,109,2015-02-20  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201501,077,2015-02-20  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201501,115,2015-02-20  RALEIGH-DURHAM                                    
 RELDT 201501,299,2015-02-20  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201502,047,2015-03-16  ATLANTA                                           
 RELDT 201502,005,2015-03-16  CHICAGO                                           
 RELDT 201502,024,2015-03-16  DALLAS-FT. WORTH                                  
 RELDT 201502,033,2015-03-16  HOUSTON-GALVESTON                                 
 RELDT 201502,003,2015-03-16  LOS ANGELES                                       
 RELDT 201502,413,2015-03-16  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201502,321,2015-03-16  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201502,001,2015-03-16  NEW YORK                                          
 RELDT 201502,007,2015-03-16  PHILADELPHIA                                      
 RELDT 201502,379,2015-03-16  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201502,009,2015-03-16  SAN FRANCISCO                                     
 RELDT 201502,215,2015-03-16  SAN JOSE                                          
*                                                                               
 RELDT 201502,021,2015-03-17  BALTIMORE                                         
 RELDT 201502,013,2015-03-17  BOSTON                                            
 RELDT 201502,035,2015-03-17  DENVER-BOULDER                                    
 RELDT 201502,011,2015-03-17  DETROIT                                           
 RELDT 201502,429,2015-03-17  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201502,027,2015-03-17  MINNEAPOLIS-ST. PAUL                              
 RELDT 201502,057,2015-03-17  PHOENIX                                           
 RELDT 201502,063,2015-03-17  SAN DIEGO                                         
 RELDT 201502,039,2015-03-17  SEATTLE-TACOMA                                    
 RELDT 201502,017,2015-03-17  ST. LOUIS                                         
 RELDT 201502,087,2015-03-17  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201502,015,2015-03-17  WASHINGTON, DC                                    
*                                                                               
 RELDT 201502,093,2015-03-18  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201502,031,2015-03-18  CINCINNATI                                        
 RELDT 201502,019,2015-03-18  CLEVELAND                                         
 RELDT 201502,045,2015-03-18  COLUMBUS, OH                                      
 RELDT 201502,041,2015-03-18  KANSAS CITY                                       
 RELDT 201502,257,2015-03-18  LAS VEGAS                                         
 RELDT 201502,131,2015-03-18  ORLANDO                                           
 RELDT 201502,023,2015-03-18  PITTSBURGH, PA                                    
 RELDT 201502,051,2015-03-18  PORTLAND, OR                                      
 RELDT 201502,065,2015-03-18  SACRAMENTO                                        
 RELDT 201502,101,2015-03-18  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201502,059,2015-03-18  SAN ANTONIO                                       
*                                                                               
 RELDT 201502,135,2015-03-19  AUSTIN                                            
 RELDT 201502,166,2015-03-19  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201502,061,2015-03-19  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201502,049,2015-03-19  INDIANAPOLIS                                      
 RELDT 201502,107,2015-03-19  JACKSONVILLE                                      
 RELDT 201502,075,2015-03-19  MEMPHIS                                           
 RELDT 201502,043,2015-03-19  MILWAUKEE-RACINE                                  
 RELDT 201502,073,2015-03-19  NASHVILLE                                         
 RELDT 201502,109,2015-03-19  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201502,077,2015-03-19  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201502,115,2015-03-19  RALEIGH-DURHAM                                    
 RELDT 201502,299,2015-03-19  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201503,047,2015-04-13  ATLANTA                                           
 RELDT 201503,005,2015-04-13  CHICAGO                                           
 RELDT 201503,024,2015-04-13  DALLAS-FT. WORTH                                  
 RELDT 201503,033,2015-04-13  HOUSTON-GALVESTON                                 
 RELDT 201503,003,2015-04-13  LOS ANGELES                                       
 RELDT 201503,413,2015-04-13  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201503,321,2015-04-13  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201503,001,2015-04-13  NEW YORK                                          
 RELDT 201503,007,2015-04-13  PHILADELPHIA                                      
 RELDT 201503,379,2015-04-13  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201503,009,2015-04-13  SAN FRANCISCO                                     
 RELDT 201503,215,2015-04-13  SAN JOSE                                          
*                                                                               
 RELDT 201503,021,2015-04-14  BALTIMORE                                         
 RELDT 201503,013,2015-04-14  BOSTON                                            
 RELDT 201503,035,2015-04-14  DENVER-BOULDER                                    
 RELDT 201503,011,2015-04-14  DETROIT                                           
 RELDT 201503,429,2015-04-14  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201503,027,2015-04-14  MINNEAPOLIS-ST. PAUL                              
 RELDT 201503,057,2015-04-14  PHOENIX                                           
 RELDT 201503,063,2015-04-14  SAN DIEGO                                         
 RELDT 201503,039,2015-04-14  SEATTLE-TACOMA                                    
 RELDT 201503,017,2015-04-14  ST. LOUIS                                         
 RELDT 201503,087,2015-04-14  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201503,015,2015-04-14  WASHINGTON, DC                                    
*                                                                               
 RELDT 201503,093,2015-04-15  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201503,031,2015-04-15  CINCINNATI                                        
 RELDT 201503,019,2015-04-15  CLEVELAND                                         
 RELDT 201503,045,2015-04-15  COLUMBUS, OH                                      
 RELDT 201503,041,2015-04-15  KANSAS CITY                                       
 RELDT 201503,257,2015-04-15  LAS VEGAS                                         
 RELDT 201503,131,2015-04-15  ORLANDO                                           
 RELDT 201503,023,2015-04-15  PITTSBURGH, PA                                    
 RELDT 201503,051,2015-04-15  PORTLAND, OR                                      
 RELDT 201503,065,2015-04-15  SACRAMENTO                                        
 RELDT 201503,101,2015-04-15  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201503,059,2015-04-15  SAN ANTONIO                                       
*                                                                               
 RELDT 201503,135,2015-04-16  AUSTIN                                            
 RELDT 201503,166,2015-04-16  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201503,061,2015-04-16  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201503,049,2015-04-16  INDIANAPOLIS                                      
 RELDT 201503,107,2015-04-16  JACKSONVILLE                                      
 RELDT 201503,075,2015-04-16  MEMPHIS                                           
 RELDT 201503,043,2015-04-16  MILWAUKEE-RACINE                                  
 RELDT 201503,073,2015-04-16  NASHVILLE                                         
 RELDT 201503,109,2015-04-16  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201503,077,2015-04-16  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201503,115,2015-04-16  RALEIGH-DURHAM                                    
 RELDT 201503,299,2015-04-16  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201504,047,2015-05-11  ATLANTA                                           
 RELDT 201504,005,2015-05-11  CHICAGO                                           
 RELDT 201504,024,2015-05-11  DALLAS-FT. WORTH                                  
 RELDT 201504,033,2015-05-11  HOUSTON-GALVESTON                                 
 RELDT 201504,003,2015-05-11  LOS ANGELES                                       
 RELDT 201504,413,2015-05-11  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201504,321,2015-05-11  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201504,001,2015-05-11  NEW YORK                                          
 RELDT 201504,007,2015-05-11  PHILADELPHIA                                      
 RELDT 201504,379,2015-05-11  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201504,009,2015-05-11  SAN FRANCISCO                                     
 RELDT 201504,215,2015-05-11  SAN JOSE                                          
*                                                                               
 RELDT 201504,021,2015-05-12  BALTIMORE                                         
 RELDT 201504,013,2015-05-12  BOSTON                                            
 RELDT 201504,035,2015-05-12  DENVER-BOULDER                                    
 RELDT 201504,011,2015-05-12  DETROIT                                           
 RELDT 201504,429,2015-05-12  MIAMI-FT. LAUDERDALE-HOLLYWOOD                    
 RELDT 201504,027,2015-05-12  MINNEAPOLIS-ST. PAUL                              
 RELDT 201504,057,2015-05-12  PHOENIX                                           
 RELDT 201504,063,2015-05-12  SAN DIEGO                                         
 RELDT 201504,039,2015-05-12  SEATTLE-TACOMA                                    
 RELDT 201504,017,2015-05-12  ST. LOUIS                                         
 RELDT 201504,087,2015-05-12  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201504,015,2015-05-12  WASHINGTON, DC                                    
*                                                                               
 RELDT 201504,093,2015-05-13  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201504,031,2015-05-13  CINCINNATI                                        
 RELDT 201504,019,2015-05-13  CLEVELAND                                         
 RELDT 201504,045,2015-05-13  COLUMBUS, OH                                      
 RELDT 201504,041,2015-05-13  KANSAS CITY                                       
 RELDT 201504,257,2015-05-13  LAS VEGAS                                         
 RELDT 201504,131,2015-05-13  ORLANDO                                           
 RELDT 201504,023,2015-05-13  PITTSBURGH, PA                                    
 RELDT 201504,051,2015-05-13  PORTLAND, OR                                      
 RELDT 201504,065,2015-05-13  SACRAMENTO                                        
 RELDT 201504,101,2015-05-13  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201504,059,2015-05-13  SAN ANTONIO                                       
*                                                                               
 RELDT 201504,135,2015-05-14  AUSTIN                                            
 RELDT 201504,166,2015-05-14  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201504,061,2015-05-14  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201504,049,2015-05-14  INDIANAPOLIS                                      
 RELDT 201504,107,2015-05-14  JACKSONVILLE                                      
 RELDT 201504,075,2015-05-14  MEMPHIS                                           
 RELDT 201504,043,2015-05-14  MILWAUKEE-RACINE                                  
 RELDT 201504,073,2015-05-14  NASHVILLE                                         
 RELDT 201504,109,2015-05-14  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201504,077,2015-05-14  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201504,115,2015-05-14  RALEIGH-DURHAM                                    
 RELDT 201504,299,2015-05-14  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201505,047,2015-06-08  ATLANTA                                           
 RELDT 201505,005,2015-06-08  CHICAGO                                           
 RELDT 201505,024,2015-06-08  DALLAS-FT. WORTH                                  
 RELDT 201505,033,2015-06-08  HOUSTON-GALVESTON                                 
 RELDT 201505,003,2015-06-08  LOS ANGELES                                       
 RELDT 201505,413,2015-06-08  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201505,321,2015-06-08  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201505,001,2015-06-08  NEW YORK                                          
 RELDT 201505,007,2015-06-08  PHILADELPHIA                                      
 RELDT 201505,379,2015-06-08  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201505,009,2015-06-08  SAN FRANCISCO                                     
 RELDT 201505,215,2015-06-08  SAN JOSE                                          
*                                                                               
 RELDT 201505,021,2015-06-09  BALTIMORE                                         
 RELDT 201505,013,2015-06-09  BOSTON                                            
 RELDT 201505,035,2015-06-09  DENVER-BOULDER                                    
 RELDT 201505,011,2015-06-09  DETROIT                                           
 RELDT 201505,429,2015-06-09  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201505,027,2015-06-09  MINNEAPOLIS-ST. PAUL                              
 RELDT 201505,057,2015-06-09  PHOENIX                                           
 RELDT 201505,063,2015-06-09  SAN DIEGO                                         
 RELDT 201505,039,2015-06-09  SEATTLE-TACOMA                                    
 RELDT 201505,017,2015-06-09  ST. LOUIS                                         
 RELDT 201505,087,2015-06-09  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201505,015,2015-06-09  WASHINGTON, DC                                    
*                                                                               
 RELDT 201505,093,2015-06-10  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201505,031,2015-06-10  CINCINNATI                                        
 RELDT 201505,019,2015-06-10  CLEVELAND                                         
 RELDT 201505,045,2015-06-10  COLUMBUS, OH                                      
 RELDT 201505,041,2015-06-10  KANSAS CITY                                       
 RELDT 201505,257,2015-06-10  LAS VEGAS                                         
 RELDT 201505,131,2015-06-10  ORLANDO                                           
 RELDT 201505,023,2015-06-10  PITTSBURGH, PA                                    
 RELDT 201505,051,2015-06-10  PORTLAND, OR                                      
 RELDT 201505,065,2015-06-10  SACRAMENTO                                        
 RELDT 201505,101,2015-06-10  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201505,059,2015-06-10  SAN ANTONIO                                       
*                                                                               
 RELDT 201505,135,2015-06-11  AUSTIN                                            
 RELDT 201505,166,2015-06-11  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201505,061,2015-06-11  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201505,049,2015-06-11  INDIANAPOLIS                                      
 RELDT 201505,107,2015-06-11  JACKSONVILLE                                      
 RELDT 201505,075,2015-06-11  MEMPHIS                                           
 RELDT 201505,043,2015-06-11  MILWAUKEE-RACINE                                  
 RELDT 201505,073,2015-06-11  NASHVILLE                                         
 RELDT 201505,109,2015-06-11  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201505,077,2015-06-11  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201505,115,2015-06-11  RALEIGH-DURHAM                                    
 RELDT 201505,299,2015-06-11  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201506,047,2015-07-07  ATLANTA                                           
 RELDT 201506,005,2015-07-07  CHICAGO                                           
 RELDT 201506,024,2015-07-07  DALLAS-FT. WORTH                                  
 RELDT 201506,033,2015-07-07  HOUSTON-GALVESTON                                 
 RELDT 201506,003,2015-07-07  LOS ANGELES                                       
 RELDT 201506,413,2015-07-07  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201506,321,2015-07-07  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201506,001,2015-07-07  NEW YORK                                          
 RELDT 201506,007,2015-07-07  PHILADELPHIA                                      
 RELDT 201506,379,2015-07-07  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201506,009,2015-07-07  SAN FRANCISCO                                     
 RELDT 201506,215,2015-07-07  SAN JOSE                                          
*                                                                               
 RELDT 201506,021,2015-07-08  BALTIMORE                                         
 RELDT 201506,013,2015-07-08  BOSTON                                            
 RELDT 201506,035,2015-07-08  DENVER-BOULDER                                    
 RELDT 201506,011,2015-07-08  DETROIT                                           
 RELDT 201506,429,2015-07-08  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201506,027,2015-07-08  MINNEAPOLIS-ST. PAUL                              
 RELDT 201506,057,2015-07-08  PHOENIX                                           
 RELDT 201506,063,2015-07-08  SAN DIEGO                                         
 RELDT 201506,039,2015-07-08  SEATTLE-TACOMA                                    
 RELDT 201506,017,2015-07-08  ST. LOUIS                                         
 RELDT 201506,087,2015-07-08  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201506,015,2015-07-08  WASHINGTON, DC                                    
*                                                                               
 RELDT 201506,093,2015-07-09  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201506,031,2015-07-09  CINCINNATI                                        
 RELDT 201506,019,2015-07-09  CLEVELAND                                         
 RELDT 201506,045,2015-07-09  COLUMBUS, OH                                      
 RELDT 201506,041,2015-07-09  KANSAS CITY                                       
 RELDT 201506,257,2015-07-09  LAS VEGAS                                         
 RELDT 201506,131,2015-07-09  ORLANDO                                           
 RELDT 201506,023,2015-07-09  PITTSBURGH, PA                                    
 RELDT 201506,051,2015-07-09  PORTLAND, OR                                      
 RELDT 201506,065,2015-07-09  SACRAMENTO                                        
 RELDT 201506,101,2015-07-09  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201506,059,2015-07-09  SAN ANTONIO                                       
*                                                                               
 RELDT 201506,135,2015-07-10  AUSTIN                                            
 RELDT 201506,166,2015-07-10  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201506,061,2015-07-10  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201506,049,2015-07-10  INDIANAPOLIS                                      
 RELDT 201506,107,2015-07-10  JACKSONVILLE                                      
 RELDT 201506,075,2015-07-10  MEMPHIS                                           
 RELDT 201506,043,2015-07-10  MILWAUKEE-RACINE                                  
 RELDT 201506,073,2015-07-10  NASHVILLE                                         
 RELDT 201506,109,2015-07-10  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201506,077,2015-07-10  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201506,115,2015-07-10  RALEIGH-DURHAM                                    
 RELDT 201506,299,2015-07-10  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201507,047,2015-08-03  ATLANTA                                           
 RELDT 201507,005,2015-08-03  CHICAGO                                           
 RELDT 201507,024,2015-08-03  DALLAS-FT. WORTH                                  
 RELDT 201507,033,2015-08-03  HOUSTON-GALVESTON                                 
 RELDT 201507,003,2015-08-03  LOS ANGELES                                       
 RELDT 201507,413,2015-08-03  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201507,321,2015-08-03  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201507,001,2015-08-03  NEW YORK                                          
 RELDT 201507,007,2015-08-03  PHILADELPHIA                                      
 RELDT 201507,379,2015-08-03  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201507,009,2015-08-03  SAN FRANCISCO                                     
 RELDT 201507,215,2015-08-03  SAN JOSE                                          
*                                                                               
 RELDT 201507,021,2015-08-04  BALTIMORE                                         
 RELDT 201507,013,2015-08-04  BOSTON                                            
 RELDT 201507,035,2015-08-04  DENVER-BOULDER                                    
 RELDT 201507,011,2015-08-04  DETROIT                                           
 RELDT 201507,429,2015-08-04  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201507,027,2015-08-04  MINNEAPOLIS-ST. PAUL                              
 RELDT 201507,057,2015-08-04  PHOENIX                                           
 RELDT 201507,063,2015-08-04  SAN DIEGO                                         
 RELDT 201507,039,2015-08-04  SEATTLE-TACOMA                                    
 RELDT 201507,017,2015-08-04  ST. LOUIS                                         
 RELDT 201507,087,2015-08-04  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201507,015,2015-08-04  WASHINGTON, DC                                    
*                                                                               
 RELDT 201507,093,2015-08-05  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201507,031,2015-08-05  CINCINNATI                                        
 RELDT 201507,019,2015-08-05  CLEVELAND                                         
 RELDT 201507,045,2015-08-05  COLUMBUS, OH                                      
 RELDT 201507,041,2015-08-05  KANSAS CITY                                       
 RELDT 201507,257,2015-08-05  LAS VEGAS                                         
 RELDT 201507,131,2015-08-05  ORLANDO                                           
 RELDT 201507,023,2015-08-05  PITTSBURGH, PA                                    
 RELDT 201507,051,2015-08-05  PORTLAND, OR                                      
 RELDT 201507,065,2015-08-05  SACRAMENTO                                        
 RELDT 201507,101,2015-08-05  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201507,059,2015-08-05  SAN ANTONIO                                       
*                                                                               
 RELDT 201507,135,2015-08-06  AUSTIN                                            
 RELDT 201507,166,2015-08-06  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201507,061,2015-08-06  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201507,049,2015-08-06  INDIANAPOLIS                                      
 RELDT 201507,107,2015-08-06  JACKSONVILLE                                      
 RELDT 201507,075,2015-08-06  MEMPHIS                                           
 RELDT 201507,043,2015-08-06  MILWAUKEE-RACINE                                  
 RELDT 201507,073,2015-08-06  NASHVILLE                                         
 RELDT 201507,109,2015-08-06  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201507,077,2015-08-06  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201507,115,2015-08-06  RALEIGH-DURHAM                                    
 RELDT 201507,299,2015-08-06  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201508,047,2015-08-31  ATLANTA                                           
 RELDT 201508,005,2015-08-31  CHICAGO                                           
 RELDT 201508,024,2015-08-31  DALLAS-FT. WORTH                                  
 RELDT 201508,033,2015-08-31  HOUSTON-GALVESTON                                 
 RELDT 201508,003,2015-08-31  LOS ANGELES                                       
 RELDT 201508,413,2015-08-31  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201508,321,2015-08-31  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201508,001,2015-08-31  NEW YORK                                          
 RELDT 201508,007,2015-08-31  PHILADELPHIA                                      
 RELDT 201508,379,2015-08-31  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201508,009,2015-08-31  SAN FRANCISCO                                     
 RELDT 201508,215,2015-08-31  SAN JOSE                                          
*                                                                               
 RELDT 201508,021,2015-09-01  BALTIMORE                                         
 RELDT 201508,013,2015-09-01  BOSTON                                            
 RELDT 201508,035,2015-09-01  DENVER-BOULDER                                    
 RELDT 201508,011,2015-09-01  DETROIT                                           
 RELDT 201508,429,2015-09-01  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201508,027,2015-09-01  MINNEAPOLIS-ST. PAUL                              
 RELDT 201508,057,2015-09-01  PHOENIX                                           
 RELDT 201508,063,2015-09-01  SAN DIEGO                                         
 RELDT 201508,039,2015-09-01  SEATTLE-TACOMA                                    
 RELDT 201508,017,2015-09-01  ST. LOUIS                                         
 RELDT 201508,087,2015-09-01  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201508,015,2015-09-01  WASHINGTON, DC                                    
*                                                                               
 RELDT 201508,093,2015-09-02  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201508,031,2015-09-02  CINCINNATI                                        
 RELDT 201508,019,2015-09-02  CLEVELAND                                         
 RELDT 201508,045,2015-09-02  COLUMBUS, OH                                      
 RELDT 201508,041,2015-09-02  KANSAS CITY                                       
 RELDT 201508,257,2015-09-02  LAS VEGAS                                         
 RELDT 201508,131,2015-09-02  ORLANDO                                           
 RELDT 201508,023,2015-09-02  PITTSBURGH, PA                                    
 RELDT 201508,051,2015-09-02  PORTLAND, OR                                      
 RELDT 201508,065,2015-09-02  SACRAMENTO                                        
 RELDT 201508,101,2015-09-02  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201508,059,2015-09-02  SAN ANTONIO                                       
*                                                                               
 RELDT 201508,135,2015-09-03  AUSTIN                                            
 RELDT 201508,166,2015-09-03  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201508,061,2015-09-03  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201508,049,2015-09-03  INDIANAPOLIS                                      
 RELDT 201508,107,2015-09-03  JACKSONVILLE                                      
 RELDT 201508,075,2015-09-03  MEMPHIS                                           
 RELDT 201508,043,2015-09-03  MILWAUKEE-RACINE                                  
 RELDT 201508,073,2015-09-03  NASHVILLE                                         
 RELDT 201508,109,2015-09-03  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201508,077,2015-09-03  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201508,115,2015-09-03  RALEIGH-DURHAM                                    
 RELDT 201508,299,2015-09-03  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201509,047,2015-09-29  ATLANTA                                           
 RELDT 201509,005,2015-09-29  CHICAGO                                           
 RELDT 201509,024,2015-09-29  DALLAS-FT. WORTH                                  
 RELDT 201509,033,2015-09-29  HOUSTON-GALVESTON                                 
 RELDT 201509,003,2015-09-29  LOS ANGELES                                       
 RELDT 201509,413,2015-09-29  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201509,321,2015-09-29  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201509,001,2015-09-29  NEW YORK                                          
 RELDT 201509,007,2015-09-29  PHILADELPHIA                                      
 RELDT 201509,379,2015-09-29  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201509,009,2015-09-29  SAN FRANCISCO                                     
 RELDT 201509,215,2015-09-29  SAN JOSE                                          
 RELDT 201509,021,2015-09-29  BALTIMORE                                         
 RELDT 201509,013,2015-09-29  BOSTON                                            
 RELDT 201509,035,2015-09-29  DENVER-BOULDER                                    
 RELDT 201509,011,2015-09-29  DETROIT                                           
 RELDT 201509,429,2015-09-29  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201509,027,2015-09-29  MINNEAPOLIS-ST. PAUL                              
 RELDT 201509,057,2015-09-29  PHOENIX                                           
 RELDT 201509,063,2015-09-29  SAN DIEGO                                         
 RELDT 201509,039,2015-09-29  SEATTLE-TACOMA                                    
 RELDT 201509,017,2015-09-29  ST. LOUIS                                         
 RELDT 201509,087,2015-09-29  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201509,015,2015-09-29  WASHINGTON, DC                                    
*                                                                               
 RELDT 201509,093,2015-09-30  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201509,031,2015-09-30  CINCINNATI                                        
 RELDT 201509,019,2015-09-30  CLEVELAND                                         
 RELDT 201509,045,2015-09-30  COLUMBUS, OH                                      
 RELDT 201509,041,2015-09-30  KANSAS CITY                                       
 RELDT 201509,257,2015-09-30  LAS VEGAS                                         
 RELDT 201509,131,2015-09-30  ORLANDO                                           
 RELDT 201509,023,2015-09-30  PITTSBURGH, PA                                    
 RELDT 201509,051,2015-09-30  PORTLAND, OR                                      
 RELDT 201509,065,2015-09-30  SACRAMENTO                                        
 RELDT 201509,101,2015-09-30  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201509,059,2015-09-30  SAN ANTONIO                                       
 RELDT 201509,135,2015-09-30  AUSTIN                                            
 RELDT 201509,166,2015-09-30  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201509,061,2015-09-30  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201509,049,2015-09-30  INDIANAPOLIS                                      
 RELDT 201509,107,2015-09-30  JACKSONVILLE                                      
 RELDT 201509,075,2015-09-30  MEMPHIS                                           
 RELDT 201509,043,2015-09-30  MILWAUKEE-RACINE                                  
 RELDT 201509,073,2015-09-30  NASHVILLE                                         
 RELDT 201509,109,2015-09-30  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201509,077,2015-09-30  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201509,115,2015-09-30  RALEIGH-DURHAM                                    
 RELDT 201509,299,2015-09-30  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201510,047,2015-10-26  ATLANTA                                           
 RELDT 201510,005,2015-10-26  CHICAGO                                           
 RELDT 201510,024,2015-10-26  DALLAS-FT. WORTH                                  
 RELDT 201510,033,2015-10-26  HOUSTON-GALVESTON                                 
 RELDT 201510,003,2015-10-26  LOS ANGELES                                       
 RELDT 201510,413,2015-10-26  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201510,321,2015-10-26  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201510,001,2015-10-26  NEW YORK                                          
 RELDT 201510,007,2015-10-26  PHILADELPHIA                                      
 RELDT 201510,379,2015-10-26  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201510,009,2015-10-26  SAN FRANCISCO                                     
 RELDT 201510,215,2015-10-26  SAN JOSE                                          
*                                                                               
 RELDT 201510,021,2015-10-27  BALTIMORE                                         
 RELDT 201510,013,2015-10-27  BOSTON                                            
 RELDT 201510,035,2015-10-27  DENVER-BOULDER                                    
 RELDT 201510,011,2015-10-27  DETROIT                                           
 RELDT 201510,429,2015-10-27  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201510,027,2015-10-27  MINNEAPOLIS-ST. PAUL                              
 RELDT 201510,057,2015-10-27  PHOENIX                                           
 RELDT 201510,063,2015-10-27  SAN DIEGO                                         
 RELDT 201510,039,2015-10-27  SEATTLE-TACOMA                                    
 RELDT 201510,017,2015-10-27  ST. LOUIS                                         
 RELDT 201510,087,2015-10-27  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201510,015,2015-10-27  WASHINGTON, DC                                    
*                                                                               
 RELDT 201510,093,2015-10-28  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201510,031,2015-10-28  CINCINNATI                                        
 RELDT 201510,019,2015-10-28  CLEVELAND                                         
 RELDT 201510,045,2015-10-28  COLUMBUS, OH                                      
 RELDT 201510,041,2015-10-28  KANSAS CITY                                       
 RELDT 201510,257,2015-10-28  LAS VEGAS                                         
 RELDT 201510,131,2015-10-28  ORLANDO                                           
 RELDT 201510,023,2015-10-28  PITTSBURGH, PA                                    
 RELDT 201510,051,2015-10-28  PORTLAND, OR                                      
 RELDT 201510,065,2015-10-28  SACRAMENTO                                        
 RELDT 201510,101,2015-10-28  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201510,059,2015-10-28  SAN ANTONIO                                       
*                                                                               
 RELDT 201510,135,2015-10-29  AUSTIN                                            
 RELDT 201510,166,2015-10-29  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201510,061,2015-10-29  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201510,049,2015-10-29  INDIANAPOLIS                                      
 RELDT 201510,107,2015-10-29  JACKSONVILLE                                      
 RELDT 201510,075,2015-10-29  MEMPHIS                                           
 RELDT 201510,043,2015-10-29  MILWAUKEE-RACINE                                  
 RELDT 201510,073,2015-10-29  NASHVILLE                                         
 RELDT 201510,109,2015-10-29  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201510,077,2015-10-29  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201510,115,2015-10-29  RALEIGH-DURHAM                                    
 RELDT 201510,299,2015-10-29  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201511,047,2015-11-23  ATLANTA                                           
 RELDT 201511,005,2015-11-23  CHICAGO                                           
 RELDT 201511,024,2015-11-23  DALLAS-FT. WORTH                                  
 RELDT 201511,033,2015-11-23  HOUSTON-GALVESTON                                 
 RELDT 201511,003,2015-11-23  LOS ANGELES                                       
 RELDT 201511,413,2015-11-23  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201511,321,2015-11-23  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201511,001,2015-11-23  NEW YORK                                          
 RELDT 201511,007,2015-11-23  PHILADELPHIA                                      
 RELDT 201511,379,2015-11-23  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201511,009,2015-11-23  SAN FRANCISCO                                     
 RELDT 201511,215,2015-11-23  SAN JOSE                                          
*                                                                               
 RELDT 201511,021,2015-11-24  BALTIMORE                                         
 RELDT 201511,013,2015-11-24  BOSTON                                            
 RELDT 201511,035,2015-11-24  DENVER-BOULDER                                    
 RELDT 201511,011,2015-11-24  DETROIT                                           
 RELDT 201511,429,2015-11-24  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201511,027,2015-11-24  MINNEAPOLIS-ST. PAUL                              
 RELDT 201511,057,2015-11-24  PHOENIX                                           
 RELDT 201511,063,2015-11-24  SAN DIEGO                                         
 RELDT 201511,039,2015-11-24  SEATTLE-TACOMA                                    
 RELDT 201511,017,2015-11-24  ST. LOUIS                                         
 RELDT 201511,087,2015-11-24  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201511,015,2015-11-24  WASHINGTON, DC                                    
*                                                                               
 RELDT 201511,093,2015-11-25  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201511,031,2015-11-25  CINCINNATI                                        
 RELDT 201511,019,2015-11-25  CLEVELAND                                         
 RELDT 201511,045,2015-11-25  COLUMBUS, OH                                      
 RELDT 201511,041,2015-11-25  KANSAS CITY                                       
 RELDT 201511,257,2015-11-25  LAS VEGAS                                         
 RELDT 201511,131,2015-11-25  ORLANDO                                           
 RELDT 201511,023,2015-11-25  PITTSBURGH, PA                                    
 RELDT 201511,051,2015-11-25  PORTLAND, OR                                      
 RELDT 201511,065,2015-11-25  SACRAMENTO                                        
 RELDT 201511,101,2015-11-25  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201511,059,2015-11-25  SAN ANTONIO                                       
*                                                                               
 RELDT 201511,135,2015-11-30  AUSTIN                                            
 RELDT 201511,166,2015-11-30  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201511,061,2015-11-30  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201511,049,2015-11-30  INDIANAPOLIS                                      
 RELDT 201511,107,2015-11-30  JACKSONVILLE                                      
 RELDT 201511,075,2015-11-30  MEMPHIS                                           
 RELDT 201511,043,2015-11-30  MILWAUKEE-RACINE                                  
 RELDT 201511,073,2015-11-30  NASHVILLE                                         
 RELDT 201511,109,2015-11-30  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201511,077,2015-11-30  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201511,115,2015-11-30  RALEIGH-DURHAM                                    
 RELDT 201511,299,2015-11-30  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201512,047,2015-12-21  ATLANTA                                           
 RELDT 201512,005,2015-12-21  CHICAGO                                           
 RELDT 201512,024,2015-12-21  DALLAS-FT. WORTH                                  
 RELDT 201512,033,2015-12-21  HOUSTON-GALVESTON                                 
 RELDT 201512,003,2015-12-21  LOS ANGELES                                       
 RELDT 201512,413,2015-12-21  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201512,321,2015-12-21  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201512,001,2015-12-21  NEW YORK                                          
 RELDT 201512,007,2015-12-21  PHILADELPHIA                                      
 RELDT 201512,379,2015-12-21  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201512,009,2015-12-21  SAN FRANCISCO                                     
 RELDT 201512,215,2015-12-21  SAN JOSE                                          
*                                                                               
 RELDT 201512,021,2015-12-22  BALTIMORE                                         
 RELDT 201512,013,2015-12-22  BOSTON                                            
 RELDT 201512,035,2015-12-22  DENVER-BOULDER                                    
 RELDT 201512,011,2015-12-22  DETROIT                                           
 RELDT 201512,429,2015-12-22  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201512,027,2015-12-22  MINNEAPOLIS-ST. PAUL                              
 RELDT 201512,057,2015-12-22  PHOENIX                                           
 RELDT 201512,063,2015-12-22  SAN DIEGO                                         
 RELDT 201512,039,2015-12-22  SEATTLE-TACOMA                                    
 RELDT 201512,017,2015-12-22  ST. LOUIS                                         
 RELDT 201512,087,2015-12-22  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201512,015,2015-12-22  WASHINGTON, DC                                    
*                                                                               
 RELDT 201512,093,2015-12-23  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201512,031,2015-12-23  CINCINNATI                                        
 RELDT 201512,019,2015-12-23  CLEVELAND                                         
 RELDT 201512,045,2015-12-23  COLUMBUS, OH                                      
 RELDT 201512,041,2015-12-23  KANSAS CITY                                       
 RELDT 201512,257,2015-12-23  LAS VEGAS                                         
 RELDT 201512,131,2015-12-23  ORLANDO                                           
 RELDT 201512,023,2015-12-23  PITTSBURGH, PA                                    
 RELDT 201512,051,2015-12-23  PORTLAND, OR                                      
 RELDT 201512,065,2015-12-23  SACRAMENTO                                        
 RELDT 201512,101,2015-12-23  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201512,059,2015-12-23  SAN ANTONIO                                       
*                                                                               
 RELDT 201512,135,2015-12-28  AUSTIN                                            
 RELDT 201512,166,2015-12-28  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201512,061,2015-12-28  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201512,049,2015-12-28  INDIANAPOLIS                                      
 RELDT 201512,107,2015-12-28  JACKSONVILLE                                      
 RELDT 201512,075,2015-12-28  MEMPHIS                                           
 RELDT 201512,043,2015-12-28  MILWAUKEE-RACINE                                  
 RELDT 201512,073,2015-12-28  NASHVILLE                                         
 RELDT 201512,109,2015-12-28  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201512,077,2015-12-28  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201512,115,2015-12-28  RALEIGH-DURHAM                                    
 RELDT 201512,299,2015-12-28  WEST PALM BEACH-BOCA RATON                        
*                                                                               
* HOLIDAY BOOK (2015)                                                           
*                                                                               
 RELDT 201523,047,2016-01-19  ATLANTA                                           
 RELDT 201523,005,2016-01-19  CHICAGO                                           
 RELDT 201523,024,2016-01-19  DALLAS-FT. WORTH                                  
 RELDT 201523,033,2016-01-19  HOUSTON-GALVESTON                                 
 RELDT 201523,003,2016-01-19  LOS ANGELES                                       
 RELDT 201523,413,2016-01-19  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201523,321,2016-01-19  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201523,001,2016-01-19  NEW YORK                                          
 RELDT 201523,007,2016-01-19  PHILADELPHIA                                      
 RELDT 201523,379,2016-01-19  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201523,009,2016-01-19  SAN FRANCISCO                                     
 RELDT 201523,215,2016-01-19  SAN JOSE                                          
*                                                                               
 RELDT 201523,021,2016-01-20  BALTIMORE                                         
 RELDT 201523,013,2016-01-20  BOSTON                                            
 RELDT 201523,035,2016-01-20  DENVER-BOULDER                                    
 RELDT 201523,011,2016-01-20  DETROIT                                           
 RELDT 201523,429,2016-01-20  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201523,027,2016-01-20  MINNEAPOLIS-ST. PAUL                              
 RELDT 201523,057,2016-01-20  PHOENIX                                           
 RELDT 201523,063,2016-01-20  SAN DIEGO                                         
 RELDT 201523,039,2016-01-20  SEATTLE-TACOMA                                    
 RELDT 201523,017,2016-01-20  ST. LOUIS                                         
 RELDT 201523,087,2016-01-20  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201523,015,2016-01-20  WASHINGTON, DC                                    
*                                                                               
 RELDT 201523,093,2016-01-21  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201523,031,2016-01-21  CINCINNATI                                        
 RELDT 201523,019,2016-01-21  CLEVELAND                                         
 RELDT 201523,045,2016-01-21  COLUMBUS, OH                                      
 RELDT 201523,041,2016-01-21  KANSAS CITY                                       
 RELDT 201523,257,2016-01-21  LAS VEGAS                                         
 RELDT 201523,131,2016-01-21  ORLANDO                                           
 RELDT 201523,023,2016-01-21  PITTSBURGH, PA                                    
 RELDT 201523,051,2016-01-21  PORTLAND, OR                                      
 RELDT 201523,065,2016-01-21  SACRAMENTO                                        
 RELDT 201523,101,2016-01-21  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201523,059,2016-01-21  SAN ANTONIO                                       
*                                                                               
 RELDT 201523,135,2016-01-22  AUSTIN                                            
 RELDT 201523,166,2016-01-22  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201523,061,2016-01-22  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201523,049,2016-01-22  INDIANAPOLIS                                      
 RELDT 201523,107,2016-01-22  JACKSONVILLE                                      
 RELDT 201523,075,2016-01-22  MEMPHIS                                           
 RELDT 201523,043,2016-01-22  MILWAUKEE-RACINE                                  
 RELDT 201523,073,2016-01-22  NASHVILLE                                         
 RELDT 201523,109,2016-01-22  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201523,077,2016-01-22  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201523,115,2016-01-22  RALEIGH-DURHAM                                    
 RELDT 201523,299,2016-01-22  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
* PPM 2016 MARKETS                                                              
*                                                                               
 RELDT 201601,047,2016-02-16  ATLANTA                                           
 RELDT 201601,005,2016-02-16  CHICAGO                                           
 RELDT 201601,024,2016-02-16  DALLAS-FT. WORTH                                  
 RELDT 201601,033,2016-02-16  HOUSTON-GALVESTON                                 
 RELDT 201601,003,2016-02-16  LOS ANGELES                                       
 RELDT 201601,413,2016-02-16  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201601,321,2016-02-16  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201601,001,2016-02-16  NEW YORK                                          
 RELDT 201601,007,2016-02-16  PHILADELPHIA                                      
 RELDT 201601,379,2016-02-16  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201601,009,2016-02-16  SAN FRANCISCO                                     
 RELDT 201601,215,2016-02-16  SAN JOSE                                          
*                                                                               
 RELDT 201601,021,2016-02-17  BALTIMORE                                         
 RELDT 201601,013,2016-02-17  BOSTON                                            
 RELDT 201601,035,2016-02-17  DENVER-BOULDER                                    
 RELDT 201601,011,2016-02-17  DETROIT                                           
 RELDT 201601,429,2016-02-17  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201601,027,2016-02-17  MINNEAPOLIS-ST. PAUL                              
 RELDT 201601,057,2016-02-17  PHOENIX                                           
 RELDT 201601,063,2016-02-17  SAN DIEGO                                         
 RELDT 201601,039,2016-02-17  SEATTLE-TACOMA                                    
 RELDT 201601,017,2016-02-17  ST. LOUIS                                         
 RELDT 201601,087,2016-02-17  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201601,015,2016-02-17  WASHINGTON, DC                                    
*                                                                               
 RELDT 201601,093,2016-02-18  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201601,031,2016-02-18  CINCINNATI                                        
 RELDT 201601,019,2016-02-18  CLEVELAND                                         
 RELDT 201601,045,2016-02-18  COLUMBUS, OH                                      
 RELDT 201601,041,2016-02-18  KANSAS CITY                                       
 RELDT 201601,257,2016-02-18  LAS VEGAS                                         
 RELDT 201601,131,2016-02-18  ORLANDO                                           
 RELDT 201601,023,2016-02-18  PITTSBURGH, PA                                    
 RELDT 201601,051,2016-02-18  PORTLAND, OR                                      
 RELDT 201601,065,2016-02-18  SACRAMENTO                                        
 RELDT 201601,101,2016-02-18  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201601,059,2016-02-18  SAN ANTONIO                                       
*                                                                               
 RELDT 201601,135,2016-02-19  AUSTIN                                            
 RELDT 201601,166,2016-02-19  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201601,061,2016-02-19  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201601,049,2016-02-19  INDIANAPOLIS                                      
 RELDT 201601,107,2016-02-19  JACKSONVILLE                                      
 RELDT 201601,075,2016-02-19  MEMPHIS                                           
 RELDT 201601,043,2016-02-19  MILWAUKEE-RACINE                                  
 RELDT 201601,073,2016-02-19  NASHVILLE                                         
 RELDT 201601,109,2016-02-19  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201601,077,2016-02-19  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201601,115,2016-02-19  RALEIGH-DURHAM                                    
 RELDT 201601,299,2016-02-19  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201602,047,2016-03-14  ATLANTA                                           
 RELDT 201602,005,2016-03-14  CHICAGO                                           
 RELDT 201602,024,2016-03-14  DALLAS-FT. WORTH                                  
 RELDT 201602,033,2016-03-14  HOUSTON-GALVESTON                                 
 RELDT 201602,003,2016-03-14  LOS ANGELES                                       
 RELDT 201602,413,2016-03-14  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201602,321,2016-03-14  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201602,001,2016-03-14  NEW YORK                                          
 RELDT 201602,007,2016-03-14  PHILADELPHIA                                      
 RELDT 201602,379,2016-03-14  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201602,009,2016-03-14  SAN FRANCISCO                                     
 RELDT 201602,215,2016-03-14  SAN JOSE                                          
*                                                                               
 RELDT 201602,021,2016-03-15  BALTIMORE                                         
 RELDT 201602,013,2016-03-15  BOSTON                                            
 RELDT 201602,035,2016-03-15  DENVER-BOULDER                                    
 RELDT 201602,011,2016-03-15  DETROIT                                           
 RELDT 201602,429,2016-03-15  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201602,027,2016-03-15  MINNEAPOLIS-ST. PAUL                              
 RELDT 201602,057,2016-03-15  PHOENIX                                           
 RELDT 201602,063,2016-03-15  SAN DIEGO                                         
 RELDT 201602,039,2016-03-15  SEATTLE-TACOMA                                    
 RELDT 201602,017,2016-03-15  ST. LOUIS                                         
 RELDT 201602,087,2016-03-15  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201602,015,2016-03-15  WASHINGTON, DC                                    
*                                                                               
 RELDT 201602,093,2016-03-16  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201602,031,2016-03-16  CINCINNATI                                        
 RELDT 201602,019,2016-03-16  CLEVELAND                                         
 RELDT 201602,045,2016-03-16  COLUMBUS, OH                                      
 RELDT 201602,041,2016-03-16  KANSAS CITY                                       
 RELDT 201602,257,2016-03-16  LAS VEGAS                                         
 RELDT 201602,131,2016-03-16  ORLANDO                                           
 RELDT 201602,023,2016-03-16  PITTSBURGH, PA                                    
 RELDT 201602,051,2016-03-16  PORTLAND, OR                                      
 RELDT 201602,065,2016-03-16  SACRAMENTO                                        
 RELDT 201602,101,2016-03-16  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201602,059,2016-03-16  SAN ANTONIO                                       
*                                                                               
 RELDT 201602,135,2016-03-17  AUSTIN                                            
 RELDT 201602,166,2016-03-17  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201602,061,2016-03-17  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201602,049,2016-03-17  INDIANAPOLIS                                      
 RELDT 201602,107,2016-03-17  JACKSONVILLE                                      
 RELDT 201602,075,2016-03-17  MEMPHIS                                           
 RELDT 201602,043,2016-03-17  MILWAUKEE-RACINE                                  
 RELDT 201602,073,2016-03-17  NASHVILLE                                         
 RELDT 201602,109,2016-03-17  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201602,077,2016-03-17  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201602,115,2016-03-17  RALEIGH-DURHAM                                    
 RELDT 201602,299,2016-03-17  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201603,047,2016-04-11  ATLANTA                                           
 RELDT 201603,005,2016-04-11  CHICAGO                                           
 RELDT 201603,024,2016-04-11  DALLAS-FT. WORTH                                  
 RELDT 201603,033,2016-04-11  HOUSTON-GALVESTON                                 
 RELDT 201603,003,2016-04-11  LOS ANGELES                                       
 RELDT 201603,413,2016-04-11  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201603,321,2016-04-11  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201603,001,2016-04-11  NEW YORK                                          
 RELDT 201603,007,2016-04-11  PHILADELPHIA                                      
 RELDT 201603,379,2016-04-11  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201603,009,2016-04-11  SAN FRANCISCO                                     
 RELDT 201603,215,2016-04-11  SAN JOSE                                          
*                                                                               
 RELDT 201603,021,2016-04-12  BALTIMORE                                         
 RELDT 201603,013,2016-04-12  BOSTON                                            
 RELDT 201603,035,2016-04-12  DENVER-BOULDER                                    
 RELDT 201603,011,2016-04-12  DETROIT                                           
 RELDT 201603,429,2016-04-12  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201603,027,2016-04-12  MINNEAPOLIS-ST. PAUL                              
 RELDT 201603,057,2016-04-12  PHOENIX                                           
 RELDT 201603,063,2016-04-12  SAN DIEGO                                         
 RELDT 201603,039,2016-04-12  SEATTLE-TACOMA                                    
 RELDT 201603,017,2016-04-12  ST. LOUIS                                         
 RELDT 201603,087,2016-04-12  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201603,015,2016-04-12  WASHINGTON, DC                                    
*                                                                               
 RELDT 201603,093,2016-04-13  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201603,031,2016-04-13  CINCINNATI                                        
 RELDT 201603,019,2016-04-13  CLEVELAND                                         
 RELDT 201603,045,2016-04-13  COLUMBUS, OH                                      
 RELDT 201603,041,2016-04-13  KANSAS CITY                                       
 RELDT 201603,257,2016-04-13  LAS VEGAS                                         
 RELDT 201603,131,2016-04-13  ORLANDO                                           
 RELDT 201603,023,2016-04-13  PITTSBURGH, PA                                    
 RELDT 201603,051,2016-04-13  PORTLAND, OR                                      
 RELDT 201603,065,2016-04-13  SACRAMENTO                                        
 RELDT 201603,101,2016-04-13  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201603,059,2016-04-13  SAN ANTONIO                                       
*                                                                               
 RELDT 201603,135,2016-04-14  AUSTIN                                            
 RELDT 201603,166,2016-04-14  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201603,061,2016-04-14  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201603,049,2016-04-14  INDIANAPOLIS                                      
 RELDT 201603,107,2016-04-14  JACKSONVILLE                                      
 RELDT 201603,075,2016-04-14  MEMPHIS                                           
 RELDT 201603,043,2016-04-14  MILWAUKEE-RACINE                                  
 RELDT 201603,073,2016-04-14  NASHVILLE                                         
 RELDT 201603,109,2016-04-14  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201603,077,2016-04-14  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201603,115,2016-04-14  RALEIGH-DURHAM                                    
 RELDT 201603,299,2016-04-14  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201604,047,2016-05-09  ATLANTA                                           
 RELDT 201604,005,2016-05-09  CHICAGO                                           
 RELDT 201604,024,2016-05-09  DALLAS-FT. WORTH                                  
 RELDT 201604,033,2016-05-09  HOUSTON-GALVESTON                                 
 RELDT 201604,003,2016-05-09  LOS ANGELES                                       
 RELDT 201604,413,2016-05-09  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201604,321,2016-05-09  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201604,001,2016-05-09  NEW YORK                                          
 RELDT 201604,007,2016-05-09  PHILADELPHIA                                      
 RELDT 201604,379,2016-05-09  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201604,009,2016-05-09  SAN FRANCISCO                                     
 RELDT 201604,215,2016-05-09  SAN JOSE                                          
*                                                                               
 RELDT 201604,021,2016-05-10  BALTIMORE                                         
 RELDT 201604,013,2016-05-10  BOSTON                                            
 RELDT 201604,035,2016-05-10  DENVER-BOULDER                                    
 RELDT 201604,011,2016-05-10  DETROIT                                           
 RELDT 201604,429,2016-05-10  MIAMI-FT. LAUDERDALE-HOLLYWOOD                    
 RELDT 201604,027,2016-05-10  MINNEAPOLIS-ST. PAUL                              
 RELDT 201604,057,2016-05-10  PHOENIX                                           
 RELDT 201604,063,2016-05-10  SAN DIEGO                                         
 RELDT 201604,039,2016-05-10  SEATTLE-TACOMA                                    
 RELDT 201604,017,2016-05-10  ST. LOUIS                                         
 RELDT 201604,087,2016-05-10  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201604,015,2016-05-10  WASHINGTON, DC                                    
*                                                                               
 RELDT 201604,093,2016-05-11  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201604,031,2016-05-11  CINCINNATI                                        
 RELDT 201604,019,2016-05-11  CLEVELAND                                         
 RELDT 201604,045,2016-05-11  COLUMBUS, OH                                      
 RELDT 201604,041,2016-05-11  KANSAS CITY                                       
 RELDT 201604,257,2016-05-11  LAS VEGAS                                         
 RELDT 201604,131,2016-05-11  ORLANDO                                           
 RELDT 201604,023,2016-05-11  PITTSBURGH, PA                                    
 RELDT 201604,051,2016-05-11  PORTLAND, OR                                      
 RELDT 201604,065,2016-05-11  SACRAMENTO                                        
 RELDT 201604,101,2016-05-11  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201604,059,2016-05-11  SAN ANTONIO                                       
*                                                                               
 RELDT 201604,135,2016-05-12  AUSTIN                                            
 RELDT 201604,166,2016-05-12  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201604,061,2016-05-12  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201604,049,2016-05-12  INDIANAPOLIS                                      
 RELDT 201604,107,2016-05-12  JACKSONVILLE                                      
 RELDT 201604,075,2016-05-12  MEMPHIS                                           
 RELDT 201604,043,2016-05-12  MILWAUKEE-RACINE                                  
 RELDT 201604,073,2016-05-12  NASHVILLE                                         
 RELDT 201604,109,2016-05-12  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201604,077,2016-05-12  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201604,115,2016-05-12  RALEIGH-DURHAM                                    
 RELDT 201604,299,2016-05-12  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201605,047,2016-06-07  ATLANTA                                           
 RELDT 201605,005,2016-06-07  CHICAGO                                           
 RELDT 201605,024,2016-06-07  DALLAS-FT. WORTH                                  
 RELDT 201605,033,2016-06-07  HOUSTON-GALVESTON                                 
 RELDT 201605,003,2016-06-07  LOS ANGELES                                       
 RELDT 201605,413,2016-06-07  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201605,321,2016-06-07  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201605,001,2016-06-07  NEW YORK                                          
 RELDT 201605,007,2016-06-07  PHILADELPHIA                                      
 RELDT 201605,379,2016-06-07  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201605,009,2016-06-07  SAN FRANCISCO                                     
 RELDT 201605,215,2016-06-07  SAN JOSE                                          
*                                                                               
 RELDT 201605,021,2016-06-08  BALTIMORE                                         
 RELDT 201605,013,2016-06-08  BOSTON                                            
 RELDT 201605,035,2016-06-08  DENVER-BOULDER                                    
 RELDT 201605,011,2016-06-08  DETROIT                                           
 RELDT 201605,429,2016-06-08  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201605,027,2016-06-08  MINNEAPOLIS-ST. PAUL                              
 RELDT 201605,057,2016-06-08  PHOENIX                                           
 RELDT 201605,063,2016-06-08  SAN DIEGO                                         
 RELDT 201605,039,2016-06-08  SEATTLE-TACOMA                                    
 RELDT 201605,017,2016-06-08  ST. LOUIS                                         
 RELDT 201605,087,2016-06-08  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201605,015,2016-06-08  WASHINGTON, DC                                    
*                                                                               
 RELDT 201605,093,2016-06-09  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201605,031,2016-06-09  CINCINNATI                                        
 RELDT 201605,019,2016-06-09  CLEVELAND                                         
 RELDT 201605,045,2016-06-09  COLUMBUS, OH                                      
 RELDT 201605,041,2016-06-09  KANSAS CITY                                       
 RELDT 201605,257,2016-06-09  LAS VEGAS                                         
 RELDT 201605,131,2016-06-09  ORLANDO                                           
 RELDT 201605,023,2016-06-09  PITTSBURGH, PA                                    
 RELDT 201605,051,2016-06-09  PORTLAND, OR                                      
 RELDT 201605,065,2016-06-09  SACRAMENTO                                        
 RELDT 201605,101,2016-06-09  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201605,059,2016-06-09  SAN ANTONIO                                       
*                                                                               
 RELDT 201605,135,2016-06-10  AUSTIN                                            
 RELDT 201605,166,2016-06-10  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201605,061,2016-06-10  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201605,049,2016-06-10  INDIANAPOLIS                                      
 RELDT 201605,107,2016-06-10  JACKSONVILLE                                      
 RELDT 201605,075,2016-06-10  MEMPHIS                                           
 RELDT 201605,043,2016-06-10  MILWAUKEE-RACINE                                  
 RELDT 201605,073,2016-06-10  NASHVILLE                                         
 RELDT 201605,109,2016-06-10  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201605,077,2016-06-10  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201605,115,2016-06-10  RALEIGH-DURHAM                                    
 RELDT 201605,299,2016-06-10  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201606,047,2016-07-05  ATLANTA                                           
 RELDT 201606,005,2016-07-05  CHICAGO                                           
 RELDT 201606,024,2016-07-05  DALLAS-FT. WORTH                                  
 RELDT 201606,033,2016-07-05  HOUSTON-GALVESTON                                 
 RELDT 201606,003,2016-07-05  LOS ANGELES                                       
 RELDT 201606,413,2016-07-05  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201606,321,2016-07-05  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201606,001,2016-07-05  NEW YORK                                          
 RELDT 201606,007,2016-07-05  PHILADELPHIA                                      
 RELDT 201606,379,2016-07-05  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201606,009,2016-07-05  SAN FRANCISCO                                     
 RELDT 201606,215,2016-07-05  SAN JOSE                                          
*                                                                               
 RELDT 201606,021,2016-07-06  BALTIMORE                                         
 RELDT 201606,013,2016-07-06  BOSTON                                            
 RELDT 201606,035,2016-07-06  DENVER-BOULDER                                    
 RELDT 201606,011,2016-07-06  DETROIT                                           
 RELDT 201606,429,2016-07-06  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201606,027,2016-07-06  MINNEAPOLIS-ST. PAUL                              
 RELDT 201606,057,2016-07-06  PHOENIX                                           
 RELDT 201606,063,2016-07-06  SAN DIEGO                                         
 RELDT 201606,039,2016-07-06  SEATTLE-TACOMA                                    
 RELDT 201606,017,2016-07-06  ST. LOUIS                                         
 RELDT 201606,087,2016-07-06  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201606,015,2016-07-06  WASHINGTON, DC                                    
*                                                                               
 RELDT 201606,093,2016-07-07  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201606,031,2016-07-07  CINCINNATI                                        
 RELDT 201606,019,2016-07-07  CLEVELAND                                         
 RELDT 201606,045,2016-07-07  COLUMBUS, OH                                      
 RELDT 201606,041,2016-07-07  KANSAS CITY                                       
 RELDT 201606,257,2016-07-07  LAS VEGAS                                         
 RELDT 201606,131,2016-07-07  ORLANDO                                           
 RELDT 201606,023,2016-07-07  PITTSBURGH, PA                                    
 RELDT 201606,051,2016-07-07  PORTLAND, OR                                      
 RELDT 201606,065,2016-07-07  SACRAMENTO                                        
 RELDT 201606,101,2016-07-07  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201606,059,2016-07-07  SAN ANTONIO                                       
*                                                                               
 RELDT 201606,135,2016-07-08  AUSTIN                                            
 RELDT 201606,166,2016-07-08  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201606,061,2016-07-08  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201606,049,2016-07-08  INDIANAPOLIS                                      
 RELDT 201606,107,2016-07-08  JACKSONVILLE                                      
 RELDT 201606,075,2016-07-08  MEMPHIS                                           
 RELDT 201606,043,2016-07-08  MILWAUKEE-RACINE                                  
 RELDT 201606,073,2016-07-08  NASHVILLE                                         
 RELDT 201606,109,2016-07-08  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201606,077,2016-07-08  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201606,115,2016-07-08  RALEIGH-DURHAM                                    
 RELDT 201606,299,2016-07-08  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201607,047,2016-08-01  ATLANTA                                           
 RELDT 201607,005,2016-08-01  CHICAGO                                           
 RELDT 201607,024,2016-08-01  DALLAS-FT. WORTH                                  
 RELDT 201607,033,2016-08-01  HOUSTON-GALVESTON                                 
 RELDT 201607,003,2016-08-01  LOS ANGELES                                       
 RELDT 201607,413,2016-08-01  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201607,321,2016-08-01  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201607,001,2016-08-01  NEW YORK                                          
 RELDT 201607,007,2016-08-01  PHILADELPHIA                                      
 RELDT 201607,379,2016-08-01  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201607,009,2016-08-01  SAN FRANCISCO                                     
 RELDT 201607,215,2016-08-01  SAN JOSE                                          
*                                                                               
 RELDT 201607,021,2016-08-02  BALTIMORE                                         
 RELDT 201607,013,2016-08-02  BOSTON                                            
 RELDT 201607,035,2016-08-02  DENVER-BOULDER                                    
 RELDT 201607,011,2016-08-02  DETROIT                                           
 RELDT 201607,429,2016-08-02  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201607,027,2016-08-02  MINNEAPOLIS-ST. PAUL                              
 RELDT 201607,057,2016-08-02  PHOENIX                                           
 RELDT 201607,063,2016-08-02  SAN DIEGO                                         
 RELDT 201607,039,2016-08-02  SEATTLE-TACOMA                                    
 RELDT 201607,017,2016-08-02  ST. LOUIS                                         
 RELDT 201607,087,2016-08-02  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201607,015,2016-08-02  WASHINGTON, DC                                    
*                                                                               
 RELDT 201607,093,2016-08-03  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201607,031,2016-08-03  CINCINNATI                                        
 RELDT 201607,019,2016-08-03  CLEVELAND                                         
 RELDT 201607,045,2016-08-03  COLUMBUS, OH                                      
 RELDT 201607,041,2016-08-03  KANSAS CITY                                       
 RELDT 201607,257,2016-08-03  LAS VEGAS                                         
 RELDT 201607,131,2016-08-03  ORLANDO                                           
 RELDT 201607,023,2016-08-03  PITTSBURGH, PA                                    
 RELDT 201607,051,2016-08-03  PORTLAND, OR                                      
 RELDT 201607,065,2016-08-03  SACRAMENTO                                        
 RELDT 201607,101,2016-08-03  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201607,059,2016-08-03  SAN ANTONIO                                       
*                                                                               
 RELDT 201607,135,2016-08-04  AUSTIN                                            
 RELDT 201607,166,2016-08-04  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201607,061,2016-08-04  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201607,049,2016-08-04  INDIANAPOLIS                                      
 RELDT 201607,107,2016-08-04  JACKSONVILLE                                      
 RELDT 201607,075,2016-08-04  MEMPHIS                                           
 RELDT 201607,043,2016-08-04  MILWAUKEE-RACINE                                  
 RELDT 201607,073,2016-08-04  NASHVILLE                                         
 RELDT 201607,109,2016-08-04  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201607,077,2016-08-04  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201607,115,2016-08-04  RALEIGH-DURHAM                                    
 RELDT 201607,299,2016-08-04  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201608,047,2016-08-29  ATLANTA                                           
 RELDT 201608,005,2016-08-29  CHICAGO                                           
 RELDT 201608,024,2016-08-29  DALLAS-FT. WORTH                                  
 RELDT 201608,033,2016-08-29  HOUSTON-GALVESTON                                 
 RELDT 201608,003,2016-08-29  LOS ANGELES                                       
 RELDT 201608,413,2016-08-29  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201608,321,2016-08-29  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201608,001,2016-08-29  NEW YORK                                          
 RELDT 201608,007,2016-08-29  PHILADELPHIA                                      
 RELDT 201608,379,2016-08-29  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201608,009,2016-08-29  SAN FRANCISCO                                     
 RELDT 201608,215,2016-08-29  SAN JOSE                                          
*                                                                               
 RELDT 201608,021,2016-08-30  BALTIMORE                                         
 RELDT 201608,013,2016-08-30  BOSTON                                            
 RELDT 201608,035,2016-08-30  DENVER-BOULDER                                    
 RELDT 201608,011,2016-08-30  DETROIT                                           
 RELDT 201608,429,2016-08-30  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201608,027,2016-08-30  MINNEAPOLIS-ST. PAUL                              
 RELDT 201608,057,2016-08-30  PHOENIX                                           
 RELDT 201608,063,2016-08-30  SAN DIEGO                                         
 RELDT 201608,039,2016-08-30  SEATTLE-TACOMA                                    
 RELDT 201608,017,2016-08-30  ST. LOUIS                                         
 RELDT 201608,087,2016-08-30  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201608,015,2016-08-30  WASHINGTON, DC                                    
*                                                                               
 RELDT 201608,093,2016-08-31  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201608,031,2016-08-31  CINCINNATI                                        
 RELDT 201608,019,2016-08-31  CLEVELAND                                         
 RELDT 201608,045,2016-08-31  COLUMBUS, OH                                      
 RELDT 201608,041,2016-08-31  KANSAS CITY                                       
 RELDT 201608,257,2016-08-31  LAS VEGAS                                         
 RELDT 201608,131,2016-08-31  ORLANDO                                           
 RELDT 201608,023,2016-08-31  PITTSBURGH, PA                                    
 RELDT 201608,051,2016-08-31  PORTLAND, OR                                      
 RELDT 201608,065,2016-08-31  SACRAMENTO                                        
 RELDT 201608,101,2016-08-31  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201608,059,2016-08-31  SAN ANTONIO                                       
*                                                                               
 RELDT 201608,135,2016-09-01  AUSTIN                                            
 RELDT 201608,166,2016-09-01  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201608,061,2016-09-01  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201608,049,2016-09-01  INDIANAPOLIS                                      
 RELDT 201608,107,2016-09-01  JACKSONVILLE                                      
 RELDT 201608,075,2016-09-01  MEMPHIS                                           
 RELDT 201608,043,2016-09-01  MILWAUKEE-RACINE                                  
 RELDT 201608,073,2016-09-01  NASHVILLE                                         
 RELDT 201608,109,2016-09-01  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201608,077,2016-09-01  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201608,115,2016-09-01  RALEIGH-DURHAM                                    
 RELDT 201608,299,2016-09-01  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201609,047,2016-09-26  ATLANTA                                           
 RELDT 201609,005,2016-09-26  CHICAGO                                           
 RELDT 201609,024,2016-09-26  DALLAS-FT. WORTH                                  
 RELDT 201609,033,2016-09-26  HOUSTON-GALVESTON                                 
 RELDT 201609,003,2016-09-26  LOS ANGELES                                       
 RELDT 201609,413,2016-09-26  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201609,321,2016-09-26  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201609,001,2016-09-26  NEW YORK                                          
 RELDT 201609,007,2016-09-26  PHILADELPHIA                                      
 RELDT 201609,379,2016-09-26  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201609,009,2016-09-26  SAN FRANCISCO                                     
 RELDT 201609,215,2016-09-26  SAN JOSE                                          
*                                                                               
 RELDT 201609,021,2016-09-27  BALTIMORE                                         
 RELDT 201609,013,2016-09-27  BOSTON                                            
 RELDT 201609,035,2016-09-27  DENVER-BOULDER                                    
 RELDT 201609,011,2016-09-27  DETROIT                                           
 RELDT 201609,429,2016-09-27  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201609,027,2016-09-27  MINNEAPOLIS-ST. PAUL                              
 RELDT 201609,057,2016-09-27  PHOENIX                                           
 RELDT 201609,063,2016-09-27  SAN DIEGO                                         
 RELDT 201609,039,2016-09-27  SEATTLE-TACOMA                                    
 RELDT 201609,017,2016-09-27  ST. LOUIS                                         
 RELDT 201609,087,2016-09-27  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201609,015,2016-09-27  WASHINGTON, DC                                    
*                                                                               
 RELDT 201609,093,2016-09-28  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201609,031,2016-09-28  CINCINNATI                                        
 RELDT 201609,019,2016-09-28  CLEVELAND                                         
 RELDT 201609,045,2016-09-28  COLUMBUS, OH                                      
 RELDT 201609,041,2016-09-28  KANSAS CITY                                       
 RELDT 201609,257,2016-09-28  LAS VEGAS                                         
 RELDT 201609,131,2016-09-28  ORLANDO                                           
 RELDT 201609,023,2016-09-28  PITTSBURGH, PA                                    
 RELDT 201609,051,2016-09-28  PORTLAND, OR                                      
 RELDT 201609,065,2016-09-28  SACRAMENTO                                        
 RELDT 201609,101,2016-09-28  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201609,059,2016-09-28  SAN ANTONIO                                       
*                                                                               
 RELDT 201609,135,2016-09-29  AUSTIN                                            
 RELDT 201609,166,2016-09-29  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201609,061,2016-09-29  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201609,049,2016-09-29  INDIANAPOLIS                                      
 RELDT 201609,107,2016-09-29  JACKSONVILLE                                      
 RELDT 201609,075,2016-09-29  MEMPHIS                                           
 RELDT 201609,043,2016-09-29  MILWAUKEE-RACINE                                  
 RELDT 201609,073,2016-09-29  NASHVILLE                                         
 RELDT 201609,109,2016-09-29  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201609,077,2016-09-29  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201609,115,2016-09-29  RALEIGH-DURHAM                                    
 RELDT 201609,299,2016-09-29  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201610,047,2016-10-24  ATLANTA                                           
 RELDT 201610,005,2016-10-24  CHICAGO                                           
 RELDT 201610,024,2016-10-24  DALLAS-FT. WORTH                                  
 RELDT 201610,033,2016-10-24  HOUSTON-GALVESTON                                 
 RELDT 201610,003,2016-10-24  LOS ANGELES                                       
 RELDT 201610,413,2016-10-24  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201610,321,2016-10-24  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201610,001,2016-10-24  NEW YORK                                          
 RELDT 201610,007,2016-10-24  PHILADELPHIA                                      
 RELDT 201610,379,2016-10-24  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201610,009,2016-10-24  SAN FRANCISCO                                     
 RELDT 201610,215,2016-10-24  SAN JOSE                                          
*                                                                               
 RELDT 201610,021,2016-10-25  BALTIMORE                                         
 RELDT 201610,013,2016-10-25  BOSTON                                            
 RELDT 201610,035,2016-10-25  DENVER-BOULDER                                    
 RELDT 201610,011,2016-10-25  DETROIT                                           
 RELDT 201610,429,2016-10-25  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201610,027,2016-10-25  MINNEAPOLIS-ST. PAUL                              
 RELDT 201610,057,2016-10-25  PHOENIX                                           
 RELDT 201610,063,2016-10-25  SAN DIEGO                                         
 RELDT 201610,039,2016-10-25  SEATTLE-TACOMA                                    
 RELDT 201610,017,2016-10-25  ST. LOUIS                                         
 RELDT 201610,087,2016-10-25  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201610,015,2016-10-25  WASHINGTON, DC                                    
*                                                                               
 RELDT 201610,093,2016-10-26  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201610,031,2016-10-26  CINCINNATI                                        
 RELDT 201610,019,2016-10-26  CLEVELAND                                         
 RELDT 201610,045,2016-10-26  COLUMBUS, OH                                      
 RELDT 201610,041,2016-10-26  KANSAS CITY                                       
 RELDT 201610,257,2016-10-26  LAS VEGAS                                         
 RELDT 201610,131,2016-10-26  ORLANDO                                           
 RELDT 201610,023,2016-10-26  PITTSBURGH, PA                                    
 RELDT 201610,051,2016-10-26  PORTLAND, OR                                      
 RELDT 201610,065,2016-10-26  SACRAMENTO                                        
 RELDT 201610,101,2016-10-26  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201610,059,2016-10-26  SAN ANTONIO                                       
*                                                                               
 RELDT 201610,135,2016-10-27  AUSTIN                                            
 RELDT 201610,166,2016-10-27  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201610,061,2016-10-27  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201610,049,2016-10-27  INDIANAPOLIS                                      
 RELDT 201610,107,2016-10-27  JACKSONVILLE                                      
 RELDT 201610,075,2016-10-27  MEMPHIS                                           
 RELDT 201610,043,2016-10-27  MILWAUKEE-RACINE                                  
 RELDT 201610,073,2016-10-27  NASHVILLE                                         
 RELDT 201610,109,2016-10-27  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201610,077,2016-10-27  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201610,115,2016-10-27  RALEIGH-DURHAM                                    
 RELDT 201610,299,2016-10-27  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201611,047,2016-11-21  ATLANTA                                           
 RELDT 201611,005,2016-11-21  CHICAGO                                           
 RELDT 201611,024,2016-11-21  DALLAS-FT. WORTH                                  
 RELDT 201611,033,2016-11-21  HOUSTON-GALVESTON                                 
 RELDT 201611,003,2016-11-21  LOS ANGELES                                       
 RELDT 201611,413,2016-11-21  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201611,321,2016-11-21  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201611,001,2016-11-21  NEW YORK                                          
 RELDT 201611,007,2016-11-21  PHILADELPHIA                                      
 RELDT 201611,379,2016-11-21  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201611,009,2016-11-21  SAN FRANCISCO                                     
 RELDT 201611,215,2016-11-21  SAN JOSE                                          
*                                                                               
 RELDT 201611,021,2016-11-22  BALTIMORE                                         
 RELDT 201611,013,2016-11-22  BOSTON                                            
 RELDT 201611,035,2016-11-22  DENVER-BOULDER                                    
 RELDT 201611,011,2016-11-22  DETROIT                                           
 RELDT 201611,429,2016-11-22  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201611,027,2016-11-22  MINNEAPOLIS-ST. PAUL                              
 RELDT 201611,057,2016-11-22  PHOENIX                                           
 RELDT 201611,063,2016-11-22  SAN DIEGO                                         
 RELDT 201611,039,2016-11-22  SEATTLE-TACOMA                                    
 RELDT 201611,017,2016-11-22  ST. LOUIS                                         
 RELDT 201611,087,2016-11-22  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201611,015,2016-11-22  WASHINGTON, DC                                    
*                                                                               
 RELDT 201611,093,2016-11-23  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201611,031,2016-11-23  CINCINNATI                                        
 RELDT 201611,019,2016-11-23  CLEVELAND                                         
 RELDT 201611,045,2016-11-23  COLUMBUS, OH                                      
 RELDT 201611,041,2016-11-23  KANSAS CITY                                       
 RELDT 201611,257,2016-11-23  LAS VEGAS                                         
 RELDT 201611,131,2016-11-23  ORLANDO                                           
 RELDT 201611,023,2016-11-23  PITTSBURGH, PA                                    
 RELDT 201611,051,2016-11-23  PORTLAND, OR                                      
 RELDT 201611,065,2016-11-23  SACRAMENTO                                        
 RELDT 201611,101,2016-11-23  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201611,059,2016-11-23  SAN ANTONIO                                       
*                                                                               
 RELDT 201611,135,2016-11-28  AUSTIN                                            
 RELDT 201611,166,2016-11-28  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201611,061,2016-11-28  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201611,049,2016-11-28  INDIANAPOLIS                                      
 RELDT 201611,107,2016-11-28  JACKSONVILLE                                      
 RELDT 201611,075,2016-11-28  MEMPHIS                                           
 RELDT 201611,043,2016-11-28  MILWAUKEE-RACINE                                  
 RELDT 201611,073,2016-11-28  NASHVILLE                                         
 RELDT 201611,109,2016-11-28  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201611,077,2016-11-28  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201611,115,2016-11-28  RALEIGH-DURHAM                                    
 RELDT 201611,299,2016-11-28  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201612,047,2016-12-19  ATLANTA                                           
 RELDT 201612,005,2016-12-19  CHICAGO                                           
 RELDT 201612,024,2016-12-19  DALLAS-FT. WORTH                                  
 RELDT 201612,033,2016-12-19  HOUSTON-GALVESTON                                 
 RELDT 201612,003,2016-12-19  LOS ANGELES                                       
 RELDT 201612,413,2016-12-19  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201612,321,2016-12-19  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201612,001,2016-12-19  NEW YORK                                          
 RELDT 201612,007,2016-12-19  PHILADELPHIA                                      
 RELDT 201612,379,2016-12-19  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201612,009,2016-12-19  SAN FRANCISCO                                     
 RELDT 201612,215,2016-12-19  SAN JOSE                                          
*                                                                               
 RELDT 201612,021,2016-12-20  BALTIMORE                                         
 RELDT 201612,013,2016-12-20  BOSTON                                            
 RELDT 201612,035,2016-12-20  DENVER-BOULDER                                    
 RELDT 201612,011,2016-12-20  DETROIT                                           
 RELDT 201612,429,2016-12-20  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201612,027,2016-12-20  MINNEAPOLIS-ST. PAUL                              
 RELDT 201612,057,2016-12-20  PHOENIX                                           
 RELDT 201612,063,2016-12-20  SAN DIEGO                                         
 RELDT 201612,039,2016-12-20  SEATTLE-TACOMA                                    
 RELDT 201612,017,2016-12-20  ST. LOUIS                                         
 RELDT 201612,087,2016-12-20  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201612,015,2016-12-20  WASHINGTON, DC                                    
*                                                                               
 RELDT 201612,093,2016-12-21  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201612,031,2016-12-21  CINCINNATI                                        
 RELDT 201612,019,2016-12-21  CLEVELAND                                         
 RELDT 201612,045,2016-12-21  COLUMBUS, OH                                      
 RELDT 201612,041,2016-12-21  KANSAS CITY                                       
 RELDT 201612,257,2016-12-21  LAS VEGAS                                         
 RELDT 201612,131,2016-12-21  ORLANDO                                           
 RELDT 201612,023,2016-12-21  PITTSBURGH, PA                                    
 RELDT 201612,051,2016-12-21  PORTLAND, OR                                      
 RELDT 201612,065,2016-12-21  SACRAMENTO                                        
 RELDT 201612,101,2016-12-21  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201612,059,2016-12-21  SAN ANTONIO                                       
*                                                                               
 RELDT 201612,135,2016-12-22  AUSTIN                                            
 RELDT 201612,166,2016-12-22  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201612,061,2016-12-22  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201612,049,2016-12-22  INDIANAPOLIS                                      
 RELDT 201612,107,2016-12-22  JACKSONVILLE                                      
 RELDT 201612,075,2016-12-22  MEMPHIS                                           
 RELDT 201612,043,2016-12-22  MILWAUKEE-RACINE                                  
 RELDT 201612,073,2016-12-22  NASHVILLE                                         
 RELDT 201612,109,2016-12-22  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201612,077,2016-12-22  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201612,115,2016-12-22  RALEIGH-DURHAM                                    
 RELDT 201612,299,2016-12-22  WEST PALM BEACH-BOCA RATON                        
*                                                                               
* HOLIDAY BOOK (2016)                                                           
*                                                                               
 RELDT 201623,047,2017-01-17  ATLANTA                                           
 RELDT 201623,005,2017-01-17  CHICAGO                                           
 RELDT 201623,024,2017-01-17  DALLAS-FT. WORTH                                  
 RELDT 201623,033,2017-01-17  HOUSTON-GALVESTON                                 
 RELDT 201623,003,2017-01-17  LOS ANGELES                                       
 RELDT 201623,413,2017-01-17  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201623,321,2017-01-17  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201623,001,2017-01-17  NEW YORK                                          
 RELDT 201623,007,2017-01-17  PHILADELPHIA                                      
 RELDT 201623,379,2017-01-17  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201623,009,2017-01-17  SAN FRANCISCO                                     
 RELDT 201623,215,2017-01-17  SAN JOSE                                          
*                                                                               
 RELDT 201623,021,2017-01-18  BALTIMORE                                         
 RELDT 201623,013,2017-01-18  BOSTON                                            
 RELDT 201623,035,2017-01-18  DENVER-BOULDER                                    
 RELDT 201623,011,2017-01-18  DETROIT                                           
 RELDT 201623,429,2017-01-18  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201623,027,2017-01-18  MINNEAPOLIS-ST. PAUL                              
 RELDT 201623,057,2017-01-18  PHOENIX                                           
 RELDT 201623,063,2017-01-18  SAN DIEGO                                         
 RELDT 201623,039,2017-01-18  SEATTLE-TACOMA                                    
 RELDT 201623,017,2017-01-18  ST. LOUIS                                         
 RELDT 201623,087,2017-01-18  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201623,015,2017-01-18  WASHINGTON, DC                                    
*                                                                               
 RELDT 201623,093,2017-01-19  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201623,031,2017-01-19  CINCINNATI                                        
 RELDT 201623,019,2017-01-19  CLEVELAND                                         
 RELDT 201623,045,2017-01-19  COLUMBUS, OH                                      
 RELDT 201623,041,2017-01-19  KANSAS CITY                                       
 RELDT 201623,257,2017-01-19  LAS VEGAS                                         
 RELDT 201623,131,2017-01-19  ORLANDO                                           
 RELDT 201623,023,2017-01-19  PITTSBURGH, PA                                    
 RELDT 201623,051,2017-01-19  PORTLAND, OR                                      
 RELDT 201623,065,2017-01-19  SACRAMENTO                                        
 RELDT 201623,101,2017-01-19  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201623,059,2017-01-19  SAN ANTONIO                                       
*                                                                               
 RELDT 201623,135,2017-01-20  AUSTIN                                            
 RELDT 201623,166,2017-01-20  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201623,061,2017-01-20  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201623,049,2017-01-20  INDIANAPOLIS                                      
 RELDT 201623,107,2017-01-20  JACKSONVILLE                                      
 RELDT 201623,075,2017-01-20  MEMPHIS                                           
 RELDT 201623,043,2017-01-20  MILWAUKEE-RACINE                                  
 RELDT 201623,073,2017-01-20  NASHVILLE                                         
 RELDT 201623,109,2017-01-20  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201623,077,2017-01-20  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201623,115,2017-01-20  RALEIGH-DURHAM                                    
 RELDT 201623,299,2017-01-20  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
* PPM 2017 MARKETS                                                              
*                                                                               
*                                                                               
 RELDT 201701,047,2017-02-21  ATLANTA                                           
 RELDT 201701,005,2017-02-21  CHICAGO                                           
 RELDT 201701,024,2017-02-21  DALLAS-FT. WORTH                                  
 RELDT 201701,033,2017-02-21  HOUSTON-GALVESTON                                 
 RELDT 201701,003,2017-02-21  LOS ANGELES                                       
 RELDT 201701,413,2017-02-21  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201701,321,2017-02-21  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201701,001,2017-02-21  NEW YORK                                          
 RELDT 201701,007,2017-02-21  PHILADELPHIA                                      
 RELDT 201701,379,2017-02-21  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201701,009,2017-02-21  SAN FRANCISCO                                     
 RELDT 201701,215,2017-02-21  SAN JOSE                                          
*                                                                               
 RELDT 201701,021,2017-02-22  BALTIMORE                                         
 RELDT 201701,013,2017-02-22  BOSTON                                            
 RELDT 201701,035,2017-02-22  DENVER-BOULDER                                    
 RELDT 201701,011,2017-02-22  DETROIT                                           
 RELDT 201701,429,2017-02-22  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201701,027,2017-02-22  MINNEAPOLIS-ST. PAUL                              
 RELDT 201701,057,2017-02-22  PHOENIX                                           
 RELDT 201701,063,2017-02-22  SAN DIEGO                                         
 RELDT 201701,039,2017-02-22  SEATTLE-TACOMA                                    
 RELDT 201701,017,2017-02-22  ST. LOUIS                                         
 RELDT 201701,087,2017-02-22  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201701,015,2017-02-22  WASHINGTON, DC                                    
*                                                                               
 RELDT 201701,093,2017-02-23  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201701,031,2017-02-23  CINCINNATI                                        
 RELDT 201701,019,2017-02-23  CLEVELAND                                         
 RELDT 201701,045,2017-02-23  COLUMBUS, OH                                      
 RELDT 201701,041,2017-02-23  KANSAS CITY                                       
 RELDT 201701,257,2017-02-23  LAS VEGAS                                         
 RELDT 201701,131,2017-02-23  ORLANDO                                           
 RELDT 201701,023,2017-02-23  PITTSBURGH, PA                                    
 RELDT 201701,051,2017-02-23  PORTLAND, OR                                      
 RELDT 201701,065,2017-02-23  SACRAMENTO                                        
 RELDT 201701,101,2017-02-23  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201701,059,2017-02-23  SAN ANTONIO                                       
*                                                                               
 RELDT 201701,135,2017-02-24  AUSTIN                                            
 RELDT 201701,166,2017-02-24  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201701,061,2017-02-24  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201701,049,2017-02-24  INDIANAPOLIS                                      
 RELDT 201701,107,2017-02-24  JACKSONVILLE                                      
 RELDT 201701,075,2017-02-24  MEMPHIS                                           
 RELDT 201701,043,2017-02-24  MILWAUKEE-RACINE                                  
 RELDT 201701,073,2017-02-24  NASHVILLE                                         
 RELDT 201701,109,2017-02-24  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201701,077,2017-02-24  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201701,115,2017-02-24  RALEIGH-DURHAM                                    
 RELDT 201701,299,2017-02-24  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201702,047,2017-03-20  ATLANTA                                           
 RELDT 201702,005,2017-03-20  CHICAGO                                           
 RELDT 201702,024,2017-03-20  DALLAS-FT. WORTH                                  
 RELDT 201702,033,2017-03-20  HOUSTON-GALVESTON                                 
 RELDT 201702,003,2017-03-20  LOS ANGELES                                       
 RELDT 201702,413,2017-03-20  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201702,321,2017-03-20  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201702,001,2017-03-20  NEW YORK                                          
 RELDT 201702,007,2017-03-20  PHILADELPHIA                                      
 RELDT 201702,379,2017-03-20  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201702,009,2017-03-20  SAN FRANCISCO                                     
 RELDT 201702,215,2017-03-20  SAN JOSE                                          
*                                                                               
 RELDT 201702,021,2017-03-21  BALTIMORE                                         
 RELDT 201702,013,2017-03-21  BOSTON                                            
 RELDT 201702,035,2017-03-21  DENVER-BOULDER                                    
 RELDT 201702,011,2017-03-21  DETROIT                                           
 RELDT 201702,429,2017-03-21  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201702,027,2017-03-21  MINNEAPOLIS-ST. PAUL                              
 RELDT 201702,057,2017-03-21  PHOENIX                                           
 RELDT 201702,063,2017-03-21  SAN DIEGO                                         
 RELDT 201702,039,2017-03-21  SEATTLE-TACOMA                                    
 RELDT 201702,017,2017-03-21  ST. LOUIS                                         
 RELDT 201702,087,2017-03-21  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201702,015,2017-03-21  WASHINGTON, DC                                    
*                                                                               
 RELDT 201702,093,2017-03-22  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201702,031,2017-03-22  CINCINNATI                                        
 RELDT 201702,019,2017-03-22  CLEVELAND                                         
 RELDT 201702,045,2017-03-22  COLUMBUS, OH                                      
 RELDT 201702,041,2017-03-22  KANSAS CITY                                       
 RELDT 201702,257,2017-03-22  LAS VEGAS                                         
 RELDT 201702,131,2017-03-22  ORLANDO                                           
 RELDT 201702,023,2017-03-22  PITTSBURGH, PA                                    
 RELDT 201702,051,2017-03-22  PORTLAND, OR                                      
 RELDT 201702,065,2017-03-22  SACRAMENTO                                        
 RELDT 201702,101,2017-03-22  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201702,059,2017-03-22  SAN ANTONIO                                       
*                                                                               
 RELDT 201702,135,2017-03-23  AUSTIN                                            
 RELDT 201702,166,2017-03-23  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201702,061,2017-03-23  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201702,049,2017-03-23  INDIANAPOLIS                                      
 RELDT 201702,107,2017-03-23  JACKSONVILLE                                      
 RELDT 201702,075,2017-03-23  MEMPHIS                                           
 RELDT 201702,043,2017-03-23  MILWAUKEE-RACINE                                  
 RELDT 201702,073,2017-03-23  NASHVILLE                                         
 RELDT 201702,109,2017-03-23  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201702,077,2017-03-23  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201702,115,2017-03-23  RALEIGH-DURHAM                                    
 RELDT 201702,299,2017-03-23  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201703,047,2017-04-18  ATLANTA                                           
 RELDT 201703,005,2017-04-18  CHICAGO                                           
 RELDT 201703,024,2017-04-18  DALLAS-FT. WORTH                                  
 RELDT 201703,033,2017-04-18  HOUSTON-GALVESTON                                 
 RELDT 201703,003,2017-04-18  LOS ANGELES                                       
 RELDT 201703,413,2017-04-18  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201703,321,2017-04-18  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201703,001,2017-04-18  NEW YORK                                          
 RELDT 201703,007,2017-04-18  PHILADELPHIA                                      
 RELDT 201703,379,2017-04-18  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201703,009,2017-04-18  SAN FRANCISCO                                     
 RELDT 201703,215,2017-04-18  SAN JOSE                                          
*                                                                               
 RELDT 201703,021,2017-04-19  BALTIMORE                                         
 RELDT 201703,013,2017-04-19  BOSTON                                            
 RELDT 201703,035,2017-04-19  DENVER-BOULDER                                    
 RELDT 201703,011,2017-04-19  DETROIT                                           
 RELDT 201703,429,2017-04-19  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201703,027,2017-04-19  MINNEAPOLIS-ST. PAUL                              
 RELDT 201703,057,2017-04-19  PHOENIX                                           
 RELDT 201703,063,2017-04-19  SAN DIEGO                                         
 RELDT 201703,039,2017-04-19  SEATTLE-TACOMA                                    
 RELDT 201703,017,2017-04-19  ST. LOUIS                                         
 RELDT 201703,087,2017-04-19  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201703,015,2017-04-19  WASHINGTON, DC                                    
*                                                                               
 RELDT 201703,093,2017-04-20  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201703,031,2017-04-20  CINCINNATI                                        
 RELDT 201703,019,2017-04-20  CLEVELAND                                         
 RELDT 201703,045,2017-04-20  COLUMBUS, OH                                      
 RELDT 201703,041,2017-04-20  KANSAS CITY                                       
 RELDT 201703,257,2017-04-20  LAS VEGAS                                         
 RELDT 201703,131,2017-04-20  ORLANDO                                           
 RELDT 201703,023,2017-04-20  PITTSBURGH, PA                                    
 RELDT 201703,051,2017-04-20  PORTLAND, OR                                      
 RELDT 201703,065,2017-04-20  SACRAMENTO                                        
 RELDT 201703,101,2017-04-20  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201703,059,2017-04-20  SAN ANTONIO                                       
*                                                                               
 RELDT 201703,135,2017-04-21  AUSTIN                                            
 RELDT 201703,166,2017-04-21  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201703,061,2017-04-21  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201703,049,2017-04-21  INDIANAPOLIS                                      
 RELDT 201703,107,2017-04-21  JACKSONVILLE                                      
 RELDT 201703,075,2017-04-21  MEMPHIS                                           
 RELDT 201703,043,2017-04-21  MILWAUKEE-RACINE                                  
 RELDT 201703,073,2017-04-21  NASHVILLE                                         
 RELDT 201703,109,2017-04-21  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201703,077,2017-04-21  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201703,115,2017-04-21  RALEIGH-DURHAM                                    
 RELDT 201703,299,2017-04-21  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201704,047,2017-05-15  ATLANTA                                           
 RELDT 201704,005,2017-05-15  CHICAGO                                           
 RELDT 201704,024,2017-05-15  DALLAS-FT. WORTH                                  
 RELDT 201704,033,2017-05-15  HOUSTON-GALVESTON                                 
 RELDT 201704,003,2017-05-15  LOS ANGELES                                       
 RELDT 201704,413,2017-05-15  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201704,321,2017-05-15  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201704,001,2017-05-15  NEW YORK                                          
 RELDT 201704,007,2017-05-15  PHILADELPHIA                                      
 RELDT 201704,379,2017-05-15  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201704,009,2017-05-15  SAN FRANCISCO                                     
 RELDT 201704,215,2017-05-15  SAN JOSE                                          
*                                                                               
 RELDT 201704,021,2017-05-16  BALTIMORE                                         
 RELDT 201704,013,2017-05-16  BOSTON                                            
 RELDT 201704,035,2017-05-16  DENVER-BOULDER                                    
 RELDT 201704,011,2017-05-16  DETROIT                                           
 RELDT 201704,429,2017-05-16  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201704,027,2017-05-16  MINNEAPOLIS-ST. PAUL                              
 RELDT 201704,057,2017-05-16  PHOENIX                                           
 RELDT 201704,063,2017-05-16  SAN DIEGO                                         
 RELDT 201704,039,2017-05-16  SEATTLE-TACOMA                                    
 RELDT 201704,017,2017-05-16  ST. LOUIS                                         
 RELDT 201704,087,2017-05-16  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201704,015,2017-05-16  WASHINGTON, DC                                    
*                                                                               
 RELDT 201704,093,2017-05-17  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201704,031,2017-05-17  CINCINNATI                                        
 RELDT 201704,019,2017-05-17  CLEVELAND                                         
 RELDT 201704,045,2017-05-17  COLUMBUS, OH                                      
 RELDT 201704,041,2017-05-17  KANSAS CITY                                       
 RELDT 201704,257,2017-05-17  LAS VEGAS                                         
 RELDT 201704,131,2017-05-17  ORLANDO                                           
 RELDT 201704,023,2017-05-17  PITTSBURGH, PA                                    
 RELDT 201704,051,2017-05-17  PORTLAND, OR                                      
 RELDT 201704,065,2017-05-17  SACRAMENTO                                        
 RELDT 201704,101,2017-05-17  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201704,059,2017-05-17  SAN ANTONIO                                       
*                                                                               
 RELDT 201704,135,2017-05-18  AUSTIN                                            
 RELDT 201704,166,2017-05-18  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201704,061,2017-05-18  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201704,049,2017-05-18  INDIANAPOLIS                                      
 RELDT 201704,107,2017-05-18  JACKSONVILLE                                      
 RELDT 201704,075,2017-05-18  MEMPHIS                                           
 RELDT 201704,043,2017-05-18  MILWAUKEE-RACINE                                  
 RELDT 201704,073,2017-05-18  NASHVILLE                                         
 RELDT 201704,109,2017-05-18  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201704,077,2017-05-18  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201704,115,2017-05-18  RALEIGH-DURHAM                                    
 RELDT 201704,299,2017-05-18  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201705,047,2017-06-12  ATLANTA                                           
 RELDT 201705,005,2017-06-12  CHICAGO                                           
 RELDT 201705,024,2017-06-12  DALLAS-FT. WORTH                                  
 RELDT 201705,033,2017-06-12  HOUSTON-GALVESTON                                 
 RELDT 201705,003,2017-06-12  LOS ANGELES                                       
 RELDT 201705,413,2017-06-12  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201705,321,2017-06-12  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201705,001,2017-06-12  NEW YORK                                          
 RELDT 201705,007,2017-06-12  PHILADELPHIA                                      
 RELDT 201705,379,2017-06-12  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201705,009,2017-06-12  SAN FRANCISCO                                     
 RELDT 201705,215,2017-06-12  SAN JOSE                                          
*                                                                               
 RELDT 201705,021,2017-06-13  BALTIMORE                                         
 RELDT 201705,013,2017-06-13  BOSTON                                            
 RELDT 201705,035,2017-06-13  DENVER-BOULDER                                    
 RELDT 201705,011,2017-06-13  DETROIT                                           
 RELDT 201705,429,2017-06-13  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201705,027,2017-06-13  MINNEAPOLIS-ST. PAUL                              
 RELDT 201705,057,2017-06-13  PHOENIX                                           
 RELDT 201705,063,2017-06-13  SAN DIEGO                                         
 RELDT 201705,039,2017-06-13  SEATTLE-TACOMA                                    
 RELDT 201705,017,2017-06-13  ST. LOUIS                                         
 RELDT 201705,087,2017-06-13  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201705,015,2017-06-13  WASHINGTON, DC                                    
*                                                                               
 RELDT 201705,093,2017-06-14  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201705,031,2017-06-14  CINCINNATI                                        
 RELDT 201705,019,2017-06-14  CLEVELAND                                         
 RELDT 201705,045,2017-06-14  COLUMBUS, OH                                      
 RELDT 201705,041,2017-06-14  KANSAS CITY                                       
 RELDT 201705,257,2017-06-14  LAS VEGAS                                         
 RELDT 201705,131,2017-06-14  ORLANDO                                           
 RELDT 201705,023,2017-06-14  PITTSBURGH, PA                                    
 RELDT 201705,051,2017-06-14  PORTLAND, OR                                      
 RELDT 201705,065,2017-06-14  SACRAMENTO                                        
 RELDT 201705,101,2017-06-14  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201705,059,2017-06-14  SAN ANTONIO                                       
*                                                                               
 RELDT 201705,135,2017-06-15  AUSTIN                                            
 RELDT 201705,166,2017-06-15  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201705,061,2017-06-15  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201705,049,2017-06-15  INDIANAPOLIS                                      
 RELDT 201705,107,2017-06-15  JACKSONVILLE                                      
 RELDT 201705,075,2017-06-15  MEMPHIS                                           
 RELDT 201705,043,2017-06-15  MILWAUKEE-RACINE                                  
 RELDT 201705,073,2017-06-15  NASHVILLE                                         
 RELDT 201705,109,2017-06-15  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201705,077,2017-06-15  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201705,115,2017-06-15  RALEIGH-DURHAM                                    
 RELDT 201705,299,2017-06-15  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201706,047,2017-07-12  ATLANTA                                           
 RELDT 201706,005,2017-07-12  CHICAGO                                           
 RELDT 201706,024,2017-07-12  DALLAS-FT. WORTH                                  
 RELDT 201706,033,2017-07-12  HOUSTON-GALVESTON                                 
 RELDT 201706,003,2017-07-12  LOS ANGELES                                       
 RELDT 201706,413,2017-07-12  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201706,321,2017-07-12  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201706,001,2017-07-12  NEW YORK                                          
 RELDT 201706,007,2017-07-12  PHILADELPHIA                                      
 RELDT 201706,379,2017-07-12  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201706,009,2017-07-12  SAN FRANCISCO                                     
 RELDT 201706,215,2017-07-12  SAN JOSE                                          
*                                                                               
 RELDT 201706,021,2017-07-13  BALTIMORE                                         
 RELDT 201706,013,2017-07-13  BOSTON                                            
 RELDT 201706,035,2017-07-13  DENVER-BOULDER                                    
 RELDT 201706,011,2017-07-13  DETROIT                                           
 RELDT 201706,429,2017-07-13  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201706,027,2017-07-13  MINNEAPOLIS-ST. PAUL                              
 RELDT 201706,057,2017-07-13  PHOENIX                                           
 RELDT 201706,063,2017-07-13  SAN DIEGO                                         
 RELDT 201706,039,2017-07-13  SEATTLE-TACOMA                                    
 RELDT 201706,017,2017-07-13  ST. LOUIS                                         
 RELDT 201706,087,2017-07-13  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201706,015,2017-07-13  WASHINGTON, DC                                    
*                                                                               
 RELDT 201706,093,2017-07-14  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201706,031,2017-07-14  CINCINNATI                                        
 RELDT 201706,019,2017-07-14  CLEVELAND                                         
 RELDT 201706,045,2017-07-14  COLUMBUS, OH                                      
 RELDT 201706,041,2017-07-14  KANSAS CITY                                       
 RELDT 201706,257,2017-07-14  LAS VEGAS                                         
 RELDT 201706,131,2017-07-14  ORLANDO                                           
 RELDT 201706,023,2017-07-14  PITTSBURGH, PA                                    
 RELDT 201706,051,2017-07-14  PORTLAND, OR                                      
 RELDT 201706,065,2017-07-14  SACRAMENTO                                        
 RELDT 201706,101,2017-07-14  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201706,059,2017-07-14  SAN ANTONIO                                       
*                                                                               
 RELDT 201706,135,2017-07-17  AUSTIN                                            
 RELDT 201706,166,2017-07-17  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201706,061,2017-07-17  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201706,049,2017-07-17  INDIANAPOLIS                                      
 RELDT 201706,107,2017-07-17  JACKSONVILLE                                      
 RELDT 201706,075,2017-07-17  MEMPHIS                                           
 RELDT 201706,043,2017-07-17  MILWAUKEE-RACINE                                  
 RELDT 201706,073,2017-07-17  NASHVILLE                                         
 RELDT 201706,109,2017-07-17  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201706,077,2017-07-17  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201706,115,2017-07-17  RALEIGH-DURHAM                                    
 RELDT 201706,299,2017-07-17  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201707,047,2017-08-07  ATLANTA                                           
 RELDT 201707,005,2017-08-07  CHICAGO                                           
 RELDT 201707,024,2017-08-07  DALLAS-FT. WORTH                                  
 RELDT 201707,033,2017-08-07  HOUSTON-GALVESTON                                 
 RELDT 201707,003,2017-08-07  LOS ANGELES                                       
 RELDT 201707,413,2017-08-07  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201707,321,2017-08-07  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201707,001,2017-08-07  NEW YORK                                          
 RELDT 201707,007,2017-08-07  PHILADELPHIA                                      
 RELDT 201707,379,2017-08-07  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201707,009,2017-08-07  SAN FRANCISCO                                     
 RELDT 201707,215,2017-08-07  SAN JOSE                                          
*                                                                               
 RELDT 201707,021,2017-08-08  BALTIMORE                                         
 RELDT 201707,013,2017-08-08  BOSTON                                            
 RELDT 201707,035,2017-08-08  DENVER-BOULDER                                    
 RELDT 201707,011,2017-08-08  DETROIT                                           
 RELDT 201707,429,2017-08-08  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201707,027,2017-08-08  MINNEAPOLIS-ST. PAUL                              
 RELDT 201707,057,2017-08-08  PHOENIX                                           
 RELDT 201707,063,2017-08-08  SAN DIEGO                                         
 RELDT 201707,039,2017-08-08  SEATTLE-TACOMA                                    
 RELDT 201707,017,2017-08-08  ST. LOUIS                                         
 RELDT 201707,087,2017-08-08  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201707,015,2017-08-08  WASHINGTON, DC                                    
*                                                                               
 RELDT 201707,093,2017-08-09  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201707,031,2017-08-09  CINCINNATI                                        
 RELDT 201707,019,2017-08-09  CLEVELAND                                         
 RELDT 201707,045,2017-08-09  COLUMBUS, OH                                      
 RELDT 201707,041,2017-08-09  KANSAS CITY                                       
 RELDT 201707,257,2017-08-09  LAS VEGAS                                         
 RELDT 201707,131,2017-08-09  ORLANDO                                           
 RELDT 201707,023,2017-08-09  PITTSBURGH, PA                                    
 RELDT 201707,051,2017-08-09  PORTLAND, OR                                      
 RELDT 201707,065,2017-08-09  SACRAMENTO                                        
 RELDT 201707,101,2017-08-09  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201707,059,2017-08-09  SAN ANTONIO                                       
*                                                                               
 RELDT 201707,135,2017-08-10  AUSTIN                                            
 RELDT 201707,166,2017-08-10  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201707,061,2017-08-10  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201707,049,2017-08-10  INDIANAPOLIS                                      
 RELDT 201707,107,2017-08-10  JACKSONVILLE                                      
 RELDT 201707,075,2017-08-10  MEMPHIS                                           
 RELDT 201707,043,2017-08-10  MILWAUKEE-RACINE                                  
 RELDT 201707,073,2017-08-10  NASHVILLE                                         
 RELDT 201707,109,2017-08-10  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201707,077,2017-08-10  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201707,115,2017-08-10  RALEIGH-DURHAM                                    
 RELDT 201707,299,2017-08-10  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201708,047,2017-09-05  ATLANTA                                           
 RELDT 201708,005,2017-09-05  CHICAGO                                           
 RELDT 201708,024,2017-09-05  DALLAS-FT. WORTH                                  
 RELDT 201708,033,2017-09-05  HOUSTON-GALVESTON                                 
 RELDT 201708,003,2017-09-05  LOS ANGELES                                       
 RELDT 201708,413,2017-09-05  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201708,321,2017-09-05  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201708,001,2017-09-05  NEW YORK                                          
 RELDT 201708,007,2017-09-05  PHILADELPHIA                                      
 RELDT 201708,379,2017-09-05  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201708,009,2017-09-05  SAN FRANCISCO                                     
 RELDT 201708,215,2017-09-05  SAN JOSE                                          
*                                                                               
 RELDT 201708,021,2017-09-06  BALTIMORE                                         
 RELDT 201708,013,2017-09-06  BOSTON                                            
 RELDT 201708,035,2017-09-06  DENVER-BOULDER                                    
 RELDT 201708,011,2017-09-06  DETROIT                                           
 RELDT 201708,429,2017-09-06  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201708,027,2017-09-06  MINNEAPOLIS-ST. PAUL                              
 RELDT 201708,057,2017-09-06  PHOENIX                                           
 RELDT 201708,063,2017-09-06  SAN DIEGO                                         
 RELDT 201708,039,2017-09-06  SEATTLE-TACOMA                                    
 RELDT 201708,017,2017-09-06  ST. LOUIS                                         
 RELDT 201708,087,2017-09-06  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201708,015,2017-09-06  WASHINGTON, DC                                    
*                                                                               
 RELDT 201708,093,2017-09-07  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201708,031,2017-09-07  CINCINNATI                                        
 RELDT 201708,019,2017-09-07  CLEVELAND                                         
 RELDT 201708,045,2017-09-07  COLUMBUS, OH                                      
 RELDT 201708,041,2017-09-07  KANSAS CITY                                       
 RELDT 201708,257,2017-09-07  LAS VEGAS                                         
 RELDT 201708,131,2017-09-07  ORLANDO                                           
 RELDT 201708,023,2017-09-07  PITTSBURGH, PA                                    
 RELDT 201708,051,2017-09-07  PORTLAND, OR                                      
 RELDT 201708,065,2017-09-07  SACRAMENTO                                        
 RELDT 201708,101,2017-09-07  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201708,059,2017-09-07  SAN ANTONIO                                       
*                                                                               
 RELDT 201708,135,2017-09-08  AUSTIN                                            
 RELDT 201708,166,2017-09-08  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201708,061,2017-09-08  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201708,049,2017-09-08  INDIANAPOLIS                                      
 RELDT 201708,107,2017-09-08  JACKSONVILLE                                      
 RELDT 201708,075,2017-09-08  MEMPHIS                                           
 RELDT 201708,043,2017-09-08  MILWAUKEE-RACINE                                  
 RELDT 201708,073,2017-09-08  NASHVILLE                                         
 RELDT 201708,109,2017-09-08  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201708,077,2017-09-08  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201708,115,2017-09-08  RALEIGH-DURHAM                                    
 RELDT 201708,299,2017-09-08  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201709,047,2017-09-29  ATLANTA                                           
 RELDT 201709,005,2017-09-29  CHICAGO                                           
 RELDT 201709,024,2017-09-29  DALLAS-FT. WORTH                                  
 RELDT 201709,033,2017-09-29  HOUSTON-GALVESTON                                 
 RELDT 201709,003,2017-09-29  LOS ANGELES                                       
 RELDT 201709,413,2017-09-29  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201709,321,2017-09-29  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201709,001,2017-09-29  NEW YORK                                          
 RELDT 201709,007,2017-09-29  PHILADELPHIA                                      
 RELDT 201709,379,2017-09-29  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201709,009,2017-09-29  SAN FRANCISCO                                     
 RELDT 201709,215,2017-09-29  SAN JOSE                                          
*                                                                               
 RELDT 201709,021,2017-09-29  BALTIMORE                                         
 RELDT 201709,013,2017-09-29  BOSTON                                            
 RELDT 201709,035,2017-09-29  DENVER-BOULDER                                    
 RELDT 201709,011,2017-09-29  DETROIT                                           
 RELDT 201709,429,2017-09-29  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201709,027,2017-09-29  MINNEAPOLIS-ST. PAUL                              
 RELDT 201709,057,2017-09-29  PHOENIX                                           
 RELDT 201709,063,2017-09-29  SAN DIEGO                                         
 RELDT 201709,039,2017-09-29  SEATTLE-TACOMA                                    
 RELDT 201709,017,2017-09-29  ST. LOUIS                                         
 RELDT 201709,087,2017-09-29  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201709,015,2017-09-29  WASHINGTON, DC                                    
*                                                                               
 RELDT 201709,093,2017-10-03  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201709,031,2017-10-03  CINCINNATI                                        
 RELDT 201709,019,2017-10-03  CLEVELAND                                         
 RELDT 201709,045,2017-10-03  COLUMBUS, OH                                      
 RELDT 201709,041,2017-10-03  KANSAS CITY                                       
 RELDT 201709,257,2017-10-03  LAS VEGAS                                         
 RELDT 201709,131,2017-10-03  ORLANDO                                           
 RELDT 201709,023,2017-10-03  PITTSBURGH, PA                                    
 RELDT 201709,051,2017-10-03  PORTLAND, OR                                      
 RELDT 201709,065,2017-10-03  SACRAMENTO                                        
 RELDT 201709,101,2017-10-03  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201709,059,2017-10-03  SAN ANTONIO                                       
*                                                                               
 RELDT 201709,135,2017-10-03  AUSTIN                                            
 RELDT 201709,166,2017-10-03  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201709,061,2017-10-03  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201709,049,2017-10-03  INDIANAPOLIS                                      
 RELDT 201709,107,2017-10-03  JACKSONVILLE                                      
 RELDT 201709,075,2017-10-03  MEMPHIS                                           
 RELDT 201709,043,2017-10-03  MILWAUKEE-RACINE                                  
 RELDT 201709,073,2017-10-03  NASHVILLE                                         
 RELDT 201709,109,2017-10-03  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201709,077,2017-10-03  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201709,115,2017-10-03  RALEIGH-DURHAM                                    
 RELDT 201709,299,2017-10-03  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201710,047,2017-10-30  ATLANTA                                           
 RELDT 201710,005,2017-10-30  CHICAGO                                           
 RELDT 201710,024,2017-10-30  DALLAS-FT. WORTH                                  
 RELDT 201710,033,2017-10-30  HOUSTON-GALVESTON                                 
 RELDT 201710,003,2017-10-30  LOS ANGELES                                       
 RELDT 201710,413,2017-10-30  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201710,321,2017-10-30  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201710,001,2017-10-30  NEW YORK                                          
 RELDT 201710,007,2017-10-30  PHILADELPHIA                                      
 RELDT 201710,379,2017-10-30  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201710,009,2017-10-30  SAN FRANCISCO                                     
 RELDT 201710,215,2017-10-30  SAN JOSE                                          
*                                                                               
 RELDT 201710,021,2017-10-31  BALTIMORE                                         
 RELDT 201710,013,2017-10-31  BOSTON                                            
 RELDT 201710,035,2017-10-31  DENVER-BOULDER                                    
 RELDT 201710,011,2017-10-31  DETROIT                                           
 RELDT 201710,429,2017-10-31  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201710,027,2017-10-31  MINNEAPOLIS-ST. PAUL                              
 RELDT 201710,057,2017-10-31  PHOENIX                                           
 RELDT 201710,063,2017-10-31  SAN DIEGO                                         
 RELDT 201710,039,2017-10-31  SEATTLE-TACOMA                                    
 RELDT 201710,017,2017-10-31  ST. LOUIS                                         
 RELDT 201710,087,2017-10-31  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201710,015,2017-10-31  WASHINGTON, DC                                    
*                                                                               
 RELDT 201710,093,2017-11-01  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201710,031,2017-11-01  CINCINNATI                                        
 RELDT 201710,019,2017-11-01  CLEVELAND                                         
 RELDT 201710,045,2017-11-01  COLUMBUS, OH                                      
 RELDT 201710,041,2017-11-01  KANSAS CITY                                       
 RELDT 201710,257,2017-11-01  LAS VEGAS                                         
 RELDT 201710,131,2017-11-01  ORLANDO                                           
 RELDT 201710,023,2017-11-01  PITTSBURGH, PA                                    
 RELDT 201710,051,2017-11-01  PORTLAND, OR                                      
 RELDT 201710,065,2017-11-01  SACRAMENTO                                        
 RELDT 201710,101,2017-11-01  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201710,059,2017-11-01  SAN ANTONIO                                       
*                                                                               
 RELDT 201710,135,2017-11-02  AUSTIN                                            
 RELDT 201710,166,2017-11-02  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201710,061,2017-11-02  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201710,049,2017-11-02  INDIANAPOLIS                                      
 RELDT 201710,107,2017-11-02  JACKSONVILLE                                      
 RELDT 201710,075,2017-11-02  MEMPHIS                                           
 RELDT 201710,043,2017-11-02  MILWAUKEE-RACINE                                  
 RELDT 201710,073,2017-11-02  NASHVILLE                                         
 RELDT 201710,109,2017-11-02  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201710,077,2017-11-02  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201710,115,2017-11-02  RALEIGH-DURHAM                                    
 RELDT 201710,299,2017-11-02  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201711,047,2017-11-29  ATLANTA                                           
 RELDT 201711,005,2017-11-29  CHICAGO                                           
 RELDT 201711,024,2017-11-29  DALLAS-FT. WORTH                                  
 RELDT 201711,033,2017-11-29  HOUSTON-GALVESTON                                 
 RELDT 201711,003,2017-11-29  LOS ANGELES                                       
 RELDT 201711,413,2017-11-29  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201711,321,2017-11-29  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201711,001,2017-11-29  NEW YORK                                          
 RELDT 201711,007,2017-11-29  PHILADELPHIA                                      
 RELDT 201711,379,2017-11-29  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201711,009,2017-11-29  SAN FRANCISCO                                     
 RELDT 201711,215,2017-11-29  SAN JOSE                                          
*                                                                               
 RELDT 201711,021,2017-11-30  BALTIMORE                                         
 RELDT 201711,013,2017-11-30  BOSTON                                            
 RELDT 201711,035,2017-11-30  DENVER-BOULDER                                    
 RELDT 201711,011,2017-11-30  DETROIT                                           
 RELDT 201711,429,2017-11-30  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201711,027,2017-11-30  MINNEAPOLIS-ST. PAUL                              
 RELDT 201711,057,2017-11-30  PHOENIX                                           
 RELDT 201711,063,2017-11-30  SAN DIEGO                                         
 RELDT 201711,039,2017-11-30  SEATTLE-TACOMA                                    
 RELDT 201711,017,2017-11-30  ST. LOUIS                                         
 RELDT 201711,087,2017-11-30  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201711,015,2017-11-30  WASHINGTON, DC                                    
*                                                                               
 RELDT 201711,093,2017-12-01  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201711,031,2017-12-01  CINCINNATI                                        
 RELDT 201711,019,2017-12-01  CLEVELAND                                         
 RELDT 201711,045,2017-12-01  COLUMBUS, OH                                      
 RELDT 201711,041,2017-12-01  KANSAS CITY                                       
 RELDT 201711,257,2017-12-01  LAS VEGAS                                         
 RELDT 201711,131,2017-12-01  ORLANDO                                           
 RELDT 201711,023,2017-12-01  PITTSBURGH, PA                                    
 RELDT 201711,051,2017-12-01  PORTLAND, OR                                      
 RELDT 201711,065,2017-12-01  SACRAMENTO                                        
 RELDT 201711,101,2017-12-01  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201711,059,2017-12-01  SAN ANTONIO                                       
*                                                                               
 RELDT 201711,135,2017-12-04  AUSTIN                                            
 RELDT 201711,166,2017-12-04  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201711,061,2017-12-04  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201711,049,2017-12-04  INDIANAPOLIS                                      
 RELDT 201711,107,2017-12-04  JACKSONVILLE                                      
 RELDT 201711,075,2017-12-04  MEMPHIS                                           
 RELDT 201711,043,2017-12-04  MILWAUKEE-RACINE                                  
 RELDT 201711,073,2017-12-04  NASHVILLE                                         
 RELDT 201711,109,2017-12-04  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201711,077,2017-12-04  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201711,115,2017-12-04  RALEIGH-DURHAM                                    
 RELDT 201711,299,2017-12-04  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201712,047,2017-12-26  ATLANTA                                           
 RELDT 201712,005,2017-12-26  CHICAGO                                           
 RELDT 201712,024,2017-12-26  DALLAS-FT. WORTH                                  
 RELDT 201712,033,2017-12-26  HOUSTON-GALVESTON                                 
 RELDT 201712,003,2017-12-26  LOS ANGELES                                       
 RELDT 201712,413,2017-12-26  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201712,321,2017-12-26  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201712,001,2017-12-26  NEW YORK                                          
 RELDT 201712,007,2017-12-26  PHILADELPHIA                                      
 RELDT 201712,379,2017-12-26  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201712,009,2017-12-26  SAN FRANCISCO                                     
 RELDT 201712,215,2017-12-26  SAN JOSE                                          
*                                                                               
 RELDT 201712,021,2017-12-27  BALTIMORE                                         
 RELDT 201712,013,2017-12-27  BOSTON                                            
 RELDT 201712,035,2017-12-27  DENVER-BOULDER                                    
 RELDT 201712,011,2017-12-27  DETROIT                                           
 RELDT 201712,429,2017-12-27  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201712,027,2017-12-27  MINNEAPOLIS-ST. PAUL                              
 RELDT 201712,057,2017-12-27  PHOENIX                                           
 RELDT 201712,063,2017-12-27  SAN DIEGO                                         
 RELDT 201712,039,2017-12-27  SEATTLE-TACOMA                                    
 RELDT 201712,017,2017-12-27  ST. LOUIS                                         
 RELDT 201712,087,2017-12-27  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201712,015,2017-12-27  WASHINGTON, DC                                    
*                                                                               
 RELDT 201712,093,2017-12-28  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201712,031,2017-12-28  CINCINNATI                                        
 RELDT 201712,019,2017-12-28  CLEVELAND                                         
 RELDT 201712,045,2017-12-28  COLUMBUS, OH                                      
 RELDT 201712,041,2017-12-28  KANSAS CITY                                       
 RELDT 201712,257,2017-12-28  LAS VEGAS                                         
 RELDT 201712,131,2017-12-28  ORLANDO                                           
 RELDT 201712,023,2017-12-28  PITTSBURGH, PA                                    
 RELDT 201712,051,2017-12-28  PORTLAND, OR                                      
 RELDT 201712,065,2017-12-28  SACRAMENTO                                        
 RELDT 201712,101,2017-12-28  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201712,059,2017-12-28  SAN ANTONIO                                       
*                                                                               
 RELDT 201712,135,2017-12-29  AUSTIN                                            
 RELDT 201712,166,2017-12-29  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201712,061,2017-12-29  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201712,049,2017-12-29  INDIANAPOLIS                                      
 RELDT 201712,107,2017-12-29  JACKSONVILLE                                      
 RELDT 201712,075,2017-12-29  MEMPHIS                                           
 RELDT 201712,043,2017-12-29  MILWAUKEE-RACINE                                  
 RELDT 201712,073,2017-12-29  NASHVILLE                                         
 RELDT 201712,109,2017-12-29  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201712,077,2017-12-29  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201712,115,2017-12-29  RALEIGH-DURHAM                                    
 RELDT 201712,299,2017-12-29  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
* HOLIDAY BOOK (2017)                                                           
*                                                                               
*                                                                               
 RELDT 201723,047,2018-01-23  ATLANTA                                           
 RELDT 201723,005,2018-01-23  CHICAGO                                           
 RELDT 201723,024,2018-01-23  DALLAS-FT. WORTH                                  
 RELDT 201723,033,2018-01-23  HOUSTON-GALVESTON                                 
 RELDT 201723,003,2018-01-23  LOS ANGELES                                       
 RELDT 201723,413,2018-01-23  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201723,321,2018-01-23  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201723,001,2018-01-23  NEW YORK                                          
 RELDT 201723,007,2018-01-23  PHILADELPHIA                                      
 RELDT 201723,379,2018-01-23  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201723,009,2018-01-23  SAN FRANCISCO                                     
 RELDT 201723,215,2018-01-23  SAN JOSE                                          
*                                                                               
 RELDT 201723,021,2018-01-24  BALTIMORE                                         
 RELDT 201723,013,2018-01-24  BOSTON                                            
 RELDT 201723,035,2018-01-24  DENVER-BOULDER                                    
 RELDT 201723,011,2018-01-24  DETROIT                                           
 RELDT 201723,429,2018-01-24  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201723,027,2018-01-24  MINNEAPOLIS-ST. PAUL                              
 RELDT 201723,057,2018-01-24  PHOENIX                                           
 RELDT 201723,063,2018-01-24  SAN DIEGO                                         
 RELDT 201723,039,2018-01-24  SEATTLE-TACOMA                                    
 RELDT 201723,017,2018-01-24  ST. LOUIS                                         
 RELDT 201723,087,2018-01-24  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201723,015,2018-01-24  WASHINGTON, DC                                    
*                                                                               
 RELDT 201723,093,2018-01-25  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201723,031,2018-01-25  CINCINNATI                                        
 RELDT 201723,019,2018-01-25  CLEVELAND                                         
 RELDT 201723,045,2018-01-25  COLUMBUS, OH                                      
 RELDT 201723,041,2018-01-25  KANSAS CITY                                       
 RELDT 201723,257,2018-01-25  LAS VEGAS                                         
 RELDT 201723,131,2018-01-25  ORLANDO                                           
 RELDT 201723,023,2018-01-25  PITTSBURGH, PA                                    
 RELDT 201723,051,2018-01-25  PORTLAND, OR                                      
 RELDT 201723,065,2018-01-25  SACRAMENTO                                        
 RELDT 201723,101,2018-01-25  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201723,059,2018-01-25  SAN ANTONIO                                       
*                                                                               
 RELDT 201723,135,2018-01-26  AUSTIN                                            
 RELDT 201723,166,2018-01-26  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201723,061,2018-01-26  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201723,049,2018-01-26  INDIANAPOLIS                                      
 RELDT 201723,107,2018-01-26  JACKSONVILLE                                      
 RELDT 201723,075,2018-01-26  MEMPHIS                                           
 RELDT 201723,043,2018-01-26  MILWAUKEE-RACINE                                  
 RELDT 201723,073,2018-01-26  NASHVILLE                                         
 RELDT 201723,109,2018-01-26  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201723,077,2018-01-26  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201723,115,2018-01-26  RALEIGH-DURHAM                                    
 RELDT 201723,299,2018-01-26  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
* PPM 2018 MARKETS                                                              
*                                                                               
*                                                                               
 RELDT 201801,047,2018-02-20  ATLANTA                                           
 RELDT 201801,005,2018-02-20  CHICAGO                                           
 RELDT 201801,024,2018-02-20  DALLAS-FT. WORTH                                  
 RELDT 201801,033,2018-02-20  HOUSTON-GALVESTON                                 
 RELDT 201801,003,2018-02-20  LOS ANGELES                                       
 RELDT 201801,413,2018-02-20  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201801,321,2018-02-20  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201801,001,2018-02-20  NEW YORK                                          
 RELDT 201801,007,2018-02-20  PHILADELPHIA                                      
 RELDT 201801,379,2018-02-20  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201801,009,2018-02-20  SAN FRANCISCO                                     
 RELDT 201801,215,2018-02-20  SAN JOSE                                          
*                                                                               
 RELDT 201801,021,2018-02-21  BALTIMORE                                         
 RELDT 201801,013,2018-02-21  BOSTON                                            
 RELDT 201801,035,2018-02-21  DENVER-BOULDER                                    
 RELDT 201801,011,2018-02-21  DETROIT                                           
 RELDT 201801,429,2018-02-21  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201801,027,2018-02-21  MINNEAPOLIS-ST. PAUL                              
 RELDT 201801,057,2018-02-21  PHOENIX                                           
 RELDT 201801,063,2018-02-21  SAN DIEGO                                         
 RELDT 201801,039,2018-02-21  SEATTLE-TACOMA                                    
 RELDT 201801,017,2018-02-21  ST. LOUIS                                         
 RELDT 201801,087,2018-02-21  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201801,015,2018-02-21  WASHINGTON, DC                                    
*                                                                               
 RELDT 201801,093,2018-02-22  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201801,031,2018-02-22  CINCINNATI                                        
 RELDT 201801,019,2018-02-22  CLEVELAND                                         
 RELDT 201801,045,2018-02-22  COLUMBUS, OH                                      
 RELDT 201801,041,2018-02-22  KANSAS CITY                                       
 RELDT 201801,257,2018-02-22  LAS VEGAS                                         
 RELDT 201801,131,2018-02-22  ORLANDO                                           
 RELDT 201801,023,2018-02-22  PITTSBURGH, PA                                    
 RELDT 201801,051,2018-02-22  PORTLAND, OR                                      
 RELDT 201801,065,2018-02-22  SACRAMENTO                                        
 RELDT 201801,101,2018-02-22  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201801,059,2018-02-22  SAN ANTONIO                                       
*                                                                               
 RELDT 201801,135,2018-02-23  AUSTIN                                            
 RELDT 201801,166,2018-02-23  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201801,061,2018-02-23  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201801,049,2018-02-23  INDIANAPOLIS                                      
 RELDT 201801,107,2018-02-23  JACKSONVILLE                                      
 RELDT 201801,075,2018-02-23  MEMPHIS                                           
 RELDT 201801,043,2018-02-23  MILWAUKEE-RACINE                                  
 RELDT 201801,073,2018-02-23  NASHVILLE                                         
 RELDT 201801,109,2018-02-23  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201801,077,2018-02-23  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201801,115,2018-02-23  RALEIGH-DURHAM                                    
 RELDT 201801,299,2018-02-23  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201802,047,2018-03-19  ATLANTA                                           
 RELDT 201802,005,2018-03-19  CHICAGO                                           
 RELDT 201802,024,2018-03-19  DALLAS-FT. WORTH                                  
 RELDT 201802,033,2018-03-19  HOUSTON-GALVESTON                                 
 RELDT 201802,003,2018-03-19  LOS ANGELES                                       
 RELDT 201802,413,2018-03-19  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201802,321,2018-03-19  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201802,001,2018-03-19  NEW YORK                                          
 RELDT 201802,007,2018-03-19  PHILADELPHIA                                      
 RELDT 201802,379,2018-03-19  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201802,009,2018-03-19  SAN FRANCISCO                                     
 RELDT 201802,215,2018-03-19  SAN JOSE                                          
*                                                                               
 RELDT 201802,021,2018-03-20  BALTIMORE                                         
 RELDT 201802,013,2018-03-20  BOSTON                                            
 RELDT 201802,035,2018-03-20  DENVER-BOULDER                                    
 RELDT 201802,011,2018-03-20  DETROIT                                           
 RELDT 201802,429,2018-03-20  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201802,027,2018-03-20  MINNEAPOLIS-ST. PAUL                              
 RELDT 201802,057,2018-03-20  PHOENIX                                           
 RELDT 201802,063,2018-03-20  SAN DIEGO                                         
 RELDT 201802,039,2018-03-20  SEATTLE-TACOMA                                    
 RELDT 201802,017,2018-03-20  ST. LOUIS                                         
 RELDT 201802,087,2018-03-20  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201802,015,2018-03-20  WASHINGTON, DC                                    
*                                                                               
 RELDT 201802,093,2018-03-21  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201802,031,2018-03-21  CINCINNATI                                        
 RELDT 201802,019,2018-03-21  CLEVELAND                                         
 RELDT 201802,045,2018-03-21  COLUMBUS, OH                                      
 RELDT 201802,041,2018-03-21  KANSAS CITY                                       
 RELDT 201802,257,2018-03-21  LAS VEGAS                                         
 RELDT 201802,131,2018-03-21  ORLANDO                                           
 RELDT 201802,023,2018-03-21  PITTSBURGH, PA                                    
 RELDT 201802,051,2018-03-21  PORTLAND, OR                                      
 RELDT 201802,065,2018-03-21  SACRAMENTO                                        
 RELDT 201802,101,2018-03-21  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201802,059,2018-03-21  SAN ANTONIO                                       
*                                                                               
 RELDT 201802,135,2018-03-22  AUSTIN                                            
 RELDT 201802,166,2018-03-22  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201802,061,2018-03-22  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201802,049,2018-03-22  INDIANAPOLIS                                      
 RELDT 201802,107,2018-03-22  JACKSONVILLE                                      
 RELDT 201802,075,2018-03-22  MEMPHIS                                           
 RELDT 201802,043,2018-03-22  MILWAUKEE-RACINE                                  
 RELDT 201802,073,2018-03-22  NASHVILLE                                         
 RELDT 201802,109,2018-03-22  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201802,077,2018-03-22  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201802,115,2018-03-22  RALEIGH-DURHAM                                    
 RELDT 201802,299,2018-03-22  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201803,047,2018-04-16  ATLANTA                                           
 RELDT 201803,005,2018-04-16  CHICAGO                                           
 RELDT 201803,024,2018-04-16  DALLAS-FT. WORTH                                  
 RELDT 201803,033,2018-04-16  HOUSTON-GALVESTON                                 
 RELDT 201803,003,2018-04-16  LOS ANGELES                                       
 RELDT 201803,413,2018-04-16  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201803,321,2018-04-16  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201803,001,2018-04-16  NEW YORK                                          
 RELDT 201803,007,2018-04-16  PHILADELPHIA                                      
 RELDT 201803,379,2018-04-16  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201803,009,2018-04-16  SAN FRANCISCO                                     
 RELDT 201803,215,2018-04-16  SAN JOSE                                          
*                                                                               
 RELDT 201803,021,2018-04-17  BALTIMORE                                         
 RELDT 201803,013,2018-04-17  BOSTON                                            
 RELDT 201803,035,2018-04-17  DENVER-BOULDER                                    
 RELDT 201803,011,2018-04-17  DETROIT                                           
 RELDT 201803,429,2018-04-17  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201803,027,2018-04-17  MINNEAPOLIS-ST. PAUL                              
 RELDT 201803,057,2018-04-17  PHOENIX                                           
 RELDT 201803,063,2018-04-17  SAN DIEGO                                         
 RELDT 201803,039,2018-04-17  SEATTLE-TACOMA                                    
 RELDT 201803,017,2018-04-17  ST. LOUIS                                         
 RELDT 201803,087,2018-04-17  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201803,015,2018-04-17  WASHINGTON, DC                                    
*                                                                               
 RELDT 201803,093,2018-04-18  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201803,031,2018-04-18  CINCINNATI                                        
 RELDT 201803,019,2018-04-18  CLEVELAND                                         
 RELDT 201803,045,2018-04-18  COLUMBUS, OH                                      
 RELDT 201803,041,2018-04-18  KANSAS CITY                                       
 RELDT 201803,257,2018-04-18  LAS VEGAS                                         
 RELDT 201803,131,2018-04-18  ORLANDO                                           
 RELDT 201803,023,2018-04-18  PITTSBURGH, PA                                    
 RELDT 201803,051,2018-04-18  PORTLAND, OR                                      
 RELDT 201803,065,2018-04-18  SACRAMENTO                                        
 RELDT 201803,101,2018-04-18  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201803,059,2018-04-18  SAN ANTONIO                                       
*                                                                               
 RELDT 201803,135,2018-04-19  AUSTIN                                            
 RELDT 201803,166,2018-04-19  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201803,061,2018-04-19  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201803,049,2018-04-19  INDIANAPOLIS                                      
 RELDT 201803,107,2018-04-19  JACKSONVILLE                                      
 RELDT 201803,075,2018-04-19  MEMPHIS                                           
 RELDT 201803,043,2018-04-19  MILWAUKEE-RACINE                                  
 RELDT 201803,073,2018-04-19  NASHVILLE                                         
 RELDT 201803,109,2018-04-19  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201803,077,2018-04-19  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201803,115,2018-04-19  RALEIGH-DURHAM                                    
 RELDT 201803,299,2018-04-19  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201804,047,2018-05-14  ATLANTA                                           
 RELDT 201804,005,2018-05-14  CHICAGO                                           
 RELDT 201804,024,2018-05-14  DALLAS-FT. WORTH                                  
 RELDT 201804,033,2018-05-14  HOUSTON-GALVESTON                                 
 RELDT 201804,003,2018-05-14  LOS ANGELES                                       
 RELDT 201804,413,2018-05-14  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201804,321,2018-05-14  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201804,001,2018-05-14  NEW YORK                                          
 RELDT 201804,007,2018-05-14  PHILADELPHIA                                      
 RELDT 201804,379,2018-05-14  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201804,009,2018-05-14  SAN FRANCISCO                                     
 RELDT 201804,215,2018-05-14  SAN JOSE                                          
*                                                                               
 RELDT 201804,021,2018-05-15  BALTIMORE                                         
 RELDT 201804,013,2018-05-15  BOSTON                                            
 RELDT 201804,035,2018-05-15  DENVER-BOULDER                                    
 RELDT 201804,011,2018-05-15  DETROIT                                           
 RELDT 201804,429,2018-05-15  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201804,027,2018-05-15  MINNEAPOLIS-ST. PAUL                              
 RELDT 201804,057,2018-05-15  PHOENIX                                           
 RELDT 201804,063,2018-05-15  SAN DIEGO                                         
 RELDT 201804,039,2018-05-15  SEATTLE-TACOMA                                    
 RELDT 201804,017,2018-05-15  ST. LOUIS                                         
 RELDT 201804,087,2018-05-15  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201804,015,2018-05-15  WASHINGTON, DC                                    
*                                                                               
 RELDT 201804,093,2018-05-16  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201804,031,2018-05-16  CINCINNATI                                        
 RELDT 201804,019,2018-05-16  CLEVELAND                                         
 RELDT 201804,045,2018-05-16  COLUMBUS, OH                                      
 RELDT 201804,041,2018-05-16  KANSAS CITY                                       
 RELDT 201804,257,2018-05-16  LAS VEGAS                                         
 RELDT 201804,131,2018-05-16  ORLANDO                                           
 RELDT 201804,023,2018-05-16  PITTSBURGH, PA                                    
 RELDT 201804,051,2018-05-16  PORTLAND, OR                                      
 RELDT 201804,065,2018-05-16  SACRAMENTO                                        
 RELDT 201804,101,2018-05-16  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201804,059,2018-05-16  SAN ANTONIO                                       
*                                                                               
 RELDT 201804,135,2018-05-17  AUSTIN                                            
 RELDT 201804,166,2018-05-17  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201804,061,2018-05-17  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201804,049,2018-05-17  INDIANAPOLIS                                      
 RELDT 201804,107,2018-05-17  JACKSONVILLE                                      
 RELDT 201804,075,2018-05-17  MEMPHIS                                           
 RELDT 201804,043,2018-05-17  MILWAUKEE-RACINE                                  
 RELDT 201804,073,2018-05-17  NASHVILLE                                         
 RELDT 201804,109,2018-05-17  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201804,077,2018-05-17  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201804,115,2018-05-17  RALEIGH-DURHAM                                    
 RELDT 201804,299,2018-05-17  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201805,047,2018-06-11  ATLANTA                                           
 RELDT 201805,005,2018-06-11  CHICAGO                                           
 RELDT 201805,024,2018-06-11  DALLAS-FT. WORTH                                  
 RELDT 201805,033,2018-06-11  HOUSTON-GALVESTON                                 
 RELDT 201805,003,2018-06-11  LOS ANGELES                                       
 RELDT 201805,413,2018-06-11  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201805,321,2018-06-11  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201805,001,2018-06-11  NEW YORK                                          
 RELDT 201805,007,2018-06-11  PHILADELPHIA                                      
 RELDT 201805,379,2018-06-11  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201805,009,2018-06-11  SAN FRANCISCO                                     
 RELDT 201805,215,2018-06-11  SAN JOSE                                          
*                                                                               
 RELDT 201805,021,2018-06-12  BALTIMORE                                         
 RELDT 201805,013,2018-06-12  BOSTON                                            
 RELDT 201805,035,2018-06-12  DENVER-BOULDER                                    
 RELDT 201805,011,2018-06-12  DETROIT                                           
 RELDT 201805,429,2018-06-12  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201805,027,2018-06-12  MINNEAPOLIS-ST. PAUL                              
 RELDT 201805,057,2018-06-12  PHOENIX                                           
 RELDT 201805,063,2018-06-12  SAN DIEGO                                         
 RELDT 201805,039,2018-06-12  SEATTLE-TACOMA                                    
 RELDT 201805,017,2018-06-12  ST. LOUIS                                         
 RELDT 201805,087,2018-06-12  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201805,015,2018-06-12  WASHINGTON, DC                                    
*                                                                               
 RELDT 201805,093,2018-06-13  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201805,031,2018-06-13  CINCINNATI                                        
 RELDT 201805,019,2018-06-13  CLEVELAND                                         
 RELDT 201805,045,2018-06-13  COLUMBUS, OH                                      
 RELDT 201805,041,2018-06-13  KANSAS CITY                                       
 RELDT 201805,257,2018-06-13  LAS VEGAS                                         
 RELDT 201805,131,2018-06-13  ORLANDO                                           
 RELDT 201805,023,2018-06-13  PITTSBURGH, PA                                    
 RELDT 201805,051,2018-06-13  PORTLAND, OR                                      
 RELDT 201805,065,2018-06-13  SACRAMENTO                                        
 RELDT 201805,101,2018-06-13  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201805,059,2018-06-13  SAN ANTONIO                                       
*                                                                               
 RELDT 201805,135,2018-06-14  AUSTIN                                            
 RELDT 201805,166,2018-06-14  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201805,061,2018-06-14  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201805,049,2018-06-14  INDIANAPOLIS                                      
 RELDT 201805,107,2018-06-14  JACKSONVILLE                                      
 RELDT 201805,075,2018-06-14  MEMPHIS                                           
 RELDT 201805,043,2018-06-14  MILWAUKEE-RACINE                                  
 RELDT 201805,073,2018-06-14  NASHVILLE                                         
 RELDT 201805,109,2018-06-14  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201805,077,2018-06-14  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201805,115,2018-06-14  RALEIGH-DURHAM                                    
 RELDT 201805,299,2018-06-14  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201806,047,2018-07-10  ATLANTA                                           
 RELDT 201806,005,2018-07-10  CHICAGO                                           
 RELDT 201806,024,2018-07-10  DALLAS-FT. WORTH                                  
 RELDT 201806,033,2018-07-10  HOUSTON-GALVESTON                                 
 RELDT 201806,003,2018-07-10  LOS ANGELES                                       
 RELDT 201806,413,2018-07-10  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201806,321,2018-07-10  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201806,001,2018-07-10  NEW YORK                                          
 RELDT 201806,007,2018-07-10  PHILADELPHIA                                      
 RELDT 201806,379,2018-07-10  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201806,009,2018-07-10  SAN FRANCISCO                                     
 RELDT 201806,215,2018-07-10  SAN JOSE                                          
*                                                                               
 RELDT 201806,021,2018-07-11  BALTIMORE                                         
 RELDT 201806,013,2018-07-11  BOSTON                                            
 RELDT 201806,035,2018-07-11  DENVER-BOULDER                                    
 RELDT 201806,011,2018-07-11  DETROIT                                           
 RELDT 201806,429,2018-07-11  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201806,027,2018-07-11  MINNEAPOLIS-ST. PAUL                              
 RELDT 201806,057,2018-07-11  PHOENIX                                           
 RELDT 201806,063,2018-07-11  SAN DIEGO                                         
 RELDT 201806,039,2018-07-11  SEATTLE-TACOMA                                    
 RELDT 201806,017,2018-07-11  ST. LOUIS                                         
 RELDT 201806,087,2018-07-11  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201806,015,2018-07-11  WASHINGTON, DC                                    
*                                                                               
 RELDT 201806,093,2018-07-12  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201806,031,2018-07-12  CINCINNATI                                        
 RELDT 201806,019,2018-07-12  CLEVELAND                                         
 RELDT 201806,045,2018-07-12  COLUMBUS, OH                                      
 RELDT 201806,041,2018-07-12  KANSAS CITY                                       
 RELDT 201806,257,2018-07-12  LAS VEGAS                                         
 RELDT 201806,131,2018-07-12  ORLANDO                                           
 RELDT 201806,023,2018-07-12  PITTSBURGH, PA                                    
 RELDT 201806,051,2018-07-12  PORTLAND, OR                                      
 RELDT 201806,065,2018-07-12  SACRAMENTO                                        
 RELDT 201806,101,2018-07-12  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201806,059,2018-07-12  SAN ANTONIO                                       
*                                                                               
 RELDT 201806,135,2018-07-13  AUSTIN                                            
 RELDT 201806,166,2018-07-13  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201806,061,2018-07-13  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201806,049,2018-07-13  INDIANAPOLIS                                      
 RELDT 201806,107,2018-07-13  JACKSONVILLE                                      
 RELDT 201806,075,2018-07-13  MEMPHIS                                           
 RELDT 201806,043,2018-07-13  MILWAUKEE-RACINE                                  
 RELDT 201806,073,2018-07-13  NASHVILLE                                         
 RELDT 201806,109,2018-07-13  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201806,077,2018-07-13  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201806,115,2018-07-13  RALEIGH-DURHAM                                    
 RELDT 201806,299,2018-07-13  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201807,047,2018-08-06  ATLANTA                                           
 RELDT 201807,005,2018-08-06  CHICAGO                                           
 RELDT 201807,024,2018-08-06  DALLAS-FT. WORTH                                  
 RELDT 201807,033,2018-08-06  HOUSTON-GALVESTON                                 
 RELDT 201807,003,2018-08-06  LOS ANGELES                                       
 RELDT 201807,413,2018-08-06  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201807,321,2018-08-06  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201807,001,2018-08-06  NEW YORK                                          
 RELDT 201807,007,2018-08-06  PHILADELPHIA                                      
 RELDT 201807,379,2018-08-06  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201807,009,2018-08-06  SAN FRANCISCO                                     
 RELDT 201807,215,2018-08-06  SAN JOSE                                          
*                                                                               
 RELDT 201807,021,2018-08-07  BALTIMORE                                         
 RELDT 201807,013,2018-08-07  BOSTON                                            
 RELDT 201807,035,2018-08-07  DENVER-BOULDER                                    
 RELDT 201807,011,2018-08-07  DETROIT                                           
 RELDT 201807,429,2018-08-07  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201807,027,2018-08-07  MINNEAPOLIS-ST. PAUL                              
 RELDT 201807,057,2018-08-07  PHOENIX                                           
 RELDT 201807,063,2018-08-07  SAN DIEGO                                         
 RELDT 201807,039,2018-08-07  SEATTLE-TACOMA                                    
 RELDT 201807,017,2018-08-07  ST. LOUIS                                         
 RELDT 201807,087,2018-08-07  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201807,015,2018-08-07  WASHINGTON, DC                                    
*                                                                               
 RELDT 201807,093,2018-08-08  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201807,031,2018-08-08  CINCINNATI                                        
 RELDT 201807,019,2018-08-08  CLEVELAND                                         
 RELDT 201807,045,2018-08-08  COLUMBUS, OH                                      
 RELDT 201807,041,2018-08-08  KANSAS CITY                                       
 RELDT 201807,257,2018-08-08  LAS VEGAS                                         
 RELDT 201807,131,2018-08-08  ORLANDO                                           
 RELDT 201807,023,2018-08-08  PITTSBURGH, PA                                    
 RELDT 201807,051,2018-08-08  PORTLAND, OR                                      
 RELDT 201807,065,2018-08-08  SACRAMENTO                                        
 RELDT 201807,101,2018-08-08  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201807,059,2018-08-08  SAN ANTONIO                                       
*                                                                               
 RELDT 201807,135,2018-08-09  AUSTIN                                            
 RELDT 201807,166,2018-08-09  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201807,061,2018-08-09  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201807,049,2018-08-09  INDIANAPOLIS                                      
 RELDT 201807,107,2018-08-09  JACKSONVILLE                                      
 RELDT 201807,075,2018-08-09  MEMPHIS                                           
 RELDT 201807,043,2018-08-09  MILWAUKEE-RACINE                                  
 RELDT 201807,073,2018-08-09  NASHVILLE                                         
 RELDT 201807,109,2018-08-09  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201807,077,2018-08-09  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201807,115,2018-08-09  RALEIGH-DURHAM                                    
 RELDT 201807,299,2018-08-09  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201808,047,2018-09-04  ATLANTA                                           
 RELDT 201808,005,2018-09-04  CHICAGO                                           
 RELDT 201808,024,2018-09-04  DALLAS-FT. WORTH                                  
 RELDT 201808,033,2018-09-04  HOUSTON-GALVESTON                                 
 RELDT 201808,003,2018-09-04  LOS ANGELES                                       
 RELDT 201808,413,2018-09-04  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201808,321,2018-09-04  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201808,001,2018-09-04  NEW YORK                                          
 RELDT 201808,007,2018-09-04  PHILADELPHIA                                      
 RELDT 201808,379,2018-09-04  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201808,009,2018-09-04  SAN FRANCISCO                                     
 RELDT 201808,215,2018-09-04  SAN JOSE                                          
*                                                                               
 RELDT 201808,021,2018-09-05  BALTIMORE                                         
 RELDT 201808,013,2018-09-05  BOSTON                                            
 RELDT 201808,035,2018-09-05  DENVER-BOULDER                                    
 RELDT 201808,011,2018-09-05  DETROIT                                           
 RELDT 201808,429,2018-09-05  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201808,027,2018-09-05  MINNEAPOLIS-ST. PAUL                              
 RELDT 201808,057,2018-09-05  PHOENIX                                           
 RELDT 201808,063,2018-09-05  SAN DIEGO                                         
 RELDT 201808,039,2018-09-05  SEATTLE-TACOMA                                    
 RELDT 201808,017,2018-09-05  ST. LOUIS                                         
 RELDT 201808,087,2018-09-05  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201808,015,2018-09-05  WASHINGTON, DC                                    
*                                                                               
 RELDT 201808,093,2018-09-06  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201808,031,2018-09-06  CINCINNATI                                        
 RELDT 201808,019,2018-09-06  CLEVELAND                                         
 RELDT 201808,045,2018-09-06  COLUMBUS, OH                                      
 RELDT 201808,041,2018-09-06  KANSAS CITY                                       
 RELDT 201808,257,2018-09-06  LAS VEGAS                                         
 RELDT 201808,131,2018-09-06  ORLANDO                                           
 RELDT 201808,023,2018-09-06  PITTSBURGH, PA                                    
 RELDT 201808,051,2018-09-06  PORTLAND, OR                                      
 RELDT 201808,065,2018-09-06  SACRAMENTO                                        
 RELDT 201808,101,2018-09-06  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201808,059,2018-09-06  SAN ANTONIO                                       
*                                                                               
 RELDT 201808,135,2018-09-07  AUSTIN                                            
 RELDT 201808,166,2018-09-07  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201808,061,2018-09-07  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201808,049,2018-09-07  INDIANAPOLIS                                      
 RELDT 201808,107,2018-09-07  JACKSONVILLE                                      
 RELDT 201808,075,2018-09-07  MEMPHIS                                           
 RELDT 201808,043,2018-09-07  MILWAUKEE-RACINE                                  
 RELDT 201808,073,2018-09-07  NASHVILLE                                         
 RELDT 201808,109,2018-09-07  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201808,077,2018-09-07  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201808,115,2018-09-07  RALEIGH-DURHAM                                    
 RELDT 201808,299,2018-09-07  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201809,047,2018-10-01  ATLANTA                                           
 RELDT 201809,005,2018-10-01  CHICAGO                                           
 RELDT 201809,024,2018-10-01  DALLAS-FT. WORTH                                  
 RELDT 201809,033,2018-10-01  HOUSTON-GALVESTON                                 
 RELDT 201809,003,2018-10-01  LOS ANGELES                                       
 RELDT 201809,413,2018-10-01  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201809,321,2018-10-01  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201809,001,2018-10-01  NEW YORK                                          
 RELDT 201809,007,2018-10-01  PHILADELPHIA                                      
 RELDT 201809,379,2018-10-01  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201809,009,2018-10-01  SAN FRANCISCO                                     
 RELDT 201809,215,2018-10-01  SAN JOSE                                          
*                                                                               
 RELDT 201809,021,2018-10-02  BALTIMORE                                         
 RELDT 201809,013,2018-10-02  BOSTON                                            
 RELDT 201809,035,2018-10-02  DENVER-BOULDER                                    
 RELDT 201809,011,2018-10-02  DETROIT                                           
 RELDT 201809,429,2018-10-02  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201809,027,2018-10-02  MINNEAPOLIS-ST. PAUL                              
 RELDT 201809,057,2018-10-02  PHOENIX                                           
 RELDT 201809,063,2018-10-02  SAN DIEGO                                         
 RELDT 201809,039,2018-10-02  SEATTLE-TACOMA                                    
 RELDT 201809,017,2018-10-02  ST. LOUIS                                         
 RELDT 201809,087,2018-10-02  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201809,015,2018-10-02  WASHINGTON, DC                                    
*                                                                               
 RELDT 201809,093,2018-10-03  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201809,031,2018-10-03  CINCINNATI                                        
 RELDT 201809,019,2018-10-03  CLEVELAND                                         
 RELDT 201809,045,2018-10-03  COLUMBUS, OH                                      
 RELDT 201809,041,2018-10-03  KANSAS CITY                                       
 RELDT 201809,257,2018-10-03  LAS VEGAS                                         
 RELDT 201809,131,2018-10-03  ORLANDO                                           
 RELDT 201809,023,2018-10-03  PITTSBURGH, PA                                    
 RELDT 201809,051,2018-10-03  PORTLAND, OR                                      
 RELDT 201809,065,2018-10-03  SACRAMENTO                                        
 RELDT 201809,101,2018-10-03  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201809,059,2018-10-03  SAN ANTONIO                                       
*                                                                               
 RELDT 201809,135,2018-10-04  AUSTIN                                            
 RELDT 201809,166,2018-10-04  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201809,061,2018-10-04  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201809,049,2018-10-04  INDIANAPOLIS                                      
 RELDT 201809,107,2018-10-04  JACKSONVILLE                                      
 RELDT 201809,075,2018-10-04  MEMPHIS                                           
 RELDT 201809,043,2018-10-04  MILWAUKEE-RACINE                                  
 RELDT 201809,073,2018-10-04  NASHVILLE                                         
 RELDT 201809,109,2018-10-04  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201809,077,2018-10-04  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201809,115,2018-10-04  RALEIGH-DURHAM                                    
 RELDT 201809,299,2018-10-04  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201810,047,2018-10-29  ATLANTA                                           
 RELDT 201810,005,2018-10-29  CHICAGO                                           
 RELDT 201810,024,2018-10-29  DALLAS-FT. WORTH                                  
 RELDT 201810,033,2018-10-29  HOUSTON-GALVESTON                                 
 RELDT 201810,003,2018-10-29  LOS ANGELES                                       
 RELDT 201810,413,2018-10-29  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201810,321,2018-10-29  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201810,001,2018-10-29  NEW YORK                                          
 RELDT 201810,007,2018-10-29  PHILADELPHIA                                      
 RELDT 201810,379,2018-10-29  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201810,009,2018-10-29  SAN FRANCISCO                                     
 RELDT 201810,215,2018-10-29  SAN JOSE                                          
*                                                                               
 RELDT 201810,021,2018-10-30  BALTIMORE                                         
 RELDT 201810,013,2018-10-30  BOSTON                                            
 RELDT 201810,035,2018-10-30  DENVER-BOULDER                                    
 RELDT 201810,011,2018-10-30  DETROIT                                           
 RELDT 201810,429,2018-10-30  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201810,027,2018-10-30  MINNEAPOLIS-ST. PAUL                              
 RELDT 201810,057,2018-10-30  PHOENIX                                           
 RELDT 201810,063,2018-10-30  SAN DIEGO                                         
 RELDT 201810,039,2018-10-30  SEATTLE-TACOMA                                    
 RELDT 201810,017,2018-10-30  ST. LOUIS                                         
 RELDT 201810,087,2018-10-30  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201810,015,2018-10-30  WASHINGTON, DC                                    
*                                                                               
 RELDT 201810,093,2018-10-31  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201810,031,2018-10-31  CINCINNATI                                        
 RELDT 201810,019,2018-10-31  CLEVELAND                                         
 RELDT 201810,045,2018-10-31  COLUMBUS, OH                                      
 RELDT 201810,041,2018-10-31  KANSAS CITY                                       
 RELDT 201810,257,2018-10-31  LAS VEGAS                                         
 RELDT 201810,131,2018-10-31  ORLANDO                                           
 RELDT 201810,023,2018-10-31  PITTSBURGH, PA                                    
 RELDT 201810,051,2018-10-31  PORTLAND, OR                                      
 RELDT 201810,065,2018-10-31  SACRAMENTO                                        
 RELDT 201810,101,2018-10-31  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201810,059,2018-10-31  SAN ANTONIO                                       
*                                                                               
 RELDT 201810,135,2018-11-01  AUSTIN                                            
 RELDT 201810,166,2018-11-01  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201810,061,2018-11-01  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201810,049,2018-11-01  INDIANAPOLIS                                      
 RELDT 201810,107,2018-11-01  JACKSONVILLE                                      
 RELDT 201810,075,2018-11-01  MEMPHIS                                           
 RELDT 201810,043,2018-11-01  MILWAUKEE-RACINE                                  
 RELDT 201810,073,2018-11-01  NASHVILLE                                         
 RELDT 201810,109,2018-11-01  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201810,077,2018-11-01  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201810,115,2018-11-01  RALEIGH-DURHAM                                    
 RELDT 201810,299,2018-11-01  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201811,047,2018-11-28  ATLANTA                                           
 RELDT 201811,005,2018-11-28  CHICAGO                                           
 RELDT 201811,024,2018-11-28  DALLAS-FT. WORTH                                  
 RELDT 201811,033,2018-11-28  HOUSTON-GALVESTON                                 
 RELDT 201811,003,2018-11-28  LOS ANGELES                                       
 RELDT 201811,413,2018-11-28  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201811,321,2018-11-28  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201811,001,2018-11-28  NEW YORK                                          
 RELDT 201811,007,2018-11-28  PHILADELPHIA                                      
 RELDT 201811,379,2018-11-28  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201811,009,2018-11-28  SAN FRANCISCO                                     
 RELDT 201811,215,2018-11-28  SAN JOSE                                          
*                                                                               
 RELDT 201811,021,2018-11-29  BALTIMORE                                         
 RELDT 201811,013,2018-11-29  BOSTON                                            
 RELDT 201811,035,2018-11-29  DENVER-BOULDER                                    
 RELDT 201811,011,2018-11-29  DETROIT                                           
 RELDT 201811,429,2018-11-29  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201811,027,2018-11-29  MINNEAPOLIS-ST. PAUL                              
 RELDT 201811,057,2018-11-29  PHOENIX                                           
 RELDT 201811,063,2018-11-29  SAN DIEGO                                         
 RELDT 201811,039,2018-11-29  SEATTLE-TACOMA                                    
 RELDT 201811,017,2018-11-29  ST. LOUIS                                         
 RELDT 201811,087,2018-11-29  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201811,015,2018-11-29  WASHINGTON, DC                                    
*                                                                               
 RELDT 201811,093,2018-11-30  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201811,031,2018-11-30  CINCINNATI                                        
 RELDT 201811,019,2018-11-30  CLEVELAND                                         
 RELDT 201811,045,2018-11-30  COLUMBUS, OH                                      
 RELDT 201811,041,2018-11-30  KANSAS CITY                                       
 RELDT 201811,257,2018-11-30  LAS VEGAS                                         
 RELDT 201811,131,2018-11-30  ORLANDO                                           
 RELDT 201811,023,2018-11-30  PITTSBURGH, PA                                    
 RELDT 201811,051,2018-11-30  PORTLAND, OR                                      
 RELDT 201811,065,2018-11-30  SACRAMENTO                                        
 RELDT 201811,101,2018-11-30  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201811,059,2018-11-30  SAN ANTONIO                                       
*                                                                               
 RELDT 201811,135,2018-12-03  AUSTIN                                            
 RELDT 201811,166,2018-12-03  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201811,061,2018-12-03  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201811,049,2018-12-03  INDIANAPOLIS                                      
 RELDT 201811,107,2018-12-03  JACKSONVILLE                                      
 RELDT 201811,075,2018-12-03  MEMPHIS                                           
 RELDT 201811,043,2018-12-03  MILWAUKEE-RACINE                                  
 RELDT 201811,073,2018-12-03  NASHVILLE                                         
 RELDT 201811,109,2018-12-03  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201811,077,2018-12-03  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201811,115,2018-12-03  RALEIGH-DURHAM                                    
 RELDT 201811,299,2018-12-03  WEST PALM BEACH-BOCA RATON                        
*                                                                               
 RELDT 201812,047,2018-12-26  ATLANTA                                           
 RELDT 201812,005,2018-12-26  CHICAGO                                           
 RELDT 201812,024,2018-12-26  DALLAS-FT. WORTH                                  
 RELDT 201812,033,2018-12-26  HOUSTON-GALVESTON                                 
 RELDT 201812,003,2018-12-26  LOS ANGELES                                       
 RELDT 201812,413,2018-12-26  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201812,321,2018-12-26  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201812,001,2018-12-26  NEW YORK                                          
 RELDT 201812,007,2018-12-26  PHILADELPHIA                                      
 RELDT 201812,379,2018-12-26  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201812,009,2018-12-26  SAN FRANCISCO                                     
 RELDT 201812,215,2018-12-26  SAN JOSE                                          
*                                                                               
 RELDT 201812,021,2018-12-27  BALTIMORE                                         
 RELDT 201812,013,2018-12-27  BOSTON                                            
 RELDT 201812,035,2018-12-27  DENVER-BOULDER                                    
 RELDT 201812,011,2018-12-27  DETROIT                                           
 RELDT 201812,429,2018-12-27  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201812,027,2018-12-27  MINNEAPOLIS-ST. PAUL                              
 RELDT 201812,057,2018-12-27  PHOENIX                                           
 RELDT 201812,063,2018-12-27  SAN DIEGO                                         
 RELDT 201812,039,2018-12-27  SEATTLE-TACOMA                                    
 RELDT 201812,017,2018-12-27  ST. LOUIS                                         
 RELDT 201812,087,2018-12-27  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201812,015,2018-12-27  WASHINGTON, DC                                    
*                                                                               
 RELDT 201812,093,2018-12-28  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201812,031,2018-12-28  CINCINNATI                                        
 RELDT 201812,019,2018-12-28  CLEVELAND                                         
 RELDT 201812,045,2018-12-28  COLUMBUS, OH                                      
 RELDT 201812,041,2018-12-28  KANSAS CITY                                       
 RELDT 201812,257,2018-12-28  LAS VEGAS                                         
 RELDT 201812,131,2018-12-28  ORLANDO                                           
 RELDT 201812,023,2018-12-28  PITTSBURGH, PA                                    
 RELDT 201812,051,2018-12-28  PORTLAND, OR                                      
 RELDT 201812,065,2018-12-28  SACRAMENTO                                        
 RELDT 201812,101,2018-12-28  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201812,059,2018-12-28  SAN ANTONIO                                       
*                                                                               
 RELDT 201812,135,2018-12-29  AUSTIN                                            
 RELDT 201812,166,2018-12-29  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201812,061,2018-12-29  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201812,049,2018-12-29  INDIANAPOLIS                                      
 RELDT 201812,107,2018-12-29  JACKSONVILLE                                      
 RELDT 201812,075,2018-12-29  MEMPHIS                                           
 RELDT 201812,043,2018-12-29  MILWAUKEE-RACINE                                  
 RELDT 201812,073,2018-12-29  NASHVILLE                                         
 RELDT 201812,109,2018-12-29  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201812,077,2018-12-29  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201812,115,2018-12-29  RALEIGH-DURHAM                                    
 RELDT 201812,299,2018-12-29  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
* HOLIDAY BOOK (2018)                                                           
*                                                                               
*                                                                               
 RELDT 201823,047,2018-01-22  ATLANTA                                           
 RELDT 201823,005,2018-01-22  CHICAGO                                           
 RELDT 201823,024,2018-01-22  DALLAS-FT. WORTH                                  
 RELDT 201823,033,2018-01-22  HOUSTON-GALVESTON                                 
 RELDT 201823,003,2018-01-22  LOS ANGELES                                       
 RELDT 201823,413,2018-01-22  MIDDLESEX-SOMERSET-UNION                          
 RELDT 201823,321,2018-01-22  NASSAU-SUFFOLK (LONG ISLAND)                      
 RELDT 201823,001,2018-01-22  NEW YORK                                          
 RELDT 201823,007,2018-01-22  PHILADELPHIA                                      
 RELDT 201823,379,2018-01-22  RIVERSIDE-SAN BERNARDINO                          
 RELDT 201823,009,2018-01-22  SAN FRANCISCO                                     
 RELDT 201823,215,2018-01-22  SAN JOSE                                          
*                                                                               
 RELDT 201823,021,2018-01-23  BALTIMORE                                         
 RELDT 201823,013,2018-01-23  BOSTON                                            
 RELDT 201823,035,2018-01-23  DENVER-BOULDER                                    
 RELDT 201823,011,2018-01-23  DETROIT                                           
 RELDT 201823,429,2018-01-23  MIAMI-FT.LAUDERDALE-HOLLYWOOD                     
 RELDT 201823,027,2018-01-23  MINNEAPOLIS-ST. PAUL                              
 RELDT 201823,057,2018-01-23  PHOENIX                                           
 RELDT 201823,063,2018-01-23  SAN DIEGO                                         
 RELDT 201823,039,2018-01-23  SEATTLE-TACOMA                                    
 RELDT 201823,017,2018-01-23  ST. LOUIS                                         
 RELDT 201823,087,2018-01-23  TAMPA-ST. PETERSBURG-CLEARWATER                   
 RELDT 201823,015,2018-01-23  WASHINGTON, DC                                    
*                                                                               
 RELDT 201823,093,2018-01-24  CHARLOTTE-GASTONIA-ROCK HILL                      
 RELDT 201823,031,2018-01-24  CINCINNATI                                        
 RELDT 201823,019,2018-01-24  CLEVELAND                                         
 RELDT 201823,045,2018-01-24  COLUMBUS, OH                                      
 RELDT 201823,041,2018-01-24  KANSAS CITY                                       
 RELDT 201823,257,2018-01-24  LAS VEGAS                                         
 RELDT 201823,131,2018-01-24  ORLANDO                                           
 RELDT 201823,023,2018-01-24  PITTSBURGH, PA                                    
 RELDT 201823,051,2018-01-24  PORTLAND, OR                                      
 RELDT 201823,065,2018-01-24  SACRAMENTO                                        
 RELDT 201823,101,2018-01-24  SALT LAKE CITY-OGDEN-PROVO                        
 RELDT 201823,059,2018-01-24  SAN ANTONIO                                       
*                                                                               
 RELDT 201823,135,2018-01-25  AUSTIN                                            
 RELDT 201823,166,2018-01-25  GREENSBORO-WINSTON SALEM-HIGH POINT               
 RELDT 201823,061,2018-01-25  HARTFORD-NEW BRITAIN-MIDDLETOWN                   
 RELDT 201823,049,2018-01-25  INDIANAPOLIS                                      
 RELDT 201823,107,2018-01-25  JACKSONVILLE                                      
 RELDT 201823,075,2018-01-25  MEMPHIS                                           
 RELDT 201823,043,2018-01-25  MILWAUKEE-RACINE                                  
 RELDT 201823,073,2018-01-25  NASHVILLE                                         
 RELDT 201823,109,2018-01-25  NORFOLK-VIRGINIA BEACH-NEWPORT NEWS               
 RELDT 201823,077,2018-01-25  PROVIDENCE-WARWICK-PAWTUCKET                      
 RELDT 201823,115,2018-01-25  RALEIGH-DURHAM                                    
 RELDT 201823,299,2018-01-25  WEST PALM BEACH-BOCA RATON                        
*                                                                               
*                                                                               
*&&DO                                                                           
*                                                                               
* SPRING 2014 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2014,223,2014-07-16  BATON ROUGE                                       
 RELDT ??2014,055,2014-07-16  LOUISVILLE                                        
 RELDT ??2014,516,2014-07-16  MONMOUTH-OCEAN                                    
 RELDT ??2014,053,2014-07-16  NEW ORLEANS                                       
 RELDT ??2014,083,2014-07-16  OKLAHOMA CITY                                     
 RELDT ??2014,540,2014-07-16  PUERTO RICO                                       
*                                                                               
 RELDT ??2014,587,2014-07-17  ALEXANDRIA, LA                                    
 RELDT ??2014,533,2014-07-17  BILOXI-GULFPORT-PASCAGOULA                        
 RELDT ??2014,095,2014-07-17  BIRMINGHAM                                        
 RELDT ??2014,037,2014-07-17  BUFFALO-NIAGARA FALLS                             
 RELDT ??2014,191,2014-07-17  GREENVILLE-SPARTANBURG                            
 RELDT ??2014,253,2014-07-17  LAFAYETTE, LA                                     
 RELDT ??2014,522,2014-07-17  LAUREL-HATTIESBURG, MS                            
 RELDT ??2014,599,2014-07-17  LAWTON, OK                                        
 RELDT ??2014,105,2014-07-17  RICHMOND                                          
 RELDT ??2014,079,2014-07-17  ROCHESTER, NY                                     
*                                                                               
 RELDT ??2014,069,2014-07-18  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2014,221,2014-07-18  ASHEVILLE                                         
 RELDT ??2014,557,2014-07-18  ELMIRA-CORNING, NY                                
 RELDT ??2014,416,2014-07-18  FREDERICKSBURG                                    
 RELDT ??2014,421,2014-07-18  OLEAN, NY                                         
 RELDT ??2014,091,2014-07-18  SYRACUSE                                          
 RELDT ??2014,543,2014-07-18  TUPELO, MS                                        
 RELDT ??2014,596,2014-07-18  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2014,527,2014-07-21  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2014,526,2014-07-21  BANGOR                                            
 RELDT ??2014,505,2014-07-21  BURLINGTON-PLATTSBURGH                            
 RELDT ??2014,426,2014-07-21  CONCORD (LAKES REGION)                            
 RELDT ??2014,067,2014-07-21  DAYTON                                            
 RELDT ??2014,127,2014-07-21  GRAND RAPIDS                                      
 RELDT ??2014,393,2014-07-21  HUDSON VALLEY                                     
 RELDT ??2014,431,2014-07-21  LEBANON-HANOVER-WHITE RIVER JUNCTION              
 RELDT ??2014,267,2014-07-21  MANCHESTER                                        
 RELDT ??2014,432,2014-07-21  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2014,197,2014-07-21  PORTLAND, ME                                      
 RELDT ??2014,247,2014-07-21  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2014,518,2014-07-21  POUGHKEEPSIE, NY                                  
 RELDT ??2014,207,2014-07-21  TUCSON                                            
 RELDT ??2014,103,2014-07-21  TULSA                                             
 RELDT ??2014,295,2014-07-21  UTICA-ROME                                        
 RELDT ??2014,577,2014-07-21  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2014,141,2014-07-22  ALBUQUERQUE                                       
 RELDT ??2014,145,2014-07-22  ALLENTOWN-BETHLEHEM                               
 RELDT ??2014,531,2014-07-22  BATTLE CREEK, MI                                  
 RELDT ??2014,593,2014-07-22  DANBURY, CT                                       
 RELDT ??2014,161,2014-07-22  EL PASO                                           
 RELDT ??2014,089,2014-07-22  FRESNO                                            
 RELDT ??2014,515,2014-07-22  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2014,589,2014-07-22  FT. SMITH, AR                                     
 RELDT ??2014,099,2014-07-22  HONOLULU                                          
 RELDT ??2014,345,2014-07-22  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2014,251,2014-07-22  KALAMAZOO                                         
 RELDT ??2014,121,2014-07-22  KNOXVILLE                                         
 RELDT ??2014,576,2014-07-22  LIMA, OH                                          
 RELDT ??2014,269,2014-07-22  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2014,566,2014-07-22  MUSKEGON, MI                                      
 RELDT ??2014,373,2014-07-22  SARASOTA-BRADENTON                                
 RELDT ??2014,547,2014-07-22  STAMFORD-NORWALK, CT                              
 RELDT ??2014,175,2014-07-22  WILKES BARRE-SCRANTON                             
*                                                                               
 RELDT ??2014,081,2014-07-23  AKRON                                             
 RELDT ??2014,143,2014-07-23  BAKERSFIELD                                       
 RELDT ??2014,231,2014-07-23  CHARLESTON, SC                                    
 RELDT ??2014,119,2014-07-23  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2014,441,2014-07-23  LAS CRUCES, NM                                    
 RELDT ??2014,536,2014-07-23  MERCED, CA                                        
 RELDT ??2014,085,2014-07-23  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2014,293,2014-07-23  VISALIA-TULARE-HANFORD                            
 RELDT ??2014,578,2014-07-23  WILLIAMSPORT, PA                                  
 RELDT ??2014,301,2014-07-23  YORK                                              
*                                                                               
 RELDT ??2014,082,2014-07-24  CANTON                                            
 RELDT ??2014,183,2014-07-24  COLUMBIA, SC                                      
 RELDT ??2014,071,2014-07-24  DES MOINES                                        
 RELDT ??2014,361,2014-07-24  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2014,524,2014-07-24  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2014,255,2014-07-24  LANCASTER                                         
 RELDT ??2014,261,2014-07-24  LINCOLN                                           
 RELDT ??2014,123,2014-07-24  LITTLE ROCK                                       
 RELDT ??2014,283,2014-07-24  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2014,510,2014-07-24  MYRTLE BEACH, SC                                  
 RELDT ??2014,563,2014-07-24  NEW LONDON, CT                                    
 RELDT ??2014,177,2014-07-24  SPOKANE                                           
 RELDT ??2014,117,2014-07-24  SPRINGFIELD, MA                                   
 RELDT ??2014,139,2014-07-24  WILMINGTON, DE                                    
*                                                                               
 RELDT ??2014,233,2014-07-25  COLORADO SPRINGS                                  
 RELDT ??2014,550,2014-07-25  GAINESVILLE-OCALA                                 
 RELDT ??2014,383,2014-07-25  HOT SPRINGS, AR                                   
 RELDT ??2014,171,2014-07-25  MADISON                                           
 RELDT ??2014,133,2014-07-25  MOBILE                                            
 RELDT ??2014,291,2014-07-25  STOCKTON                                          
 RELDT ??2014,097,2014-07-25  TOLEDO                                            
 RELDT ??2014,125,2014-07-25  WICHITA                                           
*                                                                               
 RELDT ??2014,229,2014-07-28  BOISE                                             
 RELDT ??2014,181,2014-07-28  CHATTANOOGA                                       
 RELDT ??2014,341,2014-07-28  DAYTONA BEACH                                     
 RELDT ??2014,517,2014-07-28  FT. PIERCE-STUART-VERO BEACH                      
 RELDT ??2014,391,2014-07-28  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2014,327,2014-07-28  HUNTSVILLE                                        
 RELDT ??2014,311,2014-07-28  LAKELAND-WINTER HAVEN                             
 RELDT ??2014,317,2014-07-28  PENSACOLA                                         
 RELDT ??2014,353,2014-07-28  PUEBLO                                            
 RELDT ??2014,387,2014-07-28  SALINA-MANHATTAN, KS                              
 RELDT ??2014,205,2014-07-28  TOPEKA                                            
 RELDT ??2014,442,2014-07-28  TWIN FALLS (SUN VALLEY), ID                       
*                                                                               
 RELDT ??2014,367,2014-07-29  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2014,305,2014-07-29  AUGUSTA, GA                                       
 RELDT ??2014,404,2014-07-29  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2014,165,2014-07-29  FT. WAYNE                                         
 RELDT ??2014,169,2014-07-29  JACKSON, MS                                       
 RELDT ??2014,259,2014-07-29  LEXINGTON-FAYETTE                                 
 RELDT ??2014,331,2014-07-29  MELBOURNE-TITUSVILLE-COCOA                        
 RELDT ??2014,422,2014-07-29  MUNCIE-MARION, IN                                 
 RELDT ??2014,277,2014-07-29  ROANOKE-LYNCHBURG                                 
 RELDT ??2014,579,2014-07-29  SUSSEX, NJ                                        
 RELDT ??2014,537,2014-07-29  TRENTON                                           
 RELDT ??2014,113,2014-07-29  WORCESTER                                         
*                                                                               
 RELDT ??2014,337,2014-07-30  BRIDGEPORT                                        
 RELDT ??2014,560,2014-07-30  CHEYENNE, WY                                      
 RELDT ??2014,590,2014-07-30  FT. COLLINS-GREELEY, CO                           
 RELDT ??2014,343,2014-07-30  MODESTO                                           
 RELDT ??2014,565,2014-07-30  MORRISTOWN, NJ                                    
 RELDT ??2014,062,2014-07-30  NEW HAVEN                                         
 RELDT ??2014,594,2014-07-30  OXNARD-VENTURA                                    
 RELDT ??2014,592,2014-07-30  PALM SPRINGS                                      
 RELDT ??2014,556,2014-07-30  SAN LUIS OBISPO, CA                               
 RELDT ??2014,591,2014-07-30  SANTA BARBARA, CA                                 
 RELDT ??2014,567,2014-07-30  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2014,574,2014-07-30  SANTA ROSA                                        
 RELDT ??2014,111,2014-07-30  SHREVEPORT                                        
 RELDT ??2014,287,2014-07-30  SOUTH BEND                                        
 RELDT ??2014,419,2014-07-30  VICTOR VALLEY                                     
*                                                                               
 RELDT ??2014,303,2014-07-31  APPLETON-OSHKOSH                                  
 RELDT ??2014,155,2014-07-31  CORPUS CHRISTI                                    
 RELDT ??2014,503,2014-07-31  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2014,359,2014-07-31  FAYETTEVILLE, NC                                  
 RELDT ??2014,163,2014-07-31  FLINT                                             
 RELDT ??2014,245,2014-07-31  GREEN BAY                                         
 RELDT ??2014,195,2014-07-31  LANSING-EAST LANSING                              
 RELDT ??2014,439,2014-07-31  LUFKIN-NACOGDOCHES, TX                            
 RELDT ??2014,273,2014-07-31  READING, PA                                       
 RELDT ??2014,275,2014-07-31  RENO                                              
 RELDT ??2014,281,2014-07-31  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2014,512,2014-07-31  TEXARKANA, TX-AR                                  
 RELDT ??2014,502,2014-07-31  TYLER-LONGVIEW                                    
 RELDT ??2014,129,2014-07-31  YOUNGSTOWN-WARREN                                 
*                                                                               
 RELDT ??2014,580,2014-08-01  ALBANY, GA                                        
 RELDT ??2014,581,2014-08-01  ANN ARBOR, MI                                     
 RELDT ??2014,149,2014-08-01  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2014,323,2014-08-01  BLOOMINGTON                                       
 RELDT ??2014,513,2014-08-01  BRUNSWICK, GA                                     
 RELDT ??2014,501,2014-08-01  DOTHAN, AL                                        
 RELDT ??2014,562,2014-08-01  KILLEEN-TEMPLE, TX                                
 RELDT ??2014,584,2014-08-01  LAKE CHARLES, LA                                  
 RELDT ??2014,425,2014-08-01  LASALLE-PERU, IL                                  
 RELDT ??2014,173,2014-08-01  MONTGOMERY                                        
 RELDT ??2014,365,2014-08-01  NEW BEDFORD-FALL RIVER, MA                        
 RELDT ??2014,137,2014-08-01  PEORIA                                            
 RELDT ??2014,157,2014-08-01  QUAD CITIES                                       
 RELDT ??2014,271,2014-08-01  SALISBURY-OCEAN CITY                              
 RELDT ??2014,285,2014-08-01  SAVANNAH                                          
 RELDT ??2014,203,2014-08-01  SPRINGFIELD, MO                                   
*                                                                               
 RELDT ??2014,315,2014-08-04  ANCHORAGE                                         
 RELDT ??2014,227,2014-08-04  BINGHAMTON                                        
 RELDT ??2014,153,2014-08-04  CHARLESTON, WV                                    
 RELDT ??2014,239,2014-08-04  ERIE                                              
 RELDT ??2014,241,2014-08-04  EUGENE-SPRINGFIELD                                
 RELDT ??2014,185,2014-08-04  EVANSVILLE                                        
 RELDT ??2014,412,2014-08-04  FLAGSTAFF-PRESCOTT, AZ                            
 RELDT ??2014,193,2014-08-04  HUNTINGTON-ASHLAND                                
 RELDT ??2014,265,2014-08-04  MACON                                             
 RELDT ??2014,555,2014-08-04  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2014,279,2014-08-04  ROCKFORD                                          
 RELDT ??2014,333,2014-08-04  TALLAHASSEE                                       
 RELDT ??2014,433,2014-08-04  VALDOSTA, GA                                      
 RELDT ??2014,597,2014-08-04  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2014,147,2014-08-05  AMARILLO, TX                                      
 RELDT ??2014,549,2014-08-05  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2014,530,2014-08-05  CAPE COD, MA                                      
 RELDT ??2014,235,2014-08-05  COLUMBUS, GA                                      
 RELDT ??2014,528,2014-08-05  FREDERICK, MD                                     
 RELDT ??2014,263,2014-08-05  LUBBOCK                                           
 RELDT ??2014,561,2014-08-05  ODESSA-MIDLAND, TX                                
 RELDT ??2014,509,2014-08-05  REDDING, CA                                       
 RELDT ??2014,418,2014-08-05  SHEBOYGAN, WI                                     
 RELDT ??2014,307,2014-08-05  TERRE HAUTE                                       
 RELDT ??2014,535,2014-08-05  TRAVERSE CITY-PETOSKEY, MI                        
 RELDT ??2014,371,2014-08-05  TRI-CITIES, WA                                    
 RELDT ??2014,309,2014-08-05  WACO, TX                                          
*                                                                               
 RELDT ??2014,436,2014-08-06  BEND, OR                                          
 RELDT ??2014,151,2014-08-06  CEDAR RAPIDS                                      
 RELDT ??2014,507,2014-08-06  CHAMPAIGN, IL                                     
 RELDT ??2014,508,2014-08-06  CHICO, CA                                         
 RELDT ??2014,499,2014-08-06  DECATUR, IL                                       
 RELDT ??2014,159,2014-08-06  DULUTH-SUPERIOR                                   
 RELDT ??2014,187,2014-08-06  FARGO-MOORHEAD                                    
 RELDT ??2014,554,2014-08-06  FLORENCE, SC                                      
 RELDT ??2014,572,2014-08-06  GRAND FORKS, ND-MN                                
 RELDT ??2014,506,2014-08-06  LAREDO, TX                                        
 RELDT ??2014,339,2014-08-06  MEDFORD-ASHLAND, OR                               
 RELDT ??2014,575,2014-08-06  ST. CLOUD, MN                                     
 RELDT ??2014,297,2014-08-06  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2014,569,2014-08-06  WINCHESTER, VA                                    
 RELDT ??2014,325,2014-08-06  YAKIMA, WA                                        
*                                                                               
 RELDT ??2014,546,2014-08-07  ABILENE, TX                                       
 RELDT ??2014,219,2014-08-07  ALTOONA                                           
 RELDT ??2014,225,2014-08-07  BILLINGS, MT                                      
 RELDT ??2014,564,2014-08-07  COLUMBIA, MO                                      
 RELDT ??2014,586,2014-08-07  EAU CLAIRE, WI                                    
 RELDT ??2014,381,2014-08-07  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2014,511,2014-08-07  GRAND JUNCTION, CO                                
 RELDT ??2014,534,2014-08-07  JOPLIN, MO                                        
 RELDT ??2014,559,2014-08-07  LA CROSSE, WI                                     
 RELDT ??2014,595,2014-08-07  LAFAYETTE, IN                                     
 RELDT ??2014,585,2014-08-07  MONROE, LA                                        
 RELDT ??2014,573,2014-08-07  PANAMA CITY, FL                                   
 RELDT ??2014,539,2014-08-07  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2014,532,2014-08-07  ROCHESTER, MN                                     
 RELDT ??2014,179,2014-08-07  WHEELING                                          
*                                                                               
 RELDT ??2014,558,2014-08-08  BECKLEY, WV                                       
 RELDT ??2014,582,2014-08-08  BISMARCK, ND                                      
 RELDT ??2014,520,2014-08-08  BLUEFIELD, WV                                     
 RELDT ??2014,369,2014-08-08  CASPER, WY                                        
 RELDT ??2014,519,2014-08-08  HARRISONBURG, VA                                  
 RELDT ??2014,289,2014-08-08  JACKSON, TN                                       
 RELDT ??2014,497,2014-08-08  JONESBORO, AR                                     
 RELDT ??2014,545,2014-08-08  RAPID CITY, SD                                    
 RELDT ??2014,548,2014-08-08  SAN ANGELO, TX                                    
 RELDT ??2014,523,2014-08-08  SIOUX CITY,  IA                                   
 RELDT ??2014,209,2014-08-08  WICHITA FALLS, TX                                 
*                                                                               
* SUMMER 2014 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2014,053,2014-10-07  NEW ORLEANS                                       
 RELDT ??2014,055,2014-10-07  LOUISVILLE                                        
 RELDT ??2014,083,2014-10-07  OKLAHOMA CITY                                     
 RELDT ??2014,223,2014-10-07  BATON ROUGE                                       
 RELDT ??2014,516,2014-10-07  MONMOUTH-OCEAN                                    
 RELDT ??2014,540,2014-10-07  PUERTO RICO                                       
*                                                                               
 RELDT ??2014,037,2014-10-08  BUFFALO-NIAGARA FALLS                             
 RELDT ??2014,079,2014-10-08  ROCHESTER, NY                                     
 RELDT ??2014,095,2014-10-08  BIRMINGHAM                                        
 RELDT ??2014,105,2014-10-08  RICHMOND                                          
 RELDT ??2014,191,2014-10-08  GREENVILLE-SPARTANBURG                            
 RELDT ??2014,207,2014-10-08  TUCSON                                            
*                                                                               
 RELDT ??2014,067,2014-10-09  DAYTON                                            
 RELDT ??2014,069,2014-10-09  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2014,089,2014-10-09  FRESNO                                            
 RELDT ??2014,127,2014-10-09  GRAND RAPIDS                                      
 RELDT ??2014,145,2014-10-09  ALLENTOWN-BETHLEHEM                               
 RELDT ??2014,175,2014-10-09  WILKES BARRE-SCRANTON                             
*                                                                               
 RELDT ??2014,085,2014-10-10  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2014,099,2014-10-10  HONOLULU                                          
 RELDT ??2014,103,2014-10-10  TULSA                                             
 RELDT ??2014,121,2014-10-10  KNOXVILLE                                         
 RELDT ??2014,143,2014-10-10  BAKERSFIELD                                       
*                                                                               
 RELDT ??2014,119,2014-10-13  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2014,141,2014-10-13  ALBUQUERQUE                                       
 RELDT ??2014,161,2014-10-13  EL PASO                                           
 RELDT ??2014,301,2014-10-13  YORK                                              
 RELDT ??2014,345,2014-10-13  JOHNSON CITY-KINGSPORT-BRISTOL                    
*                                                                               
 RELDT ??2014,081,2014-10-14  AKRON                                             
 RELDT ??2014,091,2014-10-14  SYRACUSE                                          
 RELDT ??2014,231,2014-10-14  CHARLESTON, SC                                    
 RELDT ??2014,283,2014-10-14  MONTEREY-SALINAS-SANTA CRUZ                       
*                                                                               
 RELDT ??2014,097,2014-10-15  TOLEDO                                            
 RELDT ??2014,117,2014-10-15  SPRINGFIELD, MA                                   
 RELDT ??2014,123,2014-10-15  LITTLE ROCK                                       
 RELDT ??2014,183,2014-10-15  COLUMBIA, SC                                      
 RELDT ??2014,361,2014-10-15  GREENVILLE-NEW BERN-JACKSONVILLE                  
*                                                                               
 RELDT ??2014,071,2014-10-16  DES MOINES                                        
 RELDT ??2014,125,2014-10-16  WICHITA                                           
 RELDT ??2014,133,2014-10-16  MOBILE                                            
 RELDT ??2014,177,2014-10-16  SPOKANE                                           
 RELDT ??2014,233,2014-10-16  COLORADO SPRINGS                                  
*                                                                               
 RELDT ??2014,111,2014-10-17  SHREVEPORT                                        
 RELDT ??2014,169,2014-10-17  JACKSON, MS                                       
 RELDT ??2014,171,2014-10-17  MADISON                                           
 RELDT ??2014,181,2014-10-17  CHATTANOOGA                                       
 RELDT ??2014,327,2014-10-17  HUNTSVILLE                                        
*                                                                               
* FALL 2014 SUMMARY DATA                                                        
*                                                                               
 RELDT ??2014,053,2015-01-05  NEW ORLEANS                                       
 RELDT ??2014,055,2015-01-05  LOUISVILLE                                        
 RELDT ??2014,083,2015-01-05  OKLAHOMA CITY                                     
 RELDT ??2014,223,2015-01-05  BATON ROUGE                                       
 RELDT ??2014,516,2015-01-05  MONMOUTH-OCEAN                                    
 RELDT ??2014,540,2015-01-05  PUERTO RICO                                       
*                                                                               
 RELDT ??2014,037,2015-01-06  BUFFALO-NIAGARA FALLS                             
 RELDT ??2014,079,2015-01-06  ROCHESTER, NY                                     
 RELDT ??2014,095,2015-01-06  BIRMINGHAM                                        
 RELDT ??2014,105,2015-01-06  RICHMOND                                          
 RELDT ??2014,191,2015-01-06  GREENVILLE-SPARTANBURG                            
 RELDT ??2014,253,2015-01-06  LAFAYETTE, LA                                     
 RELDT ??2014,522,2015-01-06  LAUREL-HATTIESBURG, MS                            
 RELDT ??2014,533,2015-01-06  BILOXI-GULFPORT-PASCAGOULA                        
*                                                                               
 RELDT ??2014,069,2015-01-07  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2014,091,2015-01-07  SYRACUSE                                          
 RELDT ??2014,221,2015-01-07  ASHEVILLE                                         
 RELDT ??2014,416,2015-01-07  FREDERICKSBURG                                    
 RELDT ??2014,421,2015-01-07  OLEAN, NY                                         
 RELDT ??2014,543,2015-01-07  TUPELO, MS                                        
 RELDT ??2014,557,2015-01-07  ELMIRA-CORNING, NY                                
 RELDT ??2014,596,2015-01-07  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2014,067,2015-01-08  DAYTON                                            
 RELDT ??2014,103,2015-01-08  TULSA                                             
 RELDT ??2014,127,2015-01-08  GRAND RAPIDS                                      
 RELDT ??2014,197,2015-01-08  PORTLAND, ME                                      
 RELDT ??2014,207,2015-01-08  TUCSON                                            
 RELDT ??2014,247,2015-01-08  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2014,267,2015-01-08  MANCHESTER                                        
 RELDT ??2014,295,2015-01-08  UTICA-ROME                                        
 RELDT ??2014,393,2015-01-08  HUDSON VALLEY                                     
 RELDT ??2014,426,2015-01-08  CONCORD (LAKES REGION)                            
 RELDT ??2014,431,2015-01-08  LEBANON-HANOVER-WHITE RIVER JUNCTION              
 RELDT ??2014,432,2015-01-08  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2014,505,2015-01-08  BURLINGTON-PLATTSBURGH                            
 RELDT ??2014,518,2015-01-08  POUGHKEEPSIE, NY                                  
 RELDT ??2014,526,2015-01-08  BANGOR                                            
 RELDT ??2014,527,2015-01-08  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2014,577,2015-01-08  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2014,089,2015-01-09  FRESNO                                            
 RELDT ??2014,099,2015-01-09  HONOLULU                                          
 RELDT ??2014,121,2015-01-09  KNOXVILLE                                         
 RELDT ??2014,141,2015-01-09  ALBUQUERQUE                                       
 RELDT ??2014,145,2015-01-09  ALLENTOWN-BETHLEHEM                               
 RELDT ??2014,161,2015-01-09  EL PASO                                           
 RELDT ??2014,175,2015-01-09  WILKES BARRE-SCRANTON                             
 RELDT ??2014,251,2015-01-09  KALAMAZOO                                         
 RELDT ??2014,269,2015-01-09  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2014,345,2015-01-09  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2014,373,2015-01-09  SARASOTA-BRADENTON                                
 RELDT ??2014,515,2015-01-09  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2014,531,2015-01-09  BATTLE CREEK, MI                                  
 RELDT ??2014,547,2015-01-09  STAMFORD-NORWALK, CT                              
 RELDT ??2014,566,2015-01-09  MUSKEGON, MI                                      
 RELDT ??2014,576,2015-01-09  LIMA, OH                                          
 RELDT ??2014,589,2015-01-09  FT. SMITH, AR                                     
 RELDT ??2014,593,2015-01-09  DANBURY, CT                                       
*                                                                               
 RELDT ??2014,081,2015-01-12  AKRON                                             
 RELDT ??2014,085,2015-01-12  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2014,119,2015-01-12  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2014,143,2015-01-12  BAKERSFIELD                                       
 RELDT ??2014,231,2015-01-12  CHARLESTON, SC                                    
 RELDT ??2014,293,2015-01-12  VISALIA-TULARE-HANFORD                            
 RELDT ??2014,301,2015-01-12  YORK                                              
 RELDT ??2014,441,2015-01-12  LAS CRUCES, NM                                    
 RELDT ??2014,536,2015-01-12  MERCED, CA                                        
 RELDT ??2014,578,2015-01-12  WILLIAMSPORT, PA                                  
*                                                                               
 RELDT ??2014,071,2015-01-13  DES MOINES                                        
 RELDT ??2014,082,2015-01-13  CANTON                                            
 RELDT ??2014,117,2015-01-13  SPRINGFIELD, MA                                   
 RELDT ??2014,123,2015-01-13  LITTLE ROCK                                       
 RELDT ??2014,139,2015-01-13  WILMINGTON, DE                                    
 RELDT ??2014,177,2015-01-13  SPOKANE                                           
 RELDT ??2014,183,2015-01-13  COLUMBIA, SC                                      
 RELDT ??2014,255,2015-01-13  LANCASTER                                         
 RELDT ??2014,261,2015-01-13  LINCOLN                                           
 RELDT ??2014,283,2015-01-13  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2014,361,2015-01-13  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2014,510,2015-01-13  MYRTLE BEACH, SC                                  
 RELDT ??2014,524,2015-01-13  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2014,563,2015-01-13  NEW LONDON, CT                                    
*                                                                               
 RELDT ??2014,097,2015-01-14  TOLEDO                                            
 RELDT ??2014,125,2015-01-14  WICHITA                                           
 RELDT ??2014,133,2015-01-14  MOBILE                                            
 RELDT ??2014,171,2015-01-14  MADISON                                           
 RELDT ??2014,233,2015-01-14  COLORADO SPRINGS                                  
 RELDT ??2014,291,2015-01-14  STOCKTON                                          
 RELDT ??2014,383,2015-01-14  HOT SPRINGS, AR                                   
 RELDT ??2014,550,2015-01-14  GAINESVILLE-OCALA                                 
*                                                                               
 RELDT ??2014,181,2015-01-15  CHATTANOOGA                                       
 RELDT ??2014,205,2015-01-15  TOPEKA                                            
 RELDT ??2014,229,2015-01-15  BOISE                                             
 RELDT ??2014,311,2015-01-15  LAKELAND-WINTER HAVEN                             
 RELDT ??2014,317,2015-01-15  PENSACOLA                                         
 RELDT ??2014,327,2015-01-15  HUNTSVILLE                                        
 RELDT ??2014,341,2015-01-15  DAYTONA BEACH                                     
 RELDT ??2014,353,2015-01-15  PUEBLO                                            
 RELDT ??2014,387,2015-01-15  SALINA-MANHATTAN, KS                              
 RELDT ??2014,391,2015-01-15  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2014,442,2015-01-15  TWIN FALLS (SUN VALLEY), ID                       
 RELDT ??2014,517,2015-01-15  FT. PIERCE-STUART-VERO BEACH                      
*                                                                               
 RELDT ??2014,113,2015-01-16  WORCESTER                                         
 RELDT ??2014,165,2015-01-16  FT. WAYNE                                         
 RELDT ??2014,169,2015-01-16  JACKSON, MS                                       
 RELDT ??2014,259,2015-01-16  LEXINGTON-FAYETTE                                 
 RELDT ??2014,277,2015-01-16  ROANOKE-LYNCHBURG                                 
 RELDT ??2014,305,2015-01-16  AUGUSTA, GA                                       
 RELDT ??2014,331,2015-01-16  MELBOURNE-TITUSVILLE-COCOA                        
 RELDT ??2014,367,2015-01-16  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2014,404,2015-01-16  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2014,422,2015-01-16  MUNCIE-MARION, IN                                 
 RELDT ??2014,537,2015-01-16  TRENTON                                           
 RELDT ??2014,579,2015-01-16  SUSSEX, NJ                                        
*                                                                               
 RELDT ??2014,062,2015-01-20  NEW HAVEN                                         
 RELDT ??2014,111,2015-01-20  SHREVEPORT                                        
 RELDT ??2014,287,2015-01-20  SOUTH BEND                                        
 RELDT ??2014,337,2015-01-20  BRIDGEPORT                                        
 RELDT ??2014,343,2015-01-20  MODESTO                                           
 RELDT ??2014,419,2015-01-20  VICTOR VALLEY                                     
 RELDT ??2014,556,2015-01-20  SAN LUIS OBISPO, CA                               
 RELDT ??2014,560,2015-01-20  CHEYENNE, WY                                      
 RELDT ??2014,565,2015-01-20  MORRISTOWN, NJ                                    
 RELDT ??2014,567,2015-01-20  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2014,574,2015-01-20  SANTA ROSA                                        
 RELDT ??2014,590,2015-01-20  FT. COLLINS-GREELEY, CO                           
 RELDT ??2014,591,2015-01-20  SANTA BARBARA, CA                                 
 RELDT ??2014,592,2015-01-20  PALM SPRINGS                                      
 RELDT ??2014,594,2015-01-20  OXNARD-VENTURA                                    
*                                                                               
 RELDT ??2014,129,2015-01-21  YOUNGSTOWN-WARREN                                 
 RELDT ??2014,155,2015-01-21  CORPUS CHRISTI                                    
 RELDT ??2014,163,2015-01-21  FLINT                                             
 RELDT ??2014,195,2015-01-21  LANSING-EAST LANSING                              
 RELDT ??2014,245,2015-01-21  GREEN BAY                                         
 RELDT ??2014,273,2015-01-21  READING, PA                                       
 RELDT ??2014,275,2015-01-21  RENO                                              
 RELDT ??2014,281,2015-01-21  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2014,303,2015-01-21  APPLETON-OSHKOSH                                  
 RELDT ??2014,359,2015-01-21  FAYETTEVILLE, NC                                  
 RELDT ??2014,439,2015-01-21  LUFKIN-NACOGDOCHES, TX                            
 RELDT ??2014,502,2015-01-21  TYLER-LONGVIEW                                    
 RELDT ??2014,503,2015-01-21  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2014,512,2015-01-21  TEXARKANA, TX-AR                                  
*                                                                               
 RELDT ??2014,137,2015-01-22  PEORIA                                            
 RELDT ??2014,149,2015-01-22  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2014,157,2015-01-22  QUAD CITIES                                       
 RELDT ??2014,173,2015-01-22  MONTGOMERY                                        
 RELDT ??2014,203,2015-01-22  SPRINGFIELD, MO                                   
 RELDT ??2014,271,2015-01-22  SALISBURY-OCEAN CITY                              
 RELDT ??2014,285,2015-01-22  SAVANNAH                                          
 RELDT ??2014,323,2015-01-22  BLOOMINGTON                                       
 RELDT ??2014,365,2015-01-22  NEW BEDFORD-FALL RIVER, MA                        
 RELDT ??2014,425,2015-01-22  LASALLE-PERU, IL                                  
 RELDT ??2014,501,2015-01-22  DOTHAN, AL                                        
 RELDT ??2014,513,2015-01-22  BRUNSWICK, GA                                     
 RELDT ??2014,562,2015-01-22  KILLEEN-TEMPLE, TX                                
 RELDT ??2014,580,2015-01-22  ALBANY, GA                                        
 RELDT ??2014,581,2015-01-22  ANN ARBOR                                         
 RELDT ??2014,584,2015-01-22  LAKE CHARLES, LA                                  
*                                                                               
 RELDT ??2014,153,2015-01-23  CHARLESTON, WV                                    
 RELDT ??2014,185,2015-01-23  EVANSVILLE                                        
 RELDT ??2014,193,2015-01-23  HUNTINGTON-ASHLAND                                
 RELDT ??2014,227,2015-01-23  BINGHAMTON                                        
 RELDT ??2014,239,2015-01-23  ERIE                                              
 RELDT ??2014,241,2015-01-23  EUGENE-SPRINGFIELD                                
 RELDT ??2014,265,2015-01-23  MACON                                             
 RELDT ??2014,279,2015-01-23  ROCKFORD                                          
 RELDT ??2014,315,2015-01-23  ANCHORAGE                                         
 RELDT ??2014,333,2015-01-23  TALLAHASSEE                                       
 RELDT ??2014,412,2015-01-23  FLAGSTAFF-PRESCOTT, AZ                            
 RELDT ??2014,433,2015-01-23  VALDOSTA, GA                                      
 RELDT ??2014,555,2015-01-23  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2014,597,2015-01-23  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2014,147,2015-01-26  AMARILLO, TX                                      
 RELDT ??2014,235,2015-01-26  COLUMBUS, GA                                      
 RELDT ??2014,263,2015-01-26  LUBBOCK                                           
 RELDT ??2014,307,2015-01-26  TERRE HAUTE                                       
 RELDT ??2014,309,2015-01-26  WACO, TX                                          
 RELDT ??2014,371,2015-01-26  TRI-CITIES, WA                                    
 RELDT ??2014,418,2015-01-26  SHEBOYGAN, WI                                     
 RELDT ??2014,509,2015-01-26  REDDING, CA                                       
 RELDT ??2014,528,2015-01-26  FREDERICK, MD                                     
 RELDT ??2014,530,2015-01-26  CAPE COD, MA                                      
 RELDT ??2014,535,2015-01-26  TRAVERSE CITY-PETOSKEY, MI                        
 RELDT ??2014,549,2015-01-26  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2014,561,2015-01-26  ODESSA-MIDLAND, TX                                
*                                                                               
 RELDT ??2014,151,2015-01-27  CEDAR RAPIDS                                      
 RELDT ??2014,159,2015-01-27  DULUTH-SUPERIOR                                   
 RELDT ??2014,187,2015-01-27  FARGO-MOORHEAD                                    
 RELDT ??2014,297,2015-01-27  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2014,325,2015-01-27  YAKIMA, WA                                        
 RELDT ??2014,339,2015-01-27  MEDFORD-ASHLAND, OR                               
 RELDT ??2014,436,2015-01-27  BEND, OR                                          
 RELDT ??2014,499,2015-01-27  DECATUR, IL                                       
 RELDT ??2014,506,2015-01-27  LAREDO, TX                                        
 RELDT ??2014,507,2015-01-27  CHAMPAIGN, IL                                     
 RELDT ??2014,508,2015-01-27  CHICO, CA                                         
 RELDT ??2014,554,2015-01-27  FLORENCE, SC                                      
 RELDT ??2014,569,2015-01-27  WINCHESTER, VA                                    
 RELDT ??2014,572,2015-01-27  GRAND FORKS, ND-MN                                
*                                                                               
 RELDT ??2014,179,2015-01-28  WHEELING                                          
 RELDT ??2014,219,2015-01-28  ALTOONA                                           
 RELDT ??2014,225,2015-01-28  BILLINGS, MT                                      
 RELDT ??2014,381,2015-01-28  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2014,511,2015-01-28  GRAND JUNCTION, CO                                
 RELDT ??2014,532,2015-01-28  ROCHESTER, MN                                     
 RELDT ??2014,534,2015-01-28  JOPLIN, MO                                        
 RELDT ??2014,539,2015-01-28  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2014,546,2015-01-28  ABILENE, TX                                       
 RELDT ??2014,559,2015-01-28  LA CROSSE, WI                                     
 RELDT ??2014,564,2015-01-28  COLUMBIA, MO                                      
 RELDT ??2014,573,2015-01-28  PANAMA CITY, FL                                   
 RELDT ??2014,585,2015-01-28  MONROE, LA                                        
 RELDT ??2014,586,2015-01-28  EAU CLAIRE, WI                                    
 RELDT ??2014,595,2015-01-28  LAFAYETTE, IN                                     
*                                                                               
 RELDT ??2014,209,2015-01-29  WICHITA FALLS, TX                                 
 RELDT ??2014,289,2015-01-29  JACKSON, TN                                       
 RELDT ??2014,497,2015-01-29  JONESBORO, AR                                     
 RELDT ??2014,519,2015-01-29  HARRISONBURG, VA                                  
 RELDT ??2014,520,2015-01-29  BLUEFIELD, WV                                     
 RELDT ??2014,523,2015-01-29  SIOUX CITY,  IA                                   
 RELDT ??2014,545,2015-01-29  RAPID CITY, SD                                    
 RELDT ??2014,548,2015-01-29  SAN ANGELO, TX                                    
 RELDT ??2014,558,2015-01-29  BECKLEY, WV                                       
 RELDT ??2014,582,2015-01-29  BISMARCK, ND                                      
*                                                                               
* WINTER 2015 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2015,053,2015-04-21  NEW ORLEANS                                       
 RELDT ??2015,055,2015-04-21  LOUISVILLE                                        
 RELDT ??2015,083,2015-04-21  OKLAHOMA CITY                                     
 RELDT ??2015,223,2015-04-21  BATON ROUGE                                       
 RELDT ??2015,516,2015-04-21  MONMOUTH-OCEAN                                    
 RELDT ??2015,540,2015-04-21  PUERTO RICO                                       
*                                                                               
 RELDT ??2015,037,2015-04-22  BUFFALO-NIAGARA FALLS                             
 RELDT ??2015,079,2015-04-22  ROCHESTER, NY                                     
 RELDT ??2015,095,2015-04-22  BIRMINGHAM                                        
 RELDT ??2015,105,2015-04-22  RICHMOND                                          
 RELDT ??2015,191,2015-04-22  GREENVILLE-SPARTANBURG                            
 RELDT ??2015,207,2015-04-22  TUCSON                                            
*                                                                               
 RELDT ??2015,067,2015-04-23  DAYTON                                            
 RELDT ??2015,069,2015-04-23  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2015,089,2015-04-23  FRESNO                                            
 RELDT ??2015,127,2015-04-23  GRAND RAPIDS                                      
 RELDT ??2015,145,2015-04-23  ALLENTOWN-BETHLEHEM                               
 RELDT ??2015,175,2015-04-23  WILKES BARRE-SCRANTON                             
*                                                                               
 RELDT ??2015,085,2015-04-24  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2015,099,2015-04-24  HONOLULU                                          
 RELDT ??2015,103,2015-04-24  TULSA                                             
 RELDT ??2015,121,2015-04-24  KNOXVILLE                                         
 RELDT ??2015,143,2015-04-24  BAKERSFIELD                                       
*                                                                               
 RELDT ??2015,119,2015-04-27  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2015,141,2015-04-27  ALBUQUERQUE                                       
 RELDT ??2015,161,2015-04-27  EL PASO                                           
 RELDT ??2015,301,2015-04-27  YORK                                              
 RELDT ??2015,345,2015-04-27  JOHNSON CITY-KINGSPORT-BRISTOL                    
*                                                                               
 RELDT ??2015,081,2015-04-28  AKRON                                             
 RELDT ??2015,091,2015-04-28  SYRACUSE                                          
 RELDT ??2015,231,2015-04-28  CHARLESTON, SC                                    
 RELDT ??2015,283,2015-04-28  MONTEREY-SALINAS-SANTA CRUZ                       
*                                                                               
 RELDT ??2015,097,2015-04-29  TOLEDO                                            
 RELDT ??2015,117,2015-04-29  SPRINGFIELD, MA                                   
 RELDT ??2015,123,2015-04-29  LITTLE ROCK                                       
 RELDT ??2015,183,2015-04-29  COLUMBIA, SC                                      
 RELDT ??2015,361,2015-04-29  GREENVILLE-NEW BERN-JACKSONVILLE                  
*                                                                               
 RELDT ??2015,071,2015-04-30  DES MOINES                                        
 RELDT ??2015,125,2015-04-30  WICHITA                                           
 RELDT ??2015,133,2015-04-30  MOBILE                                            
 RELDT ??2015,177,2015-04-30  SPOKANE                                           
 RELDT ??2015,233,2015-04-30  COLORADO SPRINGS                                  
*                                                                               
 RELDT ??2015,111,2015-05-01  SHREVEPORT                                        
 RELDT ??2015,169,2015-05-01  JACKSON, MS                                       
 RELDT ??2015,171,2015-05-01  MADISON                                           
 RELDT ??2015,181,2015-05-01  CHATTANOOGA                                       
 RELDT ??2015,327,2015-05-01  HUNTSVILLE                                        
*                                                                               
* SPRING 2015 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2015,053,2015-07-15  NEW ORLEANS                                       
 RELDT ??2015,055,2015-07-15  LOUISVILLE                                        
 RELDT ??2015,083,2015-07-15  OKLAHOMA CITY                                     
 RELDT ??2015,223,2015-07-15  BATON ROUGE                                       
 RELDT ??2015,516,2015-07-15  MONMOUTH-OCEAN                                    
 RELDT ??2015,540,2015-07-15  PUERTO RICO                                       
*                                                                               
 RELDT ??2015,037,2015-07-16  BUFFALO-NIAGARA FALLS                             
 RELDT ??2015,079,2015-07-16  ROCHESTER, NY                                     
 RELDT ??2015,095,2015-07-16  BIRMINGHAM                                        
 RELDT ??2015,105,2015-07-16  RICHMOND                                          
 RELDT ??2015,191,2015-07-16  GREENVILLE-SPARTANBURG                            
 RELDT ??2015,253,2015-07-16  LAFAYETTE, LA                                     
 RELDT ??2015,522,2015-07-16  LAUREL-HATTIESBURG, MS                            
 RELDT ??2015,533,2015-07-16  BILOXI-GULFPORT-PASCAGOULA                        
*                                                                               
 RELDT ??2015,069,2015-07-17  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2015,091,2015-07-17  SYRACUSE                                          
 RELDT ??2015,221,2015-07-17  ASHEVILLE                                         
 RELDT ??2015,416,2015-07-17  FREDERICKSBURG                                    
 RELDT ??2015,421,2015-07-17  OLEAN, NY                                         
 RELDT ??2015,543,2015-07-17  TUPELO, MS                                        
 RELDT ??2015,553,2015-07-17  CHARLOTTESVILLE, VA                               
 RELDT ??2015,557,2015-07-17  ELMIRA-CORNING, NY                                
 RELDT ??2015,596,2015-07-17  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2015,067,2015-07-20  DAYTON                                            
 RELDT ??2015,103,2015-07-20  TULSA                                             
 RELDT ??2015,127,2015-07-20  GRAND RAPIDS                                      
 RELDT ??2015,197,2015-07-20  PORTLAND, ME                                      
 RELDT ??2015,207,2015-07-20  TUCSON                                            
 RELDT ??2015,247,2015-07-20  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2015,267,2015-07-20  MANCHESTER                                        
 RELDT ??2015,295,2015-07-20  UTICA-ROME                                        
 RELDT ??2015,393,2015-07-20  HUDSON VALLEY                                     
 RELDT ??2015,426,2015-07-20  CONCORD (LAKES REGION)                            
 RELDT ??2015,431,2015-07-20  LEBANON-HANOVER-WHITE RIVER JUNCTION              
 RELDT ??2015,432,2015-07-20  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2015,505,2015-07-20  BURLINGTON-PLATTSBURGH                            
 RELDT ??2015,518,2015-07-20  POUGHKEEPSIE, NY                                  
 RELDT ??2015,526,2015-07-20  BANGOR                                            
 RELDT ??2015,527,2015-07-20  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2015,577,2015-07-20  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2015,089,2015-07-21  FRESNO                                            
 RELDT ??2015,099,2015-07-21  HONOLULU                                          
 RELDT ??2015,121,2015-07-21  KNOXVILLE                                         
 RELDT ??2015,141,2015-07-21  ALBUQUERQUE                                       
 RELDT ??2015,145,2015-07-21  ALLENTOWN-BETHLEHEM                               
 RELDT ??2015,161,2015-07-21  EL PASO                                           
 RELDT ??2015,175,2015-07-21  WILKES BARRE-SCRANTON                             
 RELDT ??2015,251,2015-07-21  KALAMAZOO                                         
 RELDT ??2015,269,2015-07-21  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2015,345,2015-07-21  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2015,373,2015-07-21  SARASOTA-BRADENTON                                
 RELDT ??2015,515,2015-07-21  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2015,531,2015-07-21  BATTLE CREEK, MI                                  
 RELDT ??2015,547,2015-07-21  STAMFORD-NORWALK, CT                              
 RELDT ??2015,566,2015-07-21  MUSKEGON, MI                                      
 RELDT ??2015,576,2015-07-21  LIMA, OH                                          
 RELDT ??2015,589,2015-07-21  FT. SMITH, AR                                     
 RELDT ??2015,593,2015-07-21  DANBURY, CT                                       
*                                                                               
 RELDT ??2015,081,2015-07-22  AKRON                                             
 RELDT ??2015,085,2015-07-22  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2015,119,2015-07-22  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2015,143,2015-07-22  BAKERSFIELD                                       
 RELDT ??2015,231,2015-07-22  CHARLESTON, SC                                    
 RELDT ??2015,293,2015-07-22  VISALIA-TULARE-HANFORD                            
 RELDT ??2015,301,2015-07-22  YORK                                              
 RELDT ??2015,441,2015-07-22  LAS CRUCES, NM                                    
 RELDT ??2015,536,2015-07-22  MERCED, CA                                        
 RELDT ??2015,578,2015-07-22  WILLIAMSPORT, PA                                  
*                                                                               
 RELDT ??2015,071,2015-07-23  DES MOINES                                        
 RELDT ??2015,082,2015-07-23  CANTON                                            
 RELDT ??2015,117,2015-07-23  SPRINGFIELD, MA                                   
 RELDT ??2015,123,2015-07-23  LITTLE ROCK                                       
 RELDT ??2015,139,2015-07-23  WILMINGTON, DE                                    
 RELDT ??2015,177,2015-07-23  SPOKANE                                           
 RELDT ??2015,183,2015-07-23  COLUMBIA, SC                                      
 RELDT ??2015,255,2015-07-23  LANCASTER                                         
 RELDT ??2015,261,2015-07-23  LINCOLN                                           
 RELDT ??2015,283,2015-07-23  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2015,361,2015-07-23  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2015,510,2015-07-23  MYRTLE BEACH, SC                                  
 RELDT ??2015,524,2015-07-23  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2015,563,2015-07-23  NEW LONDON, CT                                    
*                                                                               
 RELDT ??2015,097,2015-07-24  TOLEDO                                            
 RELDT ??2015,125,2015-07-24  WICHITA                                           
 RELDT ??2015,133,2015-07-24  MOBILE                                            
 RELDT ??2015,171,2015-07-24  MADISON                                           
 RELDT ??2015,233,2015-07-24  COLORADO SPRINGS                                  
 RELDT ??2015,291,2015-07-24  STOCKTON                                          
 RELDT ??2015,383,2015-07-24  HOT SPRINGS, AR                                   
 RELDT ??2015,550,2015-07-24  GAINESVILLE-OCALA                                 
*                                                                               
 RELDT ??2015,181,2015-07-27  CHATTANOOGA                                       
 RELDT ??2015,205,2015-07-27  TOPEKA                                            
 RELDT ??2015,229,2015-07-27  BOISE                                             
 RELDT ??2015,311,2015-07-27  LAKELAND-WINTER HAVEN                             
 RELDT ??2015,317,2015-07-27  PENSACOLA                                         
 RELDT ??2015,327,2015-07-27  HUNTSVILLE                                        
 RELDT ??2015,341,2015-07-27  DAYTONA BEACH                                     
 RELDT ??2015,353,2015-07-27  PUEBLO                                            
 RELDT ??2015,387,2015-07-27  SALINA-MANHATTAN, KS                              
 RELDT ??2015,391,2015-07-27  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2015,462,2015-07-27  TWIN FALLS - SUN VALLEY                           
 RELDT ??2015,517,2015-07-27  FT. PIERCE-STUART-VERO BEACH                      
*                                                                               
 RELDT ??2015,113,2015-07-28  WORCESTER                                         
 RELDT ??2015,165,2015-07-28  FT. WAYNE                                         
 RELDT ??2015,169,2015-07-28  JACKSON, MS                                       
 RELDT ??2015,259,2015-07-28  LEXINGTON-FAYETTE                                 
 RELDT ??2015,277,2015-07-28  ROANOKE-LYNCHBURG                                 
 RELDT ??2015,305,2015-07-28  AUGUSTA, GA                                       
 RELDT ??2015,331,2015-07-28  MELBOURNE-TITUSVILLE-COCOA                        
 RELDT ??2015,367,2015-07-28  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2015,404,2015-07-28  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2015,422,2015-07-28  MUNCIE-MARION, IN                                 
 RELDT ??2015,537,2015-07-28  TRENTON                                           
 RELDT ??2015,579,2015-07-28  SUSSEX, NJ                                        
*                                                                               
 RELDT ??2015,062,2015-07-29  NEW HAVEN                                         
 RELDT ??2015,111,2015-07-29  SHREVEPORT                                        
 RELDT ??2015,287,2015-07-29  SOUTH BEND                                        
 RELDT ??2015,337,2015-07-29  BRIDGEPORT                                        
 RELDT ??2015,343,2015-07-29  MODESTO                                           
 RELDT ??2015,419,2015-07-29  VICTOR VALLEY                                     
 RELDT ??2015,556,2015-07-29  SAN LUIS OBISPO, CA                               
 RELDT ??2015,560,2015-07-29  CHEYENNE, WY                                      
 RELDT ??2015,565,2015-07-29  MORRISTOWN, NJ                                    
 RELDT ??2015,567,2015-07-29  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2015,574,2015-07-29  SANTA ROSA                                        
 RELDT ??2015,590,2015-07-29  FT. COLLINS-GREELEY, CO                           
 RELDT ??2015,591,2015-07-29  SANTA BARBARA, CA                                 
 RELDT ??2015,592,2015-07-29  PALM SPRINGS                                      
 RELDT ??2015,594,2015-07-29  OXNARD-VENTURA                                    
*                                                                               
 RELDT ??2015,129,2015-07-30  YOUNGSTOWN-WARREN                                 
 RELDT ??2015,155,2015-07-30  CORPUS CHRISTI                                    
 RELDT ??2015,163,2015-07-30  FLINT                                             
 RELDT ??2015,195,2015-07-30  LANSING-EAST LANSING                              
 RELDT ??2015,245,2015-07-30  GREEN BAY                                         
 RELDT ??2015,273,2015-07-30  READING, PA                                       
 RELDT ??2015,275,2015-07-30  RENO                                              
 RELDT ??2015,281,2015-07-30  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2015,303,2015-07-30  APPLETON-OSHKOSH                                  
 RELDT ??2015,359,2015-07-30  FAYETTEVILLE, NC                                  
 RELDT ??2015,502,2015-07-30  TYLER-LONGVIEW                                    
 RELDT ??2015,503,2015-07-30  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2015,512,2015-07-30  TEXARKANA, TX-AR                                  
*                                                                               
 RELDT ??2015,137,2015-07-31  PEORIA                                            
 RELDT ??2015,149,2015-07-31  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2015,157,2015-07-31  QUAD CITIES                                       
 RELDT ??2015,173,2015-07-31  MONTGOMERY                                        
 RELDT ??2015,203,2015-07-31  SPRINGFIELD, MO                                   
 RELDT ??2015,271,2015-07-31  SALISBURY-OCEAN CITY                              
 RELDT ??2015,285,2015-07-31  SAVANNAH                                          
 RELDT ??2015,323,2015-07-31  BLOOMINGTON                                       
 RELDT ??2015,365,2015-07-31  NEW BEDFORD-FALL RIVER, MA                        
 RELDT ??2015,423,2015-07-31  HILTON HEAD, SC                                   
 RELDT ??2015,425,2015-07-31  LASALLE-PERU, IL                                  
 RELDT ??2015,501,2015-07-31  DOTHAN, AL                                        
 RELDT ??2015,513,2015-07-31  BRUNSWICK, GA                                     
 RELDT ??2015,562,2015-07-31  KILLEEN-TEMPLE, TX                                
 RELDT ??2015,580,2015-07-31  ALBANY, GA                                        
 RELDT ??2015,581,2015-07-31  ANN ARBOR, MI                                     
 RELDT ??2015,584,2015-07-31  LAKE CHARLES, LA                                  
*                                                                               
 RELDT ??2015,153,2015-08-03  CHARLESTON, WV                                    
 RELDT ??2015,185,2015-08-03  EVANSVILLE                                        
 RELDT ??2015,193,2015-08-03  HUNTINGTON-ASHLAND                                
 RELDT ??2015,227,2015-08-03  BINGHAMTON                                        
 RELDT ??2015,239,2015-08-03  ERIE                                              
 RELDT ??2015,241,2015-08-03  EUGENE-SPRINGFIELD                                
 RELDT ??2015,265,2015-08-03  MACON                                             
 RELDT ??2015,279,2015-08-03  ROCKFORD                                          
 RELDT ??2015,315,2015-08-03  ANCHORAGE                                         
 RELDT ??2015,333,2015-08-03  TALLAHASSEE                                       
 RELDT ??2015,412,2015-08-03  FLAGSTAFF-PRESCOTT, AZ                            
 RELDT ??2015,433,2015-08-03  VALDOSTA, GA                                      
 RELDT ??2015,555,2015-08-03  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2015,597,2015-08-03  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2015,147,2015-08-04  AMARILLO, TX                                      
 RELDT ??2015,235,2015-08-04  COLUMBUS, GA                                      
 RELDT ??2015,263,2015-08-04  LUBBOCK                                           
 RELDT ??2015,307,2015-08-04  TERRE HAUTE                                       
 RELDT ??2015,309,2015-08-04  WACO, TX                                          
 RELDT ??2015,371,2015-08-04  TRI-CITIES, WA                                    
 RELDT ??2015,418,2015-08-04  SHEBOYGAN, WI                                     
 RELDT ??2015,509,2015-08-04  REDDING, CA                                       
 RELDT ??2015,528,2015-08-04  FREDERICK, MD                                     
 RELDT ??2015,530,2015-08-04  CAPE COD, MA                                      
 RELDT ??2015,535,2015-08-04  TRAVERSE CITY-PETOSKEY, MI                        
 RELDT ??2015,549,2015-08-04  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2015,561,2015-08-04  ODESSA-MIDLAND, TX                                
*                                                                               
 RELDT ??2015,151,2015-08-05  CEDAR RAPIDS                                      
 RELDT ??2015,159,2015-08-05  DULUTH-SUPERIOR                                   
 RELDT ??2015,187,2015-08-05  FARGO-MOORHEAD                                    
 RELDT ??2015,297,2015-08-05  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2015,325,2015-08-05  YAKIMA, WA                                        
 RELDT ??2015,339,2015-08-05  MEDFORD-ASHLAND, OR                               
 RELDT ??2015,436,2015-08-05  BEND, OR                                          
 RELDT ??2015,499,2015-08-05  DECATUR, IL                                       
 RELDT ??2015,506,2015-08-05  LAREDO, TX                                        
 RELDT ??2015,507,2015-08-05  CHAMPAIGN, IL                                     
 RELDT ??2015,508,2015-08-05  CHICO, CA                                         
 RELDT ??2015,554,2015-08-05  FLORENCE, SC                                      
 RELDT ??2015,569,2015-08-05  WINCHESTER, VA                                    
 RELDT ??2015,572,2015-08-05  GRAND FORKS, ND-MN                                
*                                                                               
 RELDT ??2015,179,2015-08-06  WHEELING                                          
 RELDT ??2015,219,2015-08-06  ALTOONA                                           
 RELDT ??2015,225,2015-08-06  BILLINGS, MT                                      
 RELDT ??2015,381,2015-08-06  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2015,511,2015-08-06  GRAND JUNCTION, CO                                
 RELDT ??2015,532,2015-08-06  ROCHESTER, MN                                     
 RELDT ??2015,534,2015-08-06  JOPLIN, MO                                        
 RELDT ??2015,539,2015-08-06  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2015,546,2015-08-06  ABILENE, TX                                       
 RELDT ??2015,559,2015-08-06  LA CROSSE, WI                                     
 RELDT ??2015,564,2015-08-06  COLUMBIA, MO                                      
 RELDT ??2015,573,2015-08-06  PANAMA CITY, FL                                   
 RELDT ??2015,585,2015-08-06  MONROE, LA                                        
 RELDT ??2015,586,2015-08-06  EAU CLAIRE, WI                                    
 RELDT ??2015,595,2015-08-06  LAFAYETTE, IN                                     
*                                                                               
 RELDT ??2015,209,2015-08-07  WICHITA FALLS, TX                                 
 RELDT ??2015,289,2015-08-07  JACKSON, TN                                       
 RELDT ??2015,497,2015-08-07  JONESBORO, AR                                     
 RELDT ??2015,519,2015-08-07  HARRISONBURG, VA                                  
 RELDT ??2015,520,2015-08-07  BLUEFIELD, WV                                     
 RELDT ??2015,523,2015-08-07  SIOUX CITY,  IA                                   
 RELDT ??2015,545,2015-08-07  RAPID CITY, SD                                    
 RELDT ??2015,548,2015-08-07  SAN ANGELO, TX                                    
 RELDT ??2015,558,2015-08-07  BECKLEY, WV                                       
 RELDT ??2015,582,2015-08-07  BISMARCK, ND                                      
*                                                                               
* SUMMER 2015 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2015,053,2015-10-06  NEW ORLEANS                                       
 RELDT ??2015,055,2015-10-06  LOUISVILLE                                        
 RELDT ??2015,083,2015-10-06  OKLAHOMA CITY                                     
 RELDT ??2015,223,2015-10-06  BATON ROUGE                                       
 RELDT ??2015,540,2015-10-06  PUERTO RICO                                       
*                                                                               
 RELDT ??2015,037,2015-10-07  BUFFALO-NIAGARA FALLS                             
 RELDT ??2015,079,2015-10-07  ROCHESTER, NY                                     
 RELDT ??2015,095,2015-10-07  BIRMINGHAM                                        
 RELDT ??2015,105,2015-10-07  RICHMOND                                          
 RELDT ??2015,191,2015-10-07  GREENVILLE-SPARTANBURG                            
 RELDT ??2015,207,2015-10-07  TUCSON                                            
*                                                                               
 RELDT ??2015,067,2015-10-08  DAYTON                                            
 RELDT ??2015,069,2015-10-08  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2015,089,2015-10-08  FRESNO                                            
 RELDT ??2015,127,2015-10-08  GRAND RAPIDS                                      
 RELDT ??2015,145,2015-10-08  ALLENTOWN-BETHLEHEM                               
 RELDT ??2015,175,2015-10-08  WILKES BARRE-SCRANTON                             
*                                                                               
 RELDT ??2015,085,2015-10-09  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2015,099,2015-10-09  HONOLULU                                          
 RELDT ??2015,103,2015-10-09  TULSA                                             
 RELDT ??2015,121,2015-10-09  KNOXVILLE                                         
 RELDT ??2015,143,2015-10-09  BAKERSFIELD                                       
*                                                                               
 RELDT ??2015,119,2015-10-12  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2015,141,2015-10-12  ALBUQUERQUE                                       
 RELDT ??2015,161,2015-10-12  EL PASO                                           
 RELDT ??2015,301,2015-10-12  YORK                                              
 RELDT ??2015,345,2015-10-12  JOHNSON CITY-KINGSPORT-BRISTOL                    
*                                                                               
 RELDT ??2015,081,2015-10-13  AKRON                                             
 RELDT ??2015,091,2015-10-13  SYRACUSE                                          
 RELDT ??2015,231,2015-10-13  CHARLESTON, SC                                    
 RELDT ??2015,283,2015-10-13  MONTEREY-SALINAS-SANTA CRUZ                       
*                                                                               
 RELDT ??2015,097,2015-10-14  TOLEDO                                            
 RELDT ??2015,117,2015-10-14  SPRINGFIELD, MA                                   
 RELDT ??2015,123,2015-10-14  LITTLE ROCK                                       
 RELDT ??2015,183,2015-10-14  COLUMBIA, SC                                      
 RELDT ??2015,361,2015-10-14  GREENVILLE-NEW BERN-JACKSONVILLE                  
*                                                                               
 RELDT ??2015,071,2015-10-15  DES MOINES                                        
 RELDT ??2015,125,2015-10-15  WICHITA                                           
 RELDT ??2015,133,2015-10-15  MOBILE                                            
 RELDT ??2015,177,2015-10-15  SPOKANE                                           
 RELDT ??2015,233,2015-10-15  COLORADO SPRINGS                                  
*                                                                               
 RELDT ??2015,111,2015-10-16  SHREVEPORT                                        
 RELDT ??2015,169,2015-10-16  JACKSON, MS                                       
 RELDT ??2015,171,2015-10-16  MADISON                                           
 RELDT ??2015,181,2015-10-16  CHATTANOOGA                                       
 RELDT ??2015,327,2015-10-16  HUNTSVILLE                                        
*                                                                               
* FALL 2015 SUMMARY DATA                                                        
*                                                                               
 RELDT ??2015,053,2016-01-04  NEW ORLEANS                                       
 RELDT ??2015,055,2016-01-04  LOUISVILLE                                        
 RELDT ??2015,083,2016-01-04  OKLAHOMA CITY                                     
 RELDT ??2015,223,2016-01-04  BATON ROUGE                                       
 RELDT ??2015,540,2016-01-04  PUERTO RICO                                       
*                                                                               
 RELDT ??2015,037,2016-01-05  BUFFALO-NIAGARA FALLS                             
 RELDT ??2015,079,2016-01-05  ROCHESTER, NY                                     
 RELDT ??2015,095,2016-01-05  BIRMINGHAM                                        
 RELDT ??2015,105,2016-01-05  RICHMOND                                          
 RELDT ??2015,191,2016-01-05  GREENVILLE-SPARTANBURG                            
 RELDT ??2015,253,2016-01-05  LAFAYETTE, LA                                     
 RELDT ??2015,516,2016-01-05  MONMOUTH-OCEAN                                    
 RELDT ??2015,522,2016-01-05  LAUREL-HATTIESBURG, MS                            
 RELDT ??2015,533,2016-01-05  BILOXI-GULFPORT-PASCAGOULA                        
*                                                                               
 RELDT ??2015,069,2016-01-06  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2015,091,2016-01-06  SYRACUSE                                          
 RELDT ??2015,221,2016-01-06  ASHEVILLE                                         
 RELDT ??2015,416,2016-01-06  FREDERICKSBURG                                    
 RELDT ??2015,421,2016-01-06  OLEAN, NY                                         
 RELDT ??2015,543,2016-01-06  TUPELO, MS                                        
 RELDT ??2015,553,2016-01-06  CHARLOTTESVILLE, VA                               
 RELDT ??2015,557,2016-01-06  ELMIRA-CORNING, NY                                
 RELDT ??2015,596,2016-01-06  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2015,067,2016-01-07  DAYTON                                            
 RELDT ??2015,103,2016-01-07  TULSA                                             
 RELDT ??2015,127,2016-01-07  GRAND RAPIDS                                      
 RELDT ??2015,197,2016-01-07  PORTLAND, ME                                      
 RELDT ??2015,207,2016-01-07  TUCSON                                            
 RELDT ??2015,247,2016-01-07  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2015,267,2016-01-07  MANCHESTER                                        
 RELDT ??2015,295,2016-01-07  UTICA-ROME                                        
 RELDT ??2015,393,2016-01-07  HUDSON VALLEY                                     
 RELDT ??2015,426,2016-01-07  CONCORD (LAKES REGION)                            
 RELDT ??2015,431,2016-01-07  LEBANON-HANOVER-WHITE RIVER JUNCTION              
 RELDT ??2015,432,2016-01-07  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2015,505,2016-01-07  BURLINGTON-PLATTSBURGH                            
 RELDT ??2015,518,2016-01-07  POUGHKEEPSIE, NY                                  
 RELDT ??2015,526,2016-01-07  BANGOR                                            
 RELDT ??2015,527,2016-01-07  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2015,577,2016-01-07  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2015,089,2016-01-08  FRESNO                                            
 RELDT ??2015,099,2016-01-08  HONOLULU                                          
 RELDT ??2015,121,2016-01-08  KNOXVILLE                                         
 RELDT ??2015,141,2016-01-08  ALBUQUERQUE                                       
 RELDT ??2015,145,2016-01-08  ALLENTOWN-BETHLEHEM                               
 RELDT ??2015,161,2016-01-08  EL PASO                                           
 RELDT ??2015,175,2016-01-08  WILKES BARRE-SCRANTON                             
 RELDT ??2015,251,2016-01-08  KALAMAZOO                                         
 RELDT ??2015,269,2016-01-08  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2015,345,2016-01-08  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2015,373,2016-01-08  SARASOTA-BRADENTON                                
 RELDT ??2015,515,2016-01-08  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2015,531,2016-01-08  BATTLE CREEK, MI                                  
 RELDT ??2015,547,2016-01-08  STAMFORD-NORWALK, CT                              
 RELDT ??2015,566,2016-01-08  MUSKEGON, MI                                      
 RELDT ??2015,576,2016-01-08  LIMA, OH                                          
 RELDT ??2015,589,2016-01-08  FT. SMITH, AR                                     
 RELDT ??2015,593,2016-01-08  DANBURY, CT                                       
*                                                                               
 RELDT ??2015,081,2016-01-11  AKRON                                             
 RELDT ??2015,085,2016-01-11  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2015,119,2016-01-11  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2015,143,2016-01-11  BAKERSFIELD                                       
 RELDT ??2015,231,2016-01-11  CHARLESTON, SC                                    
 RELDT ??2015,293,2016-01-11  VISALIA-TULARE-HANFORD                            
 RELDT ??2015,301,2016-01-11  YORK                                              
 RELDT ??2015,441,2016-01-11  LAS CRUCES, NM                                    
 RELDT ??2015,536,2016-01-11  MERCED, CA                                        
 RELDT ??2015,578,2016-01-11  WILLIAMSPORT, PA                                  
*                                                                               
 RELDT ??2015,071,2016-01-12  DES MOINES                                        
 RELDT ??2015,082,2016-01-12  CANTON                                            
 RELDT ??2015,117,2016-01-12  SPRINGFIELD, MA                                   
 RELDT ??2015,123,2016-01-12  LITTLE ROCK                                       
 RELDT ??2015,139,2016-01-12  WILMINGTON, DE                                    
 RELDT ??2015,177,2016-01-12  SPOKANE                                           
 RELDT ??2015,183,2016-01-12  COLUMBIA, SC                                      
 RELDT ??2015,255,2016-01-12  LANCASTER                                         
 RELDT ??2015,261,2016-01-12  LINCOLN                                           
 RELDT ??2015,283,2016-01-12  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2015,361,2016-01-12  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2015,510,2016-01-12  MYRTLE BEACH, SC                                  
 RELDT ??2015,524,2016-01-12  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2015,563,2016-01-12  NEW LONDON, CT                                    
*                                                                               
 RELDT ??2015,097,2016-01-13  TOLEDO                                            
 RELDT ??2015,125,2016-01-13  WICHITA                                           
 RELDT ??2015,133,2016-01-13  MOBILE                                            
 RELDT ??2015,171,2016-01-13  MADISON                                           
 RELDT ??2015,233,2016-01-13  COLORADO SPRINGS                                  
 RELDT ??2015,291,2016-01-13  STOCKTON                                          
 RELDT ??2015,383,2016-01-13  HOT SPRINGS, AR                                   
 RELDT ??2015,550,2016-01-13  GAINESVILLE-OCALA                                 
*                                                                               
 RELDT ??2015,181,2016-01-14  CHATTANOOGA                                       
 RELDT ??2015,205,2016-01-14  TOPEKA                                            
 RELDT ??2015,229,2016-01-14  BOISE                                             
 RELDT ??2015,311,2016-01-14  LAKELAND-WINTER HAVEN                             
 RELDT ??2015,317,2016-01-14  PENSACOLA                                         
 RELDT ??2015,327,2016-01-14  HUNTSVILLE                                        
 RELDT ??2015,341,2016-01-14  DAYTONA BEACH                                     
 RELDT ??2015,353,2016-01-14  PUEBLO                                            
 RELDT ??2015,387,2016-01-14  SALINA-MANHATTAN, KS                              
 RELDT ??2015,391,2016-01-14  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2015,462,2016-01-14  TWIN FALLS (SUN VALLEY), ID                       
 RELDT ??2015,517,2016-01-14  FT. PIERCE-STUART-VERO BEACH                      
*                                                                               
 RELDT ??2015,113,2016-01-15  WORCESTER                                         
 RELDT ??2015,165,2016-01-15  FT. WAYNE                                         
 RELDT ??2015,169,2016-01-15  JACKSON, MS                                       
 RELDT ??2015,259,2016-01-15  LEXINGTON-FAYETTE                                 
 RELDT ??2015,277,2016-01-15  ROANOKE-LYNCHBURG                                 
 RELDT ??2015,305,2016-01-15  AUGUSTA, GA                                       
 RELDT ??2015,331,2016-01-15  MELBOURNE-TITUSVILLE-COCOA                        
 RELDT ??2015,367,2016-01-15  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2015,404,2016-01-15  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2015,422,2016-01-15  MUNCIE-MARION, IN                                 
 RELDT ??2015,537,2016-01-15  TRENTON                                           
 RELDT ??2015,579,2016-01-15  SUSSEX, NJ                                        
*                                                                               
 RELDT ??2015,062,2016-01-19  NEW HAVEN                                         
 RELDT ??2015,111,2016-01-19  SHREVEPORT                                        
 RELDT ??2015,287,2016-01-19  SOUTH BEND                                        
 RELDT ??2015,337,2016-01-19  BRIDGEPORT                                        
 RELDT ??2015,343,2016-01-19  MODESTO                                           
 RELDT ??2015,419,2016-01-19  VICTOR VALLEY                                     
 RELDT ??2015,556,2016-01-19  SAN LUIS OBISPO, CA                               
 RELDT ??2015,560,2016-01-19  CHEYENNE, WY                                      
 RELDT ??2015,565,2016-01-19  MORRISTOWN, NJ                                    
 RELDT ??2015,567,2016-01-19  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2015,590,2016-01-19  FT. COLLINS-GREELEY, CO                           
 RELDT ??2015,591,2016-01-19  SANTA BARBARA, CA                                 
 RELDT ??2015,592,2016-01-19  PALM SPRINGS                                      
 RELDT ??2015,594,2016-01-19  OXNARD-VENTURA                                    
*                                                                               
 RELDT ??2015,129,2016-01-20  YOUNGSTOWN-WARREN                                 
 RELDT ??2015,155,2016-01-20  CORPUS CHRISTI                                    
 RELDT ??2015,163,2016-01-20  FLINT                                             
 RELDT ??2015,195,2016-01-20  LANSING-EAST LANSING                              
 RELDT ??2015,245,2016-01-20  GREEN BAY                                         
 RELDT ??2015,273,2016-01-20  READING, PA                                       
 RELDT ??2015,275,2016-01-20  RENO                                              
 RELDT ??2015,281,2016-01-20  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2015,303,2016-01-20  APPLETON-OSHKOSH                                  
 RELDT ??2015,359,2016-01-20  FAYETTEVILLE, NC                                  
 RELDT ??2015,439,2016-01-20  LUFKIN-NACOGDOCHES, TX                            
 RELDT ??2015,502,2016-01-20  TYLER-LONGVIEW                                    
 RELDT ??2015,503,2016-01-20  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2015,512,2016-01-20  TEXARKANA, TX-AR                                  
*                                                                               
 RELDT ??2015,137,2016-01-21  PEORIA                                            
 RELDT ??2015,149,2016-01-21  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2015,157,2016-01-21  QUAD CITIES                                       
 RELDT ??2015,173,2016-01-21  MONTGOMERY                                        
 RELDT ??2015,203,2016-01-21  SPRINGFIELD, MO                                   
 RELDT ??2015,271,2016-01-21  SALISBURY-OCEAN CITY                              
 RELDT ??2015,285,2016-01-21  SAVANNAH                                          
 RELDT ??2015,323,2016-01-21  BLOOMINGTON                                       
 RELDT ??2015,365,2016-01-21  NEW BEDFORD-FALL RIVER, MA                        
 RELDT ??2015,423,2016-01-21  HILTON HEAD, SC                                   
 RELDT ??2015,425,2016-01-21  LASALLE-PERU, IL                                  
 RELDT ??2015,501,2016-01-21  DOTHAN, AL                                        
 RELDT ??2015,513,2016-01-21  BRUNSWICK, GA                                     
 RELDT ??2015,562,2016-01-21  KILLEEN-TEMPLE, TX                                
 RELDT ??2015,580,2016-01-21  ALBANY, GA                                        
 RELDT ??2015,581,2016-01-21  ANN ARBOR                                         
 RELDT ??2015,584,2016-01-21  LAKE CHARLES, LA                                  
*                                                                               
 RELDT ??2015,153,2016-01-22  CHARLESTON, WV                                    
 RELDT ??2015,185,2016-01-22  EVANSVILLE                                        
 RELDT ??2015,193,2016-01-22  HUNTINGTON-ASHLAND                                
 RELDT ??2015,227,2016-01-22  BINGHAMTON                                        
 RELDT ??2015,239,2016-01-22  ERIE                                              
 RELDT ??2015,241,2016-01-22  EUGENE-SPRINGFIELD                                
 RELDT ??2015,265,2016-01-22  MACON                                             
 RELDT ??2015,279,2016-01-22  ROCKFORD                                          
 RELDT ??2015,315,2016-01-22  ANCHORAGE                                         
 RELDT ??2015,333,2016-01-22  TALLAHASSEE                                       
 RELDT ??2015,412,2016-01-22  FLAGSTAFF-PRESCOTT, AZ                            
 RELDT ??2015,433,2016-01-22  VALDOSTA, GA                                      
 RELDT ??2015,555,2016-01-22  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2015,597,2016-01-22  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2015,147,2016-01-25  AMARILLO, TX                                      
 RELDT ??2015,235,2016-01-25  COLUMBUS, GA                                      
 RELDT ??2015,263,2016-01-25  LUBBOCK                                           
 RELDT ??2015,307,2016-01-25  TERRE HAUTE                                       
 RELDT ??2015,309,2016-01-25  WACO, TX                                          
 RELDT ??2015,371,2016-01-25  TRI-CITIES, WA                                    
 RELDT ??2015,418,2016-01-25  SHEBOYGAN, WI                                     
 RELDT ??2015,509,2016-01-25  REDDING, CA                                       
 RELDT ??2015,528,2016-01-25  FREDERICK, MD                                     
 RELDT ??2015,530,2016-01-25  CAPE COD, MA                                      
 RELDT ??2015,535,2016-01-25  TRAVERSE CITY-PETOSKEY, MI                        
 RELDT ??2015,549,2016-01-25  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2015,561,2016-01-25  ODESSA-MIDLAND, TX                                
*                                                                               
 RELDT ??2015,151,2016-01-26  CEDAR RAPIDS                                      
 RELDT ??2015,159,2016-01-26  DULUTH-SUPERIOR                                   
 RELDT ??2015,187,2016-01-26  FARGO-MOORHEAD                                    
 RELDT ??2015,297,2016-01-26  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2015,325,2016-01-26  YAKIMA, WA                                        
 RELDT ??2015,339,2016-01-26  MEDFORD-ASHLAND, OR                               
 RELDT ??2015,436,2016-01-26  BEND, OR                                          
 RELDT ??2015,499,2016-01-26  DECATUR, IL                                       
 RELDT ??2015,506,2016-01-26  LAREDO, TX                                        
 RELDT ??2015,507,2016-01-26  CHAMPAIGN, IL                                     
 RELDT ??2015,508,2016-01-26  CHICO, CA                                         
 RELDT ??2015,554,2016-01-26  FLORENCE, SC                                      
 RELDT ??2015,569,2016-01-26  WINCHESTER, VA                                    
 RELDT ??2015,572,2016-01-26  GRAND FORKS, ND-MN                                
*                                                                               
 RELDT ??2015,179,2016-01-27  WHEELING                                          
 RELDT ??2015,219,2016-01-27  ALTOONA                                           
 RELDT ??2015,225,2016-01-27  BILLINGS, MT                                      
 RELDT ??2015,381,2016-01-27  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2015,511,2016-01-27  GRAND JUNCTION, CO                                
 RELDT ??2015,532,2016-01-27  ROCHESTER, MN                                     
 RELDT ??2015,534,2016-01-27  JOPLIN, MO                                        
 RELDT ??2015,539,2016-01-27  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2015,546,2016-01-27  ABILENE, TX                                       
 RELDT ??2015,559,2016-01-27  LA CROSSE, WI                                     
 RELDT ??2015,564,2016-01-27  COLUMBIA, MO                                      
 RELDT ??2015,573,2016-01-27  PANAMA CITY, FL                                   
 RELDT ??2015,585,2016-01-27  MONROE, LA                                        
 RELDT ??2015,586,2016-01-27  EAU CLAIRE, WI                                    
 RELDT ??2015,595,2016-01-27  LAFAYETTE, IN                                     
*                                                                               
 RELDT ??2015,209,2016-01-28  WICHITA FALLS, TX                                 
 RELDT ??2015,289,2016-01-28  JACKSON, TN                                       
 RELDT ??2015,497,2016-01-28  JONESBORO, AR                                     
 RELDT ??2015,519,2016-01-28  HARRISONBURG, VA                                  
 RELDT ??2015,520,2016-01-28  BLUEFIELD, WV                                     
 RELDT ??2015,523,2016-01-28  SIOUX CITY,  IA                                   
 RELDT ??2015,545,2016-01-28  RAPID CITY, SD                                    
 RELDT ??2015,548,2016-01-28  SAN ANGELO, TX                                    
 RELDT ??2015,558,2016-01-28  BECKLEY, WV                                       
 RELDT ??2015,582,2016-01-28  BISMARCK, ND                                      
*                                                                               
* WINTER 2016 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2016,053,2016-04-19  NEW ORLEANS                                       
 RELDT ??2016,055,2016-04-19  LOUISVILLE                                        
 RELDT ??2016,083,2016-04-19  OKLAHOMA CITY                                     
 RELDT ??2016,223,2016-04-19  BATON ROUGE                                       
 RELDT ??2016,540,2016-04-19  PUERTO RICO                                       
*                                                                               
 RELDT ??2016,037,2016-04-20  BUFFALO-NIAGARA FALLS                             
 RELDT ??2016,079,2016-04-20  ROCHESTER, NY                                     
 RELDT ??2016,095,2016-04-20  BIRMINGHAM                                        
 RELDT ??2016,105,2016-04-20  RICHMOND                                          
 RELDT ??2016,191,2016-04-20  GREENVILLE-SPARTANBURG                            
 RELDT ??2016,207,2016-04-20  TUCSON                                            
*                                                                               
 RELDT ??2016,067,2016-04-21  DAYTON                                            
 RELDT ??2016,069,2016-04-21  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2016,089,2016-04-21  FRESNO                                            
 RELDT ??2016,127,2016-04-21  GRAND RAPIDS                                      
 RELDT ??2016,145,2016-04-21  ALLENTOWN-BETHLEHEM                               
 RELDT ??2016,175,2016-04-21  WILKES BARRE-SCRANTON                             
*                                                                               
 RELDT ??2016,085,2016-04-22  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2016,099,2016-04-22  HONOLULU                                          
 RELDT ??2016,103,2016-04-22  TULSA                                             
 RELDT ??2016,121,2016-04-22  KNOXVILLE                                         
 RELDT ??2016,143,2016-04-22  BAKERSFIELD                                       
*                                                                               
 RELDT ??2016,119,2016-04-25  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2016,141,2016-04-25  ALBUQUERQUE                                       
 RELDT ??2016,161,2016-04-25  EL PASO                                           
 RELDT ??2016,301,2016-04-25  YORK                                              
 RELDT ??2016,345,2016-04-25  JOHNSON CITY-KINGSPORT-BRISTOL                    
*                                                                               
 RELDT ??2016,081,2016-04-26  AKRON                                             
 RELDT ??2016,091,2016-04-26  SYRACUSE                                          
 RELDT ??2016,231,2016-04-26  CHARLESTON, SC                                    
 RELDT ??2016,283,2016-04-26  MONTEREY-SALINAS-SANTA CRUZ                       
*                                                                               
 RELDT ??2016,097,2016-04-27  TOLEDO                                            
 RELDT ??2016,117,2016-04-27  SPRINGFIELD, MA                                   
 RELDT ??2016,123,2016-04-27  LITTLE ROCK                                       
 RELDT ??2016,183,2016-04-27  COLUMBIA, SC                                      
 RELDT ??2016,361,2016-04-27  GREENVILLE-NEW BERN-JACKSONVILLE                  
*                                                                               
 RELDT ??2016,071,2016-04-28  DES MOINES                                        
 RELDT ??2016,125,2016-04-28  WICHITA                                           
 RELDT ??2016,133,2016-04-28  MOBILE                                            
 RELDT ??2016,177,2016-04-28  SPOKANE                                           
 RELDT ??2016,233,2016-04-28  COLORADO SPRINGS                                  
*                                                                               
 RELDT ??2016,111,2016-04-29  SHREVEPORT                                        
 RELDT ??2016,169,2016-04-29  JACKSON, MS                                       
 RELDT ??2016,171,2016-04-29  MADISON                                           
 RELDT ??2016,181,2016-04-29  CHATTANOOGA                                       
 RELDT ??2016,327,2016-04-29  HUNTSVILLE                                        
*                                                                               
* SPRING 2016 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2016,053,2016-07-13  NEW ORLEANS                                       
 RELDT ??2016,055,2016-07-13  LOUISVILLE                                        
 RELDT ??2016,083,2016-07-13  OKLAHOMA CITY                                     
 RELDT ??2016,223,2016-07-13  BATON ROUGE                                       
 RELDT ??2016,253,2016-07-13  LAFAYETTE, LA                                     
 RELDT ??2016,516,2016-07-13  MONMOUTH-OCEAN                                    
 RELDT ??2016,522,2016-07-13  LAUREL-HATTIESBURG, MS                            
 RELDT ??2016,533,2016-07-13  BILOXI-GULFPORT-PASCAGOULA                        
 RELDT ??2016,540,2016-07-13  PUERTO RICO                                       
 *                                                                              
 RELDT ??2016,037,2016-07-14  BUFFALO-NIAGARA FALLS                             
 RELDT ??2016,079,2016-07-14  ROCHESTER, NY                                     
 RELDT ??2016,095,2016-07-14  BIRMINGHAM                                        
 RELDT ??2016,105,2016-07-14  RICHMOND                                          
 RELDT ??2016,191,2016-07-14  GREENVILLE-SPARTANBURG                            
 RELDT ??2016,221,2016-07-14  ASHEVILLE                                         
 RELDT ??2016,416,2016-07-14  FREDERICKSBURG                                    
 RELDT ??2016,421,2016-07-14  OLEAN, NY                                         
 RELDT ??2016,553,2016-07-14  CHARLOTTESVILLE, VA                               
 RELDT ??2016,557,2016-07-14  ELMIRA-CORNING, NY                                
 RELDT ??2016,596,2016-07-14  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2016,069,2016-07-15  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2016,091,2016-07-15  SYRACUSE                                          
 RELDT ??2016,197,2016-07-15  PORTLAND, ME                                      
 RELDT ??2016,247,2016-07-15  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2016,267,2016-07-15  MANCHESTER                                        
 RELDT ??2016,295,2016-07-15  UTICA-ROME                                        
 RELDT ??2016,393,2016-07-15  HUDSON VALLEY                                     
 RELDT ??2016,426,2016-07-15  CONCORD (LAKES REGION)                            
 RELDT ??2016,431,2016-07-15  LEBANON-HANOVER-WHTE RVR JNCT                     
 RELDT ??2016,432,2016-07-15  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2016,505,2016-07-15  BURLINGTON-PLATTSBURGH                            
 RELDT ??2016,518,2016-07-15  POUGHKEEPSIE, NY                                  
 RELDT ??2016,526,2016-07-15  BANGOR                                            
 RELDT ??2016,527,2016-07-15  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2016,542,2016-07-15  NEWBURGH-MIDDLETOWN, NY(MD-HUD VLY)               
 RELDT ??2016,577,2016-07-15  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2016,067,2016-07-18  DAYTON                                            
 RELDT ??2016,103,2016-07-18  TULSA                                             
 RELDT ??2016,127,2016-07-18  GRAND RAPIDS                                      
 RELDT ??2016,207,2016-07-18  TUCSON                                            
 RELDT ??2016,251,2016-07-18  KALAMAZOO                                         
 RELDT ??2016,269,2016-07-18  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2016,373,2016-07-18  SARASOTA-BRADENTON                                
 RELDT ??2016,515,2016-07-18  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2016,531,2016-07-18  BATTLE CREEK, MI                                  
 RELDT ??2016,547,2016-07-18  STAMFORD-NORWALK, CT                              
 RELDT ??2016,566,2016-07-18  MUSKEGON, MI                                      
 RELDT ??2016,576,2016-07-18  LIMA, OH                                          
 RELDT ??2016,589,2016-07-18  FT. SMITH, AR                                     
 RELDT ??2016,593,2016-07-18  DANBURY, CT                                       
*                                                                               
 RELDT ??2016,089,2016-07-19  FRESNO                                            
 RELDT ??2016,099,2016-07-19  HONOLULU                                          
 RELDT ??2016,121,2016-07-19  KNOXVILLE                                         
 RELDT ??2016,141,2016-07-19  ALBUQUERQUE                                       
 RELDT ??2016,145,2016-07-19  ALLENTOWN-BETHLEHEM                               
 RELDT ??2016,161,2016-07-19  EL PASO                                           
 RELDT ??2016,175,2016-07-19  WILKES BARRE-SCRANTON                             
 RELDT ??2016,293,2016-07-19  VISALIA-TULARE-HANFORD                            
 RELDT ??2016,345,2016-07-19  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2016,434,2016-07-19  SUNBURY-SELINSGROVE-LEWISBURG, PA                 
 RELDT ??2016,441,2016-07-19  LAS CRUCES, NM                                    
 RELDT ??2016,536,2016-07-19  MERCED, CA                                        
 RELDT ??2016,578,2016-07-19  WILLIAMSPORT, PA                                  
*                                                                               
 RELDT ??2016,081,2016-07-20  AKRON                                             
 RELDT ??2016,082,2016-07-20  CANTON                                            
 RELDT ??2016,085,2016-07-20  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2016,119,2016-07-20  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2016,139,2016-07-20  WILMINGTON, DE                                    
 RELDT ??2016,143,2016-07-20  BAKERSFIELD                                       
 RELDT ??2016,231,2016-07-20  CHARLESTON, SC                                    
 RELDT ??2016,255,2016-07-20  LANCASTER                                         
 RELDT ??2016,261,2016-07-20  LINCOLN                                           
 RELDT ??2016,301,2016-07-20  YORK                                              
 RELDT ??2016,510,2016-07-20  MYRTLE BEACH, SC                                  
 RELDT ??2016,524,2016-07-20  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2016,563,2016-07-20  NEW LONDON, CT                                    
*                                                                               
 RELDT ??2016,071,2016-07-21  DES MOINES                                        
 RELDT ??2016,117,2016-07-21  SPRINGFIELD, MA                                   
 RELDT ??2016,123,2016-07-21  LITTLE ROCK                                       
 RELDT ??2016,177,2016-07-21  SPOKANE                                           
 RELDT ??2016,183,2016-07-21  COLUMBIA, SC                                      
 RELDT ??2016,283,2016-07-21  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2016,291,2016-07-21  STOCKTON                                          
 RELDT ??2016,361,2016-07-21  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2016,383,2016-07-21  HOT SPRINGS, AR                                   
 RELDT ??2016,550,2016-07-21  GAINESVILLE-OCALA                                 
*                                                                               
 RELDT ??2016,097,2016-07-22  TOLEDO                                            
 RELDT ??2016,125,2016-07-22  WICHITA                                           
 RELDT ??2016,133,2016-07-22  MOBILE                                            
 RELDT ??2016,171,2016-07-22  MADISON                                           
 RELDT ??2016,205,2016-07-22  TOPEKA                                            
 RELDT ??2016,229,2016-07-22  BOISE                                             
 RELDT ??2016,233,2016-07-22  COLORADO SPRINGS                                  
 RELDT ??2016,311,2016-07-22  LAKELAND-WINTER HAVEN                             
 RELDT ??2016,317,2016-07-22  PENSACOLA                                         
 RELDT ??2016,341,2016-07-22  DAYTONA BEACH                                     
 RELDT ??2016,353,2016-07-22  PUEBLO                                            
 RELDT ??2016,387,2016-07-22  SALINA-MANHATTAN, KS                              
 RELDT ??2016,391,2016-07-22  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2016,462,2016-07-22  TWIN FALLS-SUN VALLEY, ID                         
 RELDT ??2016,517,2016-07-22  FT. PIERCE-STUART-VERO BEACH                      
*                                                                               
 RELDT ??2016,113,2016-07-25  WORCESTER                                         
 RELDT ??2016,165,2016-07-25  FT. WAYNE                                         
 RELDT ??2016,181,2016-07-25  CHATTANOOGA                                       
 RELDT ??2016,259,2016-07-25  LEXINGTON-FAYETTE                                 
 RELDT ??2016,277,2016-07-25  ROANOKE-LYNCHBURG                                 
 RELDT ??2016,305,2016-07-25  AUGUSTA, GA                                       
 RELDT ??2016,327,2016-07-25  HUNTSVILLE                                        
 RELDT ??2016,331,2016-07-25  MELBOURNE-TITUSVILLE-COCOA                        
 RELDT ??2016,367,2016-07-25  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2016,404,2016-07-25  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2016,422,2016-07-25  MUNCIE-MARION, IN                                 
 RELDT ??2016,537,2016-07-25  TRENTON                                           
 RELDT ??2016,579,2016-07-25  SUSSEX, NJ                                        
*                                                                               
 RELDT ??2016,062,2016-07-26  NEW HAVEN                                         
 RELDT ??2016,169,2016-07-26  JACKSON, MS                                       
 RELDT ??2016,287,2016-07-26  SOUTH BEND                                        
 RELDT ??2016,337,2016-07-26  BRIDGEPORT                                        
 RELDT ??2016,343,2016-07-26  MODESTO                                           
 RELDT ??2016,419,2016-07-26  VICTOR VALLEY                                     
 RELDT ??2016,556,2016-07-26  SAN LUIS OBISPO, CA                               
 RELDT ??2016,560,2016-07-26  CHEYENNE, WY                                      
 RELDT ??2016,565,2016-07-26  MORRISTOWN, NJ                                    
 RELDT ??2016,567,2016-07-26  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2016,590,2016-07-26  FT. COLLINS-GREELEY, CO                           
 RELDT ??2016,591,2016-07-26  SANTA BARBARA, CA                                 
 RELDT ??2016,592,2016-07-26  PALM SPRINGS                                      
 RELDT ??2016,594,2016-07-26  OXNARD-VENTURA                                    
*                                                                               
 RELDT ??2016,111,2016-07-27  SHREVEPORT                                        
 RELDT ??2016,129,2016-07-27  YOUNGSTOWN-WARREN                                 
 RELDT ??2016,155,2016-07-27  CORPUS CHRISTI                                    
 RELDT ??2016,163,2016-07-27  FLINT                                             
 RELDT ??2016,195,2016-07-27  LANSING-EAST LANSING                              
 RELDT ??2016,245,2016-07-27  GREEN BAY                                         
 RELDT ??2016,273,2016-07-27  READING, PA                                       
 RELDT ??2016,275,2016-07-27  RENO                                              
 RELDT ??2016,281,2016-07-27  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2016,303,2016-07-27  APPLETON-OSHKOSH                                  
 RELDT ??2016,359,2016-07-27  FAYETTEVILLE, NC                                  
 RELDT ??2016,502,2016-07-27  TYLER-LONGVIEW                                    
 RELDT ??2016,503,2016-07-27  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2016,512,2016-07-27  TEXARKANA, TX-AR                                  
*                                                                               
 RELDT ??2016,137,2016-07-28  PEORIA                                            
 RELDT ??2016,149,2016-07-28  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2016,157,2016-07-28  QUAD CITIES                                       
 RELDT ??2016,173,2016-07-28  MONTGOMERY                                        
 RELDT ??2016,203,2016-07-28  SPRINGFIELD, MO                                   
 RELDT ??2016,271,2016-07-28  SALISBURY-OCEAN CITY                              
 RELDT ??2016,285,2016-07-28  SAVANNAH                                          
 RELDT ??2016,323,2016-07-28  BLOOMINGTON                                       
 RELDT ??2016,365,2016-07-28  NEW BEDFORD-FALL RIVER, MA                        
 RELDT ??2016,423,2016-07-28  HILTON HEAD, SC                                   
 RELDT ??2016,425,2016-07-28  LASALLE-PERU, IL                                  
 RELDT ??2016,501,2016-07-28  DOTHAN, AL                                        
 RELDT ??2016,513,2016-07-28  BRUNSWICK, GA                                     
 RELDT ??2016,562,2016-07-28  KILLEEN-TEMPLE, TX                                
 RELDT ??2016,580,2016-07-28  ALBANY, GA                                        
 RELDT ??2016,581,2016-07-28  ANN ARBOR, MI                                     
 RELDT ??2016,584,2016-07-28  LAKE CHARLES, LA                                  
*                                                                               
 RELDT ??2016,153,2016-07-29  CHARLESTON, WV                                    
 RELDT ??2016,185,2016-07-29  EVANSVILLE                                        
 RELDT ??2016,193,2016-07-29  HUNTINGTON-ASHLAND                                
 RELDT ??2016,227,2016-07-29  BINGHAMTON                                        
 RELDT ??2016,239,2016-07-29  ERIE                                              
 RELDT ??2016,241,2016-07-29  EUGENE-SPRINGFIELD                                
 RELDT ??2016,265,2016-07-29  MACON                                             
 RELDT ??2016,279,2016-07-29  ROCKFORD                                          
 RELDT ??2016,315,2016-07-29  ANCHORAGE                                         
 RELDT ??2016,333,2016-07-29  TALLAHASSEE                                       
 RELDT ??2016,412,2016-07-29  FLAGSTAFF-PRESCOTT, AZ                            
 RELDT ??2016,433,2016-07-29  VALDOSTA, GA                                      
 RELDT ??2016,555,2016-07-29  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2016,597,2016-07-29  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2016,147,2016-08-01  AMARILLO, TX                                      
 RELDT ??2016,235,2016-08-01  COLUMBUS, GA                                      
 RELDT ??2016,263,2016-08-01  LUBBOCK                                           
 RELDT ??2016,307,2016-08-01  TERRE HAUTE                                       
 RELDT ??2016,309,2016-08-01  WACO, TX                                          
 RELDT ??2016,335,2016-08-01  TRAVERSE CITY-PETOSKEY-CADILLAC, MI               
 RELDT ??2016,371,2016-08-01  TRI-CITIES, WA                                    
 RELDT ??2016,418,2016-08-01  SHEBOYGAN, WI                                     
 RELDT ??2016,509,2016-08-01  REDDING, CA                                       
 RELDT ??2016,528,2016-08-01  FREDERICK, MD                                     
 RELDT ??2016,530,2016-08-01  CAPE COD, MA                                      
 RELDT ??2016,549,2016-08-01  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2016,561,2016-08-01  ODESSA-MIDLAND, TX                                
*                                                                               
 RELDT ??2016,151,2016-08-02  CEDAR RAPIDS                                      
 RELDT ??2016,159,2016-08-02  DULUTH-SUPERIOR                                   
 RELDT ??2016,187,2016-08-02  FARGO-MOORHEAD                                    
 RELDT ??2016,297,2016-08-02  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2016,325,2016-08-02  YAKIMA, WA                                        
 RELDT ??2016,339,2016-08-02  MEDFORD-ASHLAND, OR                               
 RELDT ??2016,436,2016-08-02  BEND, OR                                          
 RELDT ??2016,499,2016-08-02  DECATUR, IL                                       
 RELDT ??2016,506,2016-08-02  LAREDO, TX                                        
 RELDT ??2016,507,2016-08-02  CHAMPAIGN, IL                                     
 RELDT ??2016,508,2016-08-02  CHICO, CA                                         
 RELDT ??2016,554,2016-08-02  FLORENCE, SC                                      
 RELDT ??2016,569,2016-08-02  WINCHESTER, VA                                    
 RELDT ??2016,572,2016-08-02  GRAND FORKS, ND-MN                                
*                                                                               
 RELDT ??2016,179,2016-08-03  WHEELING                                          
 RELDT ??2016,219,2016-08-03  ALTOONA                                           
 RELDT ??2016,225,2016-08-03  BILLINGS, MT                                      
 RELDT ??2016,381,2016-08-03  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2016,511,2016-08-03  GRAND JUNCTION, CO                                
 RELDT ??2016,532,2016-08-03  ROCHESTER, MN                                     
 RELDT ??2016,534,2016-08-03  JOPLIN, MO                                        
 RELDT ??2016,539,2016-08-03  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2016,546,2016-08-03  ABILENE, TX                                       
 RELDT ??2016,559,2016-08-03  LA CROSSE, WI                                     
 RELDT ??2016,564,2016-08-03  COLUMBIA, MO                                      
 RELDT ??2016,573,2016-08-03  PANAMA CITY, FL                                   
 RELDT ??2016,585,2016-08-03  MONROE, LA                                        
 RELDT ??2016,586,2016-08-03  EAU CLAIRE, WI                                    
 RELDT ??2016,595,2016-08-03  LAFAYETTE, IN                                     
*                                                                               
 RELDT ??2016,209,2016-08-04  WICHITA FALLS, TX                                 
 RELDT ??2016,289,2016-08-04  JACKSON, TN                                       
 RELDT ??2016,319,2016-08-04  SIOUX FALLS, SD                                   
 RELDT ??2016,497,2016-08-04  JONESBORO, AR                                     
 RELDT ??2016,519,2016-08-04  HARRISONBURG, VA                                  
 RELDT ??2016,520,2016-08-04  BLUEFIELD, WV                                     
 RELDT ??2016,523,2016-08-04  SIOUX CITY,  IA                                   
 RELDT ??2016,545,2016-08-04  RAPID CITY, SD                                    
 RELDT ??2016,548,2016-08-04  SAN ANGELO, TX                                    
 RELDT ??2016,558,2016-08-04  BECKLEY, WV                                       
 RELDT ??2016,582,2016-08-04  BISMARCK, ND                                      
*                                                                               
*                                                                               
* WINTER 2017 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2017,055,2017-04-25  LOUISVILLE                                        
 RELDT ??2017,540,2017-04-25  PUERTO RICO                                       
 RELDT ??2017,223,2017-04-25  BATON ROUGE                                       
 RELDT ??2017,053,2017-04-25  NEW ORLEANS                                       
 RELDT ??2017,083,2017-04-25  OKLAHOMA CITY                                     
*                                                                               
 RELDT ??2017,037,2017-04-26  BUFFALO-NIAGARA FALLS                             
 RELDT ??2017,191,2017-04-26  GREENVILLE-SPARTANBURG                            
 RELDT ??2017,105,2017-04-26  RICHMOND                                          
 RELDT ??2017,079,2017-04-26  ROCHESTER, NY                                     
 RELDT ??2017,095,2017-04-26  BIRMINGHAM                                        
 RELDT ??2017,207,2017-04-26  TUCSON                                            
*                                                                               
 RELDT ??2017,069,2017-04-27  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2017,145,2017-04-27  ALLENTOWN-BETHLEHEM                               
 RELDT ??2017,067,2017-04-27  DAYTON                                            
 RELDT ??2017,127,2017-04-27  GRAND RAPIDS                                      
 RELDT ??2017,175,2017-04-27  WILKES BARRE-SCRANTON                             
 RELDT ??2017,089,2017-04-27  FRESNO                                            
*                                                                               
 RELDT ??2017,099,2017-04-28  HONOLULU                                          
 RELDT ??2017,121,2017-04-28  KNOXVILLE                                         
 RELDT ??2017,143,2017-04-28  BAKERSFIELD                                       
 RELDT ??2017,085,2017-04-28  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2017,103,2017-04-28  TULSA                                             
*                                                                               
 RELDT ??2017,119,2017-05-01  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2017,345,2017-05-01  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2017,301,2017-05-01  YORK                                              
 RELDT ??2017,141,2017-05-01  ALBUQUERQUE                                       
 RELDT ??2017,161,2017-05-01  EL PASO                                           
*                                                                               
 RELDT ??2017,081,2017-05-02  AKRON                                             
 RELDT ??2017,231,2017-05-02  CHARLESTON, SC                                    
 RELDT ??2017,091,2017-05-02  SYRACUSE                                          
 RELDT ??2017,283,2017-05-02  MONTEREY-SALINAS-SANTA CRUZ                       
*                                                                               
 RELDT ??2017,183,2017-05-03  COLUMBIA, SC                                      
 RELDT ??2017,361,2017-05-03  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2017,117,2017-05-03  SPRINGFIELD, MA                                   
 RELDT ??2017,097,2017-05-03  TOLEDO                                            
 RELDT ??2017,123,2017-05-03  LITTLE ROCK                                       
*                                                                               
 RELDT ??2017,233,2017-05-04  COLORADO SPRINGS                                  
 RELDT ??2017,071,2017-05-04  DES MOINES                                        
 RELDT ??2017,133,2017-05-04  MOBILE                                            
 RELDT ??2017,177,2017-05-04  SPOKANE                                           
 RELDT ??2017,125,2017-05-04  WICHITA                                           
*                                                                               
 RELDT ??2017,181,2017-05-05  CHATTANOOGA                                       
 RELDT ??2017,327,2017-05-05  HUNTSVILLE                                        
 RELDT ??2017,169,2017-05-05  JACKSON, MS                                       
 RELDT ??2017,171,2017-05-05  MADISON                                           
 RELDT ??2017,111,2017-05-05  SHREVEPORT                                        
*                                                                               
* SPRING 2017 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2017,053,2017-07-19  NEW ORLEANS                                       
 RELDT ??2017,055,2017-07-19  LOUISVILLE                                        
 RELDT ??2017,083,2017-07-19  OKLAHOMA CITY                                     
 RELDT ??2017,223,2017-07-19  BATON ROUGE                                       
 RELDT ??2017,253,2017-07-19  LAFAYETTE, LA                                     
 RELDT ??2017,516,2017-07-19  MONMOUTH-OCEAN                                    
 RELDT ??2017,522,2017-07-19  LAUREL-HATTIESBURG, MS                            
 RELDT ??2017,533,2017-07-19  BILOXI-GULFPORT-PASCAGOULA                        
 RELDT ??2017,540,2017-07-19  PUERTO RICO                                       
 *                                                                              
 RELDT ??2017,037,2017-07-20  BUFFALO-NIAGARA FALLS                             
 RELDT ??2017,079,2017-07-20  ROCHESTER, NY                                     
 RELDT ??2017,095,2017-07-20  BIRMINGHAM                                        
 RELDT ??2017,105,2017-07-20  RICHMOND                                          
 RELDT ??2017,191,2017-07-20  GREENVILLE-SPARTANBURG                            
 RELDT ??2017,221,2017-07-20  ASHEVILLE                                         
 RELDT ??2017,416,2017-07-20  FREDERICKSBURG                                    
 RELDT ??2017,421,2017-07-20  OLEAN, NY                                         
 RELDT ??2017,553,2017-07-20  CHARLOTTESVILLE, VA                               
 RELDT ??2017,557,2017-07-20  ELMIRA-CORNING, NY                                
 RELDT ??2017,596,2017-07-20  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2017,069,2017-07-21  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2017,091,2017-07-21  SYRACUSE                                          
 RELDT ??2017,197,2017-07-21  PORTLAND, ME                                      
 RELDT ??2017,247,2017-07-21  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2017,267,2017-07-21  MANCHESTER                                        
 RELDT ??2017,295,2017-07-21  UTICA-ROME                                        
 RELDT ??2017,393,2017-07-21  HUDSON VALLEY                                     
 RELDT ??2017,426,2017-07-21  CONCORD (LAKES REGION)                            
 RELDT ??2017,431,2017-07-21  LEBANON-HANOVER-WHTE RVR JNCT                     
 RELDT ??2017,432,2017-07-21  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2017,505,2017-07-21  BURLINGTON-PLATTSBURGH                            
 RELDT ??2017,518,2017-07-21  POUGHKEEPSIE, NY                                  
 RELDT ??2017,526,2017-07-21  BANGOR                                            
 RELDT ??2017,527,2017-07-21  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2017,542,2017-07-21  NEWBURGH-MIDDLETOWN, NY(MD-HUD VLY)               
 RELDT ??2017,577,2017-07-21  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2017,067,2017-07-24  DAYTON                                            
 RELDT ??2017,103,2017-07-24  TULSA                                             
 RELDT ??2017,127,2017-07-24  GRAND RAPIDS                                      
 RELDT ??2017,207,2017-07-24  TUCSON                                            
 RELDT ??2017,251,2017-07-24  KALAMAZOO                                         
 RELDT ??2017,269,2017-07-24  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2017,373,2017-07-24  SARASOTA-BRADENTON                                
 RELDT ??2017,515,2017-07-24  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2017,531,2017-07-24  BATTLE CREEK, MI                                  
 RELDT ??2017,547,2017-07-24  STAMFORD-NORWALK, CT                              
 RELDT ??2017,566,2017-07-24  MUSKEGON, MI                                      
 RELDT ??2017,576,2017-07-24  LIMA, OH                                          
 RELDT ??2017,589,2017-07-24  FT. SMITH, AR                                     
 RELDT ??2017,593,2017-07-24  DANBURY, CT                                       
*                                                                               
 RELDT ??2017,089,2017-07-25  FRESNO                                            
 RELDT ??2017,099,2017-07-25  HONOLULU                                          
 RELDT ??2017,121,2017-07-25  KNOXVILLE                                         
 RELDT ??2017,141,2017-07-25  ALBUQUERQUE                                       
 RELDT ??2017,145,2017-07-25  ALLENTOWN-BETHLEHEM                               
 RELDT ??2017,161,2017-07-25  EL PASO                                           
 RELDT ??2017,175,2017-07-25  WILKES BARRE-SCRANTON                             
 RELDT ??2017,293,2017-07-25  VISALIA-TULARE-HANFORD                            
 RELDT ??2017,345,2017-07-25  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2017,434,2016-07-25  SUNBURY-SELINSGROVE-LEWISBURG, PA                 
** RELDT ??2017,441,2016-07-19  LAS CRUCES, NM                                  
 RELDT ??2017,536,2017-07-25  MERCED, CA                                        
 RELDT ??2017,578,2017-07-25  WILLIAMSPORT, PA                                  
*                                                                               
 RELDT ??2017,081,2017-07-26  AKRON                                             
 RELDT ??2017,082,2017-07-26  CANTON                                            
 RELDT ??2017,085,2017-07-26  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2017,119,2017-07-26  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2017,139,2016-07-26  WILMINGTON, DE                                    
 RELDT ??2017,143,2017-07-26  BAKERSFIELD                                       
 RELDT ??2017,231,2017-07-26  CHARLESTON, SC                                    
 RELDT ??2017,255,2017-07-26  LANCASTER                                         
 RELDT ??2017,261,2017-07-26  LINCOLN                                           
 RELDT ??2017,301,2017-07-26  YORK                                              
 RELDT ??2017,510,2017-07-26  MYRTLE BEACH, SC                                  
 RELDT ??2017,524,2017-07-26  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2017,563,2017-07-26  NEW LONDON, CT                                    
*                                                                               
 RELDT ??2017,071,2017-07-27  DES MOINES                                        
 RELDT ??2017,117,2017-07-27  SPRINGFIELD, MA                                   
 RELDT ??2017,123,2017-07-27  LITTLE ROCK                                       
 RELDT ??2017,177,2017-07-27  SPOKANE                                           
 RELDT ??2017,183,2017-07-27  COLUMBIA, SC                                      
 RELDT ??2017,283,2017-07-27  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2017,291,2017-07-27  STOCKTON                                          
 RELDT ??2017,361,2017-07-27  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2017,383,2017-07-27  HOT SPRINGS, AR                                   
 RELDT ??2017,550,2017-07-27  GAINESVILLE-OCALA                                 
*                                                                               
 RELDT ??2017,097,2016-07-28  TOLEDO                                            
 RELDT ??2017,125,2017-07-28  WICHITA                                           
 RELDT ??2017,133,2017-07-28  MOBILE                                            
 RELDT ??2017,171,2017-07-28  MADISON                                           
 RELDT ??2017,205,2017-07-28  TOPEKA                                            
 RELDT ??2017,229,2017-07-28  BOISE                                             
 RELDT ??2017,233,2017-07-28  COLORADO SPRINGS                                  
 RELDT ??2017,311,2017-07-28  LAKELAND-WINTER HAVEN                             
 RELDT ??2017,317,2017-07-28  PENSACOLA                                         
 RELDT ??2017,341,2017-07-28  DAYTONA BEACH                                     
 RELDT ??2017,353,2017-07-28  PUEBLO                                            
 RELDT ??2017,387,2017-07-28  SALINA-MANHATTAN, KS                              
 RELDT ??2017,391,2017-07-28  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2017,462,2017-07-28  TWIN FALLS-SUN VALLEY, ID                         
 RELDT ??2017,517,2017-07-28  FT. PIERCE-STUART-VERO BEACH                      
 RELDT ??2017,331,2017-07-28  MELBOURNE-TITUSVILLE-COCOA                        
*                                                                               
 RELDT ??2017,113,2017-07-31  WORCESTER                                         
 RELDT ??2017,165,2017-07-31  FT. WAYNE                                         
 RELDT ??2017,181,2017-07-31  CHATTANOOGA                                       
 RELDT ??2017,259,2017-07-31  LEXINGTON-FAYETTE                                 
 RELDT ??2017,277,2017-07-31  ROANOKE-LYNCHBURG                                 
 RELDT ??2017,305,2017-07-31  AUGUSTA, GA                                       
 RELDT ??2017,327,2017-07-31  HUNTSVILLE                                        
 RELDT ??2017,367,2017-07-31  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2017,404,2017-07-31  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2017,422,2017-07-31  MUNCIE-MARION, IN                                 
 RELDT ??2017,537,2017-07-31  TRENTON                                           
 RELDT ??2017,579,2017-07-31  SUSSEX, NJ                                        
*                                                                               
 RELDT ??2017,062,2017-08-01  NEW HAVEN                                         
 RELDT ??2017,169,2017-08-01  JACKSON, MS                                       
 RELDT ??2017,287,2017-08-01  SOUTH BEND                                        
 RELDT ??2017,337,2017-08-01  BRIDGEPORT                                        
 RELDT ??2017,343,2017-08-01  MODESTO                                           
 RELDT ??2017,419,2017-08-01  VICTOR VALLEY                                     
 RELDT ??2017,556,2017-08-01  SAN LUIS OBISPO, CA                               
 RELDT ??2017,560,2017-08-01  CHEYENNE, WY                                      
 RELDT ??2017,565,2017-08-01  MORRISTOWN, NJ                                    
 RELDT ??2017,567,2017-08-01  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2017,590,2017-08-01  FT. COLLINS-GREELEY, CO                           
 RELDT ??2017,591,2017-08-01  SANTA BARBARA, CA                                 
 RELDT ??2017,592,2017-08-01  PALM SPRINGS                                      
 RELDT ??2017,594,2017-08-01  OXNARD-VENTURA                                    
*                                                                               
 RELDT ??2017,111,2017-08-02  SHREVEPORT                                        
 RELDT ??2017,129,2017-08-02  YOUNGSTOWN-WARREN                                 
 RELDT ??2017,155,2017-08-02  CORPUS CHRISTI                                    
 RELDT ??2017,163,2017-08-02  FLINT                                             
 RELDT ??2017,195,2017-08-02  LANSING-EAST LANSING                              
 RELDT ??2017,245,2017-08-02  GREEN BAY                                         
 RELDT ??2017,273,2017-08-02  READING, PA                                       
 RELDT ??2017,275,2017-08-02  RENO                                              
 RELDT ??2017,281,2017-08-02  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2017,303,2017-08-02  APPLETON-OSHKOSH                                  
 RELDT ??2017,359,2017-08-02  FAYETTEVILLE, NC                                  
 RELDT ??2017,502,2017-08-02  TYLER-LONGVIEW                                    
 RELDT ??2017,503,2017-08-02  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2017,512,2017-08-02  TEXARKANA, TX-AR                                  
*                                                                               
 RELDT ??2017,137,2017-08-03  PEORIA                                            
 RELDT ??2017,149,2017-08-03  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2017,157,2017-08-03  QUAD CITIES                                       
 RELDT ??2017,173,2017-08-03  MONTGOMERY                                        
 RELDT ??2017,203,2017-08-03  SPRINGFIELD, MO                                   
 RELDT ??2017,271,2017-08-03  SALISBURY-OCEAN CITY                              
 RELDT ??2017,285,2017-08-03  SAVANNAH                                          
 RELDT ??2017,323,2017-08-03  BLOOMINGTON                                       
 RELDT ??2017,365,2017-08-03  NEW BEDFORD-FALL RIVER, MA                        
 RELDT ??2017,423,2017-08-03  HILTON HEAD, SC                                   
 RELDT ??2017,425,2017-08-03  LASALLE-PERU, IL                                  
 RELDT ??2017,501,2017-08-03  DOTHAN, AL                                        
 RELDT ??2017,513,2017-08-03  BRUNSWICK, GA                                     
 RELDT ??2017,562,2017-08-03  KILLEEN-TEMPLE, TX                                
 RELDT ??2017,580,2017-08-03  ALBANY, GA                                        
 RELDT ??2017,581,2017-08-03  ANN ARBOR, MI                                     
 RELDT ??2017,584,2017-08-03  LAKE CHARLES, LA                                  
*                                                                               
 RELDT ??2017,153,2017-08-04  CHARLESTON, WV                                    
 RELDT ??2017,185,2017-08-04  EVANSVILLE                                        
 RELDT ??2017,193,2017-08-04  HUNTINGTON-ASHLAND                                
 RELDT ??2017,227,2017-08-04  BINGHAMTON                                        
 RELDT ??2017,239,2017-08-04  ERIE                                              
 RELDT ??2017,241,2017-08-04  EUGENE-SPRINGFIELD                                
 RELDT ??2017,265,2017-08-04  MACON                                             
 RELDT ??2017,279,2017-08-04  ROCKFORD                                          
 RELDT ??2017,315,2017-08-04  ANCHORAGE                                         
 RELDT ??2017,333,2017-08-04  TALLAHASSEE                                       
*** RELDT ??2017,412,2017-07-29  FLAGSTAFF-PRESCOTT, AZ                         
 RELDT ??2017,433,2017-08-04  VALDOSTA, GA                                      
 RELDT ??2017,555,2017-08-04  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2017,597,2017-08-04  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2017,147,2017-08-07  AMARILLO, TX                                      
 RELDT ??2017,235,2017-08-07  COLUMBUS, GA                                      
 RELDT ??2017,263,2017-08-07  LUBBOCK                                           
 RELDT ??2017,307,2017-08-07  TERRE HAUTE                                       
 RELDT ??2017,309,2017-08-07  WACO, TX                                          
 RELDT ??2017,335,2017-08-07  TRAVERSE CITY-PETOSKEY-CADILLAC, MI               
 RELDT ??2017,371,2017-08-07  TRI-CITIES, WA                                    
 RELDT ??2017,418,2017-08-07  SHEBOYGAN, WI                                     
 RELDT ??2017,509,2017-08-07  REDDING, CA                                       
 RELDT ??2017,528,2017-08-07  FREDERICK, MD                                     
 RELDT ??2017,530,2017-08-07  CAPE COD, MA                                      
 RELDT ??2017,549,2017-08-07  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2017,561,2017-08-07  ODESSA-MIDLAND, TX                                
*                                                                               
 RELDT ??2017,151,2017-08-08  CEDAR RAPIDS                                      
 RELDT ??2017,159,2017-08-08  DULUTH-SUPERIOR                                   
 RELDT ??2017,187,2017-08-08  FARGO-MOORHEAD                                    
 RELDT ??2017,297,2017-08-08  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2017,325,2017-08-08  YAKIMA, WA                                        
 RELDT ??2017,339,2017-08-08  MEDFORD-ASHLAND, OR                               
 RELDT ??2017,436,2017-08-08  BEND, OR                                          
* RELDT ??2017,499,2016-08-02  DECATUR, IL                                      
 RELDT ??2017,506,2017-08-08  LAREDO, TX                                        
*RELDT ??2017,507,2016-08-02  CHAMPAIGN, IL                                     
 RELDT ??2017,508,2017-08-08  CHICO, CA                                         
 RELDT ??2017,554,2017-08-08  FLORENCE, SC                                      
 RELDT ??2017,569,2017-08-08  WINCHESTER, VA                                    
 RELDT ??2017,572,2017-08-08  GRAND FORKS, ND-MN                                
*                                                                               
 RELDT ??2017,179,2017-08-09  WHEELING                                          
 RELDT ??2017,219,2017-08-09  ALTOONA                                           
 RELDT ??2017,225,2017-08-09  BILLINGS, MT                                      
 RELDT ??2017,381,2017-08-09  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2017,511,2017-08-09  GRAND JUNCTION, CO                                
 RELDT ??2017,532,2017-08-09  ROCHESTER, MN                                     
 RELDT ??2017,534,2017-08-09  JOPLIN, MO                                        
 RELDT ??2017,539,2017-08-09  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2017,546,2017-08-09  ABILENE, TX                                       
 RELDT ??2017,559,2017-08-09  LA CROSSE, WI                                     
 RELDT ??2017,564,2017-08-09  COLUMBIA, MO                                      
 RELDT ??2017,573,2017-08-09  PANAMA CITY, FL                                   
 RELDT ??2017,585,2017-08-09  MONROE, LA                                        
 RELDT ??2017,586,2017-08-09  EAU CLAIRE, WI                                    
 RELDT ??2017,595,2017-08-09  LAFAYETTE, IN                                     
*                                                                               
 RELDT ??2017,209,2017-08-10  WICHITA FALLS, TX                                 
 RELDT ??2017,289,2017-08-10  JACKSON, TN                                       
 RELDT ??2017,319,2017-08-10  SIOUX FALLS, SD                                   
*RELDT ??2017,497,2016-08-04  JONESBORO, AR                                     
 RELDT ??2017,519,2017-08-10  HARRISONBURG, VA                                  
 RELDT ??2017,520,2017-08-10  BLUEFIELD, WV                                     
 RELDT ??2017,523,2017-08-10  SIOUX CITY,  IA                                   
 RELDT ??2017,545,2017-08-10  RAPID CITY, SD                                    
 RELDT ??2017,548,2017-08-10  SAN ANGELO, TX                                    
*RELDT ??2017,558,2016-08-04  BECKLEY, WV                                       
 RELDT ??2017,582,2017-08-10  BISMARCK, ND                                      
*                                                                               
* WINTER 2017 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2018,055,2018-04-25  LOUISVILLE                                        
 RELDT ??2018,223,2018-04-25  BATON ROUGE                                       
 RELDT ??2018,053,2018-04-25  NEW ORLEANS                                       
 RELDT ??2018,083,2018-04-25  OKLAHOMA CITY                                     
*                                                                               
 RELDT ??2018,037,2018-04-26  BUFFALO-NIAGARA FALLS                             
 RELDT ??2018,515,2018-04-26  FT. MYERS-NAPLES                                  
 RELDT ??2018,191,2018-04-26  GREENVILLE-SPARTANBURG                            
 RELDT ??2018,105,2018-04-26  RICHMOND                                          
 RELDT ??2018,079,2018-04-26  ROCHESTER, NY                                     
 RELDT ??2018,095,2018-04-26  BIRMINGHAM                                        
 RELDT ??2018,207,2018-04-26  TUCSON                                            
*                                                                               
 RELDT ??2018,069,2018-04-27  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2018,145,2018-04-27  ALLENTOWN-BETHLEHEM                               
 RELDT ??2018,067,2018-04-27  DAYTON                                            
 RELDT ??2018,127,2018-04-27  GRAND RAPIDS                                      
 RELDT ??2018,175,2018-04-27  WILKES BARRE-SCRANTON                             
 RELDT ??2018,089,2018-04-27  FRESNO                                            
*                                                                               
 RELDT ??2018,099,2018-04-30  HONOLULU                                          
 RELDT ??2018,121,2018-04-30  KNOXVILLE                                         
 RELDT ??2018,143,2018-04-30  BAKERSFIELD                                       
 RELDT ??2018,085,2018-04-30  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2018,103,2018-04-30  TULSA                                             
*                                                                               
 RELDT ??2018,119,2018-05-01  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2018,345,2018-05-01  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2018,301,2018-05-01  YORK                                              
 RELDT ??2018,141,2018-05-01  ALBUQUERQUE                                       
 RELDT ??2018,161,2018-05-01  EL PASO                                           
*                                                                               
 RELDT ??2018,081,2018-05-02  AKRON                                             
 RELDT ??2018,231,2018-05-02  CHARLESTON, SC                                    
 RELDT ??2018,091,2018-05-02  SYRACUSE                                          
 RELDT ??2018,283,2018-05-02  MONTEREY-SALINAS-SANTA CRUZ                       
*                                                                               
 RELDT ??2018,183,2018-05-03  COLUMBIA, SC                                      
 RELDT ??2018,361,2018-05-03  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2018,117,2018-05-03  SPRINGFIELD, MA                                   
 RELDT ??2018,097,2018-05-03  TOLEDO                                            
 RELDT ??2018,123,2018-05-03  LITTLE ROCK                                       
*                                                                               
 RELDT ??2018,233,2018-05-04  COLORADO SPRINGS                                  
 RELDT ??2018,071,2018-05-04  DES MOINES                                        
 RELDT ??2018,133,2018-05-04  MOBILE                                            
 RELDT ??2018,177,2018-05-04  SPOKANE                                           
 RELDT ??2018,125,2018-05-04  WICHITA                                           
*                                                                               
 RELDT ??2018,181,2018-05-07  CHATTANOOGA                                       
 RELDT ??2018,540,2018-05-07  PUERTO RICO                                       
 RELDT ??2018,327,2018-05-07  HUNTSVILLE                                        
 RELDT ??2018,169,2018-05-07  JACKSON, MS                                       
 RELDT ??2018,171,2018-05-07  MADISON                                           
 RELDT ??2018,111,2018-05-07  SHREVEPORT                                        
*                                                                               
* SPRING 2018 SUMMARY DATA                                                      
*                                                                               
 RELDT ??2018,053,2018-07-18  NEW ORLEANS                                       
 RELDT ??2018,055,2018-07-18  LOUISVILLE                                        
 RELDT ??2018,083,2018-07-18  OKLAHOMA CITY                                     
 RELDT ??2018,223,2018-07-18  BATON ROUGE                                       
 RELDT ??2018,253,2018-07-18  LAFAYETTE, LA                                     
 RELDT ??2018,516,2018-07-18  MONMOUTH-OCEAN                                    
 RELDT ??2018,522,2018-07-18  LAUREL-HATTIESBURG, MS                            
 RELDT ??2018,533,2018-07-18  BILOXI-GULFPORT-PASCAGOULA                        
 RELDT ??2018,540,2018-07-18  PUERTO RICO                                       
 *                                                                              
 RELDT ??2018,037,2018-07-19  BUFFALO-NIAGARA FALLS                             
 RELDT ??2018,079,2018-07-19  ROCHESTER, NY                                     
 RELDT ??2018,095,2018-07-19  BIRMINGHAM                                        
 RELDT ??2018,105,2018-07-19  RICHMOND                                          
 RELDT ??2018,191,2018-07-19  GREENVILLE-SPARTANBURG                            
 RELDT ??2018,221,2018-07-19  ASHEVILLE                                         
 RELDT ??2018,416,2018-07-19  FREDERICKSBURG                                    
 RELDT ??2018,421,2018-07-19  OLEAN, NY                                         
 RELDT ??2018,553,2018-07-19  CHARLOTTESVILLE, VA                               
 RELDT ??2018,557,2018-07-19  ELMIRA-CORNING, NY                                
 RELDT ??2018,596,2018-07-19  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2018,069,2018-07-20  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2018,091,2018-07-20  SYRACUSE                                          
 RELDT ??2018,197,2018-07-20  PORTLAND, ME                                      
 RELDT ??2018,247,2018-07-20  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2018,267,2018-07-20  MANCHESTER                                        
 RELDT ??2018,295,2018-07-20  UTICA-ROME                                        
 RELDT ??2018,393,2018-07-20  HUDSON VALLEY                                     
 RELDT ??2018,426,2018-07-20  CONCORD (LAKES REGION)                            
 RELDT ??2018,431,2018-07-20  LEBANON-HANOVER-WHTE RVR JNCT                     
 RELDT ??2018,432,2018-07-20  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2018,505,2018-07-20  BURLINGTON-PLATTSBURGH                            
 RELDT ??2018,518,2018-07-20  POUGHKEEPSIE, NY                                  
 RELDT ??2018,526,2018-07-20  BANGOR                                            
 RELDT ??2018,527,2018-07-20  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2018,542,2018-07-20  NEWBURGH-MIDDLETOWN, NY(MD-HUD VLY)               
 RELDT ??2018,577,2018-07-20  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2018,067,2018-07-23  DAYTON                                            
 RELDT ??2018,103,2018-07-23  TULSA                                             
 RELDT ??2018,127,2018-07-23  GRAND RAPIDS                                      
 RELDT ??2018,207,2018-07-23  TUCSON                                            
 RELDT ??2018,251,2018-07-23  KALAMAZOO                                         
 RELDT ??2018,269,2018-07-23  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2018,373,2018-07-23  SARASOTA-BRADENTON                                
 RELDT ??2018,515,2018-07-23  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2018,531,2018-07-23  BATTLE CREEK, MI                                  
 RELDT ??2018,547,2018-07-23  STAMFORD-NORWALK, CT                              
 RELDT ??2018,566,2018-07-23  MUSKEGON, MI                                      
 RELDT ??2018,576,2018-07-23  LIMA, OH                                          
 RELDT ??2018,589,2018-07-23  FT. SMITH, AR                                     
 RELDT ??2018,593,2018-07-23  DANBURY, CT                                       
*                                                                               
 RELDT ??2018,089,2018-07-24  FRESNO                                            
 RELDT ??2018,099,2018-07-24  HONOLULU                                          
 RELDT ??2018,121,2018-07-24  KNOXVILLE                                         
 RELDT ??2018,141,2018-07-24  ALBUQUERQUE                                       
 RELDT ??2018,145,2018-07-24  ALLENTOWN-BETHLEHEM                               
 RELDT ??2018,161,2018-07-24  EL PASO                                           
 RELDT ??2018,175,2018-07-24  WILKES BARRE-SCRANTON                             
 RELDT ??2018,293,2018-07-24  VISALIA-TULARE-HANFORD                            
 RELDT ??2018,345,2018-07-24  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2018,434,2016-07-24  SUNBURY-SELINSGROVE-LEWISBURG, PA                 
 RELDT ??2018,536,2018-07-24  MERCED, CA                                        
 RELDT ??2018,578,2018-07-24  WILLIAMSPORT, PA                                  
*                                                                               
 RELDT ??2018,081,2018-07-25  AKRON                                             
 RELDT ??2018,082,2018-07-25  CANTON                                            
 RELDT ??2018,085,2018-07-25  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2018,119,2018-07-25  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2018,139,2016-07-25  WILMINGTON, DE                                    
 RELDT ??2018,143,2018-07-25  BAKERSFIELD                                       
 RELDT ??2018,231,2018-07-25  CHARLESTON, SC                                    
 RELDT ??2018,255,2018-07-25  LANCASTER                                         
 RELDT ??2018,261,2018-07-25  LINCOLN                                           
 RELDT ??2018,301,2018-07-25  YORK                                              
 RELDT ??2018,510,2018-07-25  MYRTLE BEACH, SC                                  
 RELDT ??2018,524,2018-07-25  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2018,563,2018-07-25  NEW LONDON, CT                                    
*                                                                               
 RELDT ??2018,071,2018-07-26  DES MOINES                                        
 RELDT ??2018,117,2018-07-26  SPRINGFIELD, MA                                   
 RELDT ??2018,123,2018-07-26  LITTLE ROCK                                       
 RELDT ??2018,177,2018-07-26  SPOKANE                                           
 RELDT ??2018,183,2018-07-26  COLUMBIA, SC                                      
 RELDT ??2018,283,2018-07-26  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2018,291,2018-07-26  STOCKTON                                          
 RELDT ??2018,361,2018-07-26  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2018,383,2018-07-26  HOT SPRINGS, AR                                   
 RELDT ??2018,550,2018-07-26  GAINESVILLE-OCALA                                 
*                                                                               
 RELDT ??2018,097,2016-07-27  TOLEDO                                            
 RELDT ??2018,125,2018-07-27  WICHITA                                           
 RELDT ??2018,133,2018-07-27  MOBILE                                            
 RELDT ??2018,171,2018-07-27  MADISON                                           
 RELDT ??2018,205,2018-07-27  TOPEKA                                            
 RELDT ??2018,229,2018-07-27  BOISE                                             
 RELDT ??2018,233,2018-07-27  COLORADO SPRINGS                                  
 RELDT ??2018,311,2018-07-27  LAKELAND-WINTER HAVEN                             
 RELDT ??2018,317,2018-07-27  PENSACOLA                                         
 RELDT ??2018,341,2018-07-27  DAYTONA BEACH                                     
 RELDT ??2018,353,2018-07-27  PUEBLO                                            
 RELDT ??2018,387,2018-07-27  SALINA-MANHATTAN, KS                              
 RELDT ??2018,391,2018-07-27  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2018,462,2018-07-27  TWIN FALLS-SUN VALLEY, ID                         
 RELDT ??2018,517,2018-07-27  FT. PIERCE-STUART-VERO BEACH                      
 RELDT ??2018,331,2018-07-27  MELBOURNE-TITUSVILLE-COCOA                        
*                                                                               
 RELDT ??2018,113,2018-07-30  WORCESTER                                         
 RELDT ??2018,165,2018-07-30  FT. WAYNE                                         
 RELDT ??2018,181,2018-07-30  CHATTANOOGA                                       
 RELDT ??2018,259,2018-07-30  LEXINGTON-FAYETTE                                 
 RELDT ??2018,277,2018-07-30  ROANOKE-LYNCHBURG                                 
 RELDT ??2018,305,2018-07-30  AUGUSTA, GA                                       
 RELDT ??2018,327,2018-07-30  HUNTSVILLE                                        
 RELDT ??2018,367,2018-07-30  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2018,404,2018-07-30  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2018,422,2018-07-30  MUNCIE-MARION, IN                                 
 RELDT ??2018,537,2018-07-30  TRENTON                                           
 RELDT ??2018,579,2018-07-30  SUSSEX, NJ                                        
*                                                                               
 RELDT ??2018,062,2018-07-31  NEW HAVEN                                         
 RELDT ??2018,169,2018-07-31  JACKSON, MS                                       
 RELDT ??2018,287,2018-07-31  SOUTH BEND                                        
 RELDT ??2018,337,2018-07-31  BRIDGEPORT                                        
 RELDT ??2018,343,2018-07-31  MODESTO                                           
 RELDT ??2018,419,2018-07-31  VICTOR VALLEY                                     
 RELDT ??2018,556,2018-07-31  SAN LUIS OBISPO, CA                               
 RELDT ??2018,560,2018-07-31  CHEYENNE, WY                                      
 RELDT ??2018,565,2018-07-31  MORRISTOWN, NJ                                    
 RELDT ??2018,567,2018-07-31  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2018,590,2018-07-31  FT. COLLINS-GREELEY, CO                           
 RELDT ??2018,591,2018-07-31  SANTA BARBARA, CA                                 
 RELDT ??2018,592,2018-07-31  PALM SPRINGS                                      
 RELDT ??2018,594,2018-07-31  OXNARD-VENTURA                                    
*                                                                               
 RELDT ??2018,111,2018-08-01  SHREVEPORT                                        
 RELDT ??2018,129,2018-08-01  YOUNGSTOWN-WARREN                                 
 RELDT ??2018,155,2018-08-01  CORPUS CHRISTI                                    
 RELDT ??2018,163,2018-08-01  FLINT                                             
 RELDT ??2018,195,2018-08-01  LANSING-EAST LANSING                              
 RELDT ??2018,245,2018-08-01  GREEN BAY                                         
 RELDT ??2018,273,2018-08-01  READING, PA                                       
 RELDT ??2018,275,2018-08-01  RENO                                              
 RELDT ??2018,281,2018-08-01  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2018,303,2018-08-01  APPLETON-OSHKOSH                                  
 RELDT ??2018,359,2018-08-01  FAYETTEVILLE, NC                                  
 RELDT ??2018,502,2018-08-01  TYLER-LONGVIEW                                    
 RELDT ??2018,503,2018-08-01  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2018,512,2018-08-01  TEXARKANA, TX-AR                                  
*                                                                               
 RELDT ??2018,137,2018-08-02  PEORIA                                            
 RELDT ??2018,149,2018-08-02  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2018,157,2018-08-02  QUAD CITIES                                       
 RELDT ??2018,173,2018-08-02  MONTGOMERY                                        
 RELDT ??2018,203,2018-08-02  SPRINGFIELD, MO                                   
 RELDT ??2018,271,2018-08-02  SALISBURY-OCEAN CITY                              
 RELDT ??2018,285,2018-08-02  SAVANNAH                                          
 RELDT ??2018,323,2018-08-02  BLOOMINGTON                                       
 RELDT ??2018,365,2018-08-02  NEW BEDFORD-FALL RIVER, MA                        
 RELDT ??2018,423,2018-08-02  HILTON HEAD, SC                                   
 RELDT ??2018,425,2018-08-02  LASALLE-PERU, IL                                  
 RELDT ??2018,501,2018-08-02  DOTHAN, AL                                        
 RELDT ??2018,513,2018-08-02  BRUNSWICK, GA                                     
 RELDT ??2018,562,2018-08-02  KILLEEN-TEMPLE, TX                                
 RELDT ??2018,580,2018-08-02  ALBANY, GA                                        
 RELDT ??2018,581,2018-08-02  ANN ARBOR, MI                                     
 RELDT ??2018,584,2018-08-02  LAKE CHARLES, LA                                  
*                                                                               
 RELDT ??2018,153,2018-08-03  CHARLESTON, WV                                    
 RELDT ??2018,185,2018-08-03  EVANSVILLE                                        
 RELDT ??2018,193,2018-08-03  HUNTINGTON-ASHLAND                                
 RELDT ??2018,227,2018-08-03  BINGHAMTON                                        
 RELDT ??2018,239,2018-08-03  ERIE                                              
 RELDT ??2018,241,2018-08-03  EUGENE-SPRINGFIELD                                
 RELDT ??2018,265,2018-08-03  MACON                                             
 RELDT ??2018,279,2018-08-03  ROCKFORD                                          
 RELDT ??2018,315,2018-08-03  ANCHORAGE                                         
 RELDT ??2018,333,2018-08-03  TALLAHASSEE                                       
 RELDT ??2018,407,2018-08-03  BOWLING GREEN,KY                                  
 RELDT ??2018,433,2018-08-03  VALDOSTA, GA                                      
 RELDT ??2018,555,2018-08-03  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2018,597,2018-08-03  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2018,147,2018-08-06  AMARILLO, TX                                      
 RELDT ??2018,235,2018-08-06  COLUMBUS, GA                                      
 RELDT ??2018,263,2018-08-06  LUBBOCK                                           
 RELDT ??2018,307,2018-08-06  TERRE HAUTE                                       
 RELDT ??2018,309,2018-08-06  WACO, TX                                          
 RELDT ??2018,335,2018-08-06  TRAVERSE CITY-PETOSKEY-CADILLAC, MI               
 RELDT ??2018,371,2018-08-06  TRI-CITIES, WA                                    
 RELDT ??2018,418,2018-08-06  SHEBOYGAN, WI                                     
 RELDT ??2018,509,2018-08-06  REDDING, CA                                       
 RELDT ??2018,528,2018-08-06  FREDERICK, MD                                     
 RELDT ??2018,530,2018-08-06  CAPE COD, MA                                      
 RELDT ??2018,549,2018-08-06  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2018,561,2018-08-06  ODESSA-MIDLAND, TX                                
*                                                                               
 RELDT ??2018,151,2018-08-07  CEDAR RAPIDS                                      
 RELDT ??2018,159,2018-08-07  DULUTH-SUPERIOR                                   
 RELDT ??2018,187,2018-08-07  FARGO-MOORHEAD                                    
 RELDT ??2018,297,2018-08-07  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2018,575,2018-08-07  ST. CLOUD, MN                                     
 RELDT ??2018,325,2018-08-07  YAKIMA, WA                                        
 RELDT ??2018,339,2018-08-07  MEDFORD-ASHLAND, OR                               
 RELDT ??2018,436,2018-08-07  BEND, OR                                          
 RELDT ??2018,506,2018-08-07  LAREDO, TX                                        
 RELDT ??2018,508,2018-08-07  CHICO, CA                                         
 RELDT ??2018,554,2018-08-07  FLORENCE, SC                                      
 RELDT ??2018,569,2018-08-07  WINCHESTER, VA                                    
 RELDT ??2018,572,2018-08-07  GRAND FORKS, ND-MN                                
*                                                                               
 RELDT ??2018,179,2018-08-08  WHEELING                                          
 RELDT ??2018,225,2018-08-08  BILLINGS, MT                                      
 RELDT ??2018,381,2018-08-08  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2018,511,2018-08-08  GRAND JUNCTION, CO                                
 RELDT ??2018,532,2018-08-08  ROCHESTER, MN                                     
 RELDT ??2018,534,2018-08-08  JOPLIN, MO                                        
 RELDT ??2018,539,2018-08-08  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2018,546,2018-08-08  ABILENE, TX                                       
 RELDT ??2018,559,2018-08-08  LA CROSSE, WI                                     
 RELDT ??2018,564,2018-08-08  COLUMBIA, MO                                      
 RELDT ??2018,573,2018-08-08  PANAMA CITY, FL                                   
 RELDT ??2018,585,2018-08-08  MONROE, LA                                        
 RELDT ??2018,586,2018-08-08  EAU CLAIRE, WI                                    
 RELDT ??2018,595,2018-08-08  LAFAYETTE, IN                                     
*                                                                               
 RELDT ??2018,209,2018-08-09  WICHITA FALLS, TX                                 
 RELDT ??2018,289,2018-08-09  JACKSON, TN                                       
 RELDT ??2018,319,2018-08-09  SIOUX FALLS, SD                                   
 RELDT ??2018,519,2018-08-09  HARRISONBURG, VA                                  
 RELDT ??2018,523,2018-08-09  SIOUX CITY,  IA                                   
 RELDT ??2018,545,2018-08-09  RAPID CITY, SD                                    
 RELDT ??2018,548,2018-08-09  SAN ANGELO, TX                                    
 RELDT ??2018,582,2018-08-09  BISMARCK, ND                                      
*&&                                                                             
*                                                                               
* FALL 2018 SUMMARY DATA                                                        
*                                                                               
 RELDT ??2018,053,2019-01-08  NEW ORLEANS                                       
 RELDT ??2018,055,2019-01-08  LOUISVILLE                                        
 RELDT ??2018,083,2019-01-08  OKLAHOMA CITY                                     
 RELDT ??2018,223,2019-01-08  BATON ROUGE                                       
 RELDT ??2018,253,2019-01-08  LAFAYETTE, LA                                     
 RELDT ??2018,516,2019-01-08  MONMOUTH-OCEAN                                    
 RELDT ??2018,522,2019-01-08  LAUREL-HATTIESBURG, MS                            
 RELDT ??2018,533,2019-01-08  BILOXI-GULFPORT-PASCAGOULA                        
 RELDT ??2018,540,2019-01-08  PUERTO RICO                                       
 *                                                                              
 RELDT ??2018,037,2019-01-09  BUFFALO-NIAGARA FALLS                             
 RELDT ??2018,079,2019-01-09  ROCHESTER, NY                                     
 RELDT ??2018,095,2019-01-09  BIRMINGHAM                                        
 RELDT ??2018,105,2019-01-09  RICHMOND                                          
 RELDT ??2018,191,2019-01-09  GREENVILLE-SPARTANBURG                            
 RELDT ??2018,221,2019-01-09  ASHEVILLE                                         
 RELDT ??2018,416,2019-01-09  FREDERICKSBURG                                    
 RELDT ??2018,421,2019-01-09  OLEAN, NY                                         
* RELDT ??2018,553,2018-07-19  CHARLOTTESVILLE, VA                              
 RELDT ??2018,557,2019-01-09  ELMIRA-CORNING, NY                                
 RELDT ??2018,596,2019-01-09  TUSCALOOSA, AL                                    
*                                                                               
 RELDT ??2018,069,2019-01-10  ALBANY-SCHENECTADY-TROY                           
 RELDT ??2018,091,2019-01-10  SYRACUSE                                          
 RELDT ??2018,197,2019-01-10  PORTLAND, ME                                      
 RELDT ??2018,247,2019-01-10  PORTSMOUTH-DOVER-ROCHESTER                        
 RELDT ??2018,267,2019-01-10  MANCHESTER                                        
 RELDT ??2018,295,2019-01-10  UTICA-ROME                                        
 RELDT ??2018,393,2019-01-10  HUDSON VALLEY                                     
 RELDT ??2018,426,2019-01-10  CONCORD (LAKES REGION)                            
 RELDT ??2018,431,2019-01-10  LEBANON-HANOVER-WHTE RVR JNCT                     
 RELDT ??2018,432,2019-01-10  MONTPELIER-BARRE-ST. JOHNSBURY                    
 RELDT ??2018,505,2019-01-10  BURLINGTON-PLATTSBURGH                            
 RELDT ??2018,518,2019-01-10  POUGHKEEPSIE, NY                                  
 RELDT ??2018,526,2019-01-10  BANGOR                                            
 RELDT ??2018,527,2019-01-10  AUGUSTA-WATERVILLE, ME                            
 RELDT ??2018,542,2019-01-10  NEWBURGH-MIDDLETOWN, NY(MD-HUD VLY)               
 RELDT ??2018,577,2019-01-10  WATERTOWN, NY                                     
*                                                                               
 RELDT ??2018,067,2019-01-11  DAYTON                                            
 RELDT ??2018,103,2019-01-11  TULSA                                             
 RELDT ??2018,127,2019-01-11  GRAND RAPIDS                                      
 RELDT ??2018,207,2019-01-11  TUCSON                                            
 RELDT ??2018,251,2019-01-11  KALAMAZOO                                         
 RELDT ??2018,269,2019-01-11  MCALLEN-BROWNSVILLE-HARLINGEN                     
 RELDT ??2018,373,2019-01-11  SARASOTA-BRADENTON                                
 RELDT ??2018,515,2019-01-11  FT. MYERS-NAPLES-MARCO ISLAND                     
 RELDT ??2018,531,2019-01-11  BATTLE CREEK, MI                                  
 RELDT ??2018,547,2019-01-11  STAMFORD-NORWALK, CT                              
 RELDT ??2018,566,2019-01-11  MUSKEGON, MI                                      
 RELDT ??2018,576,2019-01-11  LIMA, OH                                          
 RELDT ??2018,589,2019-01-11  FT. SMITH, AR                                     
 RELDT ??2018,593,2019-01-11  DANBURY, CT                                       
*                                                                               
 RELDT ??2018,089,2019-01-14  FRESNO                                            
 RELDT ??2018,099,2019-01-14  HONOLULU                                          
 RELDT ??2018,121,2019-01-14  KNOXVILLE                                         
 RELDT ??2018,141,2019-01-14  ALBUQUERQUE                                       
 RELDT ??2018,145,2019-01-14  ALLENTOWN-BETHLEHEM                               
 RELDT ??2018,161,2019-01-14  EL PASO                                           
 RELDT ??2018,175,2019-01-14  WILKES BARRE-SCRANTON                             
 RELDT ??2018,293,2019-01-14  VISALIA-TULARE-HANFORD                            
 RELDT ??2018,345,2019-01-14  JOHNSON CITY-KINGSPORT-BRISTOL                    
 RELDT ??2018,434,2019-01-14  SUNBURY-SELINSGROVE-LEWISBURG, PA                 
 RELDT ??2018,461,2019-01-14  LAS CRUCES-DEMING, NM                             
* RELDT ??2018,536,2018-07-24  MERCED, CA                                       
 RELDT ??2018,578,2019-01-14  WILLIAMSPORT, PA                                  
*                                                                               
 RELDT ??2018,081,2019-01-15  AKRON                                             
 RELDT ??2018,082,2019-01-15  CANTON                                            
 RELDT ??2018,085,2019-01-15  OMAHA-COUNCIL BLUFFS                              
 RELDT ??2018,119,2019-01-15  HARRISBURG-LEBANON-CARLISLE                       
 RELDT ??2018,139,2016-07-25  WILMINGTON, DE                                    
 RELDT ??2018,143,2019-01-15  BAKERSFIELD                                       
 RELDT ??2018,231,2019-01-15  CHARLESTON, SC                                    
 RELDT ??2018,255,2019-01-15  LANCASTER                                         
 RELDT ??2018,261,2019-01-15  LINCOLN                                           
 RELDT ??2018,301,2019-01-15  YORK                                              
 RELDT ??2018,510,2019-01-15  MYRTLE BEACH, SC                                  
 RELDT ??2018,524,2019-01-15  HAGERSTN-CHAMBRSG-WAYNSB, MD-PA                   
 RELDT ??2018,563,2019-01-15  NEW LONDON, CT                                    
*                                                                               
 RELDT ??2018,071,2019-01-16  DES MOINES                                        
 RELDT ??2018,117,2019-01-16  SPRINGFIELD, MA                                   
 RELDT ??2018,123,2019-01-16  LITTLE ROCK                                       
 RELDT ??2018,177,2019-01-16  SPOKANE                                           
 RELDT ??2018,183,2019-01-16  COLUMBIA, SC                                      
 RELDT ??2018,283,2019-01-16  MONTEREY-SALINAS-SANTA CRUZ                       
 RELDT ??2018,291,2019-01-16  STOCKTON                                          
 RELDT ??2018,361,2019-01-16  GREENVILLE-NEW BERN-JACKSONVILLE                  
 RELDT ??2018,383,2019-01-16  HOT SPRINGS, AR                                   
 RELDT ??2018,550,2019-01-16  GAINESVILLE-OCALA                                 
*                                                                               
 RELDT ??2018,097,2019-01-17  TOLEDO                                            
 RELDT ??2018,125,2019-01-17  WICHITA                                           
 RELDT ??2018,133,2019-01-17  MOBILE                                            
 RELDT ??2018,171,2019-01-17  MADISON                                           
 RELDT ??2018,205,2019-01-17  TOPEKA                                            
 RELDT ??2018,229,2019-01-17  BOISE                                             
 RELDT ??2018,233,2019-01-17  COLORADO SPRINGS                                  
 RELDT ??2018,311,2019-01-17  LAKELAND-WINTER HAVEN                             
 RELDT ??2018,317,2019-01-17  PENSACOLA                                         
 RELDT ??2018,341,2019-01-17  DAYTONA BEACH                                     
 RELDT ??2018,353,2019-01-17  PUEBLO                                            
 RELDT ??2018,387,2019-01-17  SALINA-MANHATTAN, KS                              
 RELDT ??2018,391,2019-01-17  FT. WALTON BEACH-DESTIN, FL                       
 RELDT ??2018,462,2019-01-17  TWIN FALLS-SUN VALLEY, ID                         
 RELDT ??2018,517,2019-01-17  FT. PIERCE-STUART-VERO BEACH                      
 RELDT ??2018,331,2019-01-17  MELBOURNE-TITUSVILLE-COCOA                        
*                                                                               
 RELDT ??2018,113,2019-01-18  WORCESTER                                         
 RELDT ??2018,165,2019-01-18  FT. WAYNE                                         
 RELDT ??2018,181,2019-01-18  CHATTANOOGA                                       
 RELDT ??2018,259,2019-01-18  LEXINGTON-FAYETTE                                 
 RELDT ??2018,277,2019-01-18  ROANOKE-LYNCHBURG                                 
 RELDT ??2018,305,2019-01-18  AUGUSTA, GA                                       
 RELDT ??2018,327,2019-01-18  HUNTSVILLE                                        
 RELDT ??2018,367,2019-01-18  ATLANTIC CITY-CAPE MAY                            
 RELDT ??2018,404,2019-01-18  FLORENCE-MUSCLE SHOALS, AL                        
 RELDT ??2018,422,2019-01-18  MUNCIE-MARION, IN                                 
 RELDT ??2018,537,2019-01-18  TRENTON                                           
 RELDT ??2018,579,2019-01-18  SUSSEX, NJ                                        
*                                                                               
 RELDT ??2018,062,2019-01-22  NEW HAVEN                                         
 RELDT ??2018,169,2019-01-22  JACKSON, MS                                       
 RELDT ??2018,287,2019-01-22  SOUTH BEND                                        
 RELDT ??2018,337,2019-01-22  BRIDGEPORT                                        
 RELDT ??2018,343,2019-01-22  MODESTO                                           
 RELDT ??2018,419,2019-01-22  VICTOR VALLEY                                     
 RELDT ??2018,556,2019-01-22  SAN LUIS OBISPO, CA                               
 RELDT ??2018,560,2019-01-22  CHEYENNE, WY                                      
 RELDT ??2018,565,2019-01-22  MORRISTOWN, NJ                                    
 RELDT ??2018,567,2019-01-22  SANTA MARIA-LOMPOC, CA                            
 RELDT ??2018,590,2019-01-22  FT. COLLINS-GREELEY, CO                           
 RELDT ??2018,591,2019-01-22  SANTA BARBARA, CA                                 
 RELDT ??2018,592,2019-01-22  PALM SPRINGS                                      
 RELDT ??2018,594,2019-01-22  OXNARD-VENTURA                                    
*                                                                               
 RELDT ??2018,111,2019-01-23  SHREVEPORT                                        
 RELDT ??2018,129,2019-01-23  YOUNGSTOWN-WARREN                                 
 RELDT ??2018,155,2019-01-23  CORPUS CHRISTI                                    
 RELDT ??2018,163,2019-01-23  FLINT                                             
 RELDT ??2018,195,2019-01-23  LANSING-EAST LANSING                              
 RELDT ??2018,245,2019-01-23  GREEN BAY                                         
 RELDT ??2018,273,2019-01-23  READING, PA                                       
 RELDT ??2018,275,2019-01-23  RENO                                              
 RELDT ??2018,281,2019-01-23  SAGINAW-BAY CITY-MIDLAND, MI                      
 RELDT ??2018,303,2019-01-23  APPLETON-OSHKOSH                                  
 RELDT ??2018,359,2019-01-23  FAYETTEVILLE, NC                                  
 RELDT ??2018,502,2019-01-23  TYLER-LONGVIEW                                    
 RELDT ??2018,503,2019-01-23  FAYETTEVILLE (NW ARKANSAS)                        
 RELDT ??2018,512,2019-01-23  TEXARKANA, TX-AR                                  
*                                                                               
 RELDT ??2018,137,2019-01-24  PEORIA                                            
 RELDT ??2018,149,2019-01-24  BEAUMONT-PORT ARTHUR, TX                          
 RELDT ??2018,157,2019-01-24  QUAD CITIES                                       
 RELDT ??2018,173,2019-01-24  MONTGOMERY                                        
 RELDT ??2018,203,2019-01-24  SPRINGFIELD, MO                                   
 RELDT ??2018,271,2019-01-24  SALISBURY-OCEAN CITY                              
 RELDT ??2018,285,2019-01-24  SAVANNAH                                          
 RELDT ??2018,323,2019-01-24  BLOOMINGTON                                       
 RELDT ??2018,365,2019-01-24  NEW BEDFORD-FALL RIVER, MA                        
* RELDT ??2018,423,2018-08-02  HILTON HEAD, SC                                  
 RELDT ??2018,425,2019-01-24  LASALLE-PERU, IL                                  
 RELDT ??2018,501,2019-01-24  DOTHAN, AL                                        
 RELDT ??2018,513,2019-01-24  BRUNSWICK, GA                                     
 RELDT ??2018,562,2019-01-24  KILLEEN-TEMPLE, TX                                
 RELDT ??2018,580,2019-01-24  ALBANY, GA                                        
 RELDT ??2018,581,2019-01-24  ANN ARBOR, MI                                     
 RELDT ??2018,584,2019-01-24  LAKE CHARLES, LA                                  
*                                                                               
 RELDT ??2018,153,2019-01-25  CHARLESTON, WV                                    
 RELDT ??2018,185,2019-01-25  EVANSVILLE                                        
 RELDT ??2018,193,2019-01-25  HUNTINGTON-ASHLAND                                
 RELDT ??2018,227,2019-01-25  BINGHAMTON                                        
 RELDT ??2018,239,2019-01-25  ERIE                                              
 RELDT ??2018,241,2019-01-25  EUGENE-SPRINGFIELD                                
 RELDT ??2018,265,2019-01-25  MACON                                             
 RELDT ??2018,279,2019-01-25  ROCKFORD                                          
 RELDT ??2018,315,2019-01-25  ANCHORAGE                                         
 RELDT ??2018,333,2019-01-25  TALLAHASSEE                                       
 RELDT ??2018,407,2019-01-25  BOWLING GREEN,KY                                  
 RELDT ??2018,433,2019-01-25  VALDOSTA, GA                                      
 RELDT ??2018,555,2019-01-25  MORGANTOWN-CLARKS-FAIRMONT, WV                    
 RELDT ??2018,597,2019-01-25  WAUSAU-STEVENS POINT, WI                          
*                                                                               
 RELDT ??2018,147,2019-01-28  AMARILLO, TX                                      
 RELDT ??2018,235,2019-01-28  COLUMBUS, GA                                      
 RELDT ??2018,263,2019-01-28  LUBBOCK                                           
 RELDT ??2018,307,2019-01-28  TERRE HAUTE                                       
 RELDT ??2018,309,2019-01-28  WACO, TX                                          
 RELDT ??2018,335,2019-01-28  TRAVERSE CITY-PETOSKEY-CADILLAC, MI               
 RELDT ??2018,371,2019-01-28  TRI-CITIES, WA                                    
 RELDT ??2018,418,2019-01-28  SHEBOYGAN, WI                                     
 RELDT ??2018,509,2019-01-28  REDDING, CA                                       
 RELDT ??2018,528,2019-01-28  FREDERICK, MD                                     
 RELDT ??2018,530,2019-01-28  CAPE COD, MA                                      
 RELDT ??2018,549,2019-01-28  BRYAN-COLLEGE STATION, TX                         
 RELDT ??2018,561,2019-01-28  ODESSA-MIDLAND, TX                                
*                                                                               
 RELDT ??2018,151,2019-01-29  CEDAR RAPIDS                                      
 RELDT ??2018,159,2019-01-29  DULUTH-SUPERIOR                                   
 RELDT ??2018,187,2019-01-29  FARGO-MOORHEAD                                    
 RELDT ??2018,297,2019-01-29  WATERLOO-CEDAR FALLS, IA                          
 RELDT ??2018,575,2019-01-29  ST. CLOUD, MN                                     
 RELDT ??2018,325,2019-01-29  YAKIMA, WA                                        
* RELDT ??2018,339,2018-08-07  MEDFORD-ASHLAND, OR                              
 RELDT ??2018,436,2019-01-29  BEND, OR                                          
 RELDT ??2018,506,2019-01-29  LAREDO, TX                                        
 RELDT ??2018,508,2019-01-29  CHICO, CA                                         
 RELDT ??2018,554,2019-01-29  FLORENCE, SC                                      
 RELDT ??2018,569,2019-01-29  WINCHESTER, VA                                    
 RELDT ??2018,572,2019-01-29  GRAND FORKS, ND-MN                                
*                                                                               
 RELDT ??2018,179,2019-01-30  WHEELING                                          
 RELDT ??2018,225,2019-01-30  BILLINGS, MT                                      
 RELDT ??2018,381,2019-01-30  GRAND ISLAND-KEARNEY-HASTINGS, NE                 
 RELDT ??2018,511,2019-01-30  GRAND JUNCTION, CO                                
 RELDT ??2018,532,2019-01-30  ROCHESTER, MN                                     
 RELDT ??2018,534,2019-01-30  JOPLIN, MO                                        
 RELDT ??2018,539,2019-01-30  PARKERSBURG-MARIETTA, WV-OH                       
 RELDT ??2018,546,2019-01-30  ABILENE, TX                                       
 RELDT ??2018,559,2019-01-30  LA CROSSE, WI                                     
 RELDT ??2018,564,2019-01-30  COLUMBIA, MO                                      
 RELDT ??2018,573,2019-01-30  PANAMA CITY, FL                                   
 RELDT ??2018,585,2019-01-30  MONROE, LA                                        
 RELDT ??2018,586,2019-01-30  EAU CLAIRE, WI                                    
 RELDT ??2018,595,2019-01-30  LAFAYETTE, IN                                     
*                                                                               
 RELDT ??2018,209,2019-01-31  WICHITA FALLS, TX                                 
 RELDT ??2018,289,2019-01-31  JACKSON, TN                                       
 RELDT ??2018,319,2019-01-31  SIOUX FALLS, SD                                   
 RELDT ??2018,519,2019-01-31  HARRISONBURG, VA                                  
 RELDT ??2018,523,2019-01-31  SIOUX CITY,  IA                                   
 RELDT ??2018,545,2019-01-31  RAPID CITY, SD                                    
 RELDT ??2018,548,2019-01-31  SAN ANGELO, TX                                    
 RELDT ??2018,582,2019-01-31  BISMARCK, ND                                      
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
*                                                                               
         EJECT                                                                  
MKTLISTD DSECT                                                                  
*                                                                               
* SPARE FIELDS ARE PRESENT IN THIS TABLE ONLY SO THE ENTRIES LOOK               
* NICER IN IDF (OR IN A DUMP, HEAVEN FORBID).                                   
*                                                                               
SURVEY   DS    CL6                 ENCODED SURVEY FROM PATHNAME                 
*                                   PPM:   YYYYMM                               
         ORG   SURVEY                                                           
*                                   DIARY: NNYYYY (NN FROM ARB. DOCUM.)         
         DS    CL2                 IGNORE DIARY SEASON                          
DIARY_YR DS    CL4                 YYYY FOR DIARY MARKETS                       
         ORG                                                                    
         DS    C                   SPARE                                        
MARKET   DS    CL3                 NIELSEN MARKET CODE FROM PATHNAME            
         DS    C                   SPARE                                        
MKTKEYLQ EQU   *-MKTLISTD          L'SEARCH KEY                                 
RELDATE  DS    CL10                EMBARGO RELEASE DATE                         
         DS    CL11                SPARE                                        
MKTTABLQ EQU   *-MKTLISTD                                                       
         SPACE 3                                                                
* ++INCLUDE DEDEMTABD                                                           
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047DEARBSKED 01/17/19'                                      
         END                                                                    
