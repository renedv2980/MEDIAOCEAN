*          DATA SET RESFM0C    AT LEVEL 047 AS OF 11/14/07                      
*PHASE T8180CA                                                                  
*INCLUDE CLPACK                                                                 
         TITLE 'T8180C - RESFM0C - MARKET PROFILE REPORT'                       
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM0C (T8180C) --- MARKET PROFILE REPORT               *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JAN15/89 (MRR) --- MAKE SUPPORTING RECORD I/O NON-LOCKING       *             
*                                                                 *             
* MAY02/90 (MRR) --- LIMIT SOON AND NOW TO ONE DAYPART            *             
*                                                                 *             
* JUN12/90 (MRR) --- LIMIT NOW TO 1 AND SOON TO 3 DAYPARTS        *             
*                                                                 *             
* AUG24/94 (BU ) --- PUMP UP NSID RETURN ERROR MESSAGES           *             
*                    INSERT MARKET NUMBER INTO RANSID BLOCK       *             
*                                                                 *             
* NOV03/94 (BU ) --- MAKE REP SYSTEM NUMBER 'SOFT' RATHER THAN    *             
*                    HARD.                                        *             
*                                                                 *             
* MAR28/05 (BU ) --- ADD DEMFILV TO LIST                          *             
*                                                                 *             
* OCT24/07 (DE ) --- USE SOFT DEMO FILE OPEN LISTS                *             
*                                                                 *             
*                                                                 *             
*                    ***  END TOMBSTONE  ***                      *             
*******************************************************************             
*                                                                               
T8180C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**180C**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         L     R1,=V(CLPACK)                                                    
         A     R1,RELO                                                          
         ST    R1,ACLPACK                                                       
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
*                                                                               
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         LA    RE,STARTSAV          CLEAR OUT COMMON SAVE AREA                  
         LA    RF,SYSSPARE                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         SPACE 1                                                                
****************************************************************                
*    VALIDATE SOURCE (REQUIRED)                                *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,MPRSRCH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,CLCARB                                                        
         BE    VK10                                                             
         EX    RE,CLCNSI                                                        
         BE    VK10                                                             
         EX    RE,CLCSRC                                                        
         BNE   ERREND                                                           
VK10     MVC   CSOURCE,8(R2)                                                    
         SPACE 1                                                                
CLCARB   CLC   8(0,R2),=C'ARB '                                                 
CLCNSI   CLC   8(0,R2),=C'NSI '                                                 
CLCSRC   CLC   8(0,R2),=C'SRC '                                                 
         SPACE 1                                                                
**********************************************************************          
*    VALIDATE MARKET  (REQUIRED)                                     *          
**********************************************************************          
         SPACE 1                                                                
         LA    R2,MPRMKTH                                                       
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         MARKET NUMBER IS NUMERIC                     
         BZ    ERREND                                                           
         CLI   5(R2),4                                                          
         BE    *+14                                                             
         MVC   CONHEAD+10(L'BADMKT),BADMKT                                      
         B     MYEND                                                            
         SPACE 1                                                                
         XC    KEY,KEY             CHECK MARKET IS ON FILE                      
         LA    R6,KEY                                                           
         USING RMKTKEY,R6                                                       
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,8(R2)                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VK20                                                             
         MVC   CMARKET,8(R2)                                                    
         SPACE 1                                                                
         MVI   ERROR,SECLOCK                                                    
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    VK30                                                             
         CLI   TWAACCS,C'$'        TEST FOR STATION LIMITED ACCESS              
         BNE   VK30                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              READ MARKET RECORD FOR IDS                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        GET ELEMENT FOR VALID SIGN-ON IDS            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK19     BAS   RE,NEXTEL                                                        
         BNE   ERREND                                                           
         CLC   TWAORIG,10(R6)      SIGN-ON ID                                   
         BNE   VK19                                                             
         B     VK30                                                             
         DROP  R6                                                               
         SPACE 1                                                                
VK20     MVI   ERROR,NOTFOUND                                                   
         B     ERREND                                                           
         SPACE 1                                                                
****************************************************************                
*    VALIDATE DAYPARTS - DEFAULT IS ALL DAYPARTS  OR           *                
*    CAN ENTER FROM 1 TO 35 DAYPARTS STRUNG TOGETHER           *                
*     IF LEFT BLANK, DAYPARTS COME IN THE HARD-CODED ORDER     *                
*       BELOW.                                                 *                
*     OTHERWISE, DAYPARTS ARE PROCESSED AND PRINTED IN THE     *                
*       ORDER THEY WERE ENTERED.                               *                
****************************************************************                
         SPACE 1                                                                
VK30     LA    R2,MPRDPTH                                                       
         XC    CDAYPART,CDAYPART                                                
         CLI   TWAWHEN,4           0=ONLINE, 2=SOON, 4=OVERNIGHT                
         BE    VK35                                                             
         CLI   5(R2),0                                                          
         BNE   VK31A                                                            
         MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
VK31A    EQU   *                                                                
         CLI   TWAWHEN,2           2=SOON, ALLOW 3 DAYPARTS                     
         BNE   VK31C                                                            
         CLI   5(R2),3                                                          
         BNH   VK38                                                             
         MVC   CONHEAD+10(L'THREEDPT),THREEDPT                                  
         B     MYEND                                                            
VK31C    EQU   *                                                                
         CLI   TWAWHEN,0           0=NOW, ALLOW 1 DAYPARTS                      
         BNE   VK31C                                                            
         CLI   5(R2),1                                                          
         BNH   VK38                                                             
         MVC   CONHEAD+10(L'ONEDPT),ONEDPT                                      
         B     MYEND                                                            
VK35     MVC   CDAYPART(35),=C'MDERATLWKFNPVSJOUXYZBCGHIQ123456789'             
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         XC    CDAYPART,CDAYPART                                                
VK38     ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CDAYPART(0),8(R2)                                                
         SPACE 1                                                                
****************************************************************                
*    VALIDATE BOOKS (AT LEAST 1 REQUIRED- 9 IS MAX)            *                
****************************************************************                
         SPACE 1                                                                
VK40     LA    R2,MPRBKSH                                                       
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VALIBOK                                                          
         MVC   CNUMBKS,ACTUAL      SAVE NUMBER OF BOOKS                         
         CLI   ACTUAL,9            REALLY 9 BOOKS ALLOWED                       
         BNH   VK42                                                             
         MVC   CONHEAD+10(L'MANYBKS),MANYBKS                                    
         B     MYEND                                                            
         SPACE 1                                                                
VK42     LA    R3,CBOOKS           HISTORICAL BOOKS ONLY -                      
VK44     TM    0(R3),X'BE'         NO PREFIXES ALLOWED                          
         BNZ   VK46                                                             
         LA    R3,4(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    VK50                                                             
         B     VK44                                                             
VK46     MVC   CONHEAD+10(L'HISTORY),HISTORY                                    
         B     MYEND                                                            
         SPACE 1                                                                
********************************************************************            
*    VALIDATE PROJECTION EXPRESSIONS (PJ1 IS REQUIRED)             *            
*             AND UPGRADE OVERRIDE EXPRESSIONS (OPTIONAL)          *            
*      VALID INPUT FOR PROJECTION IS SCHEME/PERIOD                 *            
*                 OR SCHEME/PERIOD-YEAR                            *            
*      VALID INPUT FOR UPGRADE IS UPT=ANY VALID UPGRADE EXPRESSION *            
*                                                                  *            
* IF THERE IS AN UPGRADE, ON THE SCREEN, IT OVERRIDES THE UPGRADE  *            
* FROM THE SCHEME                                                  *            
********************************************************************            
         SPACE 1                                                                
VK50     LA    R2,MPRPJ1H                                                       
         GOTO1 ANY                                                              
         XC    CSCHEME(16),CSCHEME                                              
         BAS   RE,SWISPOT          SWITCH TO SPOT                               
         MVI   BYTE,1              FIRST PJ FIELD                               
         BAS   RE,VALPJ                                                         
         LA    R2,MPRPJ2H                                                       
         CLI   5(R2),0             THIS FIELD IS OPTIONAL                       
         BNE   VK53                                                             
         LA    R2,MPROV2H          BUT IF NO PJ, THEN NO UPGRADE                
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BNE   ERREND                                                           
         B     VK55                                                             
VK53     MVI   BYTE,2              SECOND PJ FIELD                              
         BAS   RE,VALPJ                                                         
VK55     BAS   RE,SWIREP           SWITCH BACK TO REP                           
         SPACE 1                                                                
         LA    R2,MPROV1H                                                       
         CLI   5(R2),0                                                          
         BE    VK57                                                             
         LA    R3,CUPTYP1                                                       
         BAS   RE,VALUPT                                                        
VK57     LA    R2,MPROV2H                                                       
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         LA    R3,CUPTYP2                                                       
         BAS   RE,VALUPT                                                        
         SPACE 1                                                                
****************************************************************                
*   VALIDATE OPTION 2 - Y=14-18 DEMOS, (DEFAULT=1-13 DEMOS)    *                
*       14-18 DEMOS (WIDE PRINTING) IS ONLY VALID THROUGH      *                
*             SOON OR OVERNIGHT, NOT ONLINE                    *                
****************************************************************                
         SPACE 1                                                                
VK60     LA    R2,MPROP2H                                                       
         MVI   COP2,C'N'                                                        
         CLI   5(R2),0                                                          
         BE    VK65                                                             
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,CLCNO                                                         
         BE    VK65                                                             
         MVI   COP2,C'Y'                                                        
         EX    RE,CLCYES                                                        
         BNE   VK63                                                             
         CLI   TWAWHEN,0           0=ONLINE,2=SOON,4=OVERNIGHT                  
         BNE   VK65                                                             
         MVC   CONHEAD+10(L'NOTNOW),NOTNOW                                      
         B     MYEND                                                            
VK63     MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 2                                                                
****************************************************************                
*    VALIDATE DEMOS (REQUIRED)                                 *                
*      CAN INPUT DEMO MENU, 'ALL' FOR DEFAULT DEMO MENU        *                
*      OR 1-13 DEMOS ONLINE OR 1-18 DEMOS IF OFFLINE/SOON AND  *                
*      OPTION 2 = Y (WIDE PRINTING)                            *                
*    IF DEMO MENU HAS MORE DEMOS THAN ALLOW, USE ONLY THE      *                
*    FIRST 13 (OR 18)                                          *                
****************************************************************                
         SPACE 1                                                                
VK65     LA    R2,MPRDEM1H                                                      
         MVI   ERROR,INVALID                                                    
         CLC   8(3,R2),=C'ALL'     DEFAULT MENU                                 
         BE    VK70                                                             
         CLC   8(2,R2),=C'M='      OR SPECIFIC MENU                             
         BNE   VK80                                                             
         CLI   5(R2),4             MENU IS 2 A/N CHARACTERS                     
         BE    VK70                                                             
         MVC   CONHEAD+10(L'BADMENU),BADMENU                                    
         B     MYEND                                                            
         SPACE 1                                                                
VK70     MVI   ERROR,NOTFOUND                                                   
         XC    KEY,KEY             VALIDATE DEMO MENU                           
         MVI   KEY,X'23'                                                        
         MVC   KEY+23(2),AGENCY    USE MAIN REP                                 
         MVC   KEY+25(2),=C'ZZ'    DEFAULT MENU                                 
         CLC   8(3,R2),=C'ALL'                                                  
         BE    *+10                                                             
         MVC   KEY+25(2),10(R2)    OR CHOSEN MENU                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         MVC   CNUMDEM,62(R6)      SAVE NUMBER OF DEMOS                         
         MVI   ELCODE,X'02'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         ZIC   RE,1(R6)            LENGTH OF ELEMENT                            
         SH    RE,=H'3'            LESS ELCODE, EL LENGTH,+ 1 FOR EX            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CDEMOS(0),2(R6)        SAVE DEMOS                                
         SPACE 1                                                                
         CLI   COP2,C'Y'                                                        
         BNE   VK75                                                             
         CLI   CNUMDEM,18          MAXIMUM OF 18 DEMOS FOR WIDE                 
         BNH   VK110                                                            
         MVI   CNUMDEM,18                                                       
         LA    R1,19                                                            
         B     VK77                                                             
         SPACE 1                                                                
VK75     CLI   CNUMDEM,13          MAXIMUM OF 13 DEMOS                          
         BNH   VK110                                                            
         MVI   CNUMDEM,13                                                       
         LA    R1,13                                                            
VK77     MH    R1,=H'3'                                                         
         LA    R1,CDEMOS(R1)                                                    
         MVI   0(R1),X'FF'         AND INDICATE NEW END OF LIST                 
         B     VK110                                                            
         SPACE 1                                                                
VK80     MVI   CNDEMFLD,4          INDICATE 4 LINES OF DEMOS ON SCREEN          
         MVI   MAX,24              FUDGE SO I CAN DO ERROR MESSAGE              
         GOTO1 ANY                                                              
         GOTO1 VALIDEM                                                          
         MVC   CNUMDEM,ACTUAL      SAVE NUMBER OF DEMOS                         
         CLI   COP2,C'Y'           WIDE PRINTING                                
         BNE   VK90                                                             
         CLI   ACTUAL,18                                                        
         BNH   VK110                                                            
         MVC   CONHEAD+10(L'MANYDEMW),MANYDEMW                                  
         B     MYEND                                                            
VK90     CLI   ACTUAL,13                                                        
         BNH   VK110                                                            
         MVC   CONHEAD+10(L'MANYDEMS),MANYDEMS                                  
         B     MYEND                                                            
         SPACE 1                                                                
****************************************************************                
*    VALIDATE INDEX BOOK FIELD (OPTIONAL)                      *                
****************************************************************                
         SPACE 1                                                                
VK110    LA    R2,MPRNDEXH                                                      
         CLI   5(R2),0                                                          
         BE    VK150                                                            
         MVC   CBLOCK(8),CBOOKS      SAVE 1ST AND 2ND BOOK                      
         MVI   MAX,2               DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VALIBOK                                                          
         CLI   ACTUAL,1                                                         
         BE    *+14                                                             
         MVC   CONHEAD+10(L'MANYBK2),MANYBK2                                    
         B     MYEND                                                            
         MVC   CINDEX,CBOOKS       SAVE INDEX BOOK                              
         MVC   CBOOKS(8),CBLOCK    RESTORE 1ST AND 2ND BOOK                     
         SPACE 1                                                                
****************************************************************                
*    VALIDATE OPTIONS FIELD  (OPTIONAL)                        *                
*   OPTION 1 - Y=SUPPRESS REPORT/REQUESTOR, PJ'S (DEFAULT=N)   *                
****************************************************************                
         SPACE 1                                                                
VK150    LA    R2,MPROP1H                                                       
         MVI   COP1,C'N'                                                        
         CLI   5(R2),0                                                          
         BE    VK900                                                            
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,CLCNO                                                         
         BE    VK900                                                            
         MVI   COP1,C'Y'                                                        
         EX    RE,CLCYES                                                        
         BE    VK900                                                            
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 2                                                                
CLCNO    CLC   8(0,R2),=C'NO '                                                  
CLCYES   CLC   8(0,R2),=C'YES'                                                  
         SPACE 2                                                                
VK900    B     XIT                                                              
         EJECT                                                                  
*  ROUTINE VALIDATES PROJECTION FIELD                                           
*  ON ENTRY, R2 POINTS TO PJ FIELD ON SCREEN                                    
*  BYTE = 1 FOR 1ST PJ FIELD, BYTE = 2 FOR 2ND PJ FIELD                         
*     ENTRY CAN BE SCHEME/PERIOD                                                
*        OR SCHEME/PERIOD-YEAR (VALIDATE THROUGH RANSID IN SPOT)                
         SPACE 1                                                                
VALPJ    NTR1                                                                   
         SPACE 1                                                                
         CLI   CAGYMED,0           DO WE ALREADY HAVE AGENCY/MEDIA              
         BNE   *+8                 YES                                          
         BAS   RE,GETAGY           NO, SO GO GET IT                             
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),C',=/='  CHANGE DELIMETER            
         LA    R4,BLOCK                                                         
         CLI   DMCB+4,2                                                         
         BL    VPJ60               MUST HAVE SCHEME AND PERIOD                  
         CLI   DMCB+4,3            ALSO CAN HAVE YEAR                           
         BH    VPJ60                                                            
         SPACE 1                                                                
         CLI   0(R4),2             SCHEME IS 2                                  
         BL    VPJ60                                                            
         CLI   0(R4),3             OR 3 CHARACTERS                              
         BH    VPJ60                                                            
         CLI   BYTE,1                                                           
         BE    VPJ30                                                            
         MVC   CSCHEME2,12(R4)                                                  
         B     *+10                                                             
VPJ30    MVC   CSCHEME,12(R4)                                                   
         SPACE 1                                                                
         LA    R4,32(R4)                                                        
         CLI   0(R4),4             PERIOD IS UP TO 4 CHARACTERS                 
         BH    VPJ60                                                            
         CLI   BYTE,1                                                           
         BE    VPJ40                                                            
         MVC   CPERIOD2,12(R4)                                                  
         B     *+10                                                             
VPJ40    MVC   CPERIOD,12(R4)                                                   
         SPACE 1                                                                
         CLI   DMCB+4,2                                                         
         BE    VPJ70                                                            
         LA    R4,32(R4)                                                        
         CLI   0(R4),2             YEAR IS 2 CHARACTERS                         
         BNE   VPJ60                                                            
         TM    2(R4),X'80'         MUST BE NUMERIC                              
         BZ    VPJ60                                                            
         CLI   BYTE,1                                                           
         BE    VPJ50                                                            
         MVC   CYEAR2,7(R4)                                                     
         B     VPJ70                                                            
VPJ50    MVC   CYEAR,7(R4)         BINARY VALUE OF YEAR                         
         B     VPJ70                                                            
         SPACE 1                                                                
VPJ60    MVC   CONHEAD+10(L'BADFRMT),BADFRMT                                    
         B     MYEND                                                            
         SPACE 1                                                                
VPJ70    XC    WORK,WORK           READ SID PROFILE-MUST BE REP SCHEME          
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVI   WORK+22,C'T'                                                     
         OC    CSCHEME2,CSCHEME2                                                
         BZ    VPJ73                                                            
         CLC   CSCHEME2,=C'ALL'                                                 
         BE    VPJ75                                                            
         MVC   WORK+23(3),CSCHEME2                                              
         B     VPJ75                                                            
VPJ73    CLC   CSCHEME,=C'ALL'                                                  
         BE    *+10                                                             
         MVC   WORK+23(3),CSCHEME                                               
VPJ75    GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    VPJ77                                                            
         CLI   WORK+1,C'Y'         ERROR IF NOT A Y                             
         BE    VPJ79                                                            
VPJ77    MVC   CONHEAD+10(L'REPSCMS),REPSCMS                                    
         B     MYEND                                                            
         SPACE 1                                                                
VPJ79    L     R5,AIO2             USE IO2 FOR RANSID BLOCK                     
         USING SRBLKD,R5                                                        
         LA    RE,SRBLK            CLEAR BLOCK                                  
         LA    RF,SRBLKLN                                                       
         XCEF                                                                   
         SPACE 1                                                                
         MVC   SRASIR,AIO1         USI IO1 FOR NSID RECORDS                     
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,ACLPACK                                                 
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SRAMASTC,TWAMASTC                                                
         SPACE 1                                                                
         MVC   SRSELSCH,CSCHEME                                                 
         MVC   SRSELPER,CPERIOD                                                 
         MVC   SRSELYR,CYEAR                                                    
         OC    CSCHEME2,CSCHEME2                                                
         BZ    VPJ80                                                            
         MVC   SRSELSCH,CSCHEME2                                                
         MVC   SRSELPER,CPERIOD2                                                
         MVC   SRSELYR,CYEAR2                                                   
VPJ80    MVC   SRSELAM,CAGYMED                                                  
         MVC   SRSELAGY,AGENCY                                                  
         MVI   SRSELMED,C'T'                                                    
         SPACE 1                                                                
*                                                                               
*   CONVERT THE MARKET NUMBER, AND INSERT IT INTO THE RANSID BLOCK.             
*      PREVIOUSLY, THIS WASN'T BEING DONE, AND RESULTED IN A VERY               
*      CONFUSING ERROR.                                                         
*                                                                               
         XC    DUB,DUB             CLEAR WORK AREA                              
         LA    R6,MPRMKTH          A(MARKET NUMBER HEADER)                      
         ZIC   RF,5(R6)            GET LENGTH OF INPUT                          
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,VPJPACK          PACK BY LENGTH                               
         CVB   RF,DUB              CONVERT RESULT TO BINARY                     
         STH   RF,DUB              SAVE THE HALF-WORD                           
         MVC   SRSELMKT,DUB        LOAD HALF-WORD TO BLOCK                      
         B     VPJ85                                                            
VPJPACK  PACK  DUB(8),8(0,R6)                                                   
*                                                                               
VPJ85    EQU   *                                                                
*                                                                               
         GOTO1 RANSID,DMCB,(R5)                                                 
         SPACE 1                                                                
         CLI   SRERROR,SRNOERR     TEST FOR ERROR                               
         BE    VPJ110                                                           
         CLI   SRERROR,SRNOSCH                                                  
         BNE   VPJ90                                                            
         MVC   CONHEAD+10(L'NOSCHM),NOSCHM   SCHEME NOT FOUND                   
         B     MYEND                                                            
VPJ90    CLI   SRERROR,SRNOPER                                                  
         BNE   VPJ100                                                           
         MVC   CONHEAD+10(L'NOPERD),NOPERD   PERIOD NOT FOUND                   
         B     MYEND                                                            
VPJ100   EQU   *                   SOMETHING ELSE IS WRONG                      
         MVC   CONHEAD+10(L'SIDERR),SIDERR                                      
         LA    R6,CONHEAD+45                                                    
         EDIT  SRERROR,(3,(R6))                                                 
         B     MYEND                                                            
*VPJ100   MVI   ERROR,INVALID       SOMETHING ELSE IS WRONG                     
*         B     ERREND                                                          
VPJ110   CLI   SRMODE,SRONEREC                                                  
         BE    XIT                                                              
         MVC   CONHEAD+10(L'NORECS),NORECS   NO RECORDS FOR SCHEME/PER          
         B     MYEND                                                            
         EJECT                                                                  
*  THIS ROUTINE VALIDATE AN UPGRADE EXPRESSION                                  
*  VALID UPGRADE EXPRESSIONS ARE UPT=PUT/MMMYY                                  
*                                UPT=HUT/MMMYY                                  
*                                UPT=HPT/MMMYY/INDEX                            
*  ON ENTRY, R3 POINTS TO EITHER UPGRADE AREA 1 OR 2                            
         SPACE 1                                                                
VALUPT   NTR1                                                                   
         USING UPGD,R3                                                          
         MVI   ERROR,INVALID                                                    
         CLC   8(8,R2),=C'UPT=PUT/'                                             
         BE    VUP5                                                             
         CLC   8(8,R2),=C'UPT=HUT/'                                             
         BE    VUP5                                                             
         CLC   8(8,R2),=C'UPT=HPT/'                                             
         BNE   ERREND                                                           
VUP5     XC    BLOCK(256),BLOCK                                                 
         XC    DMCB+8(4),DMCB+8                                                 
         GOTO1 SCANNER,DMCB,(24,(R2)),BLOCK                                     
         LA    R4,BLOCK                                                         
* READ INPUT STRING FOR UPGRADE EXPRESSION AND BUILD FLDHDR                     
         SPACE 1                                                                
         MVC   CUPPRG,22(R4)       MOVE DATA TO SAVE AREA                       
         XC    ELEM,ELEM                                                        
         IC    RE,1(R4)            GET EXPRESSION LENGTH                        
         STC   RE,ELEM+5           SET INPUT STRING LENGTH                      
         BCTR  RE,0                SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4)    MOVE DATA                                    
         LA    RE,9(RE)            ADJUXT LEN TO INCLUDE FLDHDR                 
         STC   RE,ELEM             AND SET IN FLDHDR                            
         SPACE 1                                                                
* GET UPVAL ADDRESS                                                             
         GOTO1 UPVAL,DMCB,ELEM,WORK,(C'/',ACOMFACS)                             
         CLI   0(R1),0             TEST VALID EXPRESSION                        
         BNE   *+14                ERROR                                        
         MVC   CONHEAD+10(L'BADUPGRD),BADUPGRD                                  
         B     MYEND                                                            
         MVC   CUPTYPE(8),WORK+4                                                
         MVC   CUPBTYPE,WORK+3     SAVE BOOK TYPE                               
         SPACE 1                                                                
VUP10    LA    R4,46(R4)           NOT NON-STANDARD SCANNER                     
         OC    0(2,R4),0(R4)       TEST ANY MORE FIELDS                         
         BZ    VUPXIT                                                           
         SPACE 1                                                                
         CLC   =C'BK',12(R4)                                                    
         BE    VUP20                                                            
         CLC   =C'DT',12(R4)                                                    
         BE    VUP40                                                            
         CLC   =C'ST',12(R4)                                                    
         BE    VUP70                                                            
         SPACE 1                                                                
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 1                                                                
* EDIT OVERRIDE FOR SHARE BOOK                                                  
         SPACE 1                                                                
VUP20    CLI   1(R4),0                                                          
         BNE   *+14                                                             
VUP30    MVC   CONHEAD+10(L'BADOBOOK),BADOBOOK                                  
         B     MYEND                                                            
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4)    **EXECUTED**                                 
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         SPACE 1                                                                
         GOTO1 BOOKVAL,DMCB,(C'N',ELEM),(1,DUB),(C'B',SCANNER),BOOKTYPE         
         CLI   4(R1),0                                                          
         BE    VUP30                                                            
         TM    DUB,X'BF'           TEST ANY GARBAGE OPTIONS SPECIFIED           
         BNZ   VUP30                                                            
         CLI   BOOKTYPE,0          TEST ANY BOOK TYPE RETURNED                  
         BE    VUP35               NO -- OK                                     
         CLI   CUPBTYPE,0          TEST ANY PRIOR BOOK TYPE                     
         BNE   *+14                YES                                          
         MVC   CUPBTYPE,BOOKTYPE   NO -- SAVE BOOK TYPE                         
         B     VUP35                                                            
         CLC   CUPBTYPE,BOOKTYPE   DOES IT MATCH SCHEME BOOKTYPE?               
         BE    VUP35               YES -- OK                                    
         MVC   CONHEAD+10(L'BADBTYPE),BADBTYPE                                  
         B     MYEND                                                            
         SPACE 1                                                                
VUP35    MVC   CUPFBK,DUB+1                                                     
         B     VUP10                                                            
         SPACE 1                                                                
*EDIT OVERRIDE DAY/TIME                                                         
         SPACE 1                                                                
VUP40    MVI   ERROR,INVDAY                                                     
         ZIC   R0,1(R4)            GET INPUT FIELD LENGTH                       
         LA    R5,22(R4)           POINT TO INPUT STRING                        
         SR    R6,R6               CLEAR COUNTER                                
VUP50    CLI   0(R5),C'/'          FIND DELIMITER                               
         BE    VUP60                                                            
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R0,VUP50                                                         
         B     ERREND                                                           
         SPACE 1                                                                
VUP60    ST    R0,DUB              SAVE REMAINING LENGTH                        
         ST    R5,DUB+4            AND INPUT ADDRESS                            
         SPACE 1                                                                
         GOTO1 DAYVAL,DMCB,((R6),22(R4)),WORK,WORK+1                            
         CLI   WORK,0                                                           
         BE    ERREND                                                           
         MVC   CUPUDAY,WORK                                                     
         SPACE 1                                                                
* EDIT TIME                                                                     
         SPACE 1                                                                
         MVI   ERROR,INVTIME                                                    
         L     R0,DUB              PICK UP REMAINING STRING LENGTH              
         BCTR  R0,0                ADJUST LENGHT FOR DELIMITER                  
         LTR   R0,R0                                                            
         BNP   ERREND                                                           
         L     R5,DUB+4            RESTORE INPUT POINTER                        
         LA    R5,1(R5)            ADJUST FOR DELIMITER                         
         GOTO1 TIMVAL,DMCB,((R0),(R5)),WORK                                     
         CLI   0(R1),X'FF'                                                      
         BE    ERREND                                                           
         MVC   CUPUTIM,WORK                                                     
         B     VUP10                                                            
         SPACE 1                                                                
* EDIT STATION OVERRIDE                                                         
         SPACE 1                                                                
VUP70    CLI   1(R4),3                                                          
         BL    VUP80                                                            
         CLI   1(R4),4                                                          
         BH    VUP80                                                            
         MVC   CUPSTA,22(R4)                                                    
         MVI   CUPSTA+4,C'T'       FORCE MEDIA T                                
         B     VUP10                                                            
VUP80    MVC   CONHEAD+10(L'BADOSTA),BADOSTA                                    
         B     MYEND                                                            
         SPACE 1                                                                
VUPXIT   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* THIS ROUTINE WILL GET THE AGENCY/MEDIA CODE                                   
         SPACE 1                                                                
GETAGY   ST    RE,FULL                                                          
* FIRST GET AGENCY/MEDIA CODE                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     VPJ20                                                            
VPJ10    BAS   RE,NEXTEL                                                        
VPJ20    BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),C'T'          WANT TELEVISION                              
         BNE   VPJ10                                                            
         MVC   CAGYMED,3(R6)                                                    
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*  SWITCH TO SPOT SYSTEM TO GET AGENCY/MEDIA CODE AND FOR RANSID                
         SPACE 1                                                                
SWISPOT  NTR1                                                                   
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   SS10                                                             
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         MVI   4(R5),X'32'         PUT SPOT SENUM IN UTL                        
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         L     RE,TSPFUSER                                                      
         CLC   =C'SPOT',0(RE)                                                   
         BE    SS40                                                             
         MVC   0(4,RE),=C'SPOT'                                                 
         DROP  RE                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',SPTFLIST,AIO2                   
         B     SS40                                                             
         SPACE                                                                  
SS10     L     RF,SWITCH                                                        
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'32'          SPOT Q SYSTEM NUMBER                         
         CLC   AGENCY,=C'SJ'       FOR TESTING                                  
         BNE   *+8                                                              
         MVI   DMCB,X'02'          USE SPOT 1 (SYSTEM NUMBER IS 2)              
         GOTO1 (RF),DMCB                                                        
         SPACE 1                                                                
         CLI   4(R1),2             TEST SYSTEM NOT OPERATIONAL                  
         BNE   *+14                                                             
         MVC   CONHEAD+10(27),=C'SPOT SYSTEM NOT OPERATIONAL'                   
         B     MYEND                                                            
         SPACE 1                                                                
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*  SET UP INTERNAL VALUES FOR SPOT                                              
SS40     MVI   SYSTEM,C'S'                                                      
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   REQFILE,=C'SPTREQ '                                              
         B     XIT                                                              
         EJECT                                                                  
* SWITCH BACK TO REP                                                            
SWIREP   NTR1                                                                   
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   SR20                                                             
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
*          DATA SET RESFM0D    AT LEVEL 033 AS OF 03/22/93                      
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         CLI   CTLSWTCH,C'Y'       SE # ALREADY GOTTEN?                         
         BE    SR10                YES                                          
         MVI   CTLSWTCH,C'Y'       NO  - GO GET IT                              
         BAS   RE,CTRLSET                                                       
*                                                                               
SR10     EQU   *                                                                
*******  GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO2                    
         B     SR40                                                             
*                                                                               
SR20     L     RF,SWITCH                                                        
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*  RESET INTERNAL VALUES FOR REP                                                
SR40     MVI   SYSTEM,C'R'                                                      
         MVC   LKEY,=H'27'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*   CTLSET:  GETS SE # FROM CONTROL FILE FOR THE RUN.  ONLY DONE ONE            
*     TIME, BASED ON VALUE OF 'CTLSWTCH'.                                       
*             R5  =  A(UTL)                                                     
*                                                                               
CTRLSET  NTR1                                                                   
         MVI   4(R5),X'0A'         SET UTL SE TO CTFILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AIO2,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'5'           FIND CONTROL FILE ACCESS RECORD              
         MVC   WORK+23,AGENCY      INSERT POWER CODE                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO2                     
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIO2                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         ST    R1,FULL             SAVE A(X'21' ELEMENT)                        
*****>   GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'CONTROL'                              
*                                  CLOSE CONTROL FILE AS X'0A'                  
         L     R1,FULL             RESET A(X'21' ELEMENT)                       
         MVC   4(1,R5),3(R1)       OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* MY OWN ERROR MESSAGES                                                         
         SPACE 2                                                                
BADFRMT  DC    C'FORMAT IS SCHEME/PERIOD/(YEAR)'                                
MANYBKS  DC    C'TOO MANY BOOKS - MAXIMUM IS 9'                                 
MANYBK2  DC    C'ONLY 1 INDEX BOOK ALLOWED'                                     
MANYDEMS DC    C'TOO MANY DEMOS - MAXIMUM IS 13'                                
MANYDEMW DC    C'TOO MANY DEMOS - MAXIMUM IS 18'                                
NOSCHM   DC    C'SCHEME NOT FOUND'                                              
NOPERD   DC    C'PERIOD NOT FOUND'                                              
NORECS   DC    C'NO RECORDS FOR SCHEME/PERIOD/(YEAR)'                           
SIDERR   DC    C'PROBLEM ACCESSING SID INFO: CODE = '                           
BADMENU  DC    C'DEMO MENU NOT FOUND'                                           
BADMKT   DC    C'MARKET MUST BE 4 NUMBERS'                                      
BADOBOOK DC    C'INVALID OVERRIDE BOOK'                                         
BADUPGRD DC    C'INVALID UPGRADE EXPRESSION'                                    
BADOSTA  DC    C'INVALID STATION OVERRIDE'                                      
REPSCMS  DC    C'ONLY REP SCHEMES MAY BE USED'                                  
ONEDPT   DC    C'ONLY ONE DAYPART FOR A NOW REPORT REQUEST'                     
THREEDPT DC    C'ONLY 3 DAYPARTS FOR A SOON REPORT REQUEST'                     
NOTNOW   DC    C'ONLY 1-13 DEMOS ALLOWED ON NOW REQUEST'                        
HISTORY  DC    C'HISTORICAL BOOKS ONLY'                                         
BADBTYPE DC    C'CANNOT MIX SPECIAL SURVEYS'                                    
         SPACE 4                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
CTLSWTCH DC    C'N'                CONTROL FILE SWITCH                          
         EJECT                                                                  
UPGD     DSECT                                                                  
CUPTYPE  DS    XL1                 UPGRADE TYPE                                 
*CUPTRTG EQU   2                   RATING UPGRADE                               
*CUPTHUT EQU   3                   HUT UPGRADE (CUPSTYP NE P)                   
*CUPTPUT EQU   3                   PUT UPGRADE (CUPSTYP EQ P)                   
*                                  CUPFLD1 = OLDHPT SOURCE BOOK                 
*CUPTNDX EQU   4                   INDEX UPGRADE                                
*CUPTHPT EQU   6                   H/P/T UPGRADE                                
CUPSTYP  DS    XL1                 UPGRADE SUB-TYPE (P=PUT UPGRADE)             
*                                  UGRADE BOOK/INDEX VALUES                     
CUPFLD1  DS    XL2                                                              
CUPFLD2  DS    XL2                                                              
CUPFLD3  DS    XL2                                                              
CUPFBK   DS    XL2                 FROM BOOK (SHARES)                           
CUPUDAY  DS    XL1                 DAY CODE                                     
CUPUTIM  DS    XL4                 START AND END TIMES (BINARY)                 
CUPSTA   DS    CL5                 STATION CALL LETTERS                         
CUPBTYPE DS    CL1                 UPGRADE BOOK TYPE                            
         DS    XL9                 *** SPARE ***                                
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMFCD                                                                      
* REGENMKT                                                                      
* SRBLKD DSECT                                                                  
*      SPRANSIDD                                                                
* RESFMWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMFCD                                                       
         EJECT                                                                  
       ++INCLUDE REGENMKT                                                       
         EJECT                                                                  
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
ACLPACK  DS    A                                                                
BOOKTYPE DS    C                                                                
         EJECT                                                                  
*  DDREPMASTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*  DEDBLOCK                                                                     
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
*  TWADCOND                                                                     
TWADCOND DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDTWADCONS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047RESFM0C   11/14/07'                                      
         END                                                                    
