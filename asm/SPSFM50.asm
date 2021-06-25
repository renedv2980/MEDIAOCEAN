*          DATA SET SPSFM50    AT LEVEL 054 AS OF 03/24/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T21750A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21750  -- PRODUCT MAINTENANCE                       *         
*                                                                     *         
*  COMMENTS:     MAINTAINS PRODUCT RECORDS                            *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21900), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM70 (MAINT)                               *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- RECORD                                         *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21750 - PRODUCT MAINTENANCE'                                   
T21750   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1750**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BRAS  RE,SETUP                                                         
         BNE   ERRSEC2                                                          
         CLI   MODE,VALRA          NEW RECORD ACTION?                           
         JNE   *+12                YES, DISPLAY INIT'D SCR                      
         BRAS  RE,SETSAP                                                        
         J     XIT                                                              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DEL                                                              
         CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    PP                                                               
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BE    PP                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       MVC   PRDMEDN,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    PRDMEDNH+6,X'80'      FROM SCREEN                                
         MVC   PRDCLIN,SPACES                                                   
         OI    PRDCLINH+6,X'80'                                                 
*                                                                               
         LA    R2,PRDMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   PRDMEDN,MEDNM         MEDIA NAME                                 
         OI    PRDMEDNH+6,X'80'                                                 
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    VK03                                                             
         CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY                         
         BNE   VK03                                                             
         CLI   QMED,C'C'           MEDIUM C AND N ONLY FOR DISPLAY              
         BE    ERRINV                                                           
         CLI   QMED,C'N'                                                        
         BE    ERRINV                                                           
*                                                                               
VK03     L     R6,AIO                                                           
         USING AGYHDRD,R6          SAVING SOME IMPORTANT AGENCY INFO            
*                                                                               
         MVC   SVAGYFL1,AGYFLAG1                                                
         MVC   ACCOFC,AGYOFC2                                                   
         MVC   SVCTAGY,AGYCTAGY                                                 
*                                                                               
         MVI   ELCODE,X'03'        ACC AGY LIST ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ACCAGY,2(R6)        SAVE ACC AGENCY LIST                         
         DROP  R6                                                               
*                                                                               
VK05     LA    R2,PRDCLIKH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
         MVC   PRDCLIN,CLTNM         CLIENT NAME                                
         OI    PRDCLINH+6,X'80'                                                 
*                                                                               
         L     RE,AIO                                                           
         USING CLTHDR,RE                                                        
*                                                                               
         MVI   PRDSECFL,C'N'                                                    
         TM    COPT3,COP3PSEC      PRODUCT SECURITY USED?                       
         BZ    *+8                                                              
         MVI   PRDSECFL,C'Y'                                                    
         MVC   SVP1USER,CPU1         SAVE CLIENT RECORD DATA                    
         MVC   SVP1TYPE,CPU1TYPE                                                
         MVC   SVP1LEN,CPU1LEN                                                  
         MVC   SVP1FLG1,CPU1FLG1                                                
         MVC   SVP1FLG2,CPU2FLG2                                                
         MVC   SVP2USER,CPU2                                                    
         MVC   SVP2TYPE,CPU2TYPE                                                
         MVC   SVP2LEN,CPU2LEN                                                  
         MVC   SVP2FLG1,CPU2FLG1                                                
         MVC   SVP2FLG2,CPU2FLG2                                                
         MVC   SVCLOP1,COPT1                                                    
         MVC   SVCLOP2,COPT2                                                    
         MVC   SVCLPROF,CPROF                                                   
         DROP  RE                                                               
*                                                                               
***** USER DESCRIPTION FIELDS                                                   
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK19                                                             
*                                                                               
         OI    PRDUSR1H+6,X'80'      TRANSMIT                                   
         OI    PRDDSC1H+6,X'80'                                                 
*                                                                               
         CLC   SPACES(L'SVP1USER),SVP1USER                                      
         BNL   VK13                  CPU1 EMPTY                                 
*                                                                               
         CLI   PRDUSR1H+4,X'20'    VALIDATED PREVIOUSLY?                        
         BE    VK15                SAME WITH USR DEF FIELD 2                    
*                                                                               
         NI    PRDUSR1H+1,X'FF'-X'20'  UNPROTECT DESCRIPTION LINE               
         MVC   PRDDSC1,SVP1USER                                                 
         OI    PRDUSR1H+4,X'20'        SET TO VALIDATED PREVIOUSLY              
         B     VK15                                                             
*                                                                               
VK13     XC    PRDUSR1,PRDUSR1                                                  
         XC    PRDDSC1,PRDDSC1                                                  
         OI    PRDUSR1H+1,X'20'      PROTECT                                    
         OI    PRDDSC1H+1,X'20'                                                 
*                                                                               
VK15     OI    PRDUSR2H+6,X'80'      TRANSMIT                                   
         OI    PRDDSC2H+6,X'80'                                                 
*                                                                               
         CLC   SPACES(L'SVP2USER),SVP2USER                                      
         BNL   VK17                  CPU2 EMPTY                                 
*                                                                               
         CLI   PRDUSR2H+4,X'20'    VALIDATED PREVIOUSLY?                        
         BE    VK19                SAME WITH USR DEF FIELD 2                    
*                                                                               
         NI    PRDUSR2H+1,X'FF'-X'20'  UNPROTECT DESCRIPTION LINE               
         MVC   PRDDSC2,SVP2USER                                                 
         OI    PRDUSR2H+4,X'20'        SET TO VALIDATED PREVIOUSLY              
         B     VK19                                                             
*                                                                               
VK17     XC    PRDUSR2,PRDUSR2                                                  
         XC    PRDDSC2,PRDDSC2                                                  
         OI    PRDUSR2H+1,X'20'      PROTECT                                    
         OI    PRDDSC2H+1,X'20'                                                 
*                                                                               
VK19     LA    R2,PRDPROKH           PRODUCT                                    
         CLI   5(R2),0               PRODUCT CODE REQUIRED                      
         BE    ERRMIS                                                           
*                                                                               
         CLI   8(R2),C'#'            IF PRODUCT NUMBER GIVEN ...                
         BNE   VK30                                                             
         MVC   ERRNUM,=AL2(VKERR1)   PRO NUMBER MUST BE 3 LONG                  
         CLI   5(R2),3                                                          
         BNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(VKERR2)   PRO NUMBER MUST BE VALID HEX               
         GOTO1 HEXIN,DMCB,9(R2),BYTE,2                                          
         OC    DMCB+12(4),DMCB+12                                               
         BZ    SPERREX                                                          
*                                                                               
         LA    R6,SVCLIST            FIND PRD # IN CLIENT'S LIST                
VK20     CLI   0(R6),0               END OF PRODUCT TABLE?                      
         BE    ERRINV                                                           
         CLC   BYTE,3(R6)            PRODUCT FOUND?                             
         BE    *+12                                                             
         LA    R6,4(R6)              BUMP TO NEXT PRODUCT                       
         B     VK20                                                             
         MVC   8(3,R2),0(R6)         MOVE FOUND PRO CODE TO SCREEN              
         OI    6(R2),X'80'           AND TRANSMIT                               
*                                                                               
         CLI   10(R2),C' '           IS PRODUCT NAME 2 CHARS?                   
         BNE   *+8                                                              
         MVI   5(R2),2               LENGTH GOES FROM 3 TO 2                    
         LLC   RE,5(R2)                                                         
*                                                                               
VK30     CLI   ACTEQU,ACTADD         ACTION ADD?                                
         BE    VK40                                                             
*                                                                               
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD               ACTION IS NOT ADD SO ...                   
         MVI   AAAOK,C'N'                                                       
         CLI   ACTEQU,ACTDEL         ACTION IS NOT DELETE SO ...                
         BE    VK90                                                             
         MVC   PRDPRON,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    PRDPRONH+6,X'80'      PRODUCT NAME                               
         B     VK90                                                             
*                                                                               
VK40     MVC   ERRNUM,=AL2(VKERR3)   ACTION IS ADD SO ...                       
         CLC   PRDPROK,=C'ZZZ'       DON'T ALLOW ADD OF PRD CODE ZZZ            
         BE    SPERREX                                                          
         MVC   ERRNUM,=AL2(NOUNAPRD) ACTION IS ADD SO ...                       
         CLC   PRDPROK,=C'UNA'       DON'T ALLOW ADD OF PRD CODE UNA            
         BE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(VKERR4)   IF AGENCY IS CK, DON'T ALLOW ADD           
         CLC   AGENCY,=C'CK'         OF PRD TO CLIENTS OTHER THAN CC            
         BNE   VK50                                                             
         CLC   QCLT,=C'CC '                                                     
         BE    VK50                                                             
         B     SPERREX                                                          
*                                                                               
VK50     MVC   ERRNUM,=AL2(VKERR5)   FIRST CHARACTER MUST BE ALPHABETIC         
         CLI   8(R2),C'A'                                                       
         BL    SPERREX                                                          
         CLI   8(R2),C'Z'                                                       
         BH    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(VKERR6)   MUST BE 2 OR 3 CHARACTERS LONG             
         CLI   5(R2),2                                                          
         BL    SPERREX                                                          
         CLI   5(R2),3                                                          
         BH    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(VKERR7)   2ND CHAR MUST BE ALPHANUMERIC              
         LA    R1,9(R2)                                                         
VK60     CLI   0(R1),C'A'                                                       
         BL    SPERREX                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   VK70                                                             
         CLI   0(R1),C'0'                                                       
         BL    SPERREX                                                          
         CLI   0(R1),C'9'                                                       
         BH    SPERREX                                                          
VK70     CLI   5(R2),2                                                          
         BE    VK75                                                             
         LA    R1,1(R1)              3RD CHAR MUST BE ALPHANUM OR #             
         CLI   0(R1),C'#'                                                       
         BE    VK72                                                             
         CLI   0(R1),C'A'                                                       
         BL    SPERREX                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   VK75                                                             
         CLI   0(R1),C'0'                                                       
         BL    SPERREX                                                          
         CLI   0(R1),C'9'                                                       
         BH    SPERREX                                                          
*                                                                               
VK72     TM    SVCLOP2,COP2DIY     DIY TRADE                                    
         BNO   VK75                                                             
         BAS   RE,VALDIY           CASH PRD EXISTS- <127 NON TRD PRDS           
*                                                                               
VK75     MVC   QPRD,8(R2)                                                       
         OC    QPRD,SPACES                                                      
         MVC   ERRNUM,=AL2(VKERR8)   "ALL" AND "NO" ARE INVALID                 
         CLC   =C'ALL',QPRD          PRODUCT CODES                              
         BE    SPERREX                                                          
         MVC   ERRNUM,=AL2(VKERR9)                                              
         CLC   =C'NO ',QPRD                                                     
         BE    SPERREX                                                          
*                                                                               
VK90     LA    R2,KEY                KEY FIELDS HAVE BEEN VALIDATED             
         USING PRDHDR,R2             NOW BUILD KEY ...                          
         XC    KEY,KEY                                                          
         MVI   PKEYTYPE,X'00'        RECORD TYPE X'00'                          
         MVC   PKEYAM,BAGYMD         MEDIA...                                   
         MVC   PKEYCLT,BCLT          CLIENT...                                  
         MVC   PKEYPRD,QPRD          AND PRODUCT CODES                          
         OC    PKEYPRD,SPACES                                                   
         MVC   PROKEY,KEY            SAVE AS PROKEY                             
VKX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       MVI   NEWLEN,C'N'                                                      
         L     R4,AIO                PRODUCT RECORD AT AIO                      
         USING PRDHDR,R4                                                        
         CLC   PLEN,=Y(PRDHDRNL)     IF BIG, KEEP IT BIG                        
         BE    VR00                                                             
         MVC   PLEN,=Y(PRDHDRL)      SET LENGTH WITH NO SAP CODE                
         CLI   SAPAGY,C'Y'                                                      
         JNE   VR00                                                             
         MVC   PLEN,=Y(PRDHDRNL)     MAKE REC BIG FOR SAP AGENCY                
         MVI   NEWLEN,C'Y'                                                      
*                                                                               
*******    MEDIA/TRAFFIC OFFICE NUMBERS                                         
*                                                                               
VR00     CLI   ACTNUM,ACTADD                                                    
         BNE   VR01                                                             
         MVI   PTRAFOFC,X'0'                                                    
         MVI   POFFICE,X'0'                                                     
VR01     DS    0H                                                               
*                                                                               
         CLI   PRDSECFL,C'Y'       PRODUCT LEVEL SECURITY USED?                 
         BNE   VR03A                                                            
         MVC   SVOFFICE,POFFICE                                                 
*                                                                               
         MVI   POFFICE,0                                                        
         MVI   PTRAFOFC,0                                                       
*                                                                               
         CLI   PRDMOFFH+5,0                                                     
         BE    VR02                                                             
         LA    R2,PRDMOFFH                                                      
         GOTO1 ANY                                                              
         CLI   WORK,C'='            INVALID OFFICE NUMBERS                      
         BE    ERRINV                                                           
         CLI   WORK,C','                                                        
         BE    ERRINV                                                           
         CLI   WORK,C'-'                                                        
         BE    ERRINV                                                           
         MVC   POFFICE,WORK                                                     
VR02     DS    0H                                                               
         CLI   PRDTOFFH+5,0                                                     
         BE    VR03                                                             
         LA    R2,PRDTOFFH                                                      
         GOTO1 ANY                                                              
         CLI   WORK,C'='            INVALID OFFICE NUMBERS                      
         BE    ERRINV                                                           
         CLI   WORK,C','                                                        
         BE    ERRINV                                                           
         CLI   WORK,C'-'                                                        
         BE    ERRINV                                                           
         MVC   PTRAFOFC,WORK                                                    
*                                                                               
VR03     BRAS  RE,VALACC           VALIDATE ACCOUNTING OFFICE                   
*                                                                               
VR03A    LA    R2,PRDPRONH           PRODUCT NAME IS REQUIRED                   
         MVC   ERRNUM,=AL2(PNREQER)                                             
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         CLI   ACTEQU,ACTDEL                                                    
         BE    VR5                                                              
         MVC   ERRNUM,=AL2(PNDELER)  CANNOT BE "DELETE"                         
         CLC   8(6,R2),=C'DELETE'                                               
         BE    SPERREX                                                          
*                                                                               
VR5      MVC   PNAME,PRDPRON         SAVE PRODUCT NAME INTO RECORD              
*                                                                               
         LA    R2,PRDACCTH                                                      
         MVC   ERRNUM,=AL2(CPERR1)   CLT/PRD CODE IS REQUIRED                   
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         LARL  R1,AGYTAB             FIND PRODUCT'S AGENCY IN AGYTAB            
VR10     CLI   0(R1),X'FF'           END OF AGYTAB?                             
         BE    VR20                                                             
         CLC   0(2,R1),AGENCY        AGENCY FOUND?                              
         BE    VR40                                                             
         LA    R1,2(R1)              BUMP TO NEXT AGENCY                        
         B     VR10                                                             
*                                                                               
VR20     DS    0H                                                               
         CLC   AGENCY,=C'H9'         FOR H9 CODE MAY BE 3 OR 4 CHARS            
         BNE   VR25                                                             
         MVC   ERRNUM,=AL2(CPERR5)                                              
         CLI   5(R2),3                                                          
         BH    SPERREX                                                          
         B     VR30                                                             
*                                                                               
VR25     DS    0H                                                               
         MVC   ERRNUM,=AL2(CPERR2)   AGENCY NOT FOUND, THEN CLT/PRD             
         CLI   5(R2),4               MUST BE 4 CHARACTERS LONG                  
         BNE   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(CPERR3)   AGENCY NOT FOUND, BUT IF CLIENT            
         CLC   QCLT,=C'BM '          IS BRISTOL MYERS AND AGENCY IS BO          
         BNE   VR30                  THE CLT/PRD MUST BE 4 NUMERICS             
         CLC   =C'BO',AGENCY                                                    
         BNE   VR30                                                             
         TM    4(R2),X'08'                                                      
         BZ    SPERREX                                                          
VR30     MVC   PACCT(4),8(R2)        SAVE CLT/PRD CODE TO RECORD                
         B     VR50                                                             
*                                                                               
VR40     MVC   ERRNUM,=AL2(CPERR4)   FOR AGENCIES FOUND IN TABLE (GY,           
         CLI   5(R2),5               DR,GN,CE,FM,RE) CLT/PRD CODE MUST          
         BNE   SPERREX               BE 5 VALID NUMERICS                        
         TM    4(R2),X'08'                                                      
         BZ    SPERREX                                                          
         PACK  PACCT(4),8(5,R2)      SAVE (PACKED) CLT/PRD CODE TO              
         MVI   PACCT,X'FF'           RECORD                                     
*                                                                               
VR50     CLI   SAPAGY,C'Y'         TEST SAP AGY                                 
         BNE   VR55                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,PRDSAPH          NOTE - SAP CODE NOT REQUIRED!                
         CLI   5(R2),0             AND BLANK OUT IF NOT ENTERED                 
         JE    VR55                                                             
         GOTO1 ANY                                                              
VR55     MVC   PSAPCODE,WORK                                                    
         EJECT                                                                  
         LA    R2,PRDCLASH           PRODUCT CLASS                              
         MVI   BYTE,0                                                           
*                                                                               
         CLI   5(R2),0               CLASS IS NOT REQUIRED                      
         BE    VR60                                                             
         CLC   8(2,R2),SPACES                                                   
         BE    VR60                                                             
*                                                                               
         MVC   FULL,8(R2)                                                       
         MVC   BYTE,FULL             STORE FIRST CLASS INTO BYTE                
         CLI   FULL+1,C' '           IS THERE A SECOND CLASS?                   
         BNH   VR60                                                             
*                                                                               
         MVC   ERRNUM,=AL2(PCLERR1)  TWO CLASSES...                             
         CLI   FULL,C'A'             FIRST CLASS MUST BE ALPHABETIC             
         BL    SPERREX               BETWEEN A AND I                            
         CLI   FULL,C'I'                                                        
         BH    SPERREX                                                          
         CLI   FULL+1,C'A'           SECOND CLASS MUST BE ALPHABETIC            
         BL    SPERREX               BETWEEN A AND I                            
         CLI   FULL+1,C'I'                                                      
         BH    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(PCLERR2)  TWO CLASSES CANNOT BE EQUAL                
         CLC   FULL(1),FULL+1                                                   
         BE    SPERREX                                                          
*                                                                               
         NI    FULL,X'0F'            HOLD AS 2 NIBBLES 1-9                      
         NI    FULL+1,X'0F'                                                     
         PACK  BYTE,FULL(1)          1ST CLASS                                  
         OC    BYTE,FULL+1           2ND CLASS                                  
VR60     MVC   PCLASS,BYTE           SAVE PRODUCT CLASS INTO RECORD             
*                                                                               
VR70     LA    R2,PRDBNAMH                                                      
         MVC   ERRNUM,=AL2(BTNERR)   BILL-TO-NAME IS REQUIRED                   
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         XC    PADDR1(120),PADDR1                                               
         MVC   PADDR1,PRDBNAM        SAVE BILL-TO-NAME INTO RECORD              
         OC    PADDR1,SPACES         AND PAD WITH SPACES                        
*                                                                               
         LA    R2,PRDADD2H           BILL-TO-ADDRESSES NOT REQUIRED             
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR2,8(R2)          SAVE 1ST ADDRESS LINE TO RECORD            
         OC    PADDR2,SPACES         AND PAD WITH SPACES                        
*                                                                               
         LA    R2,PRDADD3H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR3,8(R2)          SAVE 2ND ADDRESS LINE TO RECORD            
         OC    PADDR3,SPACES                                                    
*                                                                               
         LA    R2,PRDADD4H                                                      
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         MVC   PADDR4,8(R2)          SAVE 3RD ADDRESS LINE TO RECORD            
         OC    PADDR4,SPACES                                                    
*                                                                               
VR80     LA    R2,PRDOAFH                                                       
         XC    PAGYFEE,PAGYFEE                                                  
         CLI   5(R2),0               OTHER-AGENCY-FEE IS NOT REQUIRED           
         BE    VR100                                                            
*                                                                               
         LLC   R0,5(R2)              CONVERT OAF INTO PENNIES                   
         GOTO1 CASHVAL,DMCB,(2,PRDOAF),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
*                                                                               
         MVC   ERRNUM,=AL2(OAFERR)   CANNOT EXCEED $9.99                        
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'999'                                                      
         BH    SPERREX                                                          
         ZAP   PAGYFEE,DUB           SAVE OAF INTO RECORD                       
*                                                                               
VR100    LA    R2,PRDBBASH                                                      
         XC    PBILLBAS(5),PBILLBAS  BILL BASIS IS NOT REQUIRED                 
         CLI   5(R2),0                                                          
         BE    VR140                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'SB1X'   DOES PROFILE EXIST FOR THIS                
         NI    WORK+16,X'FF'-X'40'   MAKE S LOWERCASE                           
         MVC   WORK+20(2),AGENCY     AGENCY, MEDIA, CLIENT?                     
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),QCLT                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
*                                                                               
         CLI   WORK+11,C' '          BILL FORMULA NOT ALLOWED IF                
         BNH   VR120                 OPTION 12 IS SET                           
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VR120    LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         LA    R5,8(R2)                                                         
*                                                                               
         CLI   8(R2),C'C'            CHECK BILL BASIS FOR COMMISION             
         BNE   VR130                                                            
         OI    PBILLBAS,X'40'                                                   
         BCTR  RE,0                                                             
         MVC   ERRNUM,=AL2(BBERR)    CANNOT BE JUST A 'C'                       
         CLI   5(R2),1                                                          
         BE    SPERREX                                                          
         LA    R5,1(R5)              BUMP PAST THE 'C'                          
*                                                                               
VR130    MVC   ERRNUM,=AL2(BBERR)    BILL-BASIS MUST BE EITHER                  
         EX    RE,*+8                "GROSS" OR "NET"                           
         B     *+10                                                             
         CLC   0(0,R5),=C'GROSS'                                                
         BE    VR140                                                            
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'NET'                                                  
         BNE   SPERREX               BILL-BASIS X'40' FOR COMMISION,            
         OI    PBILLBAS,X'10'        X'10' FOR NET                              
*                                                                               
VR140    LA    R2,PRDCPCTH                                                      
         MVC   ERRNUM,=AL2(COMPERR1) COMM.% REQUIRED IF COMM BASIS              
         CLI   5(R2),0               IS PRESENT                                 
         BNE   VR150                                                            
         CLI   PRDCBASH+5,0                                                     
         BNE   SPERREX                                                          
         B     VR170                                                            
*                                                                               
VR150    MVC   ERRNUM,=AL2(COMPERR5) FIRST CHARACTER OF COMM.% MUST             
         CLI   PRDCPCT,C'+'          BE EITHER + OR -                           
         BE    VR155                                                            
         CLI   PRDCPCT,C'-'                                                     
         BNE   SPERREX                                                          
*                                                                               
VR155    MVC   ERRNUM,=AL2(COMPERR2) COMM.% MUST BE VALID NUMERIC               
         LLC   R0,5(R2)              (EXCEPT FIRST CHAR)                        
         BCTR  R0,0                                                             
         GOTO1 CASHVAL,DMCB,(4,PRDCPCT+1),(R0)                                  
         CLI   DMCB,X'FF'                                                       
         BE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(COMPERR3) 100% IS MAXIMUM COMM.%                     
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'                                                   
         BH    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(COMPERR4) 1% IS MINIMUM COMM.%                       
         C     R0,=F'0'                                                         
         BNH   SPERREX                                                          
*                                                                               
         CLI   PRDCPCT,C'+'                                                     
         BE    VR160                                                            
         LCR   R0,R0                 MAKE COMM.% NEGATIVE AND SAVE              
VR160    ST    R0,FULL               INTO RECORD                                
         MVC   PBILLCOM,FULL                                                    
*                                                                               
VR170    LA    R2,PRDCBASH                                                      
         MVC   ERRNUM,=AL2(CBASERR1) COM.BASIS REQUIRED IF COM.% IS             
         CLI   5(R2),0               PRESENT                                    
         BNE   VR180                                                            
         CLI   PRDCPCTH+5,0                                                     
         BNE   SPERREX                                                          
         B     VR190                                                            
*                                                                               
VR180    CLI   WORK+11,C' '                                                     
         BNH   VR185                                                            
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
VR185    MVC   ERRNUM,=AL2(CBASERR2) COM.BASIS MUST BE EITHER "GROSS"           
         LLC   RE,5(R2)              OR "NET"                                   
         BCTR  RE,0                                                             
         LA    R5,8(R2)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'GROSS'                                                
         BE    VR190                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'NET'                                                  
         BNE   SPERREX                                                          
*                                                                               
         OI    PBILLBAS,X'01'                                                   
VR190    OC    PBILLBAS(5),PBILLBAS                                             
         BNZ   VR200                                                            
         CLI   PRDBBASH+5,0                                                     
         BE    VR220                                                            
         B     VR210                                                            
*                                                                               
VR200    OC    PBILLCOM,PBILLCOM                                                
         BNZ   VR220                                                            
         CLI   PBILLBAS,X'40'                                                   
         BNE   VR220                 BILL BASIS SET X'01' FOR COM BASIS         
VR210    OI    PBILLBAS,X'80'        NET, X'80' FOR COM BASIS GROSS             
*                                                                               
***********************************************************************         
*                                                                               
VR220    LA    R2,PRDEDATH           EFFECTIVE DATE OF BILL FORMULA             
         OC    PBILLBAS(5),PBILLBAS                                             
         BNZ   VR230                                                            
*                                                                               
         MVC   ERRNUM,=AL2(EDERR1)   CANNOT BE AN EFFECTIVE DATE WITH-          
         XC    PBILLDT,PBILLDT       OUT A BILL OR COMM BASIS                   
         CLI   5(R2),0                                                          
         BE    VR250                                                            
         B     SPERREX                                                          
*                                                                               
VR230    MVC   ERRNUM,=AL2(EDERR2)   EFFECTIVE DATE REQUIRED IF BILL            
         CLI   5(R2),0               BASIS OR COM BASIS IS PRESENT              
         BE    SPERREX                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,PRDEDAT),WORK                                     
         MVC   ERRNUM,=AL2(BADDATE)                                             
         OC    DMCB(4),DMCB          VALIDATE EFFECTIVE DATE                    
         BZ    SPERREX                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
VR240    MVC   PBILLDT,WORK+10       SAVE EFFECTIVE DATE INTO RECORD            
*                                                                               
***********************************************************************         
*                                                                               
VR250    LA    R2,PRDGSTH            GST INPUT NOT REQUIRED                     
         MVI   PGSTCODE,0                                                       
         CLI   5(R2),0                                                          
         BE    VR280                                                            
*                                                                               
         MVC   ERRNUM,=AL2(GSTERR)                                              
         LARL  RE,GSTTAB             FIND GST CODE IN GSTTAB                    
VR260    CLI   0(RE),X'FF'           END OF GSTTAB?                             
         BE    SPERREX                                                          
         CLC   0(1,RE),PRDGST        GST FOUND?                                 
         BE    VR270                                                            
         LA    RE,1(RE)              BUMP TO NEXT GST CODE                      
         B     VR260                                                            
*                                                                               
VR270    MVC   PGSTCODE,PRDGST       SAVE GST CODE INTO RECORD                  
*                                                                               
VR280    XC    PPST,PPST             CLEAR THE PST CODE IN THE RECORD           
         LA    R2,PRDPSTH            PST FIELD HEADER                           
         CLI   5(R2),0               ANY INPUT?                                 
         BE    VR285                 NO - PST CODE NOT REQUIRED                 
         GOTO1 =A(VALPST),RR=RELO    VALIDATE THE PST                           
         MVC   PPST,PSTOUT           10 CHAR BLOCK RETURNED IN PSTOUT           
*                                                                               
VR285    XC    PMPST,PMPST           CLEAR THE MAIN PST CODE IN THE REC         
         LA    R2,PRDMPSTH           MAIN PST FIELD HEADER                      
         CLI   5(R2),0               ANY INPUT?                                 
         BE    VR290                 NO - MAIN PST CODE NOT REQUIRED            
         GOTO1 =A(VALPST),RR=RELO    VALIDATE THE MAIN PST                      
*                                                                               
         LA    R2,PSTOUT             REFORMAT THE OUTPUT                        
         LA    R3,10                 10 CHAR PST STRING                         
*                                                                               
         CLI   0(R2),0               ANY PST INPUT?                             
         BNE   VR286                 YES                                        
         LA    R2,1(R2)              NO - BUMP TO NEXT PST POSITION             
         BCT   R3,*-12               CHECK NEXT PST POSITION                    
         B     VR290                 WHAT HAPPENED?                             
*                                                                               
VR286    MVC   PMPST+1(1),0(R2)      MOVE THE PST IN                            
         LA    R3,PSTOUT             A(PST INPUT STRING)                        
         SR    R2,R3                 R2 = INDEX                                 
         AHI   R2,1                  START INDEXING FROM 1, NOT 0!              
         STC   R2,PMPST              SAVE THE INDEX IN THE RECORD               
*                                                                               
VR290    XC    USERDATA,USERDATA                                                
         OC    SVP1USER,SVP1USER     ANY "PRODUCT 1" INFO?                      
         BZ    VR300                                                            
*                                                                               
         LA    R2,PRDUSR1H           SET UP PARAMETERS FOR CALL TO              
         ST    R2,AUSR               EDTUSR WITH PRODUCT 1 INFO                 
         MVC   UTYPE,SVP1TYPE        TYPE                                       
         MVC   LEN,SVP1LEN           LENGTH                                     
         MVC   FLAG1,SVP1FLG1        1ST FLAG                                   
         MVC   FLAG2,SVP1FLG2        2ND FLAG                                   
         BAS   RE,EDTUSR                                                        
*                                                                               
VR300    MVC   PUSER1,USERDATA       SAVE PRODUCT 1 INFO INTO RECORD            
         MVC   PRDUSR1,USERDATA      CLEAR OR RETRANSMIT USER 1 FLD.            
         OI    PRDUSR1H+6,X'80'                                                 
*                                                                               
         XC    USERDATA,USERDATA                                                
         OC    SVP2USER,SVP2USER     ANY "PRODUCT 2" INFO?                      
         BZ    VR310                                                            
*                                                                               
         LA    R2,PRDUSR2H           SET UP PARAMETERS FOR CALL TO              
         ST    R2,AUSR               EDTUSR WITH PRODUCT 2 INFO                 
         MVC   UTYPE,SVP2TYPE        TYPE                                       
         MVC   LEN,SVP2LEN           LENGTH                                     
         MVC   FLAG1,SVP2FLG1        1ST FLAG                                   
         MVC   FLAG2,SVP2FLG2        2ND FLAG                                   
         BAS   RE,EDTUSR                                                        
*                                                                               
VR310    MVC   PUSER2,USERDATA       SAVE PRODUCT 2 INFO INTO RECORD            
         MVC   PRDUSR2,USERDATA      CLEAR OR RETRANSMIT USER 2 FLD.            
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
         NI    POPT1,X'FF'-POPT1_NOBILL  TURN OFF FLAG IN CASE ERASED           
         NI    POPT1,X'FF'-POPT1_THTR    TURN OFF THTR IN CASE ERASED           
         NI    POPT1,X'FF'-POPT1_THNT    TURN OFF THNT IN CASE ERASED           
*                                                                               
         MVI   PRATE,0                                                          
         LA    R2,PRDOPTNH           CALL SCANNER TO BREAK UP OPTIONS           
         MVI   OPTNFLAG,0            FIELD                                      
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         ZICM  R0,DMCB+4             # OF OPTIONS SCANNED                       
         BZ    VR340                                                            
*                                                                               
         LA    R5,BLOCK                                                         
         MVI   FIELDERR,1            COUNTS FIELD NUMBER FOR ERRORS             
VR320    ZICM  R1,0(R5)              ANY OPTIONS LEFT?                          
         BZ    VR340                                                            
*                                                                               
         MVC   ERRNUM,=AL2(INVOPT)                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'NTP'                                                 
         BE    VR325                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'RATE'                                                
         BE    VR326                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'BILL'                                                
         BE    VR328                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'CPPRS'                                               
         BE    VR328A                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'THTR'                                                
         BNE   *+20                                                             
         TM    POPT1,POPT1_THNT                                                 
         BO    SPCURSER                                                         
         OI    POPT1,POPT1_THTR                                                 
         B     VR329                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'THNT'                                                
         BNE   SPCURSER                                                         
         TM    POPT1,POPT1_THTR                                                 
         BO    SPCURSER                                                         
         OI    POPT1,POPT1_THNT                                                 
         B     VR329                                                            
*                                                                               
VR325    MVC   ERRNUM,=AL2(NTPERR3)  NTP CAN ONLY BE SET ONCE                   
         TM    OPTNFLAG,TALOPTN                                                 
         BO    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(GMIERR)   GMI MUST BE SETUP FOR CLIENT               
         TM    SVCLOP1,COP1GMI                                                  
         BNO   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(TALERR)   NTP NOT VALID FOR AAA OR POL               
         CLC   QPRD,=C'AAA'          PRODUCTS                                   
         BE    SPERREX                                                          
         CLC   QPRD,=C'POL'                                                     
         BE    SPERREX                                                          
*                                                                               
         TM    3(R5),X'80'           NTP MUST BE VALID NUMERIC ...              
         BNO   ERRINV                BETWEEN 0 AND 3 INCLUSIVE                  
         MVC   ERRNUM,=AL2(OPTERR2)                                             
         CLI   1(R5),1                                                          
         BNE   SPERREX                                                          
         CLI   11(R5),3                                                         
         BH    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(TALERR2)  IF PRODUCT ALREADY HAD AN                  
         CLI   ACTNUM,ACTADD         NTP, CANNOT CHANGE                         
         BE    *+14                                                             
         CLC   PTAL,11(R5)                                                      
         BNE   SPERREX                                                          
*                                                                               
         MVC   PTAL,11(R5)           NTP HAS BEEN VALIDATED ...                 
         OI    OPTNFLAG,TALOPTN      BUMP TO NEXT OPTION                        
         B     VR329                                                            
*                                                                               
VR326    MVC   ERRNUM,=AL2(RATERR1)  RATE CAN ONLY BE SET ONCE                  
         CLI   PRATE,0                                                          
         BNE   SPCURSER                                                         
*                                                                               
         MVC   ERRNUM,=AL2(RATERR2)  IF CLIENT DOESN'T ALLOW SPECIAL            
         CLI   SVCLPROF+14,C'*'      RATES ... RATE OPTION INVALID              
         BE    SPCURSER                                                         
*                                                                               
         MVC   ERRNUM,=AL2(RATERR3)  VALID RATES ARE * OR 0-9                   
         CLI   1(R5),1               CHECK LENGTH                               
         BNE   SPCURSER                                                         
         CLI   22(R5),C'*'                                                      
         BE    VR327                                                            
         CLI   22(R5),C'0'                                                      
         BL    SPCURSER                                                         
         CLI   22(R5),C'9'                                                      
         BH    SPCURSER                                                         
*                                                                               
VR327    MVC   PRATE,22(R5)          RATE HAS BEEN VALIDATED ...                
         B     VR329                                                            
*                                                                               
VR328    DS    0H                                                               
         MVC   ERRNUM,=AL2(BILLER1)  VALID OPTIONS ARE BILL=YES OR NO           
         ZICM  R1,1(R5),1            LEN OF SECOND HALF OF FIELD                
         BZ    SPCURSER                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R5),=C'YES'                                                 
         BE    VR329                                                            
*                                                                               
         MVC   ERRNUM,=AL2(BILLER1)  VALID OPTIONS ARE BILL=YES OR NO           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R5),=C'NO'                                                  
         BNE   SPCURSER                                                         
         OI    POPT1,POPT1_NOBILL                                               
         B     VR329                                                            
*                                                                               
VR328A   DS    0H                                                               
         MVC   ERRNUM,=AL2(INVCPPRS)                                            
         CLI   1(R5),1                                                          
         BNE   SPCURSER                                                         
*                                                                               
         MVC   ERRNUM,=AL2(INVCPPRS)                                            
         CLI   22(R5),C'Y'                                                      
         BNE   *+12                                                             
         MVI   PCPPRS,C'Y'                                                      
         B     VR329                                                            
*                                                                               
         CLI   22(R5),C'N'                                                      
         BNE   SPCURSER                                                         
         MVI   PCPPRS,C'N'                                                      
*                                                                               
VR329    LA    R5,32(R5)             BUMP TO NEXT OPTION                        
         LLC   R1,FIELDERR          INCREMENT FIELD #                           
         LA    R1,1(R1)                                                         
         STC   R1,FIELDERR                                                      
         BCT   R0,VR320                                                         
*                                                                               
VR340    MVC   KEY,PROKEY            VALIDATION IS COMPLETE ...                 
         MVC   PPROF(30),=30C'0'                                                
         CLI   ACTEQU,ACTADD                                                    
         BNE   VR680                                                            
*                                                                               
***********************************************************************         
*        ADDING A PRODUCT RECORD ... READ IN THE CLIENT RECORD                  
***********************************************************************         
*                                                                               
VR360    XC    KEY+4(3),KEY+4        CLEAR PRODUCT CODE OUT OF KEY              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VR370                                                            
         DC    H'0'                                                             
VR370    MVC   AIO,AIO2              READ CLIENT RECORD INTO AIO2               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R5,AIO2                                                          
         USING CLTHDR,R5             POINT R1 AT CLIENT'S PRODUCT LIST          
         LA    R1,CLIST                                                         
*                                                                               
***********************************************************************         
*        POL PRODUCT SECTION                                                    
***********************************************************************         
*                                                                               
         CLC   QPRD,=C'POL'          IF POL PRODUCT ...                         
         BNE   VR400                 CHANGE 2ND BYTE OF PCODE TO X'FF'          
         MVI   PCODE+1,X'FF'                                                    
VR380    CLI   0(R1),0               END OF CLIST?                              
         BE    VR390                                                            
         CLI   3(R1),X'FF'           POL ALREADY IN CLIST                       
         BE    VR580                                                            
         LA    R1,4(R1)              BUMP TO NEXT PRODUCT                       
         B     VR380                                                            
*                                                                               
VR390    MVC   ERRNUM,=AL2(CLTFULL)  ADD NEW POL PRODUCT TO CLIST...            
         LA    R3,CLIST+876                                                     
         CR    R1,R3                 CAN'T ADD IF CLIST IS FULL                 
         BNL   SPERREX                                                          
         MVC   0(3,R1),=C'POL'       MOVE C'PO' INTO 1ST 2 BYTES,               
         MVI   3(R1),X'FF'           X'FF' INTO THIRD BYTE                      
         B     VR580                                                            
*                                                                               
***********************************************************************         
*        HOW MANY PRODUCTS IN THE CLIENT'S LIST?                                
***********************************************************************         
*                                                                               
VR400    XC    ELEM(220),ELEM        220 = MAX NUMBER OF PRODUCTS               
         LA    R0,220                IN CLIST ... R6 = NUMBER OF                
         SR    R6,R6                 PRODUCTS IN CLIST                          
*                                                                               
VR410    CLI   0(R1),0               END OF CLIST?                              
         BE    VR440                                                            
         CLI   3(R1),X'FF'                                                      
         BNE   VR420                                                            
         LA    R6,1(R6)              POL PRODUCT ... INCREMENT PRO-             
         MVC   0(3,R1),=3X'FF'       DUCT COUNTER AND MODIFY TO BE              
         B     VR430                 LAST ENTRY OF BINSRCH TABLE                
*                                                                               
VR420    LLC   R5,3(R1)              NON-POL PRODUCT ... SET USED               
         LA    R5,ELEM-1(R5)         FLAG AND INCREMENT PRODUCT                 
         MVI   0(R5),X'FF'           COUNTER                                    
         LA    R6,1(R6)                                                         
VR430    LA    R1,4(R1)              BUMP TO NEXT ENTRY OF CLIST                
         J     VR410                                                            
*                                                                               
VR440    STC   R6,CCOUNT             CCOUNT = # OF PROS IN CLIST                
*                                                                               
         LA    R1,ELEM                                                          
VR460    CLI   0(R1),0               FIND FIRST FREE ENTRY                      
         JE    VR470                                                            
         LA    R1,1(R1)                                                         
         J     VR460                                                            
*                                                                               
VR470    LA    R0,ELEM-1                                                        
         SR    R1,R0                 SAVE INTO RECORD                           
         STCM  R1,3,PCODE                                                       
*                                                                               
         TM    SVCLOP2,COP2DIY       DIY TRADE ON                               
         JZ    VR480                                                            
         CLI   QPRD+2,C'#'           FOR TRADE PRODUCT                          
         JNE   VR480                                                            
         XC    PCODE,PCODE                                                      
         MVC   PCODE+1(1),SVPRDNUM   PRD NUM = CASH PRD# +X'80'                 
         CLI   PCODE+1,0                                                        
         JE    *+2                                                              
                                                                                
***********************************************************************         
*        IF TRAFFIC CLIENT MASTER RECORD EXISTS FOR THIS CLIENT ...             
*        TEST THAT PRODUCT IS IN TRAFFIC CLIENT MASTER RECORD                   
***********************************************************************         
*                                                                               
VR480    L     R5,AIO2               READ TRAFFIC MASTER CLIENT REC             
         MVC   WORK(3),QPRD                                                     
         MVC   WORK+3(1),PCODE+1                                                
         OC    CMCLTCOD,CMCLTCOD     ANY TRAFFIC MASTER CLT?                    
         BZ    VR520                                                            
         CLI   CMCLTPRD,0            IF TRAFFIC PRD NO NEED TO CHECK            
         BNE   VR520                                                            
         MVC   WORK2(13),0(R5)       SAVE CURRENT CLIENT RECORD                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),PROKEY+1     SET UP KEY TO READ TRAFFIC MASTER          
         MVC   KEY+2(2),CMCLTCOD     CLIENT RECORD INTO AIO3                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRMIS                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R2,AIO3               PRODUCT MUST BE IN TRAFFIC MASTER          
         LA    R2,CLIST-CLTHDR(R2)   CLIENT'S PRODUCT LIST                      
VR490    OC    0(4,R2),0(R2)                                                    
         BZ    VR500                                                            
         CLC   QPRD,0(R2)            PRODUCT FOUND?                             
         BE    *+12                                                             
         LA    R2,4(R2)              BUMP TO NEXT PRODUCT                       
         B     VR490                                                            
*                                                                               
         CLC   PCODE+1(1),3(R2)      SEQUENCE NUMBERS MUST MATCH                
         BE    VR510                                                            
VR500    LA    R2,PRDPROKH                                                      
         MVC   ERRNUM,=AL2(PRDERR)                                              
         B     SPERREX                                                          
*                                                                               
VR510    MVC   KEY(13),WORK2         READ CLIENT RECORD BACK INTO               
         GOTO1 HIGH                  AIO2                                       
         CLC   KEY(13),WORK2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
***********************************************************************         
*        IF TRADE CLIENT OR TRADE AGENCY ... ADD TRADE PRODUCT TO               
*        CLIENT RECORD                                                          
***********************************************************************         
*                                                                               
VR520    DS    0H                                                               
         LA    R2,PRDPROKH                                                      
         CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY                         
         BE    VR525               DON'T ADD TRADE PRODUCT TO CLT REC           
         BAS   RE,TESTTRAD           TEST FOR TRADE                             
VR525    DS    0H                                                               
*                                                                               
***********************************************************************         
*        ADD PRODUCT TO CLIENT'S PRODUCT LIST                                   
***********************************************************************         
*                                                                               
         LLC   R3,CCOUNT                                                        
         GOTO1 BINSRCH,DMCB,(X'01',WORK),CLIST,(R3),4,(0,3),218                 
         OC    DMCB(4),DMCB                                                     
         BNZ   VR540                 IF TOO MANY PRODUCTS IN CLIST,             
         MVC   ERRNUM,=AL2(CLTFULL)  RETURN ERROR                               
         B     SPERREX                                                          
*                                                                               
VR540    CLI   DMCB,1                                                           
         BE    VR550                                                            
         L     R1,DMCB               PRODUCT ALREADY IN LIST                    
         MVC   PCODE+1(1),3(R1)      ADDRESS OF PRODUCT IN R1                   
         CLI   PCODE+1,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                    PRODUCT NOT ALREADY IN LIST                
VR550    LA    R1,CLIST              ADDRESS OF CLIST IN R1                     
*                                                                               
VR560    CLI   0(R1),0               END OF CLIST?                              
         BE    VR580                                                            
         CLC   0(3,R1),=3X'FF'       POL PRODUCT?                               
         BE    VR570                                                            
*                                                                               
         CLI   3(R1),0             ** MAKE SURE NO PRD CODES ARE 0 **           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,4(R1)              BUMP TO NEXT PRODUCT                       
         B     VR560                                                            
*                                                                               
VR570    MVC   0(3,R1),=C'POL'       RESET POL PRODUCT FROM 3X'FF'              
*                                    TO 'POL'                                   
VR580    LA    R3,4                                                             
         LA    R4,SVCLIST            SAVE CLIST IN SVCLIST                      
         LA    R5,CLIST                                                         
         MVC   0(220,R4),0(R5)                                                  
         LA    R4,220(R4)                                                       
         LA    R5,220(R5)                                                       
         BCT   R3,*-14                                                          
         BRAS  RE,PTREC              PUT CLIENT RECORD                          
                                                                                
***********************************************************************         
*        IF CANADIAN AGENCY ADDS A PRODUCT FOR TV - MUST MODIFY CLIENT          
*        REC FOR MEDIA N(03) AND MEDIA C(08)                                    
***********************************************************************         
                                                                                
         BRAS  RE,CANTVC                                                        
                                                                                
***********************************************************************         
*        ADD PRODUCT REC                                                        
***********************************************************************         
                                                                                
VR670    MVC   AIO,AIO1              RESET AIO TO PRODUCT                       
         L     R4,AIO                                                           
         MVC   KEY,PROKEY                                                       
         MVC   PKEY,PROKEY                                                      
         CLC   PLEN,=Y(PRDHDRNL)     IF BIG, KEEP IT BIG                        
         BE    *+10                                                             
         MVC   PLEN,=Y(PRDHDRL)    ELSE DON'T NEED THE EXTRA ROOM               
*                                                                               
         BRAS  RE,ADREC              ADD THE PRODUCT RECORD                     
*                                                                               
         BAS   RE,GENREQ             GENERATE REQUEST RECORD                    
                                                                                
***********************************************************************         
*        ADD TRADE PRODUCT                                                      
***********************************************************************         
                                                                                
         CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY                         
         BE    VR685                                                            
*                                                                               
         TM    SVAGYFL1,X'02'      CLIENT MUST BE EITHER A TRADE                
         BO    VR675               AGENCY OR A TRADE CLIENT                     
         TM    SVCLOP2,COP2TRAD                                                 
         BZ    VR685                                                            
*                                                                               
VR675    MVI   KEY+6,C'#'                                                       
         MVI   PKEYPRD+2,C'#'      SET KEY AND RECORD TO TRADE PRODUCT          
         MVI   QPRD+2,C'#'         AND QPRD FOR GENREQ                          
         OI    PCODE+1,X'80'       SET TRADE BIT IN PRODUCT #                   
         BRAS  RE,ADREC            ADD TRADE PRODUCT                            
         BAS   RE,GENREQ                                                        
*                                                                               
         MVI   QPRD+2,C'C'         DONE ADDING PRODUCTS                         
         B     VR685                                                            
                                                                                
***********************************************************************         
*        PUT MODIFIED PRODUCT RECORD                                            
***********************************************************************         
                                                                                
VR680    GOTO1 HIGH                                                             
         CLI   NEWLEN,C'Y'                                                      
         JE    VR682                                                            
         BRAS  RE,PTREC                                                         
         BAS   RE,GENREQ          GENERATE REQUEST RECORD                       
         B     VR685                                                            
                                                                                
* ADD LONGER RECORD AND DO NOT DIE ON DUP KEY ON ADD                            
* THEN UPDATE DISK ADDRESS IN ACTIVE POINTER                                    
                                                                                
VR682    GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
*                                                                               
         MVC   KEYSAVE,KEY         SAVE KEY WITH NEW DISK ADDRESS               
         GOTO1 (RF),(R1),=C'DMRDHI',=C'SPTDIR',KEY,KEY                          
         CLC   KEY(13),KEYSAVE     MAKE SURE I FOUND IT                         
         JNE   *+2                                                              
         MVC   KEY,KEYSAVE         MOVE KEY WITH NEW D/A                        
         GOTO1 (RF),(R1),=C'DMWRT'                                              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
                                                                                
***********************************************************************         
*        IF CANADIAN AGENCY ADDS A PRODUCT FOR TV - MUST ADD PRODUCT            
*        REC FOR MEDIA N(03) AND MEDIA C(08)                                    
***********************************************************************         
                                                                                
VR685    L     R4,AIO                                                           
         USING PRDHDR,R4                                                        
         BRAS  RE,CANTV                                                         
*                                                                               
***********************************************************************         
*        END VALREC AND CALL DISPREC                                            
***********************************************************************         
*                                                                               
VRX      OI    GENSTAT2,RETEQSEL                                                
         B     DR                    REDISPLAY RECORD                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
*                                                                               
DR       LA    R0,PRDOPTNH          LAST FIELD                                  
         LA    R2,PRDPNUMH          FIRST FIELD                                 
*                                                                               
DR10     LLC   R1,0(R2)             LENGTH OF FIRST FIELD                       
         SHI   R1,9                 MINUS HEADER AND 1 FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES       BLANK CURRENT FIELD                         
         OI    6(R2),X'80'          TRANSMIT                                    
DR20     LLC   R1,0(R2)             R1 = LENGTH OF FIELD + HEADER               
         AR    R2,R1                NEXT FIELD                                  
         CR    R2,R0                END OF SCREEN?                              
         BH    *+16                                                             
         TM    1(R2),X'20'          NO, IS FIELD PROTECTED?                     
         BZ    DR10                 NO, CLEAR IT                                
         B     DR20                 YES, BUMP TO NEXT FIELD                     
*                                                                               
***********************************************************************         
*                                                                               
         USING PRDHDR,R4            PRODUCT RECORD                              
         L     R4,AIO                                                           
*                                                                               
***********************************************************************         
*                                                                               
         CLI   TWAOFFC,C'*'         PRODUCT NUMBER                              
         BNE   DR25                                                             
         GOTO1 HEXOUT,DMCB,PCODE,PRDPNUM,2                                      
         OI    PRDPNUMH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
DR25     MVC   PRDPRON,PNAME         PRODUCT NAME                               
         OI    PRDPRONH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         CLI   PACCT,X'FF'                                                      
         BNE   DR30                                                             
*                                                                               
         UNPK  PRDACCT(5),PACCT+1(3) ACCOUNT NUMBER PACKED                      
         OI    PRDACCTH+6,X'80'                                                 
         B     DR40                                                             
*                                                                               
DR30     MVC   PRDACCT(4),PACCT      ACCOUNT NUMBER NOT PACKED                  
         OI    PRDACCTH+6,X'80'                                                 
*                                                                               
******MEDIA, TRAFFIC OFFICE NUMBERS ***********************************         
*                                                                               
         OI    PRDMOHH+1,X'2C'     SET INTENSITY TO ZERO + PROTECT              
         OI    PRDTOHH+1,X'2C'                                                  
         OI    PRDMOFFH+1,X'2C'    SET INTENSITY TO ZERO +PROTECT               
         OI    PRDTOFFH+1,X'2C'                                                 
         CLI   PRDSECFL,C'Y'       PRODUCT LEVEL SECURITY USED?                 
         BNE   DR32                                                             
         NI    PRDMOHH+1,X'FF'-X'0C'     SET INTENSITY TO NORMAL                
         NI    PRDTOHH+1,X'FF'-X'0C'     SET INTENSITY TO NORMAL                
         NI    PRDMOFFH+1,X'FF'-X'2C'    SET INTENSITY TO NORMAL+UNPROT         
         NI    PRDTOFFH+1,X'FF'-X'2C'    SET INTENSITY TO NORMAL                
*                                                                               
         MVC   PRDMOFF,POFFICE                                                  
         MVC   PRDTOFF,PTRAFOFC                                                 
*                                                                               
DR32     DS    0H                                                               
         OI    PRDMOHH+6,X'80'                                                  
         OI    PRDTOHH+6,X'80'                                                  
         OI    PRDMOFFH+6,X'80'                                                 
         OI    PRDTOFFH+6,X'80'                                                 
*                                                                               
****ACCOUNTING OFFICE**************************************************         
         OI    PRDAOHH+1,X'2C'     SET INTENSITY TO ZERO + PROTECT              
         OI    PRDAOFCH+1,X'2C'    SET INTENSITY TO ZERO +PROTECT               
                                                                                
         CLI   PRDSECFL,C'Y'       PRODUCT LEVEL SECURITY USED?                 
         BNE   DR33                                                             
         NI    PRDAOHH+1,X'FF'-X'0C'     SET INTENSITY TO NORMAL                
         NI    PRDAOFCH+1,X'FF'-X'2C'    SET INTENSITY TO NORMAL+UNPROT         
DR33     DS    0H                                                               
         BRAS  RE,DISACC             DISPLAY ACCOUNTING AGENCY                  
         OI    PRDAOHH+6,X'80'                                                  
         OI    PRDAOFCH+6,X'80'                                                 
***********************************************************************         
*                                                                               
         LA    R2,PRDSAPH                                                       
         XC    8(10,R2),8(R2)                                                   
         CLI   SAPAGY,C'Y'         TEST SAP AGENCY                              
         BNE   DR34                                                             
         MVC   8(10,R2),PSAPCODE                                                
         OI    6(R2),X'80'                                                      
*                                                                               
DR34     MVC   FULL,SPACES                                                      
         CLI   PCLASS,0              PRODUCT CLASS?                             
         BE    DR35                                                             
*                                                                               
         MVC   FULL(1),PCLASS        YES                                        
         MVI   FULL+1,C' '           SPACE IN SECOND BYTE                       
*                                                                               
         CLI   PCLASS,X'99'                                                     
         BH    DR35                                                             
         PACK  FULL(1),PCLASS        RECORD HAS 2 PRODUCT CLASSES...            
         NI    FULL,X'0F'                                                       
         OI    FULL,X'C0'                                                       
         MVC   FULL+1(1),PCLASS                                                 
         NI    FULL+1,X'0F'                                                     
         OI    FULL+1,X'C0'                                                     
*                                                                               
DR35     MVC   PRDCLAS,FULL          NO MORE CLASSES TO PROCESS                 
         OI    PRDCLASH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
DR40     MVC   PRDBNAM,PADDR1                                                   
         OI    PRDBNAMH+6,X'80'      BILL-TO NAME AND ADDRESSES                 
         MVC   PRDADD2,PADDR2        TO SCREEN                                  
         OI    PRDADD2H+6,X'80'                                                 
         MVC   PRDADD3,PADDR3                                                   
         OI    PRDADD3H+6,X'80'                                                 
         MVC   PRDADD4,PADDR4                                                   
         OI    PRDADD4H+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         OC    PAGYFEE,PAGYFEE       OTHER AGENCY FEE?                          
         BNZ   DR50                                                             
*                                                                               
         MVC   PRDOAF,SPACES                                                    
         OI    PRDOAFH+6,X'80'       NO, SEND BLANK FIELD TO SCREEN             
         B     DR60                                                             
*                                                                               
DR50     EDIT  (P2,PAGYFEE),(4,PRDOAF),2     YES, OTHER AGENCY FEE              
         OI    PRDOAFH+6,X'80'                                                  
*                                                                               
***********************************************************************         
*                                                                               
DR60     OC    PBILLBAS(5),PBILLBAS  BILLING FORMULA?                           
         BNZ   DR70                                                             
*                                                                               
         MVC   PRDBBAS,SPACES                                                   
         OI    PRDBBASH+6,X'80'      NO BILLING FORMULA, SEND BLANK             
         MVC   PRDCPCT,SPACES        FIELDS FOR BILL BASIS, COMM PCT            
         OI    PRDCPCTH+6,X'80'      AND COM BASIS TO SCREEN                    
         MVC   PRDCBAS,SPACES                                                   
         OI    PRDCBASH+6,X'80'                                                 
         B     DR80                                                             
*                                                                               
*                                    BILL BASIS                                 
DR70     MVC   PRDBBAS,=CL5'CNET'    X'10' AND X'40' BIT ON ...                 
         TM    PBILLBAS,X'50'        SET BILL BASIS FIELD TO 'CNET'             
         BO    DR75                                                             
*                                                                               
         MVC   PRDBBAS,=CL5'NET'     X'10' BIT ON ...                           
         TM    PBILLBAS,X'10'        SET BILL BASIS FIELD TO 'NET'              
         BO    DR75                                                             
*                                                                               
         MVC   PRDBBAS,=C'CGROS'     X'40' BIT ON ...                           
         TM    PBILLBAS,X'40'        SET BILL BASIS FIELD TO 'CROSS'            
         BO    DR75                                                             
*                                    OTHERWISE...                               
         MVC   PRDBBAS,=C'GROSS'     SET BILL BASIS FIELD TO 'GROSS'            
DR75     OI    PRDBBASH+6,X'80'                                                 
*                                                                               
         ICM   R5,15,PBILLCOM        COM. PERCENTAGE                            
         LTR   R5,R5                                                            
         BNZ   DR75B                                                            
*                                                                               
         MVC   PRDCPCT,SPACES        IF COM BASIS IS ZERO ...                   
         OI    PRDCPCTH+6,X'80'      MOVE BLANK COMMISION PERCENTAGE            
         MVC   PRDCBAS,SPACES        AND BLANK COM BASIS TO SCREEN              
         OI    PRDCBASH+6,X'80'                                                 
         B     DR80                                                             
*                                                                               
DR75B    LPR   RF,R5                                                            
         C     RF,=F'1000000'        IF COM BASIS IS 100% MOVE 100              
         BNE   DR75C                 TO SCREEN                                  
         MVC   PRDCPCT+1(3),=C'100'                                             
         B     DR75D                                                            
*                                    OTHERWISE ...                              
*                                    EDIT COM.% INTO SCREEN FIELD               
DR75C    EDIT  (R5),(8,PRDCPCT),4,FLOAT=+,ALIGN=LEFT                            
*                                                                               
DR75D    LTR   R5,R5                                                            
         BNM   *+8                   COMMISION % NEGATIVE, MINUS SIGN           
         MVI   PRDCPCT,C'-'                                                     
         OI    PRDCPCTH+6,X'80'                                                 
*                                                                               
*                                    COMM.BASIS                                 
         MVC   PRDCBAS,=C'GROSS'     X'01' BIT ON ...                           
         TM    PBILLBAS,X'01'        SET COMM.BASIS FIELD TO 'GROSS'            
         BZ    *+10                                                             
         MVC   PRDCBAS,=CL5'NET'     OTHERWISE ...                              
         OI    PRDCBASH+6,X'80'      SET COMM.BASIS FIELD TO 'NET'              
*                                                                               
DR80     OC    PBILLDT,PBILLDT       EFFECTIVE MONTH/YEAR                       
         BNZ   DR85                                                             
         MVC   PRDEDAT,SPACES        IF NO, SEND BLANK FIELD TO SCREEN          
         OI    PRDEDATH+6,X'80'                                                 
         B     DR90                                                             
*                                    YES ...                                    
*                                    CONVERT EFFECTIVE MONTH/YEAR               
DR85     GOTO1 DATCON,DMCB,(3,PBILLDT),(6,PRDEDAT)                              
         OI    PRDEDATH+6,X'80'                                                 
*                                                                               
DR90     MVC   PRDGST,PGSTCODE       GST CODE                                   
         OI    PRDGSTH+6,X'80'                                                  
*                                                                               
         LA    R2,PRDPSTH            PST FIELD                                  
         LA    R3,L'PRDPST-1         PST FIELD LENGTH -1 FOR EX                 
         MVC   WORK(10),PPST         PASS IN THE PST                            
         BRAS  RE,DISPPST            DISPLAY THE PST CODE                       
*                                                                               
         XC    WORK(10),WORK         CLEAR WORK                                 
         LA    R2,WORK               BUILD MAIN PST STRING IN WORK              
         LLC   R3,PMPST              INDEX INTO WORK                            
         BCTR  R3,0                  -1                                         
         AR    R2,R3                 PUT MAIN PST CODE HERE                     
         MVC   0(1,R2),PMPST+1       MAIN PST CODE                              
         LA    R2,PRDMPSTH           MAIN PST FIELD                             
         LA    R3,L'PRDMPST-1        MAIN PST FIELD LENGTH -1 FOR EX            
         BRAS  RE,DISPPST            DISPLAY THE MAIN PST CODE                  
*                                                                               
*                                    USER DESCRIPTION FIELDS                    
         LA    RF,PRDUSR1H           FIRST USER                                 
         LA    RE,PRDDSC1H           FIRST DESCIPTION                           
*                                                                               
         OI    PRDUSR1H+1,X'20'      FIRST DESCRIPTION LINE                     
         MVC   ELEM(L'SVP1USER),SVP1USER                                        
         MVC   8(L'SVP1USER,RE),ELEM                                            
         CLC   SPACES(L'SVP1USER),ELEM                                          
         BL    DR100                                                            
*                                                                               
         LR    R0,RE                 FIRST DESCIPTION ADDRESS                   
         LLC   RE,0(RF)              RE = L(HEADER) + L(INPUT)                  
         SHI   RE,8                  RE = L(INPUT)                              
         TM    1(RF),X'02'           CHECK FOR EXTENSION                        
         BZ    *+8                                                              
         SHI   RE,8                                                             
         BCTR  RE,0                                                             
         EX    RE,*+10                                                          
         LR    RE,R0                                                            
         B     DR110                                                            
         XC    8(0,RF),8(RF)         CLEAR GARBAGE                              
DR100    NI    1(RF),X'FF'-X'20'     UNPROTECT INPUT FIELD                      
DR110    OI    6(RE),X'80'           TRANSMIT                                   
*                                                                               
         XC    PRDUSR1,PRDUSR1       COPY USER1 FIELD FROM RECORD INTO          
         OC    SVP1USER,SVP1USER     SCREEN AND TRANSMIT                        
         BZ    *+10                                                             
         MVC   PRDUSR1,PUSER1                                                   
         OI    PRDUSR1H+6,X'80'                                                 
*                                                                               
         LA    RF,PRDUSR2H           SECOND USER                                
         LA    RE,PRDDSC2H           SECOND DESCIPTION                          
*                                                                               
         OI    PRDUSR2H+1,X'20'              PROTECT FIELD                      
         MVC   ELEM(L'SVP2USER),SVP2USER     DISPLAY 2ND DESC LINE              
         MVC   8(L'SVP2USER,RE),ELEM                                            
         CLC   SPACES(L'SVP2USER),ELEM       SHOW DISC                          
         BL    DR120                                                            
*                                                                               
         LR    R0,RE                 SAVE C(RE)                                 
         LLC   RE,0(RF)              RE = L(HEADER) + L(INPUT)                  
         SHI   RE,8                  RE = L(INPUT)                              
         TM    1(RF),X'02'           CHECK FOR EXTENSION                        
         BZ    *+8                                                              
         SHI   RE,8                                                             
         BCTR  RE,0                                                             
         EX    RE,*+10                                                          
         LR    RE,R0                                                            
         B     DR130                                                            
         XC    8(0,RF),8(RF)         CLEAR GARBAGE                              
DR120    NI    1(RF),X'FF'-X'20'     UNPROTECT INPUT FIELD                      
DR130    OI    6(RE),X'80'           TRANSMIT                                   
         XC    PRDUSR2,PRDUSR2       COPY USER2 FIELD FROM RECORD INTO          
         OC    SVP2USER,SVP2USER     SCREEN AND TRANSMIT                        
         BZ    *+10                                                             
         MVC   PRDUSR2,PUSER2                                                   
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R5,PRDOPTN            OPTIONS FIELD                              
         MVC   PRDOPTN,SPACES                                                   
*                                                                               
         TM    SVCLOP1,COP1GMI       NTP NEVER PRESENT FOR CLIENTS W/0          
         BNO   DR135                 GMI SETUP OR ANY AAA OR POL PRDS           
         CLC   QPRD,=C'AAA'                                                     
         BE    DR135                                                            
         CLC   QPRD,=C'POL'                                                     
         BE    DR135                                                            
*                                                                               
         MVC   0(4,R5),=C'NTP='      COPY NTP INTO OPTIONS FIELD                
         LLC   R1,PTAL                                                          
         CVD   R1,DUB                                                           
         UNPK  4(1,R5),DUB                                                      
         OI    4(R5),X'F0'                                                      
         MVI   5(R5),C','            BUMP UP OPTIONS FIELD FOR                  
         LA    R5,6(R5)              POSSIBLE PRATE                             
*                                                                               
DR135    OC    PRATE,PRATE                                                      
         BZ    DR138                                                            
         MVC   0(5,R5),=C'RATE='     COPY PRATE INTO OPTIONS FIELD              
         LA    R5,5(R5)                                                         
         MVC   0(1,R5),PRATE                                                    
         MVI   1(R5),C','                                                       
         LA    R5,2(R5)                                                         
*                                                                               
DR138    TM    POPT1,POPT1_NOBILL                                               
         BNO   DR138A                                                           
         MVC   0(7,R5),=C'BILL=NO'                                              
         LA    R5,7(R5)                                                         
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
DR138A   DS    0H                                                               
         CLI   PCPPRS,C'N'                                                      
         BNE   DR140                                                            
         MVC   0(7,R5),=C'CPPRS=N'                                              
         LA    R5,7(R5)                                                         
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
DR140    TM    POPT1,POPT1_THTR                                                 
         BNO   DR145                                                            
         MVC   0(4,R5),=C'THTR'                                                 
         LA    R5,4(R5)                                                         
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
DR145    TM    POPT1,POPT1_THNT                                                 
         BNO   *+14                                                             
         MVC   0(4,R5),=C'THNT'                                                 
         B     DR500                                                            
*                                                                               
         BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
*                                                                               
DR500    OI    PRDOPTNH+6,X'80'      TRANSMIT OPTIONS FIELD                     
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
*                                                                               
DK       L     R4,AIO                                                           
         USING PKEY,R4                                                          
         MVC   BYTE,PKEYAM           ISOLATE MEDIA CODE                         
         NI    BYTE,X'0F'                                                       
         LARL  R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   PRDMEDK,0(R5)                                                    
         OI    PRDMEDKH+6,X'80'                                                 
         MVI   PRDMEDKH+5,1          TRANSMIT MEDIA CODE TO SCREEN              
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,PKEYCLT),PRDCLIK                          
         OI    PRDCLIKH+6,X'80'                                                 
         MVI   PRDCLIKH+5,3          TRANSMIT CLIENT CODE TO SCREEN             
*                                                                               
***********************************************************************         
*                                                                               
         MVC   PRDPROK,PKEYPRD                                                  
         MVI   PRDPROKH+5,3                                                     
         OI    PRDPROKH+6,X'80'      TRANSMIT PRODUCT CODE TO SCREEN            
*                                                                               
         CLI   THISLSEL,C'C'         SELECT FOR CHANGE                          
         BNE   DKX                                                              
*                                                                               
         CLI   T217FFD+1,C'*'        TEST DDS TERM                              
         BE    DKX                                                              
         TM    T217FFD+12,X'20'                                                 
         BO    ERRSEC2               ON = NO CHANGE                             
*                                                                               
DKX      B     VK                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DELETE PRODUCT RECORDS                                       *         
***********************************************************************         
*                                                                               
DEL      CLC   AGENCY,=C'CK'       NO DELETES FOR YOU                           
         BNE   DEL1                                                             
         LA    R2,CONACTH          POINT TO ACTION                              
         B     ERRINV                                                           
*                                                                               
DEL1     MVC   KEY,PROKEY            READ CLIENT RECORD INTO AIO2               
         XC    KEY+4(3),KEY+4                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AIO2                                                          
         USING CLTHDR,R5                                                        
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,PRDPRONH                                                      
         CLC   QPRD,=C'POL'                                                     
         BNE   DEL10                                                            
*                                                                               
         MVC   ERRNUM,=AL2(DELERR8)  POL PRODUCT CAN ONLY BE DELETED            
         CLC   CLIST(3),=C'POL'      IF IT'S THE ONLY PRODUCT IN THE            
         BE    DEL10                 CLIENT'S CLIST                             
         B     SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
DEL10    MVC   ERRNUM,=AL2(DELERR9)  USER MUST INPUT 'DELETE' IN                
         CLC   8(6,R2),=C'DELETE'    PRODUCT NAME FIELD                         
         BNE   SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
         USING PRDHDR,R6                                                        
         MVC   AIO,AIO1              PRODUCT CANNOT BE DELETED IF IT'S          
         L     R6,AIO                IN A PRODUCT GROUP                         
*                                                                               
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'       RECORD TYPE PRODUCT GROUP                  
         MVC   KEY+2(1),BAGYMD       AGENCY/MEDIA ...                           
         MVC   KEY+3(2),BCLT         CLIENT                                     
         GOTO1 HIGH                                                             
*                                                                               
DEL20    CLC   KEY(5),KEYSAVE                                                   
         BNE   DEL30                                                            
         CLC   KEY+8(3),QPRD         PRODUCT GROUP W/ SAME BASE KEY             
         BNE   DEL25                 (A/M,CLT) FOUND, IF PRODUCT CODE           
         MVC   ERRNUM,=AL2(DELERR1)  ALSO MATCHES THEN PRODUCT CANNOT           
* AS PER WHOA: THE FOLLOWING INSTRUCTION CANNOT WORK ANYWAY                     
*NOP     MVC   CONHEADH+32,KEY+5     BE DELETED                                 
         B     SPERREX                                                          
DEL25    GOTO1 SEQ                   LOOP THROUGH ALL PRODUCTS FOR              
         B     DEL20                 PRODUCT GROUP                              
*                                                                               
***********************************************************************         
*                                                                               
DEL30    LA    R2,PRDCLINH                                                      
         MVC   ERRNUM,=AL2(DELERR2)  DELETE NOT ALLOWED IF CLIENT HAS           
         OC    CMCLTCOD,CMCLTCOD     MASTER TRAFFIC REFERENCE                   
         BZ    DEL35                                                            
         B     SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
DEL35    LA    R2,PRDPRONH                                                      
         MVC   KEY,PROKEY            MOVE X'01' TO THE END OF PROKEY            
         MVI   KEY+7,X'01'           TO READ FIRST ESTIMATE OR BILL             
         GOTO1 HIGH                  RECORD                                     
         B     DEL50                                                            
*                                                                               
DEL40    GOTO1 SEQ                   READ EST. OR BILL RECORD AFTER             
*                                    THE FIRST                                  
*                                                                               
DEL50    CLC   KEYSAVE(7),KEY        IF NO MORE ESTIMATE OR BILL                
         BNE   DEL110                RECORDS FOUND BRANCH TO DEL110             
         CLI   KEY+8,0               IF BILL RECORD BRANCH TO DEL90             
         BNE   DEL90                                                            
*                                                                               
***********************************************************************         
*                                                                               
E        USING ESTHDR,R4                                                        
DEL60    MVC   AIO,AIO3              ESTIMATE RECORD EXISTS ... READ IT         
         L     R4,AIO                INTO AIO3                                  
         GOTO1 GETREC                                                           
*                                                                               
         MVC   ERRNUM,=AL2(DELERR4)                                             
         LA    R1,26               CAN'T DELETE IF EST ORD OR PAID $            
         LA    R5,E.EORD                                                        
         CP    0(6,R5),=PL6'0'                                                  
         BNE   SPERREX             CHECK IF OK                                  
         LA    R5,6(R5)                                                         
         BCT   R1,*-14                                                          
*                                                                               
         LA    R1,26                                                            
         LA    R5,E.EPAID                                                       
         CP    0(6,R5),=PL6'0'                                                  
         BNE   SPERREX             CHECK IF OK                                  
         LA    R5,6(R5)                                                         
         BCT   R1,*-14                                                          
*                                                                               
         MVC   ERRNUM,=AL2(1261)                                                
         TM    E.EFLAG1,EF1SDE     SUPERDESK AUTHORIZATION OPEN?                
         BNZ   SPERREX                                                          
*                                                                               
         B     DEL40                                                            
         DROP  E                                                                
*                                                                               
***********************************************************************         
*                                                                               
DEL90    MVC   ERRNUM,=AL2(DELERR5)  BILL RECORD EXISTS ... PRODUCT             
         B     SPERREX               CANNOT BE DELETED                          
*                                                                               
***********************************************************************         
*                                                                               
DEL110   XC    KEY,KEY                                                          
         MVI   KEY,X'02'             RECORD TYPE GOAL                           
         MVC   KEY+1(3),KEYSAVE+1    AGENCY/MEDIA & CLIENT                      
         MVC   KEY+4(1),BPRD         PRODUCT CODE                               
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE        IF GOAL RECORD EXISTS FOR PRODUCT          
         BNE   DEL120                CANNOT DELETE                              
         MVC   ERRNUM,=AL2(DELERR6)                                             
         B     SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
DEL120   XC    KEY,KEY               RECORD TYPE BUY                            
         MVC   KEY(4),KEYSAVE+1      AGENCY/MEDIA & CLIENT                      
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE        IF BUY RECORD EXISTS FOR PRODUCT           
         BNE   DEL130                CANNOT DELETE                              
         MVC   ERRNUM,=AL2(DELERR7)                                             
         B     SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
DEL130   CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY, MEDIA TV ...           
         BNE   DEL150                                                           
         CLI   QMED,C'T'                                                        
         BNE   DEL150                                                           
*                                                                               
         MVC   WORK(1),KEYSAVE                                                  
         NI    WORK,X'0F'                                                       
         CLI   WORK,X'08'                                                       
         BE    DEL150                                                           
         CLI   WORK,X'03'                                                       
         BNE   DEL140                                                           
*                                                                               
         MVC   KEY,PROKEY            READ IN PRODUCT RECORD FOR                 
         MVI   KEY+7,X'01'           MEDIA C(08) ... TEST FOR EST,              
         NI    KEY+1,X'F0'           BILL, GOAL AND BUY RECORDS FOR             
         OI    KEY+1,X'08'           MEDIA C(08)                                
         GOTO1 HIGH                                                             
         B     DEL50                                                            
*                                                                               
DEL140   MVC   KEY,PROKEY            READ IN PRODUCT RECORD FOR                 
         MVI   KEY+7,X'01'           MEDIA N(03) ... TEST FOR EST,              
         NI    KEY+1,X'F0'           BILL, GOAL AND BUY RECORDS FOR             
         OI    KEY+1,X'03'           MEDIA N(03)                                
         GOTO1 HIGH                                                             
         B     DEL50                                                            
*                                                                               
***********************************************************************         
*                                                                               
DEL150   XC    KEY,KEY               GET CLIENT RECORD TO PREPARE               
         MVC   KEY(4),PROKEY         FOR PUTREC                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AIO2                                                          
         LA    R4,CLIST              FIND PRODUCT RECORD IN CLIST               
DEL160   CLC   0(3,R4),PROKEY+4                                                 
         BE    DEL170                                                           
         LA    R4,4(R4)                                                         
         CLI   3(R4),0                                                          
         BNE   DEL160                                                           
         DC    H'0'                  IF NOT FOUND - FATAL ERROR                 
*                                                                               
DEL170   MVC   0(4,R4),4(R4)         BUMP PRODUCT OUT OF CLIST AND              
         LA    R4,4(R4)              WRITE BACK REVISED CLIENT RECORD           
         CLI   3(R4),0                                                          
         BNE   DEL170                                                           
         BRAS  RE,PTREC                                                         
*                                                                               
***********************************************************************         
*                                                                               
         CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY, MEDIA TV ...           
         BNE   DEL220                                                           
         CLI   QMED,C'T'                                                        
         BNE   DEL220                                                           
*                                                                               
         XC    KEY,KEY               ATTEMPT TO READ IN CLIENT RECORD           
         MVC   KEY(4),PROKEY         FOR MEDIA N(03)                            
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DEL180                                                           
*                                                                               
         L     R5,AIO2               IF CLIENT RECORD FOR MEDIA N(03)           
         MVC   0(13,R5),KEYSAVE      NOT FOUND ... ADD IT                       
         MVC   KEY(13),KEYSAVE                                                  
         MVC   AIO,AIO2                                                         
         BRAS  RE,ADREC                                                         
         B     DEL190                                                           
*                                                                               
DEL180   MVC   AIO,AIO3              CLIENT RECORD FOR MEDIA N(03)              
         GOTO1 GETREC                FOUND ...                                  
         L     R2,AIO2                                                          
         MVC   0(13,R2),KEY          COPY KEYS                                  
         MVC   AIO,AIO2                                                         
         BRAS  RE,PTREC                                                         
*                                                                               
*                                                                               
DEL190   XC    KEY,KEY               ATTEMPT TO READ IN CLIENT RECORD           
         MVC   KEY(4),PROKEY         FOR MEDIA C(08)                            
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DEL200                                                           
*                                                                               
         L     R2,AIO2               IF CLIENT RECORD FOR MEDIA C(08)           
         MVC   0(13,R2),KEYSAVE      NOT FOUND ... ADD IT                       
         MVC   KEY(13),KEYSAVE                                                  
         BRAS  RE,ADREC                                                         
         B     DEL220                                                           
*                                                                               
DEL200   MVC   AIO,AIO3              CLIENT RECORD FOR MEDIA C(08)              
         GOTO1 GETREC                FOUND ...                                  
         L     R2,AIO2                                                          
         MVC   0(13,R2),KEY          COPY KEYS                                  
         MVC   AIO,AIO2                                                         
         BRAS  RE,PTREC                                                         
*                                                                               
***********************************************************************         
*                                                                               
DEL220   MVC   KEY,PROKEY            VALIDATION COMPLETE - OK TO DELETE         
DEL230   GOTO1 HIGH                  PRODUCT - READ IT BACK IN                  
         B     DEL250                                                           
*                                                                               
DEL240   GOTO1 SEQ                   READ PRODUCT'S ESTIMATE RECORDS            
DEL250   CLC   KEYSAVE(7),KEY                                                   
         BNE   DEL290                                                           
*                                                                               
         CLI   KEY+EKEYEST-EKEY,0    HAVE ESTIMATE RECORD?                      
         BE    *+8                    NO, SKIP THIS                             
         BRAS  RE,DLESTPAS           DELETE ESTIMATE PASSIVE                    
*                                                                               
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AIO,AIO3              GET PRODUCT OR ESTIMATE RECORD             
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         OI    15(RE),X'80'          MARK RECORD DELETED                        
         BRAS  RE,PTREC                                                         
*                                                                               
         B     DEL240                                                           
***********************************************************************         
*                                                                               
DEL290   CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY, MEDIA TV ...           
         BNE   DELX                                                             
         CLI   QMED,C'T'                                                        
         BNE   DELX                                                             
*                                                                               
         MVC   WORK(1),KEYSAVE+1                                                
         NI    WORK,X'0F'                                                       
         CLI   WORK,X'08'                                                       
         BE    DELX                                                             
         CLI   WORK,X'03'                                                       
         BNE   DEL300                                                           
*                                                                               
         MVC   KEY,PROKEY            MARK PRODUCT RECORD FOR MEDIA              
         NI    KEY+1,X'F0'           C(08) AND IT'S ESTIMATE RECORDS            
         OI    KEY+1,X'08'           DELETED                                    
         B     DEL230                                                           
*                                                                               
DEL300   MVC   KEY,PROKEY            MARK PRODUCT RECORD FOR MEDIA              
         NI    KEY+1,X'F0'           N(03) AND IT'S ESTIMATE RECORDS            
         OI    KEY+1,X'03'           DELETED                                    
         B     DEL230                                                           
*                                                                               
***********************************************************************         
*                                                                               
DELX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        TAKE CARE OF PASSIVE POINTERS                                *         
***********************************************************************         
PP       DS    0H                                                               
         BRAS  RE,OFCPTR                                                        
         B     XIT                                                              
***********************************************************************         
*        VALIDATE DO IT YOURSELF TRADE DIY                            *         
*        - CASH PRD MUST EXIST AND MAX OF 127 NON TRADE PRODUCTS      *         
***********************************************************************         
VALDIY   NTR1                                                                   
         MVC   WORK(3),8(R2)                                                    
         MVI   WORK+2,C'C'         LOOK FOR CASH PRD ENDING IN C                
*                                                                               
         LA    R3,SVCLIST                                                       
VDIY10   CLI   0(R3),0                                                          
         BE    ERRINV              NO CASH PRODUCT FOUND                        
         CLC   0(3,R3),WORK        MATCH                                        
         BE    VDIY20                                                           
         LA    R3,4(R3)                                                         
         B     VDIY10                                                           
VDIY20   MVC   SVPRDNUM,3(R3)      SAVE HEX PRD NUMBER                          
         OI    SVPRDNUM,X'80'      TURN ON TRADE BIT                            
*                                                                               
         XR    R1,R1                                                            
         LA    R3,SVCLIST          NOT MORE THAN 127 NON TRADE PRDS             
VDIY30   CLI   0(R3),0                                                          
         BE    VDIY40                                                           
         TM    3(R3),X'80'         SKIP TRADE                                   
         BO    *+8                                                              
         LA    R1,1(R1)            +1                                           
         LA    R3,4(R3)                                                         
         B     VDIY30                                                           
VDIY40   CHI   R1,127                                                           
         BH    ERRINV              TOO MANY NON TRADE PRDS                      
*                                                                               
VDIYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        EDTUSR                                                       *         
***********************************************************************         
EDTUSR   NTR1                                                                   
         L     R3,AUSR               ANY INPUT IN USER FIELD?                   
         CLI   5(R3),0                                                          
         BNE   EDTUSR10                                                         
*                                                                               
         TM    FLAG1,CFLGREQQ        NO INPUT ... WAS IT REQUIRED?              
         BZ    XIT                   IF NOT, EXIT                               
         B     ERRMIS                IF YES, ERROR                              
*                                                                               
EDTUSR10 MVC   ERRNUM,=AL2(TOOBIG)                                              
         CLC   LEN,5(R3)             CHECK LENGTH OF INPUT                      
         BL    SPERREX                                                          
*                                                                               
         CLI   UTYPE,C' '            IS TYPE SUPPOSED TO BE 'WILD'?             
         BNH   EDTUSR80                                                         
*                                                                               
         CLI   UTYPE,C'C'            IF TYPE IS CHARACTER...                    
         BNE   EDTUSR60              INPUT CANNOT BE NUMERIC                    
         LA    R4,8(R3)                                                         
         LLC   R1,5(R3)                                                         
EDTUSR40 MVC   ERRNUM,=AL2(EDTERR1)                                             
         CLI   0(R4),C'0'                                                       
         BL    EDTUSR50                                                         
         CLI   0(R4),C'9'                                                       
         BNH   SPERREX                                                          
EDTUSR50 LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'            IF TYPE IS NUMERIC...                      
         BNE   EDTUSR70                                                         
         BAS   RE,CHKNTYP            INPUT MUST BE ALL NUMERIC                  
         BE    EDTUSR80                                                         
         MVC   ERRNUM,=AL2(EDTERR2)                                             
         B     SPERREX                                                          
*                                                                               
EDTUSR70 MVC   ERRNUM,=AL2(BADDATE)                                             
         CLI   UTYPE,C'D'            IS TYPE DATE...                            
         BE    *+6                                                              
         DC    H'0'                  IF NO, BAD TYPE                            
         GOTO1 DATVAL,DMCB,(0,8(R3)),WORK                                       
         OC    DMCB(4),DMCB          INPUT MUST BE VALID DATE                   
         BZ    SPERREX                                                          
         L     R1,0(R1)                                                         
         LLC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   SPERREX                                                          
*                                                                               
EDTUSR80 LLC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)     MOVE INPUT INTO USERDATA                   
         J     XIT                                                              
***********************************************************************         
*        CHKNTYP                                                      *         
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF PASSED VARIABLE IS VALID NUMERIC                
***********************************************************************         
CHKNTYP  NTR1                                                                   
         LA    R4,8(R3)              R4 ---> INPUT                              
         LLC   R1,5(R3)              R1= L(INPUT)                               
CHKN10   LARL  R5,VALDNTBL           R5 = TABLE OF VALID DIGITS                 
CHKN20   CLC   0(1,R4),0(R5)         VALID DIGIT?                               
         BE    CHKN30                                                           
         LA    R5,1(R5)              BUMP TO NEXT DIGIT                         
         CLI   0(R5),X'FF'           END OF TABLE?                              
         BE    XCHKN                                                            
         B     CHKN20                NO, TRY AGAIN                              
*                                                                               
CHKN30   LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,CHKN10                                                        
XCHKN    LTR   R1,R1                                                            
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        TESTTRAD                                                     *         
***********************************************************************         
* THIS ROUTINE CHECKS IF CLIENT IS TRADE AND IF SO ADDS THE 'TRADE'             
* TYPE PRODUCT CODE AND NUMBER TO THE CLIENT RECORD TABLE.                      
***********************************************************************         
TESTTRAD NTR1                                                                   
         USING PRDHDR,R6                                                        
         L     R6,AIO1                                                          
         TM    SVAGYFL1,X'02'        CLIENT MUST BE EITHER A TRADE              
         BO    TTRD10                AGENCY OR TRADE CLIENT OR SUB-             
         TM    SVCLOP2,COP2TRAD      ROUTINE EXITS                              
         BZ    TTRDX                                                            
TTRD10   CLI   QPRD+2,C'C'           AND MUST BE A CASH PRODUCT                 
         BE    TTRD20                                                           
         MVC   ERRNUM,=AL2(ERRTRAD2)                                            
         B     SPERREX                                                          
*                                                                               
TTRD20   MVC   ERRNUM,=AL2(CLTFULL)  TOO MANY PRODUCTS ERROR CODE               
         CLI   PCODE+1,104           MORE THAN 104 PRODUCTS?                    
         BH    SPERREX               YES - SO ERROR                             
*                                                                               
         L     R5,AIO2                                                          
         USING CLTHDR,R5                                                        
         L     R4,WORK               NO - STORE OLD PRD CODE AND #              
         MVI   WORK+2,C'#'           SET THE TRADE PROD CODE                    
         OI    WORK+3,X'80'          TURN ON THE TRADE BIT IN PRD #             
         LLC   R3,CCOUNT                                                        
         GOTO1 BINSRCH,DMCB,(X'01',WORK),CLIST,(R3),4,(0,3),218                 
*                                                                               
         MVC   CCOUNT,DMCB+11        NEW COUNT OF PRODUCTS IN TABLE             
         CLI   DMCB,X'01'            WAS PRODUCT CODE ALREADY THERE?            
         BE    TTRD30                                                           
         MVC   ERRNUM,=AL2(ERRTRADE) IF YES - RETURN ERROR                      
         B     SPERREX                                                          
*                                                                               
TTRD30   ST    R4,WORK               RESTORE CASH PRODUCT CODE/#                
         XR    R0,R0                 SET GOOD CC                                
TTRDX    J     XIT                                                              
         DROP  R5,R6                                                            
***********************************************************************         
*        GENREQ                                                       *         
***********************************************************************         
GENREQ   NTR1                                                                   
         MVC   AIO,AIO3               BUILD REQUEST RECORD AT AIO3              
         L     R1,AIO                                                           
         XC    0(150,R1),0(R1)                                                  
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,26(R1)                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),QMED                                                     
         MVC   5(3,R1),QCLT                                                     
         MVC   11(3,R1),QPRD                                                    
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'P'                                                      
         MVI   63(R1),C'A'                                                      
*                                                                               
         CLI   ACTEQU,ACTADD         IF ACTION = ADD, GENERATE                  
         BE    *+8                   REQUEST RECORD                             
         MVI   63(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERROFC   MVC   ERRNUM,=AL2(INVOFC)                                              
         B     SPERREX                                                          
ERR1OFC  MVC   ERRNUM,=AL2(OFCONE)                                              
         B     SPERREX                                                          
ERR2OFC  MVC   ERRNUM,=AL2(OFCTWO)                                              
         B     SPERREX                                                          
ERRAAGY  MVC   ERRNUM,=AL2(INVAAGY)                                             
         B     SPERREX                                                          
ERRSYS   MVC   ERRNUM,=AL2(SYSNOPEN)                                            
         B     SPERREX                                                          
ERRSWS   MVC   ERRNUM,=AL2(SYSWS)                                               
         B     SPERREX                                                          
ERRCMP   MVC   ERRNUM,=AL2(NOACCCMP)                                            
         B     SPERREX                                                          
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
PSTERRR  MVC   ERRNUM,=AL2(PSTERR1)                                             
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
INVALDCR MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     GNCURSER                                                         
*                                                                               
SPCURSER OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM      ERROR MESSAGE NUMBER                         
         MVI   GTMTYP,GTMERR       ERROR TYPE                                   
         MVI   GTMSYS,2            SPOT SYSTEM                                  
         DROP  RF                                                               
GNCURSER GOTO1 CURSERR                                                          
*                                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
NOTAUTH  EQU   175                 NOT AUTHRORIZED FOR THIS FUNCTION            
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
GMIERR   EQU   486                 GMI NOT SET UP FOR CLIENT                    
BIGERR   EQU   487                 OTHER AGENCY FEE TOO LARGE                   
TALERR   EQU   488                 NTP NOT VALID FOR PRODUCT                    
TALREQ   EQU   489                 NTP REQUIRED FOR THIS CLIENT                 
CLTFULL  EQU   485                 TOO MANY PRODUCTS                            
PRDERR   EQU   491                 PRD ERROR CHECK TRAFFIC MASTER PRD           
SCRTYERR EQU   492                 ACCESS TO CLIENT NOT AUTHORIZED              
ERRTRADE EQU   493                 TRADE PRD CODE ALREAD IN CLIST               
ERRTRAD2 EQU   494                 THIRD CHAR IN PRD CODE MUST BE 'C'           
CLNTERR  EQU   40                  CLIENT NOT FOUND                             
PRODERR  EQU   41                  PRODUCT NOT FOUND                            
NTPERR   EQU   495                 NTP MUST FIT BETWEEN 0 AND 3                 
BADDATE  EQU   20                  INVALID DATE                                 
PNREQER  EQU   496                 PRD NAME REQUIRED                            
PNDELER  EQU   497                 PRD NAME CANNOT BE DELETE                    
CPERR1   EQU   498                 CLT/PRD CODE REQUIRED                        
CPERR2   EQU   499                 CLT/PRD CODE MUST BE 4 ALPHANUMER            
CPERR3   EQU   503                 CLT/PRD CODE MUST BE 4 NUMERICS              
CPERR4   EQU   505                 CLT/PRD CODE MUST BE 5 NUMERICS              
PCLERR1  EQU   506                 CLASSES MUST BE A - I                        
PCLERR2  EQU   507                 CLASSES CANNOT BE EQUAL                      
BTNERR   EQU   508                 BILL-TO-NAME REQUIRED                        
OAFERR   EQU   509                 OAF CANNOT EXCEED 9.99                       
BBERR    EQU   510                 BILL-BASIS IS CGROSS OR CNET                 
COMPERR1 EQU   511                 COM % REQUIRED                               
COMPERR2 EQU   515                 COM % VALID NUMERIC                          
COMPERR3 EQU   512                 100% IS MAX COM %                            
COMPERR4 EQU   513                 0% IS MIN COM %                              
COMPERR5 EQU   514                 1ST CHAR IS + OR -                           
CBASERR1 EQU   522                 COMM BASIS REQUIRED                          
CBASERR2 EQU   521                 COMM BASIS GROSS OR NET                      
EDERR1   EQU   542                 DATE REQUIRES BILL BASIS                     
EDERR2   EQU   523                 DATE REQUIRED FOR BILL BASIS                 
GSTERR   EQU   524                 INVALID GST CODE                             
PSTERR1  EQU   528                 INVALID PST CODE                             
EDTERR1  EQU   530                 INPUT CANNOT BE NUMERIC                      
EDTERR2  EQU   529                 INPUT MUST BE NUMERIC                        
OPTERR1  EQU   531                 OPTION MUST BE NTP                           
OPTERR2  EQU   532                 NTP MUST BE 0-3                              
VKERR1   EQU   533                 MUST BE 3 LONG                               
VKERR2   EQU   534                 MUST BE VALID HEX                            
VKERR3   EQU   535                 ZZZ INVALID PRD CODE                         
VKERR4   EQU   536                 CLIENT MUST BE 'CC'                          
VKERR5   EQU   537                 1ST CHAR MUST BE ALPHA                       
VKERR6   EQU   538                 MUST BE 2 OR 3 CHARS LONG                    
VKERR7   EQU   539                 2ND AND 3RD CHARS ALPHANUMERIC               
VKERR8   EQU   540                 ALL IS INVALID PRD CODE                      
VKERR9   EQU   541                 NO IS INVALID PRD CODE                       
TALERR2  EQU   546                 CANNOT CHANGE FIELD                          
NTPERR3  EQU   547                 NTP CAN ONLY BE SET ONCE                     
DELERR1  EQU   548                 PRODUCT EXISTS FOR GROUP IN ID               
DELERR2  EQU   549                 MASTER TRAFFIC CLT ALREADY EXISTS            
DELERR3  EQU   550                 BILL RECS SHOULD SUM TO ZERO                 
DELERR4  EQU   551                 ORDERED OR PAID $ ON EST                     
DELERR5  EQU   552                 BILL ON FILE- CANNOT DEL PRD                 
DELERR6  EQU   553                 NO GOALS-- GO CHECK FOR BUYS                 
DELERR7  EQU   554                 HAS BUYS, CANNOT DELETE                      
DELERR8  EQU   555                 CANNOT DELETE POL PRODUCT                    
DELERR9  EQU   560                 'DELETE' MUST BE IN PROD NAME                
PFERR    EQU   559                 INVALID PFKEY                                
RATERR1  EQU   598                 RATE SET ONLY ONCE                           
RATERR2  EQU   599                 RATE NOT ALLOWED                             
RATERR3  EQU   639                 RATES: *, 0-9                                
TOOBIG   EQU   678                 USER FIELD TOO LONG                          
BILLER1  EQU   783                 VALID OPTIONS ARE BILL=YES OR NO             
INVOFC   EQU   544                 INVALID OFFICE CODE #                        
INVAAGY  EQU   545                 INVALID ACCOUNT AGENCY CODE                  
INVOPT   EQU   603                 INVALID OPTION                               
CPERR5   EQU   829                 CLT/PRD CODE MUST BE 3 OR 4 ALPHNUM          
INVCPPRS EQU   840                 INVALID OPTION                               
OFCONE   EQU   1068                ONE CHAR OFFICE CODE REQUIRED                
OFCTWO   EQU   1069                TWO CHAR OFFICE CODE REQUIRED                
SYSNOPEN EQU   1071                ACC SYSTEM NOT OPEN                          
SYSWS    EQU   1072                CAN'T SWITCH TO ACC SYSTEM                   
NOACCCMP EQU   1073                UNABLE TO READ ACC COMPANY REC               
NOUNAPRD EQU   1303                PRODUCT CODE 'ZZZ' NOT ALLOWED               
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
***********************************************************************         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
***********************************************************************         
*        VALID NUMERICS TABLE                                         *         
***********************************************************************         
VALDNTBL DC    C' 0123456789-/'                                                 
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        AGENCY TABLE                                                 *         
***********************************************************************         
         DS    0H                                                               
AGYTAB   DC   C'GY'                                                             
         DC   C'DR'                                                             
         DC   C'GN'                                                             
         DC   C'CE'                                                             
         DC   C'FM'                                                             
         DC   C'RE'                                                             
         DC   X'FF'                                                             
*                                                                               
***********************************************************************         
*        GST TABLE                                                    *         
***********************************************************************         
         DS    0H                                                               
GSTTAB   DC   C'S'                                                              
         DC   C'U'                                                              
         DC   C'X'                                                              
         DC   C'Z'                                                              
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
         DS    0H                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*        ONLY BASE=* SUBROUTINES AFTER THIS POINT                               
***********************************************************************         
         DROP  R7,RB                                                            
***********************************************************************         
* NOW DELETE PASSIVE KEY(S) IF IT EXISTS                                        
***********************************************************************         
DLESTPAS NTR1  BASE=*,LABEL=*                                                   
         LAY   R2,MEDTAB                                                        
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO3             GET ESTIMATE RECORD                         
         GOTO1 GETREC               EST RECORD IN AIO3                          
*                                                                               
DLEP005  L     R6,AIO3                                                          
         USING ESTHDR,R6                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
EP       USING EPKEY,R3                                                         
         MVI   EP.EPKEYTYP,EPKEYTYQ X'0D'                                       
         MVI   EP.EPKEYSUB,EPKEYSBQ X'F2'                                       
         MVC   EP.EPKEYAM,EKEYAM    A/M                                         
*                                                                               
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         JNE   DLEP010              NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         JNE   DLEP010              NO, ONLY CHANGE 1 RECORD                    
         NI    EP.EPKEYAM,X'F0'        TURN OFF MEDIA BIT                       
         OC    EP.EPKEYAM,1(R2)        USE THIS MEDIA                           
*                                                                               
DLEP010  MVC   EP.EPKEYCLT,EKEYCLT  CLT                                         
         GOTOR DATCON,DMCB,(0,ESTART),(2,EP.EPKEYSDT)                           
         GOTOR DATCON,DMCB,(0,EEND),(2,EP.EPKEYEDT)                             
         MVC   EP.EPKEYEST,EKEYEST  EST                                         
         MVC   EP.EPKEYPRD,EKEYPRD  PRD                                         
         GOTOR HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND?                                      
         JNE   DLEP020              NO                                          
         OI    KEY+13,X'80'         MARK RECORD FOR DELETION                    
         GOTOR WRITE                                                            
*                                                                               
DLEP020  CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         JNE   DLEP040              NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         JNE   DLEP040              NO, ONLY CHANGE 1 RECORD                    
*                                                                               
DLEP030  LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         JE    DLEP040              YES                                         
         CLI   SVCXTRA+8,C'P'       TEST P&G                                    
         JE    *+12                 YES - NO MEDIA N                            
         CLI   0(R2),C'N'           PROCESS MEDIA N?                            
         JE    DLEP005              YES                                         
         CLI   0(R2),C'C'           PROCESS MEDIA C?                            
         JE    DLEP005              YES                                         
         J     DLEP030                                                          
         DROP  EP                                                               
*                                                                               
DLEP040  MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTOR HIGH                RESTORE READ SEQUENCE                        
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPPST                                                      *         
***********************************************************************         
DISPPST  NTR1  BASE=*,LABEL=*                                                   
         EX    R3,*+8                EXECUTE                                    
         B     *+10                                                             
         MVC   8(0,R2),SPACES        CLEAR PST CODE ON SCREEN                   
         OI    6(R2),X'80'           TRANSMIT                                   
*                                                                               
         OC    WORK(10),WORK         ANY PST CODE TO DISPLAY?                   
         BZ    DPX                   IF NO, SEND BLANK FIELD TO SCREEN          
*                                                                               
         LA    R4,ELEM               IF YES, USE PROVINCIAL TAX VALID-          
         USING PSTBLKD,R4            ATION BLOCK - SET UP PARAMETERS            
         XC    ELEM,ELEM             FOR CALL TO PSTVAL                         
         XC    PSTOUT,PSTOUT                                                    
         MVI   PSTACT,PSTFMTQ        ACTION = FORMAT                            
         LA    R1,WORK                                                          
         ST    R1,PSTADIN            INPUT = WORK                               
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT           OUTPUT = PSTADOUT                          
         MVI   DMCB+7,QPSTVAL                                                   
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
*                                                                               
         EX    R3,*+8                EXECUTE                                    
         B     *+10                                                             
         MVC   8(0,R2),PSTOUT        COPY VALIDATED PST CODES TO SCREEN         
DPX      J     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
*        VALPST                                                       *         
***********************************************************************         
VALPST   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4            SETUP PARAMETERS FOR PSTVAL CALL           
         XC    ELEM,ELEM             (VALIDATE PST CODES)                       
         XC    PSTOUT,PSTOUT                                                    
         MVI   PSTACT,PSTVALQ        ACTION = VALIDIATE                         
         ST    R2,PSTADIN            INPUT = PST SCREEN FIELD                   
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT           OUTPUT = PSTADOUT                          
         MVC   PSTACOM,ACOMFACS                                                 
         MVI   DMCB+7,QPSTVAL                                                   
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB               VALID PST CODES RETURNED IN PPST           
         GOTO1 (RF),DMCB,(R4)                                                   
         CLI   PSTERR,0              ANY ERRORS?                                
         JNE   PSTERRR               YES - GIVE ERROR MESSAGE                   
*                                                                               
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD OR DELETE OF PASSIVE OFFICE POINTERS                                      
***********************************************************************         
OFCPTR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDR,R6                                                        
*                                                                               
OFCPTR0  CLC   SVOFFICE,POFFICE    TEST OFFICE CHANGED                          
         BE    OFCPTRX             NO                                           
********                                                                        
* DELETE OLD OFFICE (IF ANY)                                                    
********                                                                        
         CLI   SVOFFICE,C' '                                                    
         BNH   OFCPTR10                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DF0'                                                  
         MVC   KEY+2(1),PROKEY+1      A/M                                       
         MVC   KEY+3(2),PROKEY+2      CLT                                       
         MVC   KEY+5(1),SVOFFICE                                                
         MVC   KEY+6(3),PROKEY+4      PRODUCT CODE                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR2                                                          
         OI    KEY+13,X'80'      MARK RECORD FOR DELETION                       
         GOTO1 WRITE                                                            
*                                                                               
OFCPTR2  CLI   SVAPROF+7,C'C'    CANADIAN AGENCY                                
         BNE   OFCPTR10                                                         
         CLI   PRDMEDK,C'T'       TV ONLY                                       
         BNE   OFCPTR10                                                         
********                                                                        
* IF CANADIAN CLIENT, DELETE PASSIVE POINTERS FOR MEDIA N(X'03')                
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR4                                                          
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
********                                                                        
* AND FOR MEDIA C (X'08')                                                       
OFCPTR4  NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR10                                                         
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
********                                                                        
* NOW ADD NEW (OR UNDELETE) PASSIVE KEY                                         
********                                                                        
OFCPTR10 CLI   POFFICE,C' '        TEST NEW OFFICE PRESENT                      
         BNH   OFCPTRX             IF NO NEW OFFICE, NO NEED TO ADD             
         MVC   KEY(20),PROKEY                                                   
*                                                                               
         GOTO1 HIGH                READ PROD REC TO GET DISK ADDRESS            
*                                                                               
         GOTO1 =A(ADDOFC),RR=RELO                                               
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   OFCPTRX                                                          
         CLI   QMED,C'T'                                                        
         BNE   OFCPTRX                                                          
********                                                                        
* DO MEDIA N                                                                    
********                                                                        
         MVC   KEY(20),PROKEY                                                   
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(ADDOFC),RR=RELO                                               
********                                                                        
* DO MEDIA C                                                                    
********                                                                        
         MVC   KEY(20),PROKEY                                                   
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(ADDOFC),RR=RELO                                               
*                                                                               
OFCPTRX  NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
***********************************************************************         
ADDOFC   NTR1  BASE=*,LABEL=*                                                   
***********************************************************************         
         MVC   WORK(20),KEY        SAVE PRODUCT KEY AND DISK ADDRESS            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DF0'                                                  
         MVC   KEY+2(1),WORK+1     A/M                                          
         MVC   KEY+3(2),WORK+2     CLT                                          
         MVC   KEY+5(1),POFFICE    OFFICE CODE                                  
         MVC   KEY+6(3),WORK+4     PRODUCT CODE                                 
         MVC   KEY+14(4),WORK+14   SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ADDOFC10                                                         
         NI    KEY+13,X'7F'                                                     
         GOTO1 WRITE                                                            
         B     ADDOFCX                                                          
*                                                                               
ADDOFC10 MVC   KEY,KEYSAVE     RESTORE KEY                                      
         GOTO1 ADD                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDOFCX  DS    0H                                                               
         DROP  R6                                                               
         J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
VALACC   NTR1  BASE=*,LABEL=*      *VALIDATE AGENCY CODE*                       
         LA    R2,PRDAOFCH                                                      
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    *+12                ALWAYS VALIDATE ON ADD                       
*        TM    4(R2),X'20'         HAS FIELD CHANGED                            
         TM    PRDAOFCH+4,X'20'    HAS FIELD CHANGED?                           
         BO    VAXX                NO, THEN EXIT                                
*                                                                               
         L     RF,ACOMFACS         GET SYS NUM TO SWITCH BACK                   
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS                                                      
         DROP  R1                                                               
*                                                                               
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         MVI   COMPCD,0                                                         
*                                                                               
         L     R4,AIO                                                           
         USING PRODUCT,R4                                                       
         LA    R3,PRDAOFC          WHAT DID THEY ENTER                          
         SR    R1,R1              RESET INPUT LENGTH COUNTER                    
         CLI   PRDAOFCH+5,0                                                     
         BE    VA10                                                             
         LA    R1,1                                                             
         LA    R3,1(R3)                                                         
         CLI   PRDAOFCH+5,1                                                     
         BE    VA10                                                             
         LA    R1,2                                                             
         LA    R3,2(R3)                                                         
         CLI   PRDAOFCH+5,2                                                     
         BE    VA10                                                             
*                                                                               
         LA    R3,PRDAOFC          WHAT DID THEY ENTER                          
         LA    RE,5                                                             
         LA    R1,0                                                             
VAL      CLI   0(R3),C','                                                       
         BE    VA10                                                             
         CLI   0(R3),C'/'                                                       
         BE    VA10                                                             
         CLI   0(R3),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,VAL                                                           
*                                                                               
VA10     STC   R1,OFFLEN           SAVE OFFICE LENGTH                           
         MVC   ACCOFF(2),PRDAOFC                                                
         CLI   OFFLEN,1                                                         
         BNE   *+8                                                              
         MVI   ACCOFF+1,C' '                                                    
         CLI   1(R3),C' '          IS THERE AN OVERRIDE AGENCY                  
         BNH   *+10                                                             
         MVC   POWCODE,1(R3)       YES - SAVE POWER CODE                        
*                                                                               
         CLI   OFFLEN,2                                                         
*        BH    ERROFC              NO OFFICE LENGTH > 2                         
         JH    ERROFC              NO OFFICE LENGTH > 2                         
         CLI   OFFLEN,0                                                         
         BNE   VA20                                                             
         CLI   ACCOFC,C'Y'         NOTHING ENTERED - 2 REQD                     
*        BE    ERR2OFC                                                          
         JE    ERR2OFC                                                          
         XC    PACCOFC,PACCOFC     DEFAULT TO SPOT OFFICE                       
         XC    PACCAGY,PACCAGY                                                  
         MVC   PACCOFC,POFFICE                                                  
         MVI   PACCOFC+1,C' '                                                   
         B     VAX                                                              
*                                                                               
VA20     CLI   OFFLEN,2            IF THEY ENTERED 2 CHAR OFF                   
         BNE   VA25                                                             
         CLI   ACCOFC,C'Y'         BUT 2 CHAR NOT REQUIRED                      
*        BNE   ERR1OFC             =ERROR                                       
         JNE   ERR1OFC             =ERROR                                       
*                                                                               
VA25     CLI   OFFLEN,1            IF THEY ENTERED 1 CHAR OFF                   
         BNE   VA30                                                             
         CLI   ACCOFC,C'Y'         BUT 2 CHAR REQUIRED                          
*        BE    ERR2OFC                                                          
         JE    ERR2OFC                                                          
*                                                                               
VA30     DS    0H                                                               
         OC    POWCODE,POWCODE     OVERRIDE AGY?                                
         BNZ   VA40                                                             
         CLI   ACCOFC,C'Y'         2 CHAR REQUIRED                              
         BNE   VA90                IF NOT THEN SKIP SWITCH                      
         L     RF,ACOMFACS         SWITCH TO ACC SYSTEM                         
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'ACC',0                                              
         B     VA70                                                             
*                                                                               
VA40     DS    0H                  VALIDATE AGAINST AGY HDR CODE LIST           
         LA    RE,ACCAGY                                                        
         LA    R1,8                                                             
VA40L    CLC   0(2,RE),POWCODE     MATCH?                                       
         BE    VA50                                                             
         CLI   0(RE),C' '                                                       
*        BNH   ERRAAGY                                                          
         JNH   ERRAAGY                                                          
         LA    RE,2(RE)                                                         
         BCT   R1,VA40L                                                         
*        B     ERRAAGY                                                          
         J     ERRAAGY                                                          
*                                                                               
VA50     MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         XC    CTKEY,CTKEY         ACC AGY CODE                                 
         LA    R6,CTKEY                                                         
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         L     R5,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,(R5)                
         CLI   8(R1),0             ERRORS?                                      
*        BNE   ERRAAGY                                                          
         JNE   ERRAAGY                                                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO2                                                          
         USING CTSYSD,R6                                                        
         MVI   ELCODE,X'21'        GET SE NUM FOR ACC FILE                      
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
VA60NX   BRAS  RE,NEXTEL                                                        
*        BNE   ERRAAGY             ERROR IF NOT FOUND                           
         JNE   ERRAAGY             ERROR IF NOT FOUND                           
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
*        BNE   ERRAAGY             ERROR IF NOT FOUND                           
         JNE   ERRAAGY             ERROR IF NOT FOUND                           
         CLI   CTSYSNUM,X'06'      ACC??                                        
         BNE   VA60NX                                                           
*                                                                               
         MVC   COMPCD(1),CTSYSAGB  AGY BINARY CD                                
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CTSYSSE     SE NUM                                       
         L     RF,ACOMFACS         SWITCH TO THAT ACC SYSTEM                    
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
*                                                                               
VA70     CLI   4(R1),2             SYSTEM NOT OPEN                              
*        BE    ERRSYS                                                           
         JE    ERRSYS                                                           
         CLI   4(R1),1             ANY OTHER ERRORS?                            
*        BE    ERRSWS                                                           
         JE    ERRSWS                                                           
         CLI   COMPCD,0              TEST SAVED SF CODE BEFORE                  
         BNE   *+10                  YES - BETTER NOT DO IT AGAIN !             
         MVC   COMPCD,0(R1)          SAVE RETURNED AGENCY BINARY CODE           
*                                                                               
         MVC   MYACCKEY,SPACES       READ ACC COMPANY REC                       
         MVC   MYACCKEY(1),COMPCD                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,(R6)                
         CLI   8(R1),0                                                          
*        BNE   ERRCMP                                                           
         JNE   ERRCMP                                                           
         AH    R6,=Y(AC$ACCORFST)  FIRST ELEM (IN OLD FILE FORMAT)              
VA80     CLI   0(R6),AC$CPYELQ     X'10' COMPANY ELEM                           
         BE    VA85                                                             
         LLC   R0,0(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   VA80                                                             
*        B     ERRCMP                                                           
         J     ERRCMP                                                           
         USING AC$CPYELD,R6                                                     
VA85     TM    AC$CPYSTAT4,AC$CPYSOFF2 2 CHAR REQ'D                             
         BO    VA88                YES = VALIDATE OFFICE                        
         CLI   OFFLEN,1            MUST BE ONE                                  
*        BNE   ERR1OFC                                                          
         JNE   ERR1OFC                                                          
         B     VA90                OK                                           
         DROP  R6                                                               
*                                                                               
         USING AC$OFFRECD,R6                                                    
VA88     CLI   OFFLEN,2            MUST BE TWO                                  
*        BNE   ERR2OFC                                                          
         JNE   ERR2OFC                                                          
         LA    R6,MYACCKEY         NEW OFFICE -- LOOK FOR OFFICE REC            
         MVC   MYACCKEY,SPACES                                                  
         MVI   AC$OFFKTYP,AC$OFFKTYPQ X'01'                                     
         MVC   AC$OFFKCPY,COMPCD                                                
         MVC   AC$OFFKOFF(2),ACCOFF                                             
         L     R5,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,(R5)                
         CLI   8(R1),0                                                          
*        BNE   ERROFC                                                           
         JNE   ERROFC                                                           
*                                                                               
VA90     DS    0H                  OFFICE CODE IS GOOD                          
         MVC   PACCOFC,ACCOFF      SAVE OFFICE CODE                             
         MVC   PACCAGY,POWCODE     SAVE AGY CODE                                
*                                                                               
VAX      DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW       ORIGINAL SYS                                 
         L     RF,ACOMFACS         SWITCH BACK                                  
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,=H'24'     FOR SPOT                                     
VAXX     J     XIT                                                              
         DROP  R6                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CANTV PRODUCT                                                *         
***********************************************************************         
*        IF CANADIAN AGENCY ADDS OR MODIFIES A PRODUCT FOR TV - MUST  *         
*        ADD OR MODIFY PRODUCT RECORD FOR MEDIA N(03) AND MEDIA C(08) *         
***********************************************************************         
CANTV    NTR1  BASE=*,LABEL=*                                                   
         USING PRDHDR,R4                                                        
         L     R4,AIO1                                                          
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTX                                                              
         CLI   QMED,C'T'             TV?                                        
         BNE   CTX                                                              
*                                                                               
         XC    KEY,KEY               MEDIA N(03)                                
         MVC   KEY,PROKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT10                                                             
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEYSAVE      RECORD DOESN'T EXIST                       
         MVC   KEY(13),KEYSAVE       MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADREC                                                         
         B     CT20                                                             
*                                                                               
CT10     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         BRAS  RE,CTPUT              PUT N(03) RECORD                           
*                                                                               
CT20     XC    KEY,KEY               MEDIA C(08)                                
         MVC   KEY,PROKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT30                                                             
         L     R5,AIO1               RECORD DOESN'T EXIST                       
         MVC   0(13,R5),KEYSAVE      MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),KEYSAVE                                                  
         BRAS  RE,ADREC                                                         
         B     CT40                                                             
*                                                                               
CT30     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         BRAS  RE,CTPUT              PUT C(08) RECORD                           
*                                                                               
CT40     MVC   KEY,PROKEY            RESTORE KEY                                
         NI    1(R5),X'F0'                                                      
         OI    1(R5),X'01'                                                      
CTX      J     XIT                                                              
*                                                                               
CTPUT    NTR1                                                                   
         CLI   NEWLEN,C'Y'                                                      
         JE    CTPUT2                                                           
*                                                                               
         BRAS  RE,PTREC                                                         
         J     CTX                                                              
*                                                                               
CTPUT2   GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
*                                                                               
         MVC   KEYSAVE,KEY         SAVE KEY WITH NEW DISK ADDRESS               
         GOTO1 (RF),(R1),=C'DMRDHI',=C'SPTDIR',KEY,KEY                          
         CLC   KEY(13),KEYSAVE     MAKE SURE I FOUND IT                         
         JNE   *+2                                                              
         MVC   KEY,KEYSAVE         MOVE KEY WITH NEW D/A                        
         GOTO1 (RF),(R1),=C'DMWRT'                                              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         J     CTX                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CANTVC CLIENT                                                *         
***********************************************************************         
*        IF CANADIAN AGENCY ADDS A PRODUCT FOR TV ADD CLIENT RECORD   *         
*        FOR MEDIA N(03) AND MEDIA C(08)                              *         
***********************************************************************         
CANTVC   NTR1  BASE=*,LABEL=*                                                   
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTCX                                                             
         CLI   QMED,C'T'             TV?                                        
         BNE   CTCX                                                             
*                                                                               
         XC    KEY,KEY               SET UP CLIENT KEY FOR MEDIA N(03)          
         MVC   KEY(4),PROKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CTC10                                                            
         DC    H'0'                                                             
*                                                                               
CTC10    MVC   AIO,AIO3              READ CLIENT INTO AIO3                      
         GOTO1 GETREC                                                           
         L     R5,AIO2                                                          
         MVC   0(13,R5),KEY          COPY N(03) KEY OVER CLIENT RECORD          
         MVC   AIO,AIO2              AND PUT                                    
         BRAS  RE,PTREC                                                         
*                                                                               
CTC20    XC    KEY,KEY               SET UP CLIENT KEY FOR MEDIA C(08)          
         MVC   KEY(4),PROKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CTC30                                                            
         DC    H'0'                                                             
*                                                                               
CTC30    MVC   AIO,AIO3              READ IT INTO AIO3                          
         GOTO1 GETREC                                                           
         L     R5,AIO2                                                          
         MVC   0(13,R5),KEY          COPY C(08) KEY OVER CLIENT RECORD          
         MVC   AIO,AIO2              AND PUT                                    
         BRAS  RE,PTREC                                                         
*                                                                               
CTC40    MVC   KEY,PROKEY            RESTORE KEY                                
         NI    1(R5),X'F0'                                                      
         OI    1(R5),X'01'                                                      
CTCX     J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       PUT RECORD                                    *         
***********************************************************************         
*                                                                               
PTREC    NTR1  BASE=*,LABEL=*                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       ADD RECORD                                    *         
***********************************************************************         
*                                                                               
ADREC    NTR1  BASE=*,LABEL=*                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
DISACC   NTR1  BASE=*,LABEL=*      *DISPLAY AGENCY CODE*                        
***********************************************************************         
         L     R6,AIO                                                           
         USING PRODUCT,R6                                                       
         MVC   PRDAOFC,PACCOFC                                                  
         OC    PACCAGY,PACCAGY                                                  
         BNZ   DISACC10                                                         
         MVC   PRDAOFC+2(3),=C'   '                                             
         B     DISACCX                                                          
DISACC10 MVC   PRDAOFC+2(1),=C'/'                                               
         MVC   PRDAOFC+3(2),PACCAGY                                             
DISACCX  OI    PRDAOFCH+6,X'80'                                                 
         OI    PRDAOFCH+4,X'20'         SET VALIDATED                           
         J     XIT                                                              
         DROP  R6                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CONACTH          POINT TO ACTION                              
         CLI   T217FFD+1,C'*'      TEST DDS TERM                                
         BE    SETUP01                                                          
         TM    T217FFD+12,X'20'                                                 
         BNO   SETUP01             NOT ON = ALL OK                              
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    SETUPNQX            CHANGE NOT ALLOWED                           
         CLI   ACTNUM,ACTADD                                                    
         BE    SETUPNQX            ADD NOT ALLOWED                              
*                                                                               
SETUP01  MVI   USEIO,C'N'                                                       
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         OI    PRDREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         CLI   CALLSP,0                                                         
         BE    *+8                                                              
         NI    PRDREH+1,X'FF'-X'04'  LIGHT UP PF12 FIELD                        
         OI    PRDREH+6,X'80'                                                   
*                                                                               
SETUP10  DS    0H                                                               
         L     R3,=A(PFTABLE)                                                   
         A     R3,RELO                                                          
         GOTO1 INITPFKY,DMCB,(R3)    PF TABLE                                   
*                                                                               
SETUPX   J     EQXIT                                                            
*                                                                               
SETUPNQX J     NEQXIT                                                           
         LTORG                                                                  
*          DATA SET PRSFM1D    AT LEVEL 103 AS OF 02/12/19                      
         EJECT                                                                  
*===========================================================                    
* MODE=NEWSCR CLEAR SAP FIELDS IF NOT AN SAP AGENCY                             
* GET THE ACCESS RECORD TO SEE IF SAP AGENCY                                    
*==============================================================                 
*                                                                               
SETSAP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2            USE IO2 AS IO1 USED BY KEYMERGE!             
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,AIO                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAPAGY,C'N'                                                      
         L     R4,AIO                                                           
         LA    R4,CT5DATA-CT5REC(R4)                                            
         SR    R0,R0                                                            
*                                                                               
SETSAP2  CLI   0(R4),0                                                          
         JE    SETSAP10                                                         
         CLI   0(R4),X'B4'                                                      
         BE    SETSAP4                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SETSAP2                                                          
*                                                                               
         USING CTAGDD,R4                                                        
*                                                                               
SETSAP4  TM    CTAGOPTS,CTAGSAP    TEST SAP AGY                                 
         JZ    SETSAP10                                                         
         MVI   SAPAGY,C'Y'                                                      
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
*=========================================================                      
* NOT AN SAP AGENCY - CLEAR TITLE AND PROTECT SAP FIELD                         
*=========================================================                      
                                                                                
SETSAP10 XC    PRDSAP,PRDSAP       SAP INPUT FIELD                              
         OI    PRDSAPH+6,X'80'                                                  
         OI    PRDSAPH+1,X'20'     SET TO PROTECTED                             
*                                                                               
         XC    PRDSAPI,PRDSAPI     SAP TITLE                                    
         OI    PRDSAPIH+6,X'80'                                                 
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
PFTABLE DS   0H                                                                 
*        CLIENT MAINT DISPLAY                                                   
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'CM '                 MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'CM2'                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
MPF05X   EQU  *                                                                 
*                                                                               
*        PRODUCT LIST DISPLAY                                                   
         DC   AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                 
         DC   CL3'PL '                 LIST                                     
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
LPF03    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDPROK-1),AL2(PRDPROK-T217FFD)                    
LPF03X   EQU  *                                                                 
*                                                                               
*        CLIENT LIST DISPLAY                                                    
         DC   AL1(LPF06X-*,06,PFTCPROG,(LPF06X-LPF06)/KEYLNQ,0)                 
         DC   CL3'CL '                 LIST                                     
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
LPF06    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
LPF06X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
PRODUCT DSECT                                                                   
       ++INCLUDE SPGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY RECORD                                
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*PREFIX=AC$                                                                     
       ++INCLUDE ACGENFILE                                                      
*PREFIX=                                                                        
         EJECT                                                                  
CLIENT  DSECT                                                                   
       ++INCLUDE SPGENCLT          CLIENT  RECORD                               
         EJECT                                                                  
       ++INCLUDE SPGENEST          ESTIMATE RECORDS                             
         EJECT                                                                  
       ++INCLUDE SPGENESTD         ESTIMATE PASSIVE BY DATE                     
         EJECT                                                                  
       ++INCLUDE SPGENBILL         BILLING RECORDS                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM70D          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SCSFM71D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE SPGENADV          ADVERTISER HEADER RECORD                     
         EJECT                                                                  
       ++INCLUDE DDPSTBLK          PROVINCIAL TAX VALIDATION                    
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
PROKEY   DS    CL13                                                             
SAVEKEY  DS    XL13                                                             
*                                                                               
SVCLTDA  DS    XL4                                                              
SVPRDDA  DS    XL4                                                              
SVAGYFL1 DS    XL1                                                              
ERRAREA  DS    X                                                                
FADDR    DS    A                                                                
SVP1USER DS    CL20                                                             
SVP1TYPE DS    CL1                                                              
SVP1LEN  DS    XL1                                                              
SVP1FLG1 DS    XL1                                                              
SVP1FLG2 DS    XL1                                                              
*                                                                               
SVP2USER DS    CL20                                                             
SVP2TYPE DS    CL1                                                              
SVP2LEN  DS    XL1                                                              
SVP2FLG1 DS    XL1                                                              
SVP2FLG2 DS    XL1                                                              
*                                                                               
SVCLOP1  DS    XL1                                                              
SVCLOP2  DS    XL1                                                              
SVCLPROF DS    CL15                                                             
*                                                                               
AUSR     DS    A                                                                
PSTOUT   DS    CL64                                                             
OPTNFLAG DS    XL1                                                              
TALOPTN  EQU   X'80'                                                            
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    CL1                                                              
USERDATA DS    CL32                                                             
*                                                                               
SVPRDNUM DS    XL1                                                              
SVADVLST DS    CL30                                                             
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
NEWLEN   DS    CL1                                                              
WORK2    DS    CL48                                                             
SVADVDA  DS    CL4                                                              
SVADVAGY DS    XL1                                                              
SVADVKEY DS    XL13                                                             
BYTE2    DS    XL1                                                              
CCOUNT   DS    XL1                                                              
PRDSECFL DS    XL1                 PRODUCT SECURITY FLAG                        
SVOFFICE DS    XL1                                                              
GTFACTB  DS    CL88                                                             
SYSSW    DS    XL1                 SE NUM FOR SWITCH                            
POWCODE  DS    CL2                 ACCOUNT AGENCY OVERRIDE                      
ACCOFF   DS    CL2                 ACCOUNT OFFICE CODE                          
COMPCD   DS    XL1                 AGENCY BINARY CODE                           
OFFLEN   DS    XL1                 ACCOUNT OFFICE LENGTH                        
ACCOFC   DS    XL1                 AGYOFC2                                      
ACCAGY   DS    XL24                AGYACCAG                                     
CTKEY    DS    CL28                                                             
MYACCKEY DS    CL42                ACCOUNT REC KEY                              
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
         DS    CL2                                                              
         DS    CL3                                                              
         DS    CL3                                                              
LSPRDC   DS    CL3                                                              
         DS    CL12                                                             
LSPRDN   DS    CL20                                                             
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPSFM50   03/24/20'                                      
         END                                                                    
