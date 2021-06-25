*          DATA SET PRSFM24    AT LEVEL 047 AS OF 04/27/04                      
*PHASE T41C24A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C24 - GFEST MAINT/LIST                                       
*                                                                               
* KWAN 09/05/02 CONVERT GFEST FROM FIL TO SFM                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, DEL, SEL, REST, LIST, REP    *         
*                                                                     *         
*  INPUTS       SCREEN T41CCD (MAINTENANCE)                           *         
*               SCREEN T41CCE (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED GROUP DEFINITION RECORDS                      *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - GROUP DEFINITION RECORD                         *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C24 - GFEST MAINT/LIST'                                      
*                                                                               
T41C24   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C24                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
* USE FOLLOWING MODE WHEN PASSIVE POINTERS NEED TO BE ADDED/DELETED             
*                                                                               
* * * *  CLI   MODE,XRECADD        AFTER ADD                                    
* * * *  BE    DR                                                               
* * * *  CLI   MODE,XRECPUT        AFTER PUT                                    
* * * *  BE    DR                                                               
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
JUMPXIT1 XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,GFEMEDH          POINT TO MEDIA FLD                           
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTMEDH                                                       
*                                                                               
         GOTO1 VALIMED                                                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK10                                                             
         MVC   GFEMEDN,MEDNM                                                    
         OI    GFEMEDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK10     XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY                                                         
         USING PGSTKEY,R4          GFEST DSECT IS SAME AS PGEST                 
*                                                                               
         MVC   PGSTKAGY,AGENCY                                                  
         MVC   PGSTKMED,QMED                                                    
         MVI   PGSTKRCD,X'0B'      RECORD CODE FOR GFEST                        
*                                                                               
         XC    CLTLSFLT,CLTLSFLT   CLEAR CLT LIST FILTER                        
         LA    R2,GFECLTH          POINT TO CLIENT FLD                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JNE   MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS?                            
         BZ    VK30                NO                                           
         CLI   CONWHENH+5,0        NOW, SOON, OR OV?                            
         BE    VK30                NO                                           
*                                                                               
         J     CLTRQERR            SECURITY - CLIENT REQUIRED                   
*                                                                               
VK20     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALICLT                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK20H               FOR REPORT, NO NEED TO VALICLT               
         CLI   5(R2),0             NO INPUT?                                    
         BE    VK30                                                             
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         BE    VK30                YES, LEAVE CLIENT FILTER NULLS               
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CLTLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    CLTLSFLT,SPACES     SPACE PADDED                                 
         B     VK30                                                             
*                                                                               
VK20H    GOTO1 VALICLT                                                          
         MVC   PGSTKCLT,QCLT                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK30                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK30                                                             
         MVC   GFECLTN,CLTNM                                                    
         OI    GFECLTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK30     LA    R2,GFEPRDH          POINT TO PRODUCT FLD                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTPRDH                                                       
         XC    PRDLSFLT,PRDLSFLT   CLEAR PRD LIST FILTER                        
         CLI   5(R2),0                                                          
         BNE   VK40                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK50                YES - OPTIONAL                               
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK50                YES - OPTIONAL                               
         J     MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
VK40     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALIPRD                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK40H               FOR REPORT, NO NEED TO VALIPRD               
         CLC   =C'ALL',8(R2)       ALL PRODUCT?                                 
         BE    VK50                YES, LEAVE PRODUCT FILTER NULLS              
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRDLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    PRDLSFLT,SPACES     SPACE PADDED                                 
         B     VK50                                                             
*                                                                               
VK40H    GOTO1 VALIPRD                                                          
         MVC   PGSTKPRD,QPRD                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK50                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK50                                                             
         MVC   GFEPRDN,PRDNM                                                    
         OI    GFEPRDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK50     LA    R2,GFEESTH          VALIDATE ESTIMATE                            
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTESTH                                                       
         XC    ESTLSFLT,ESTLSFLT   CLEAR EST LIST FILTER                        
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES - OPTIONAL                               
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VKX                 YES - OPTIONAL                               
         J     MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
VK60     CLI   5(R2),3             MAX 3 DIGITS                                 
         JH    INVFDERR                                                         
         TM    4(R2),X'08'         NUMERIC?                                     
         JNO   INVFDERR                                                         
         BAS   RE,PACK             R0 WILL RETURN BINARY ESTIMATE               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALIEST                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK60B               FOR REPORT, NO NEED TO VALIEST               
         STCM  R0,3,ESTLSFLT       BINARY ESTIMATE                              
         B     VKX                                                              
*                                                                               
* GFEST - ALLOW ESTIMATE=0 IF PRODUCT IS NOT ZZZ                                
*                                                                               
VK60B    LTR   R0,R0               ESTIMATE IS ZERO?                            
         BNZ   VK60E               NO, GO VALIDATE ESTIMATE                     
         CLC   QPRD,=C'ZZZ'        PRD "ZZZ"?                                   
         JE    INVFDERR            CANNOT BE ZERO FOR "ZZZ"                     
*                                                                               
VK60C    CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES, EST NAME IS NOT DISPLAYED               
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VKX                 YES, EST NAME IS NOT DISPLAYED               
         XC    GFEESTN,GFEESTN                                                  
         OI    GFEESTNH+6,X'80'    CLEAR ESTIMATE NAME AND TRANSMIT             
         J     VKX                                                              
*                                                                               
VK60E    STCM  R0,3,PGSTKEST       BINARY ESTIMATE                              
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES, NO NEED TO LOOK UP EST NAME             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VKX                 YES, NO NEED TO LOOK UP EST NAME             
*                                                                               
         XC    KEY,KEY             CANNOT USE VALIEST                           
         MVC   KEY+00(02),AGENCY                                                
         MVC   KEY+02(01),QMED                                                  
         MVI   KEY+03,X'07'        ESTIMATE RECORD CODE                         
         MVC   KEY+04(03),QCLT                                                  
         MVC   KEY+07(03),QPRD                                                  
         STCM  R0,3,KEY+10         BINARY ESTIMATE                              
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     ESTIMATE ON FILE?                            
         BNE   VK60C                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),X'07'        FIRST ESTIMATE REC ELEM PRESENT?             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ESTNM,35(R6)        GET ESTIMATE NAME                            
*                                                                               
         MVC   GFEESTN,ESTNM                                                    
         OI    GFEESTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVI   USEIONUM,1          RECORD WILL BE READ INTO AIO1                
         B     JUMPXIT1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE DATA FOR GENERAL FOODS              
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM             REMOVE X'10' ELEM FROM GFEST                 
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PGSTELEM,R6                                                      
         MVI   PGSTEID,PGSTEIDQ                                                 
         MVI   PGSTELN,PGSTELNQ                                                 
*                                                                               
         LA    R2,GFEDBCH          FIRST ELEMENT                                
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'01'                                                   
         MVC   PGSTNAME,=C'DIVBRNCD'                                            
         MVC   PGSTDATA(L'GFEDBC),GFEDBC                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFEPRCH          SECOND ELEMENT                               
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'02'                                                   
         MVC   PGSTNAME,=C'PROD CD '                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'GFEPRC),GFEPRC                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFEGFNH          THIRD ELEMENT                                
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'03'                                                   
         MVC   PGSTNAME,=C'GF NTRL '                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'GFEGFN),GFEGFN                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFEGFSH          FOURTH ELEMENT                               
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'04'                                                   
         MVC   PGSTNAME,=C'GFSBNTRL'                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'GFEGFS),GFEGFS                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFEBAGH          FIFTH ELEMENT                                
         B     *+8                 BYPASS, UNTIL REQUIREMENTS DEFINIED          
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'05'                                                   
         MVC   PGSTNAME,=C'BILL AGY'                                            
         XC    PGSTDATA,PGSTDATA                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    *+18                NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFEBAG),SPACES                                        
         LA    RF,PGSTDATA         POINT RF TO DATA FIELD FOR ELEM              
         BAS   RE,VREXMOVL         DO EX MOVE TO LEFT-JUSTIFY DATA              
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFEXPTH          SIXTH ELEMENT                                
         B     *+8                 BYPASS, UNTIL REQUIREMENTS DEFINIED          
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'06'                                                   
         MVC   PGSTNAME,=C'EXP TYPE'                                            
         XC    PGSTDATA,PGSTDATA                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    *+18                NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFEXPT),SPACES                                        
         LA    RF,PGSTDATA         POINT RF TO DATA FIELD FOR ELEM              
         BAS   RE,VREXMOVL         DO EX MOVE TO LEFT-JUSTIFY DATA              
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFEPIDH          SEVENTH ELEMENT                              
         B     *+8                 BYPASS, UNTIL REQUIREMENTS DEFINIED          
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'07'                                                   
         MVC   PGSTNAME,=C'PROD ID '                                            
         XC    PGSTDATA,PGSTDATA                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR_07H              NO - LEAVE PGSTDATA AS NULLS                 
         TM    4(R2),X'08'         NUMERIC?                                     
         JZ    NOTNMERR            NO                                           
         MVI   PGSTDATA,C'0'                                                    
         MVC   PGSTDATA+1(L'GFEPID-1),PGSTDATA                                  
         LA    RF,PGSTDATA         POINT RF TO DATA FIELD FOR ELEM              
         LA    RF,L'GFEPID(RF)     BUMP RF TO LAST BYTE FOR THIS FIELD          
         BAS   RE,VREXMOVR         DO EX MOVE TO RIGHT-JUSTIFY DATA             
VR_07H   GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFECAGH          EIGHTH ELEMENT                               
         B     *+8                 BYPASS, UNTIL REQUIREMENTS DEFINIED          
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'08'                                                   
         MVC   PGSTNAME,=C'CRTV AGY'                                            
         XC    PGSTDATA,PGSTDATA                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    *+18                NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFECAG),SPACES                                        
         LA    RF,PGSTDATA         POINT RF TO DATA FIELD FOR ELEM              
         BAS   RE,VREXMOVL         DO EX MOVE TO LEFT-JUSTIFY DATA              
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFESAGH          NINTH ELEMENT                                
         B     *+8                 BYPASS, UNTIL REQUIREMENTS DEFINIED          
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'09'                                                   
         MVC   PGSTNAME,=C'SRCE AGY'                                            
         XC    PGSTDATA,PGSTDATA                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    *+18                NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFESAG),SPACES                                        
         LA    RF,PGSTDATA         POINT RF TO DATA FIELD FOR ELEM              
         BAS   RE,VREXMOVL         DO EX MOVE TO LEFT-JUSTIFY DATA              
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFETMKH          TENTH ELEMENT                                
         B     *+8                 BYPASS, UNTIL REQUIREMENTS DEFINIED          
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'0A'                                                   
         MVC   PGSTNAME,=C'TRGT MKT'                                            
         XC    PGSTDATA,PGSTDATA                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    *+18                NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFETMK),SPACES                                        
         LA    RF,PGSTDATA         POINT RF TO DATA FIELD FOR ELEM              
         BAS   RE,VREXMOVL         DO EX MOVE TO LEFT-JUSTIFY DATA              
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,GFEDGVH          ELEVENTH ELEMENT                             
         B     *+8                 BYPASS, UNTIL REQUIREMENTS DEFINIED          
         BAS   RE,VRCKEST          CK FOR ZERO ESTIMATE                         
         MVI   PGSTESEQ,X'0B'                                                   
         MVC   PGSTNAME,=C'DEAL# GV'                                            
         XC    PGSTDATA,PGSTDATA                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    *+18                NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFEDGV),SPACES                                        
         LA    RF,PGSTDATA         POINT RF TO DATA FIELD FOR ELEM              
         BAS   RE,VREXMOVL         DO EX MOVE TO LEFT-JUSTIFY DATA              
         GOTO1 ADDELEM                                                          
*                                                                               
VRX      DS    0H                                                               
         B     JUMPXIT1                                                         
*                                                                               
VRCKEST  CLI   5(R2),0                                                          
         BE    *+16                                                             
         OC    KEY+10(2),KEY+10    ZERO ESTIMATE?                               
         JZ    INVFDERR            INVALID IF EST IS ZERO                       
         BR    RE                                                               
         OC    KEY+10(2),KEY+10    ZERO ESTIMATE?                               
         JNZ   MSSNGERR            INPUT IS REQUIRED FOR ZERO EST               
         BR    RE                                                               
*                                                                               
VREXMOVL ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   0(0,RF),8(R2)                                                    
*                                                                               
VREXMOVR ZIC   R1,5(R2)                                                         
         SR    RF,R1               POINT RF TO START OF TARGET FIELD            
         BCTR  R1,0                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   0(0,RF),8(R2)                                                    
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         L     R6,AIO                                                           
         CLI   3(R6),X'0B'         GFEST RECORD CODE?                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGSTELEM,R6                                                      
         MVI   ELCODE,PGSTEIDQ     GFEST ELEM ID                                
*                                                                               
         LA    R2,GFEDBCH                                                       
         BAS   RE,GETEL                                                         
         JNE   INVFDERR                                                         
         FOUT  GFEDBCH,PGSTDATA,2                                               
*                                                                               
         LA    R2,GFEPRCH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  GFEPRCH,PGSTDATA,4                                               
*                                                                               
         LA    R2,GFEGFNH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  GFEGFNH,PGSTDATA,3                                               
*                                                                               
         LA    R2,GFEGFSH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  GFEGFSH,PGSTDATA,3                                               
*                                                                               
         LA    R2,GFEBAGH                                                       
         XC    GFEBAG,GFEBAG                                                    
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         BAS   RE,EXOUT                                                         
*                                                                               
         LA    R2,GFEXPTH                                                       
         XC    GFEXPT,GFEXPT                                                    
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         BAS   RE,EXOUT                                                         
*                                                                               
         LA    R2,GFEPIDH                                                       
         XC    GFEPID,GFEPID                                                    
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         BAS   RE,EXOUT                                                         
*                                                                               
* ELIMINATE LEADING ZEROS AND TRANSMIT OUTPUT AGAIN                             
*                                                                               
         EDIT  (C10,GFEPID),(10,8(R2)),ALIGN=LEFT                               
         OI    GFEPIDH+6,X'80'                                                  
*                                                                               
         LA    R2,GFECAGH                                                       
         XC    GFECAG,GFECAG                                                    
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         BAS   RE,EXOUT                                                         
*                                                                               
         LA    R2,GFESAGH                                                       
         XC    GFESAG,GFESAG                                                    
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         BAS   RE,EXOUT                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         LA    R2,GFETMKH                                                       
         XC    8(L'GFETMK,R2),8(R2)                                             
         BAS   RE,EXOUT                                                         
*                                                                               
         LA    R2,GFEDGVH                                                       
         XC    GFEDGV,GFEDGV                                                    
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         BAS   RE,EXOUT                                                         
*                                                                               
DRX      DS    0H                                                               
         B     JUMPXIT1                                                         
*                                                                               
EXOUT    LA    R4,L'PGSTDATA       MAX LUP COUNTER                              
         LA    R5,PGSTDATA                                                      
         LA    R3,PGSTDATA+L'PGSTDATA-1                                         
*                                                                               
EXOLUP   CLI   0(R3),C' '          ANY DATA AT END?                             
         BH    EXOMLUP             YES - GO DO OUTPUT                           
         AHI   R3,-1               MOVE TO LEFT IN PGSTDATA                     
         BCT   R4,EXOLUP                                                        
         B     EXOTMT              NO DATA, TRANSMIT CLEATREEARED FIELD         
*                                                                               
EXOMLUP  CLI   0(R5),C' '          ANY DATA AT FRONT?                           
         BH    EXOMEX              YES - GO MOVE                                
         LA    R5,1(R5)            BUMP OVER                                    
         B     EXOMLUP                                                          
*                                                                               
EXOMEX   SR    R3,R5               SET R3 FOR EXECUTED MOVE                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R5)                                                    
*                                                                               
EXOTMT   OI    6(R2),X'80'         TRANSMIT                                     
         BR    RE                                                               
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         L     R6,AIO              RECORD SELECTED                              
         CLI   3(R6),X'0B'         GFEST RECORD CODE?                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGSTKEY,R6                                                       
         MVC   GFEMED,PGSTKMED                                                  
         MVC   GFECLT,PGSTKCLT                                                  
         MVC   GFEPRD,PGSTKPRD                                                  
         EDIT  (B2,PGSTKEST),(3,GFEEST),0,ALIGN=RIGHT,FILL=0                    
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    GFEMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    GFEMEDN,GFEMEDN                                                  
         OI    GFEMEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   GFEMEDH+5,1         INPUT LENGTH                                 
         LA    R2,GFEMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   GFEMEDN,MEDNM                                                    
         OI    GFEMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    GFECLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    GFECLTN,GFECLTN                                                  
         OI    GFECLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   GFECLTH+5,3         INPUT LENGTH                                 
         LA    R2,GFECLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   GFECLTN,CLTNM                                                    
         OI    GFECLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         OI    GFEPRDH+6,X'80'     TRANSMIT PRODUCT CODE                        
         XC    GFEPRDN,GFEPRDN                                                  
         OI    GFEPRDNH+6,X'80'    CLEARED PRODUCT NAME                         
         MVI   GFEPRDH+5,3         INPUT LENGTH                                 
         LA    R2,GFEPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   GFEPRDN,PRDNM                                                    
         OI    GFEPRDNH+6,X'80'    TRANSMIT PRODUCT NAME                        
*                                                                               
         OI    GFEESTH+6,X'80'     TRANSMIT ESTIMATE CODE                       
         XC    GFEESTN,GFEESTN                                                  
         OI    GFEESTNH+6,X'80'    CLEARED ESTIMATE NAME                        
         MVI   GFEESTH+5,3         INPUT LENGTH                                 
         OI    GFEESTH+4,X'08'     INPUT IS NUMERIC                             
         LA    R2,GFEESTH                                                       
         BAS   RE,PACK                                                          
         LTR   R0,R0               ESTIMATE IS ZERO?                            
         BZ    DKX                 YS, NO NEED TO VALIDATE ESTIMATE             
*                                                                               
         XC    KEY,KEY             CANNOT USE VALIEST                           
         MVC   KEY+00(02),AGENCY                                                
         MVC   KEY+02(01),QMED                                                  
         MVI   KEY+03,X'07'        ESTIMATE RECORD CODE                         
         MVC   KEY+04(03),QCLT                                                  
         MVC   KEY+07(03),QPRD                                                  
         STCM  R0,3,KEY+10         BINARY ESTIMATE                              
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     ESTIMATE ON FILE?                            
         BNE   DKX                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),X'07'        FIRST ESTIMATE REC ELEM PRESENT?             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ESTNM,35(R6)        GET ESTIMATE NAME                            
         MVC   GFEESTN,ESTNM                                                    
         OI    GFEESTNH+6,X'80'    TRANSMIT ESTIMATE NAME                       
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         B     JUMPXIT1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                  LIST RECORDS                                 
         OC    KEY,KEY                                                          
         BNZ   LR20                CONTINUE LISTING                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGSTKEY,R4                                                       
*                                                                               
         MVC   KEY(25),SVKEY       KEY BUILT FROM VK                            
*                                                                               
LR20     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LR40                                                             
*                                                                               
LR30     GOTO1 SEQ                 NEXT RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR40     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   LRX                                                              
*                                                                               
         CLC   KEY+04(3),CLTLSFLT  CLT IS HIGHER THAN FILTER?                   
         BL    LR30                                                             
         CLC   KEY+07(3),PRDLSFLT  PRD IS HIGHER THAN FILTER?                   
         BL    LR30                                                             
         CLC   KEY+10(2),ESTLSFLT  EST IS HIGHER THAN FILTER?                   
         BL    LR30                                                             
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   SVWORK(L'KEY),KEY   RESTORING SEQUENCE LATER                     
         MVI   USEIONUM,2          USE AIO2 FOR CLT/PRD/EST RECORDS             
*                                                                               
         XC    KEY+07(18),KEY+07   PREPARE CLIENT REC KEY                       
         MVI   KEY+03,X'02'        CLIENT RECORD CODE                           
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     CLIENT ON FILE?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+04(3),SVWORK+04 SAME CLIENT AS IN SAVED KEY?                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         CLI   33(R6),X'02'        FIRST CLIENT REC ELEM PRESENT?               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSCLT+00(03),KEY+04 PUT CLT CODE TO SCR                          
         MVI   LSCLT+03,C'/'                                                    
         MVC   LSCLT+04(16),35(R6) PUT CLT NAME TO SCR (1ST 16 CHARS)           
*                                                                               
         MVC   KEY,SVWORK                                                       
         XC    KEY+10(15),KEY+10   PREPARE PRODUCT REC KEY                      
         MVI   KEY+03,X'06'        PRODUCT RECORD CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     PRODUCT ON FILE?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+07(3),SVWORK+07 SAME PRODUCT AS IN SAVED KEY?                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         CLI   33(R6),X'06'        FIRST PRODUCT REC ELEM PRESENT?              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSPRD+00(03),KEY+07 PUT PRD CODE TO SCR                          
         MVI   LSPRD+03,C'/'                                                    
         MVC   LSPRD+04(16),35(R6) PUT PRD NAME TO SCR (1ST 16 CHARS)           
*                                                                               
* ZERO ESTIMATE IS ALLOWED IN GFEST/PGEST, BUT NOT IN ESTIMATE                  
*                                                                               
         OC    SVWORK+10(2),SVWORK+10                                           
         BNZ   LR50                                                             
         MVC   LSEST+00(3),=C'000'                                              
         MVI   LSEST+03,C'/'                                                    
         B     LR80                                                             
*                                                                               
LR50     MVC   KEY,SVWORK                                                       
         XC    KEY+12(13),KEY+12   PREPARE ESTIMATE REC KEY                     
         MVI   KEY+03,X'07'        ESTIMATE RECORD CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     ESTIMATE ON FILE?                            
         BE    LR60                                                             
         EDIT  (B2,KEYSAVE+10),(3,LSEST),0,ALIGN=RIGHT,FILL=0                   
         MVI   LSEST+03,C'/'                                                    
         B     LR80                                                             
*                                                                               
LR60     CLC   KEY+12(2),SVWORK+12 SAME ESTIMATE AS IN SAVED KEY?               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         CLI   33(R6),X'07'        FIRST ESTIMATE REC ELEM PRESENT?             
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,KEY+10),(3,LSEST),0,ALIGN=RIGHT,FILL=0                       
         MVI   LSEST+03,C'/'                                                    
         MVC   LSEST+04(16),35(R6) PUT EST NAME TO SCR (1ST 16 CHARS)           
*                                                                               
LR80     MVC   KEY,SVWORK                                                       
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(25),KEYSAVE     SAME RECORD GOTTEN BACK?                     
         BE    *+6                                                              
         DC    H'0'                SEQUENCE IS NOT PROPERLY RESTORED            
         MVC   DMDSKADD,KEY+27     DISK ADDRESS FOR LISTMON                     
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
*                                                                               
         CLI   MODE,PRINTREP       REPORTING USING LIST ACTION?                 
         BNE   LR30                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),PGSTEIDQ     GFEST ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    P1,P1               CLEAR PRINT LINE                             
         BRAS  RE,GETREPLN         GET REPORT LINE                              
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
*                                                                               
         B     LR30                GET NEXT RECORD                              
*                                                                               
LRX      DS    0H                                                               
         B     JUMPXIT1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
         OC    KEY,KEY                                                          
         BNZ   PR20                CONTINUE LISTING                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGSTKEY,R4                                                       
*                                                                               
         MVC   KEY(25),SVKEY       KEY BUILT FROM VK                            
*                                                                               
PR20     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PR40                                                             
*                                                                               
PR30     GOTO1 SEQ                 NEXT RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PR40     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   PRX                                                              
*                                                                               
         CLC   KEY+04(3),CLTLSFLT  CLT IS HIGHER THAN FILTER?                   
         BL    PR30                                                             
         CLC   KEY+07(3),PRDLSFLT  PRD IS HIGHER THAN FILTER?                   
         BL    PR30                                                             
         CLC   KEY+10(2),ESTLSFLT  EST IS HIGHER THAN FILTER?                   
         BL    PR30                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),PGSTEIDQ     PGEST ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    P1,P1               CLEAR PRINT LINE                             
         BRAS  RE,GETREPLN         GET REPORT LINE                              
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
         B     PR30                GET NEXT RECORD                              
*                                                                               
PRX      DS    0H                                                               
         B     JUMPXIT1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MSSNGERR MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
NOTNMERR MVI   ERROR,NOTNUM                                                     
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,INVRCACT      INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,85            SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         OI    GENSTAT4,CONFDEL    GIVE USER CHANCE TO CONFIRM DELETE           
*                                                                               
         LA    RE,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    RE,SPECS                                                         
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(01),QMED                                                   
*                                                                               
         J     JUMPXIT1                                                         
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    J     JUMPXIT1                                                         
*                                                                               
HEDSPECS SSPEC H2,01,C'MEDIA'                                                   
         SSPEC H1,56,C' GFEST REPORT  '                                         
         SSPEC H2,56,C'-------------- '                                         
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H6,01,C'                BILLING    EXPENSE   PRODUCT'            
         SSPEC H7,01,C'CLT  PRD  EST   AGENCY     TYPE      ID'                 
         SSPEC H8,01,C'---  ---  ---   --------   -------   ----------'         
         SSPEC H6,51,C'CREATIVE   SOURCE     TARGET   DEAL#'                    
         SSPEC H7,51,C'AGENCY     AGENCY     MARKET   (GEVALIA)'                
         SSPEC H8,51,C'--------   --------   ------   ----------'               
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREPLN NTR1  BASE=*,LABEL=*      R6 POINTS TO RECORD                          
*                                                                               
         LA    R7,P1                                                            
         USING REP_LINE,R7                                                      
*                                                                               
         MVC   R_CLT,04(R6)                                                     
         MVC   R_PRD,07(R6)                                                     
         EDIT  (B2,10(R6)),(3,R_EST),0,ALIGN=RIGHT,FILL=0                       
*                                                                               
         USING PGSTELEM,R6                                                      
         MVI   ELCODE,PGSTEIDQ     PGEST ELEM ID                                
*                                                                               
         BRAS  RE,GETEL            1ST ELEM, DIVISION/BRAND CODE                
         JE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,NEXTEL           2ND ELEM, PRODUCT CODE                       
         JE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,NEXTEL           3RD ELEM, GF NATURAL                         
         JE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,NEXTEL           4TH ELEM, GF SUB-NATURAL                     
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+12                                                             
         LA    R2,R_BILAGY         BILLING AGENCY                               
         BAS   RE,GREPEX                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+12                                                             
         LA    R2,R_EXPTYP         EXPENSE TYPE                                 
         BAS   RE,GREPEX                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   GREP50                                                           
         LA    R2,R_PRDID          PRODUCT ID                                   
         BAS   RE,GREPEX                                                        
*                                                                               
* ELIMINATE LEADING ZEROS AND TRANSMIT OUTPUT AGAIN                             
*                                                                               
         EDIT  (C10,R_PRDID),(10,R_PRDID),ALIGN=LEFT                            
*                                                                               
GREP50   BRAS  RE,NEXTEL                                                        
         JNE   *+12                                                             
         LA    R2,R_CREAGY         CREATIVE AGENCY                              
         BAS   RE,GREPEX                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+12                                                             
         LA    R2,R_SOUAGY         SOURCE AGENCY                                
         BAS   RE,GREPEX                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+12                                                             
         LA    R2,R_TARMKT         TARGET MARKET                                
         BAS   RE,GREPEX                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+12                                                             
         LA    R2,R_DEALN          DEAL# (GEVALIA)                              
         BAS   RE,GREPEX                                                        
*                                                                               
         J     JUMPXIT1                                                         
*                                                                               
GREPEX   LA    R4,L'PGSTDATA       MAX LOOP COUNTER                             
         LA    R5,PGSTDATA                                                      
         LA    R3,PGSTDATA+L'PGSTDATA-1                                         
*                                                                               
GREPEX10 CLI   0(R3),C' '          ANY DATA AT END?                             
         BH    GREPEX20            YES - GO DO OUTPUT                           
         AHI   R3,-1               MOVE TO LEFT IN PGSTDATA                     
         BCT   R4,GREPEX10                                                      
         BR    RE                  NO DATA, RETURN VIA RE                       
*                                                                               
GREPEX20 CLI   0(R5),C' '          ANY DATA AT FRONT?                           
         BH    GREPEX30            YES - GO MOVE                                
         LA    R5,1(R5)            BUMP OVER                                    
         B     GREPEX20                                                         
*                                                                               
GREPEX30 SR    R3,R5               SET R3 FOR EXECUTED MOVE                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)                                                    
         BR    RE                  RETURN VIA RE                                
*                                                                               
         DROP  R7,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMCDD          GFEST MAINT SCREEN                           
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMCED          GFEST LIST SCREEN                            
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGESTREC          DSECT FOR BOTH PGEST/GFEST                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND            FIELD INDICATOR EQUATES                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
CLTLSFLT DS    CL3                 CLT LIST FILTER                              
PRDLSFLT DS    CL3                 PRD LIST FILTER                              
ESTLSFLT DS    XL2                 EST LIST FILTER                              
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSCLT    DS    CL20                CLT CODE AND 16 CHARS NAME                   
         DS    CL5                                                              
LSPRD    DS    CL20                PRD CODE AND 16 CHARS NAME                   
         DS    CL5                                                              
LSEST    DS    CL20                EST CODE AND 16 CHARS NAME                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REP_LINE DSECT                     REPORT DSECT FOR PRINTING LINES              
*                                                                               
R_CLT    DS    CL3                 CLIENT CODE                                  
         DS    XL2                                                              
R_PRD    DS    CL3                 PRODUCT CODE                                 
         DS    XL2                                                              
R_EST    DS    CL3                 ESTIMATE NUMBER                              
         DS    XL3                                                              
R_BILAGY DS    CL8                 BILLING AGENCY                               
         DS    XL3                                                              
R_EXPTYP DS    CL6                 EXPENSE TYPE                                 
         DS    XL4                                                              
R_PRDID  DS    CL10                PRODUCT ID                                   
         DS    XL3                                                              
R_CREAGY DS    CL8                 CREATIVE AGENCY                              
         DS    XL3                                                              
R_SOUAGY DS    CL8                 SOURCE AGENCY                                
         DS    XL3                                                              
R_TARMKT DS    CL1                 TARGET MARKET                                
         DS    XL8                                                              
R_DEALN  DS    CL10                DEAL# (GEVALIA)                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047PRSFM24   04/27/04'                                      
         END                                                                    
