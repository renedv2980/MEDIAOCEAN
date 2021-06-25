*          DATA SET SPSFM29    AT LEVEL 014 AS OF 10/21/19                      
*PHASE T21729A                                                                  
*                                                                               
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-35894   10/21/19 COMSCORE SUPPORT                         *         
***********************************************************************         
***********************************************************************         
*  AKAT  3/04/09 DISPLAY 4TH DEMO CORRECTLY                                     
*  BPOO  4/20/98 ADD CODE TO CHANGE PW RECORDS FOR PW CLIENTS                   
*  BPOO  1/15/98 NEW STATION LOCKIN PROGRAM                                     
***********************************************************************         
*                                                                     *         
*  TITLE:        T21729  -- STATION LOCKIN FILE                                 
*                                                                     *         
*  COMMENTS:     MAINTAINS OFFICE RECORDS ON SPFILE                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMA8, AND SPSFMA9                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED OFFICE RECORDS                               *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
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
         TITLE 'T21729 - OFFICE RECORD MAINTENANCE'                             
T21729   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1729**,R7,RR=R3                                              
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
         MVC   DMCB+4(4),=X'D9000AD9'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOVAL,DMCB                                                    
*                                                                               
         MVC   DMCB+4(4),=X'D9000A79'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPPWCALC,DMCB                                                    
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY         DISPLAY RECORD                              
         BE    DK                                                               
         CLI   MODE,XRECPUT         DISPLAY RECORD                              
         BE    XR                                                               
         CLI   MODE,SETFILE        SET FILES                                    
         BE    SF                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
*                   SET FILE                                      *             
*******************************************************************             
SF       BAS   RE,SSV                                                           
         B     XIT                                                              
********************************************************************            
*                      VALIDATE KEY                                             
********************************************************************            
VK       XC    SEQNUM,SEQNUM                                                    
         XC    FAKEFLD,FAKEFLD                                                  
         XC    LEN1,LEN1                                                        
         XC    LEN2,LEN2                                                        
         XC    PRD1,PRD1                                                        
         XC    PRD2,PRD2                                                        
         XC    PRDNM,PRDNM                                                      
         XC    PRD2NM,PRD2NM                                                    
         XC    SPLEN1,SPLEN1                                                    
         XC    SPLEN1LN,SPLEN1LN                                                
         XC    SPLEN2,SPLEN2                                                    
         XC    SPLEN2LN,SPLEN2LN                                                
         XC    PRD1LN,PRD1LN                                                    
         XC    PRD2LN,PRD2LN                                                    
         XC    STLMDN,STLMDN                                                    
         OI    STLMDNH+6,X'80'                                                  
         XC    STLCLN,STLCLN                                                    
         OI    STLCLNH+6,X'80'                                                  
         XC    STLMKN,STLMKN                                                    
         OI    STLMKNH+6,X'80'                                                  
         XC    STLPN1,STLPN1                                                    
         OI    STLPN1H+6,X'80'                                                  
         XC    STLENM,STLENM                                                    
         OI    STLENMH+6,X'80'                                                  
         XC    STLDAY,STLDAY                                                    
         OI    STLDAYH+6,X'80'                                                  
         XC    PWCLTF,PWCLTF                                                    
********************************************************************            
*                                                                               
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         LA    R2,STLMEDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         GOTO1 VALIMED                                                          
         MVC   STLMDN(L'MEDNM),MEDNM                                            
         OI    STLMDNH+6,X'80'                                                  
*                                                                               
         LA    R2,STLCLTH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   STLCLN(L'CLTNM),CLTNM                                            
         OI    STLCLNH+6,X'80'                                                  
         L     R6,AIO           LOOK AT CLIENT RECORD FROM VALICLT              
         USING CLTHDR,R6                                                        
         OC    CPWPCT,CPWPCT                                                    
         BZ    VK05                                                             
         MVI   PWCLTF,C'Y'                                                      
         DROP  R6                                                               
*                                                                               
VK05     LA    R2,STLSTAH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALISTA                                                          
         MVC   STLMKN,MKTNM                                                     
*                                                                               
         OI    STLMKNH+6,X'80'                                                  
*                                                                               
         LA    R2,STLPD1H                                                       
         ST    R2,ACURFORC                                                      
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
*                                                                               
         XC    BLOCK(150),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(0,STLPD1H),(1,BLOCK),C',=,-'                       
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         USING SCANBLKD,R4                                                      
         LA    R4,BLOCK                                                         
         MVC   PRD1,SC1STFLD                                                    
         MVC   PRD2,SC2NDFLD                                                    
         MVC   PRD2LN,SC2NDLEN                                                  
         MVC   PRD1LN,SC1STLEN                                                  
*                                                                               
         CLI   SC1STLEN,3                                                       
         BH    ERRINV                                                           
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLD,11                                                       
         LLC   RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD+8(0),SC1STFLD                                            
         MVC   FAKEFLD+5(L'SC1STLEN),SC1STLEN                                   
         LA    R2,FAKEFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
*                                                                               
         CLI   PRD2LN,0                                                         
         BE    VK10                                                             
         CLI   PRD2LN,3                                                         
         BH    ERRINV                                                           
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLD,11                                                       
         LLC   RE,PRD2LN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD+8(0),PRD2                                                
         MVC   FAKEFLD+5(L'PRD2LN),PRD2LN                                       
         LA    R2,FAKEFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLD,11                                                       
         MVC   BPRD2,BPRD                                                       
         XC    PRD2NM,PRD2NM                                                    
         MVC   PRD2NM,PRDNM                                                     
*                                                                               
         LLC   RE,PRD1LN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD+8(0),PRD1                                                
         MVC   FAKEFLD+5(L'PRD1LN),PRD1LN                                       
         LA    R2,FAKEFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         B     *+10                                                             
*                                                                               
VK10     XC    BPRD2,BPRD2                                                      
         XC    ACURFORC,ACURFORC                                                
*                                                                               
         MVC   BLOCK(100),SPACES                                                
         MVC   BLOCK(L'PRDNM),PRDNM                                             
         OC    PRD2NM,PRD2NM                                                    
         BZ    VK30                                                             
         MVC   BLOCK+6(100),SPACES                                              
         MVI   BLOCK+10,C'/'                                                    
         MVC   BLOCK+13(L'PRD2NM),PRD2NM                                        
VK30     GOTO1 SQUASHER,DMCB,BLOCK,40                                           
         MVC   STLPN1,BLOCK                                                     
         OI    STLPN1H+6,X'80'                                                  
*                                                                               
         DROP  R4                                                               
         LA    R2,STLESTH                                                       
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         MVI   USEIONUM,1                                                       
         GOTO1 VALIEST                                                          
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,STLENM)                               
         GOTO1 DATCON,DMCB,(0,ESTART),(2,ESTARTD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,STLENM+11)                              
         GOTO1 DATCON,DMCB,(0,EEND),(2,ESENDD)                                  
         MVI   STLENM+9,C'-'                                                    
         OI    STLENMH+6,X'80'                                                  
         MVC   MYESTART,ESTART                                                  
         MVC   MYEEND,EEND                                                      
         XC    COMDLIST,COMDLIST                                                
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         BNH   *+10                                                             
         MVC   COMDLIST,ENONTDMS                                                
         DROP  R6                                                               
*                                                                               
         LA    R2,STLDPTH                                                       
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         CLI   STLDPT,C'Z'                                                      
         BH    ERRINV                                                           
         CLI   STLDPT,C'A'                                                      
         BL    ERRINV                                                           
*****************  SPOT LENGTH 1 *******************                            
VK70     LA    R2,STLLN1H                                                       
         ST    R2,ACURFORC                                                      
         CLI   0(R2),0                                                          
         BE    ERRINV                                                           
*                                                                               
         XC    BLOCK(150),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(0,STLLN1H),(1,BLOCK),C',=,-'                       
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         USING SCANBLKD,R4                                                      
         LA    R4,BLOCK                                                         
         MVC   SPLEN1,SC1STFLD                                                  
         MVC   SPLEN2,SC2NDFLD                                                  
         MVC   SPLEN2LN,SC2NDLEN                                                
         MVC   SPLEN1LN,SC1STLEN                                                
*                                                                               
         CLI   SPLEN1LN,3                                                       
         BH    ERRINV                                                           
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLD,10                                                       
         LLC   RE,SPLEN1LN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD+8(0),SPLEN1                                              
         MVC   FAKEFLD+5(L'SPLEN1LN),SPLEN1LN                                   
         LA    R2,FAKEFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALISLN                                                          
         BAS   RE,SLEN                                                          
         MVC   LEN1,BYTE                                                        
*                                                                               
         CLI   SPLEN2LN,0                                                       
         BE    VK90                                                             
         CLI   SPLEN2LN,3                                                       
         BH    ERRINV                                                           
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLD,10                                                       
         LLC   RE,SPLEN2LN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD+8(0),SPLEN2                                              
         MVC   FAKEFLD+5(L'SPLEN2LN),SPLEN2LN                                   
         LA    R2,FAKEFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALISLN                                                          
*****************  SPOT LENGTH 2  **********************                        
         BAS   RE,SLEN                                                          
         MVC   LEN2,BYTE                                                        
         B     *+10                                                             
VK90     XC    LEN2,LEN2                                                        
         XC    ACURFORC,ACURFORC                                                
         DROP  R4                                                               
*****************  BUILD THE KEY  * 0D73 ***************                        
VK100    BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SLKRECD,R4                                                       
         MVI   SLKKTYP,SLKKTYPQ                                                 
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,BAGYMD                                                  
         MVC   SLKKCLT,BCLT                                                     
         MVC   SLKKMKT,BMKT                                                     
         MVC   SLKKSTA,BSTA                                                     
         MVC   SLKKPRD,BPRD                                                     
         MVC   SLKKPRD2,BPRD2                                                   
         MVC   SLKKEST,BEST                                                     
         MVC   SLKKDPT,STLDPT                                                   
         MVC   SLKKLEN,LEN1                                                     
         MVC   SLKKLEN2,LEN2                                                    
         DROP  R4                                                               
*                                                                               
*                                                                               
VKX      MVC  AIO,AIO1                                                          
         B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
*&&UK                                                                           
*******************************************************************             
*                      VALREC                                                   
*  PRECONDITIONS: A TABLE BUILT FROM WHATS ON THE SCREEN                        
*  POSTCONDITIONS: THE TABLE HAS ONLY CHANGED OR NEW ELEMENTS                   
*                  WHICH NEED TO BE ADDED                                       
*******************************************************************             
*                                                                               
VR       CLI   PWCLTF,C'Y'                                                      
         BNE   VR05                                                             
         GOTO1 =A(PROTWK),DMCB,RR=Y                                             
*                                                                               
VR05     MVI   ELCODE,LOKELCDQ                                                  
         XC    ERRFLAG,ERRFLAG                                                  
*  GET THE LENGTH OF EACH LINE                                                  
*        LA    RE,STLD4H                                                        
         LA    RE,STLD3H                                                        
         LA    R2,STLWKH                                                        
         SR    RE,R2                                                            
         STCM  RE,15,LINELN                                                     
*  GET THE DISPLACEMENT FROM THE WEEK FIELD TO DAY OF THE WEEK FIELD            
         LA    RE,STLDA1H                                                       
         LA    R2,STLWKH                                                        
         SR    RE,R2                                                            
         STCM  RE,15,DAYLLN                                                     
*                                                                               
VR10     L     R6,AIO                                                           
         USING LOKEL,R6                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R5,TABLE                                                         
         USING TABLED,R5                                                        
*                                                                               
         LA    R4,PTABLE                                                        
         USING PTABLED,R4                                                       
         XC    0(L'PTABLE,R4),0(R4)                                             
         XC    PTABEND,PTABEND                                                  
*                                                                               
         BAS   RE,GETEL                                                         
         BE    VR30                                                             
         XC    STLDAY,STLDAY      STLDAY HAS  DAY OF WEEK THAT IS VALID         
         OI    STLDAYH+6,X'80'                                                  
         LA    R2,STLWKH                                                        
         B     VR50                                                             
*                                                                               
VR30     LA    R2,STLWKH                                                        
         LLC   RE,LOKELLN                                                       
         STC   RE,ELEMLN                                                        
         AR    RE,R6                                                            
         LR    R3,RE              R6 POINTS TO FIRST 03 ELEMENT                 
         B     VR45                                                             
*                                                                               
VR40     BAS   RE,NEXTEL                                                        
         BE    VR45                                                             
         BAS   RE,CLRDAY         NO ELEMENTS CLEAR DAY OF LINE                  
         B     VR50                                                             
VR45     CLC   STARTWK,LOKWEEK   START GETTING ELEMENTS FROM STARTAT            
         BNH   VR50                                                             
* COUNTER                                                                       
         B     VR40                                                             
*                                                                               
VR50     XC    DATETEMP,DATETEMP                                                
*    THIS PART SEES IF ITS A NEW LINE OR AN EMPTY FIELD                         
         LR    RE,R2                                                            
         ZICM  R0,DAYLLN,4                                                      
         AR    RE,R0                                                            
         OC    8(L'DAY,RE),8(RE)                                                
         BNZ   VR60                                                             
*                                                                               
*    IF NO DAY AND NO WEEK INPUT IT MUST BE A BLANK LINE SO SKIP                
         OC    8(L'TABWEEK,R2),8(R2)                                            
         BNZ   VR60                                                             
         ZICM  RE,LINELN,4                                                      
         AR    R2,RE                                                            
         XC    TABWEEK(L'TABREC),TABWEEK                                        
         B     VR200                                                            
************************ VALIDATE EACH LINE *************************           
*                                                                               
VR60     GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         CLC   DATETEMP,=C'000000'                                              
         BE    ERRINV                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,DATETEMP),(2,ELEM)                                
         GOTO1 DATCON,DMCB,(0,DATETEMP),(10,8(R2))                              
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 GETDAY,DMCB,DATETEMP,DAY                                         
         OC    DAY,DAY                                                          
         BZ    ERRINV                                                           
*   IF ITS SECOND ELEMENT WE CAN CHANGE THE DAY OF WEEK                         
         CR    R6,R3                                                            
         BE    VR85                                                             
         B     VR90                                                             
*    SECOND ELEMENT DISPLAY THE DAY OF WEEK                                     
VR85     XC    STLDAY,STLDAY                                                    
         MVC   STLDAY(L'DAY),DAY                                                
         OI    STLDAYH+6,X'80'                                                  
*                                                                               
*   THIS PART  CHECKS IF DAY OF WEEK OF DATE ENTERED MATCHES VALID DAY          
VR90     LR    RE,R2                                                            
         ZICM  R0,DAYLLN,4                                                      
         AR    RE,R0                                                            
         MVC   8(L'DAY,RE),DAY                                                  
         OI    6(RE),X'80'                                                      
         CLC   8(L'DAY,RE),STLDAY                                               
         BE    VR138   WAS 130                                                  
         CR    R6,R3                                                            
         BL    VR100                                                            
         BE    VR100                                                            
         OC    STLDAY,STLDAY                                                    
         BZ    VR100                                                            
         BAS   RE,UNMARK10                                                      
         B     ERRWEEK2                                                         
*    IF 1ST AND NOT EQUAL IT MUST BE EST START DATE                             
VR100    CR    R6,R3                                                            
         BL    VR135                                                            
         B     VR138                                                            
*    ONLY FIRST DATE IS WILL HAVE TO MEET THIS REQUIREMENT                      
VR135    CLC   ESTARTD,ELEM                                                     
         BE    VR138                                                            
         OC    STLDAY,STLDAY                                                    
         BZ    *+12                                                             
         BAS   RE,UNMARK10                                                      
         B     ERRWEEK4                                                         
*                                                                               
* THIS PART CHECKS IF DATES  ARE WITH EST START AND END DATE                    
VR138    CLC   ESTARTD,ELEM                                                     
         BNH   *+12                                                             
         BAS   RE,UNMARK10                                                      
         B     ERRWEEK3                                                         
*                                                                               
         CLC   ESENDD,ELEM                                                      
         BNL   *+12                                                             
         BAS   RE,UNMARK10                                                      
         B     ERRWEEK3                                                         
*                                                                               
         XC    TESTWK,TESTWK                                                    
         MVC   TESTWK,ELEM                                                      
*                                                                               
         CLC   2(L'LOKWEEK,R6),ELEM                                             
         BE    VR140                                                            
         BAS   RE,CHKWK                                                         
         BC    8,ERRWEEK                                                        
         BAS   RE,DUPWK                                                         
*                                                                               
*   WRITE IN SPOT LENGTH AFTER VALIDATION                                       
VR140    BAS   RE,NXTFLD                                                        
         CLC   8(3,R2),=C'DEL' IF EQUALS "DEL" WE DELETE THIS ELEMENT           
         BNE   VR150                                                            
         BAS   RE,NXTFLD       SKIP DOLLAR SIGN                                 
         BAS   RE,NXTFLD       SKIP DOLLAR                                      
         BAS   RE,NXTFLD       SKIP DOLLAR                                      
         BAS   RE,NXTFLD       SKIP DEMOVALUES                                  
         BAS   RE,NXTFLD                                                        
         BAS   RE,NXTFLD                                                        
         BAS   RE,NXTFLD                                                        
         XC    ELEM,ELEM                                                        
         B     VR185                                                            
*                                                                               
VR150    TM    4(R2),X'08'                                                      
         BNO   ERRINV                                                           
*                                                                               
         XC    DUB,DUB                                                          
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,ELEM+2                                                      
*                                                                               
*   WRITE IN DOLLARS AFTER VALIDATION                                           
         BAS   RE,NXTFLD                     SKIP THE DOLLAR SIGN FIELD         
         BAS   RE,NXTFLD                                                        
*        TM    4(R2),X'08'                                                      
*        BNO   ERRINV                                                           
*                                                                               
*        XC    DUB,DUB                                                          
*        LLC   RE,5(R2)                                                         
*        BCTR  RE,0                                                             
*        EX    RE,*+8                                                           
*        B     *+10                                                             
*        PACK  DUB,8(0,R2)                                                      
*        CVB   RE,DUB                                                           
*        STCM  RE,15,ELEM+4                                                     
*                                                                               
         LLC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(2,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         MVC   ELEM+4(4),DMCB+4                                                 
* DAY OF WEEK                                                                   
         BAS   RE,NXTFLD                                                        
****************  DEMO VALUES  ********************                             
VR160    BAS   RE,NXTFLD                                                        
         LLC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         MVC   ELEM+8(4),DMCB+4                                                 
*                                                                               
VR165    BAS   RE,NXTFLD                                                        
         OC    STLDM2,STLDM2                                                    
         BZ    VR170                                                            
         LLC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         MVC   ELEM+12(4),DMCB+4                                                
*                                                                               
VR170    BAS   RE,NXTFLD                                                        
         OC    STLDM3,STLDM3                                                    
         BZ    VR175                                                            
         LLC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,8(R2))                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         MVC   ELEM+16(4),DMCB+4                                                
*                                                                               
VR175    DS    0H                                                               
*VR175    BAS   RE,NXTFLD                                                       
*         OC    STLDM4,STLDM4                                                   
*         BZ    VR180                                                           
*         LLC   RE,5(R2)                                                        
*         ST    RE,DMCB+4                                                       
*         GOTO1 CASHVAL,DMCB,(1,8(R2))                                          
*         CLI   DMCB,X'FF'                                                      
*         BE    ERRINV                                                          
*         MVC   ELEM+20(4),DMCB+4                                               
*****************************************************************               
*     ADD NEW ELEMENT                                                           
VR180    CLI   LOKELCD,LOKELCDQ                                                 
         BNE   VR190                                                            
         LLC   RE,LOKELLN         COMP FOR LENGTH OF ELEMENT                    
         BCTR  RE,0               MINUS ELEMENT CODE                            
         BCTR  RE,0               AND   ELEMENT LENGTH                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ELEM(0),2(R6)                                                    
*        CLC   ELEM(L'TABREC),2(R6)                                             
         BNE   VR185                                                            
         XC    TABWEEK(L'TABREC),TABWEEK                                        
         B     VR200                                                            
*                                                                               
VR185    MVI   0(R6),X'FF'                                                      
VR190    XC    TABWEEK(L'TABREC),TABWEEK                                        
         MVC   TABWEEK(L'TABREC),ELEM                                           
* NEW TABLE FOR PW RECORDS                                                      
         MVC   PWEEK,TABWEEK                                                    
         MVC   PDOLS,TABDOLS                                                    
         MVC   POLDWK,LOKWEEK                                                   
         MVC   POLDDOLS,LOKDOLS                                                 
         MVC   PSPOTS,TABSPOTS                                                  
         MVC   POLDSPTS,LOKSPOTS                                                
         LA    R4,L'PTABREC(R4)                                                 
*                                                                               
         CLC   ELEM(L'TABWEEK),2(R6)                                            
         BE    *+8                                                              
         MVI   TABFLAG,X'F'                                                     
*                                                                               
*    COMP TO SEE IF AT END OF SCREEN IF SO JUST EXIT                            
VR200    BAS   RE,NXTFLD                                                        
         LA    R0,STLLAST                                                       
         CR    R2,R0                                                            
         BE    VRX                                                              
         BH    VRX                                                              
         LA    R5,L'TABREC(R5)                                                  
         B     VR40                                                             
*********************************************************                       
VRX      MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*   SEARCH TABLE FOR NEW ELEMENTS TO ADD AND RECORD EARLIEST DATE               
         LA    R5,TABLE                                                         
         XC    LOW,LOW                                                          
         MVC   LOW,TABWEEK                                                      
VRX10    LA    R0,TABEND                                                        
         CR    R5,R0                                                            
         BE    VRXX                                                             
         XC    ELEM,ELEM                                                        
         OC    TABWEEK(L'TABREC-1),TABWEEK      LAST BYTE HAS FLAG              
         BZ    VRX20                                                            
         MVI   ELEM,LOKELCDQ                                                    
*        MVI   ELEM+1,26                                                        
         MVC   ELEM+1(L'ELEMLN),ELEMLN                                          
         MVC   ELEM+2(L'TABREC-1),TABWEEK                                       
         GOTO1 ADDELEM                                                          
         OC    LOW,LOW                                                          
         BZ    *+14                                                             
         CLC   LOW,TABWEEK                                                      
         BL    VRX20                                                            
         CLI   TABFLAG,X'F'      F MARKS ELEMENTS WHERE ONLY THE WEEK           
         BE    *+14              CHANGED.  START AT ONLY WHEN WEEK              
         XC    LOW,LOW           CHANGED.                                       
         B     VRX20                                                            
         MVC   LOW,TABWEEK                                                      
VRX20    LA    R5,L'TABREC(R5)                                                  
         B     VRX10                                                            
*                                                                               
VRXX     OC    LOW,LOW                                                          
         BZ    VRXXX                                                            
         GOTO1 DATCON,DMCB,(2,LOW),(10,STLSWKH+8)                               
         OI    STLSWKH+6,X'80'                                                  
VRXXX    B     DR                                                               
*                                                                               
*                                                                               
*                                                                               
*&&                                                                             
*                                                                               
*                                                                               
VR       DS    0H                                                               
         B     DR                                                               
*                                                                               
*******************************************************************             
*        DISPREC                                                                
*******************************************************************             
DR       BAS   RE,CLR                                                           
         XC    BYTE,BYTE        USED AS FLAG                                    
         XC    HALF,HALF                                                        
*                                                                               
         LA    R3,TABLE                                                         
         XCEF  (R3),325                                                         
         USING TABLED,R3                                                        
*  OUTPUT WEEK DATE, WAS IN COMPRESSED FORM                                     
         LA    R2,STLSWKH                                                       
         XC    HALF,HALF                                                        
         XC    STARTWK,STARTWK                                                  
         CLI   5(R2),0                                                          
         BE    DR20                                                             
         XC    DATETEMP,DATETEMP                                                
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         CLC   DATETEMP,=C'000000'                                              
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(2,HALF)                                
         XC    STARTWK,STARTWK                                                  
         MVC   STARTWK,HALF                                                     
*                                                                               
DR20     L     R6,AIO              A(SLOCKIN RECORD)                            
         MVI   ELCODE,X'50'        COMSCORE DEMO NAME ELEM                      
         BAS   RE,GETEL            HAVE ONE?                                    
         BNE   DR25                NO - DEFUALT TO EST COMSCORE NAMES           
         XC    COMDLIST,COMDLIST   CLEAR THE COMSCORE DEMO NAMES                
         LLC   RE,1(R6)            ELEMENT LENGTH                               
         SHI   RE,3                -2 FOR ELEM OVERHEAD -1 FOR EX               
         EX    RE,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   COMDLIST(0),2(R6)   ** EXECUTED **                               
*                                                                               
DR25     L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     DR50                                                             
*                                                                               
         USING  SLKEL,R6                                                        
*                                                                               
         XC     DBLIST,DBLIST                                                   
         MVC    DBLIST(3),SLKDEM1                                               
         MVC    DBLIST+3(3),SLKDEM2                                             
         MVC    DBLIST+6(3),SLKDEM3                                             
         MVC    DBLIST+9(3),SLKDEM4                                             
         MVI    DBLIST+12,X'FF'                                                 
         LA     R4,DBLIST                                                       
*                                                                               
         XC     ELEM,ELEM                                                       
         LA     R5,ELEM                                                         
         USING  DBLOCK,R5                                                       
         MVC    DBCOMFCS,ACOMFACS                                               
         MVC    DBFILE,=C'TP '                                                  
***      MVC    DBSELMED,STLMED                                                 
         MVI    DBSELMED,C'T'      DEFAULT TO MEDIA T                           
         CLI    STLMED,C'R'        MEDIA R?                                     
         BNE    *+12               NO                                           
         MVI    DBSELMED,C'R'      YES - SET MEDIA R                            
         B      DR25A              DONE                                         
*                                                                               
         CLI    SVCXTRA,C'U'       US DEMOS?                                    
         BE     DR25A              YES - LEAVE AS MEDIA T                       
*                                                                               
         CLI    SVAPROF+7,C'C'     CANADIAN AGENCY?                             
         BNE    DR25A              NO                                           
         MVI    DBSELMED,C'C'      YES - MEDIA IS ALWAYS C                      
*                                                                               
DR25A    L      RE,AIO2                                                         
         XC     0(60,RE),0(RE)                                                  
         GOTO1 DEMOCON,DMCB,(6,0(R4)),(2,AIO2),(C'S',DBLOCK),0,COMDLIST         
         DROP   R5                                                              
*                                                                               
         L      R6,AIO2               A(DEMOS)                                  
         LA     R2,STLDM1H            A(FIRST DEMO HEADER)                      
         LA     R1,4                  4 DEMOS                                   
*                                                                               
DR30     MVC    9(6,R2),0(R6)         ASSUME 6 CHARACTER DEMO                   
         CLI    6(R6),X'40'           7 CHARACTER DEMO NAME?                    
         BNH    *+10                  NO                                        
         MVC    8(7,R2),0(R6)         MOVE 7 CHARACTER DEMO NAME                
         OI     6(R2),X'80'           TRANSMIT FIELD                            
         BAS    RE,NXTFLD             BUMP TO NEXT DEMO FIELD ON SCREEN         
         LA     R6,7(R6)              BUMP TO NEXT DEMO NAME                    
         BCT    R1,DR30               PROCESS NEXT DEMO                         
*                                                                               
DR50     L      R6,AIO                                                          
         USING  LOKEL,R6                                                        
*        XC     BYTE,BYTE                                                       
         LA     R2,STLWKH                                                       
         MVI    ELCODE,X'03'                                                    
         BAS    RE,GETEL                                                        
         BNE    DRX                                                             
*                                                                               
         B     DR100                                                            
*                                                                               
DR80     BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
*                                                                               
DR100    CLC   HALF,LOKWEEK                                                     
         BNH   DR120                                                            
* COUNTER                                                                       
         B     DR80                                                             
*                                                                               
DR120    GOTO1 DATCON,DMCB,(2,LOKWEEK),(10,8(R2))                               
         OI    6(R2),X'80'                                                      
         XC    DATETEMP,DATETEMP                                                
         XC    DAY,DAY                                                          
         GOTO1 DATCON,DMCB,(2,LOKWEEK),(0,DATETEMP)                             
         GOTO1 GETDAY,DMCB,DATETEMP,DAY                                         
         OC    DAY,DAY                                                          
         BZ    ERRINV                                                           
         MVC   TABWEEK,LOKWEEK                                                  
*  OUTPUT NUMBER OF SPOTS                                                       
         BAS   RE,NXTFLD                                                        
         EDIT  (B2,LOKSPOTS),(L'STLSPT,8(R2)),ZERO=NOBLANK,ALIGN=LEFT           
         OI    6(R2),X'80'                                                      
         MVC   TABSPOTS,LOKSPOTS                                                
*  OUTPUT DOLLAR VALUE                                                          
*        BAS   RE,NXTFLD                                                        
*        MVI   8(R2),C'$'                                                       
*        OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NXTFLD                                                        
         EDIT  (B4,LOKDOLS),(L'STLDOL,8(R2)),2,ZERO=NOBLANK,ALIGN=LEFT          
         OI    6(R2),X'80'                                                      
         MVC   TABDOLS,LOKDOLS                                                  
*  OUTPUT COST2                                                                 
         BAS   RE,NXTFLD                                                        
         EDIT  (B4,LOKDOL2),(L'STLDOL2,8(R2)),2,ZERO=NOBLANK,ALIGN=LEFT         
         OI    6(R2),X'80'                                                      
*                                                                               
*        BAS   RE,NXTFLD                                                        
*        MVC   8(L'DAY,R2),DAY                                                  
*        OI    6(R2),X'80'                                                      
*  OUTPUT DEMO VALUES (RATINGS)                                                 
         BAS   RE,NXTFLD                                                        
         ICM   R1,15,LOKDEM                                                     
         BAS   RE,EDTDEM                                                        
         OI    STLD1H+6,X'80'                                                   
         MVC   TABDEM1,LOKDEM                                                   
*                                                                               
         BAS   RE,NXTFLD                                                        
         OC    STLDM2,STLDM2                                                    
         BZ    DR140                                                            
         ICM   R1,15,LOKDEM+4                                                   
         BAS   RE,EDTDEM                                                        
         OI    STLD2H+6,X'80'                                                   
         MVC   TABDEM2,LOKDEM+4                                                 
*                                                                               
DR140    BAS   RE,NXTFLD                                                        
         OC    STLDM3,STLDM3                                                    
         BZ    DR150                                                            
         ICM   R1,15,LOKDEM+8                                                   
         BAS   RE,EDTDEM                                                        
         OI    STLD3H+6,X'80'                                                   
         MVC   TABDEM3,LOKDEM+8                                                 
*                                                                               
DR150    BAS   RE,NXTFLD                                                        
         OC    STLDM4,STLDM4                                                    
         BZ    DR200                                                            
         ICM   R1,15,LOKDEM+12                                                  
         BAS   RE,EDTDEM                                                        
         OI    STLD4H+6,X'80'                                                   
         MVC   TABDEM4,LOKDEM+12                                                
         B     DR200                                                            
*                                                                               
EDTDEM   SLA   R1,2                STRIP OFF POSSIBLE X'40' BIT                 
         BO    EDTDEM2             ON OVERFLOW, IT'S A 2 DECIMAL DEMO           
         SRA   R1,2                                                             
         EDIT  (R1),(8,8(R2)),1                                                 
         BR    RE                                                               
EDTDEM2  SRL   R1,2                                                             
         EDIT  (R1),(8,8(R2)),2                                                 
         BR    RE                                                               
*                                                                               
********** COMP TO SEE IF AT END OF SCREEN IF SO EXIT ********                  
DR200    BAS   RE,NXTFLD              WRAP AROUND TO NEXT SCREEN LINE           
         LA    R0,STLLAST                                                       
         CR    R2,R0                                                            
         BNL   DRX                                                              
         LA    R3,L'TABREC(R3)                                                  
         MVI   BYTE,X'F'                                                        
         B     DR80                                                             
         DROP R6                                                                
         DROP  R3                                                               
*                                                                               
DRX      B     XIT                                                              
*******************************************************************             
*================  XRECPUT  =====================================               
*  ON ENTRY TABLE HAS THE ENTIRE COPY OF THE ELEMENTS ON SCREEN                 
* AFTER CHANGE MADE TO SCREEN.  ON EXIT OF VREC THE TABLE ONLY HAS              
* ELEMENTS OF THE CHANGED ELEMENTS, BUT DISPREC REBUILDS THE TABLE              
* TO WHAT IS DISPLAYED AFTER ALL CHANGES                                        
*******************************************************************             
XR       BAS   RE,RSV              RESET SYSTEM VALUES                          
         BAS   RE,BLTPWTAB                                                      
*                                                                               
XR05     LA    R2,STLCLTH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         L     R6,AIO           LOOK AT CLIENT RECORD FROM VALICLT              
         USING CLTHDR,R6                                                        
         OC    CPWPCT,CPWPCT                                                    
         BZ    XRX                                                              
         DROP  R6                                                               
* PW CLIENT                                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PWRECD,R4                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMD                                                   
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD                                                      
         MVC   PWKEST,BEST                                                      
         MVC   PWKMKT,BMKT                                                      
         MVC   PWKSTA,BSTA                                                      
*                                                                               
XR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'PWFKEY),KEYSAVE                                            
         BNE   NOPWREC                                                          
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,PWTABLE                                                       
         USING PWTABLED,R5                                                      
*                                                                               
*                                                                               
         MVI   ELCODE,PWDOLCDQ                                                  
XR20     ZICM  RE,ATABEND,4      ADDRESS OF END TABLE                           
         CR    R5,RE                                                            
         BNL   XR150                                                            
         OC    PWDOLS,PWDOLS     IF NO DOLLARS CHANGED GET NEXT PWTABLE         
         BNZ   *+14              ENTRY                                          
         OC    PWSPOTS,PWSPOTS   IF NO DOLLARS CHANGED GET NEXT PWTABLE         
         BZ    XR105             ENTRY                                          
*        BZ    XR110             ENTRY                                          
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   NOPWERR                                                          
         USING PWDOLEL,R6                                                       
XR25     CLC   PWDOLWK,ESTARTD   CHECK IF PW AND EST DATES ARE UP TO            
         BNE   BADEST            DATE, AND CONSISTENT                           
         B     XR50                                                             
*                                                                               
XR30     BAS   RE,NEXTEL                                                        
         BE    XR50                                                             
         XC    STLSWK,STLSWK                                                    
*        B     NOPWEL                                                           
NOPWERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'PW WEEKLY DOLLAR ELEMENT MISSING'                 
         MVC   CONHEAD+33(14),=C'(MARKET LEVEL)'                                
         OC    PWKSTA,PWKSTA                                                    
         BZ    *+10             MARKET LEVEL SKIP NEXT MOVE                     
         MVC   CONHEAD+33(15),=C'(STATION LEVEL)'                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,PWSTART),(10,CONHEAD+51)                          
*                                                                               
         OI    CONHEADH+6,X'80'                                                 
         DC    H'0',C'$ABEND'                                                   
*                                                                               
XR50     CLC   PWSTART,PWDOLWK                                                  
         BNE   XR30                                                             
         OC    PWDOLS,PWDOLS     IF NO DOLLARS CHANGED GET NEXT PWTABLE         
         BZ    XR105             ENTRY                                          
* FOUND ELEM TO CHANGE WITH DATE WE WANT                                        
         L     R3,AIO3                                                          
         USING PWBLKD,R3                                                        
         XC    0(PWBLKL,R3),0(R3)                                               
         MVI   PWACT,PWGETPW                                                    
         MVC   PWACTBUY,PWDOLWG                                                 
         MVC   PWADJBUY,PWDOLCG                                                 
         MVC   PWPCT,MYPWPCT                                                    
         GOTO1 SPPWCALC,DMCB,(R3)                                               
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYPWPCT,PWVAL           SAVE PW PCT                              
         DROP  R3                                                               
*                                                                               
         ZICM  RE,PWDOLWG,4                                                     
         ZICM  R0,PWDOLS,4                                                      
         CLI   PWSTATUS,C'+'                                                    
         BNE   *+14                                                             
         AR    RE,R0                                                            
         STCM  RE,15,PWDOLWG                                                    
         B     XR100                                                            
*                                                                               
         CLI   PWSTATUS,C'-'  '+' OR '-' REPRESENTS CREDIT OR DEBIT             
         BNE   *+14                                                             
         SR    RE,R0                                                            
         STCM  RE,15,PWDOLWG                                                    
         B     XR100                                                            
         DC    H'0'           DEATH IF NOT +, OR '-' TABLE ERROR                
*                                                                               
*                                                                               
XR100    ZICM  R3,PWDOLWG,4                                                     
         LA    RE,85                                                            
         MR    R2,RE                                                            
         LA    RE,100                                                           
         DR    R2,RE        RESULT IN R3                                        
         STCM  R3,15,PWDOLWN                                                    
*  CALC PWDOLCG                                                                 
         L     R3,AIO3                                                          
         USING PWBLKD,R3                                                        
         XC    0(PWBLKL,R3),0(R3)                                               
         MVI   PWACT,PWGETBUY                                                   
         MVC   PWACTBUY,PWDOLWG                                                 
         MVC   PWPCT,MYPWPCT                                                    
         MVC   PWTAX,PWDOLTAX                                                   
         GOTO1 SPPWCALC,DMCB,(R3)                                               
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PWDOLCG,PWVAL                                                    
         DROP  R3                                                               
*  CALC PWDOLCN                                                                 
         L     R3,AIO3                                                          
         USING PWBLKD,R3                                                        
         XC    0(PWBLKL,R3),0(R3)                                               
         MVI   PWACT,PWGETBUY                                                   
         MVC   PWACTBUY,PWDOLWN                                                 
         MVC   PWPCT,MYPWPCT                                                    
         MVC   PWTAX,PWDOLTAX                                                   
         GOTO1 SPPWCALC,DMCB,(R3)                                               
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PWDOLCN,PWVAL           SAVE PW PCT                              
* LETS ADD THE DIFFERENCE IN SPOT ALSO                                          
*                                                                               
XR105    OC    PWSPOTS,PWSPOTS  IF NO SPOTS WERE CHANGED                        
         BZ    XR110                                                            
         ZICM  RE,PWDOLSPT,4                                                    
****     OR    RE,RE                                                            
****     BNZ   *+6                                                              
****     DC    H'0'             SPOTS CAN'T BE ZERO!!!                          
         ZICM  RF,PWSPOTS,2                                                     
         CLI   PWSTSTAT,C'+'                                                    
         BE    *+10                                                             
         SR    RE,RF                                                            
         B     *+6                                                              
         AR    RE,RF                                                            
         C     RE,=F'0'        SPOTS CAN'T BE NEGATIVE,IF SO SOMETHING          
         BNL   *+6             IS REALLY FUCKED UP.                             
         DC    H'0'                                                             
         ST    RE,PWDOLSPT                                                      
*                                                                               
         DROP  R3                                                               
*                                                                               
*                                                                               
XR110    LA    R5,L'PWTABREC(R5)                                                
         B     XR20                                                             
         DROP R6                                                                
*                                                                               
*R150    GOTO1 PUTREC                                                           
*  CHECK OVERRIDE ELEMENT FOR WEEKS CHANGED                                     
**************************************************************                  
XR150    LA    R5,PWTABLE                                                       
         USING PWTABLED,R5                                                      
         MVI   ELCODE,PWCLLCDQ                                                  
*                                                                               
XR160    ZICM  RE,ATABEND,4      ADDRESS OF END TABLE                           
         CR    R5,RE                                                            
         BNL   XR250                                                            
         OC    PWDOLS,PWDOLS     IF NO DOLLARS CHANGED GET NEXT PWTABLE         
         BZ    XR210             ENTRY                                          
*                                                                               
         L     R6,AIO                                                           
         USING PWCLLEL,R6                                                       
         BAS   RE,GETEL                                                         
         BE    XR180                                                            
         B     XR250                                                            
*                                                                               
XR170    BAS   RE,NEXTEL                                                        
         BE    XR180                                                            
         B     XR210                                                            
*                                                                               
XR180    CLC   PWSTART,PWCLLWK  CMP WK  CHANGED WITH OVERRIDE WEEK              
         BNE   XR170                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'MUST REMOVE PW OVERRIDE BEFORE CHANGING'          
         OI    CONHEADH+6,X'80'                                                 
         DC    H'0',C'$ABEND'                                                   
*                                                                               
XR210    LA    R5,L'PWTABREC(R5)                                                
         B     XR160                                                            
*                                                                               
         DROP R6                                                                
*                                                                               
XR250    GOTO1 PUTREC           PUTREC ON STA/MKT LEVEL PW REC                  
**************************************************************                  
*------- DO EXACTLY THE SAME FOR MARKET LEVEL--------------*                    
XR300    DS    0X                                                               
         OC    PWKSTA,PWKSTA                                                    
         BZ    XRX            IF ALREADY DID MARKET LEVEL THEN END              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PWRECD,R4                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMD                                                   
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD                                                      
         MVC   PWKEST,BEST                                                      
         MVC   PWKMKT,BMKT                                                      
         XC    PWKSTA,PWKSTA                                                    
         B     XR10                                                             
**************************************************************                  
XRX      B     XIT                                                              
*******************************************************************             
*        DISPKEY                                                                
*******************************************************************             
*                                                                               
DK       XC     PRD1,PRD1                                                       
         XC     PRD2,PRD2                                                       
         XC     PRD2NM,PRD2NM                                                   
         XC     PRDNM,PRDNM                                                     
         XC     PRDNUM,PRDNUM                                                   
         XC     PRDNUM2,PRDNUM2                                                 
         XC     PWCLTF,PWCLTF                                                   
*        XC     STLPD1,STLPD1                                                   
         MVC    AIO,AIO1                                                        
*                                                                               
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         L      R6,AIO                                                          
         USING  SLKRECD,R6                                                      
         MVC    MEDIA,SLKKAGMD                                                  
         MVC    PRDNUM,SLKKPRD                                                  
         MVC    PRDNUM2,SLKKPRD2                                                
         MVC    ESTIM,SLKKEST                                                   
         MVC    BMKT,SLKKMKT                                                    
         MVC    BSTA,SLKKSTA                                                    
*                                                                               
         LA     R2,STLMEDH                                                      
         MVI    USEIONUM,2                                                      
         GOTO1  VALIMED                                                         
         MVC    STLMDN(L'MEDNM),MEDNM                                           
         OI     STLMDNH+6,X'80'                                                 
*                                                                               
         LA     R2,STLCLTH                                                      
         MVI    USEIONUM,2                                                      
         GOTO1  VALICLT                                                         
         MVC    STLCLN(L'CLTNM),CLTNM                                           
         OI     STLCLNH+6,X'80'                                                 
         L     RE,AIO2          LOOK AT CLIENT RECORD FROM VALICLT              
         USING CLTHDR,RE                                                        
         OC    CPWPCT,CPWPCT                                                    
         BZ    *+8                                                              
         MVI   PWCLTF,C'Y'                                                      
         DROP  RE                                                               
*                                                                               
         GOTO1  MSUNPK,DMCB,BMKTSTA,FULL,STLSTA                                 
         OI     STLSTAH+6,X'80'                                                 
*                                                                               
         LA     R2,STLSTAH                                                      
         MVI    USEIONUM,2                                                      
         GOTO1  VALISTA                                                         
         MVC   STLMKN,QMKT                                                      
         MVC   STLMKN+5(L'STLMKN-5),MKTNM                                       
         OI     STLMKNH+6,X'80'                                                 
*                                                                               
         MVC    STLDPT(L'SLKKDPT),SLKKDPT                                       
         OI     STLDPTH+6,X'80'                                                 
*                                                                               
         XC     STLLN1,STLLN1                                                   
         EDIT   (B1,SLKKLEN),(3,STLLN1),ALIGN=LEFT                              
         CLI    SLKKLEN2,0                                                      
         BE     DK40                                                            
         CLI    SLKKLEN,100                                                     
         BNL    DK30                                                            
         EDIT   (B1,SLKKLEN2),(3,STLLN1+3),ALIGN=LEFT                           
         MVI    STLLN1+2,C'-'                                                   
         B      DK40                                                            
DK30     EDIT   (B1,SLKKLEN2),(3,STLLN1+4),ALIGN=LEFT                           
         MVI    STLLN1+3,C'-'                                                   
DK40     OI     STLLN1H+6,X'80'                                                 
**************** PRODUCT *****************************                          
*                                                                               
         DROP   R6                                                              
         LA     R2,STLCLTH                                                      
         MVI    USEIONUM,2                                                      
         GOTO1  VALICLT                                                         
*                                                                               
         L      R6,AIO                                                          
         USING  CLTHDR,R6                                                       
         LA     R5,CLIST                                                        
         LA     R2,STLPD1H                                                      
*                                                                               
DK50     CLC    PRDNUM,3(R5)                                                    
         BE     *+12                                                            
         LA     R5,4(R5)                                                        
         B      DK50                                                            
         XC     STLPD1,STLPD1                                                   
         OI     STLPD1H+6,X'80'                                                 
         MVC    STLPD1(3),0(R5)                                                 
         MVC    PRD1,0(R5)                                                      
*                                                                               
         LA     R2,FAKEFLD                                                      
         XC     FAKEFLD,FAKEFLD                                                 
         MVC    FAKEFLD+8(3),PRD1                                               
         MVI    FAKEFLD+5,3                                                     
         MVI    USEIONUM,3                                                      
         GOTO1  VALIPRD                                                         
*                                                                               
         CLI    PRDNUM2,0                                                       
         BE     DK500                                                           
         LA     R5,CLIST                                                        
DK80     CLC    PRDNUM2,3(R5)                                                   
         BE     *+12                                                            
         LA     R5,4(R5)                                                        
         B      DK80                                                            
         MVI    STLPD1+3,C'-'                                                   
         MVC    STLPD1+4(3),0(R5)                                               
         MVC    PRD2,0(R5)                                                      
         OI     STLPD1H+6,X'80'                                                 
*                                                                               
         LA     R2,FAKEFLD                                                      
         XC     FAKEFLD,FAKEFLD                                                 
         MVC    FAKEFLD+8(3),PRD2        SECOND PRODUCT                         
         MVI    FAKEFLD+5,3                                                     
         MVI    USEIONUM,3                                                      
         GOTO1  VALIPRD                                                         
         MVC    PRD2NM,PRDNM                                                    
*                                                                               
         LA     R2,FAKEFLD                                                      
         XC     FAKEFLD,FAKEFLD                                                 
         MVC    FAKEFLD+8(3),PRD1        REDO FIRST PRODUCT                     
         MVI    FAKEFLD+5,3                                                     
         MVI    USEIONUM,3                                                      
         GOTO1  VALIPRD                                                         
         DROP   R6                                                              
*                                                                               
DK500    MVC   BLOCK(100),SPACES                                                
         MVC   BLOCK(L'PRDNM),PRDNM                                             
         OC    PRD2NM,PRD2NM                                                    
         BZ    DK560                                                            
         MVC   BLOCK+6(100),SPACES                                              
         MVI   BLOCK+10,C'/'                                                    
         MVC   BLOCK+13(L'PRD2NM),PRD2NM                                        
DK560    GOTO1 SQUASHER,DMCB,BLOCK,40                                           
         MVC   STLPN1,BLOCK                                                     
         OI    STLPN1H+6,X'80'                                                  
*                                                                               
         LA     R2,STLESTH                                                      
         CLI    ESTIM,10                                                        
         BNL    *+12                                                            
         MVI    5(R2),1                                                         
         B      DK580                                                           
*                                                                               
         CLI    ESTIM,100                                                       
         BNL    *+12                                                            
         MVI    5(R2),2                                                         
         B      DK580                                                           
*                                                                               
         MVI    5(R2),3                                                         
DK580    MVI    0(R2),11                                                        
         EDIT   (B1,ESTIM),(3,STLEST),ALIGN=LEFT                                
         OI     STLESTH+6,X'80'                                                 
         OI     4(R2),X'08'                                                     
*                                                                               
         MVI    USEIONUM,2                                                      
         GOTO1  VALIEST                                                         
         L      R6,AIO                                                          
         USING  ESTHDR,R6                                                       
         GOTO1  DATCON,DMCB,(0,ESTART),(10,STLENM)                              
         GOTO1  DATCON,DMCB,(0,EEND),(10,STLENM+11)                             
         GOTO1  DATCON,DMCB,(0,ESTART),(2,ESTARTD)                              
         GOTO1  DATCON,DMCB,(0,EEND),(2,ESENDD)                                 
         MVI    STLENM+9,C'-'                                                   
         OI     STLENMH+6,X'80'                                                 
         MVC    MYESTART,ESTART                                                 
         MVC    MYEEND,EEND                                                     
         XC    COMDLIST,COMDLIST                                                
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         BNH   *+10                                                             
         MVC   COMDLIST,ENONTDMS                                                
         DROP   R6                                                              
*                                                                               
DKX      MVC    AIO,AIO1                                                        
         BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
         B      XIT                                                             
         EJECT                                                                  
*******************************************************************             
ERRWEEK  MVC   ERRNUM,=AL2(BADWEEK)                                             
         B     SPERREX                                                          
ERRWEEK2 MVC   ERRNUM,=AL2(BADWEEK2)                                            
         B     SPERREX                                                          
ERRWEEK3 MVC   ERRNUM,=AL2(BADWEEK3)                                            
         B     SPERREX                                                          
ERRWEEK4 MVC   ERRNUM,=AL2(BADWEEK4)                                            
         B     SPERREX                                                          
ERRWEEK5 MVC   ERRNUM,=AL2(BADWEEK5)                                            
         B     SPERREX                                                          
ERRREC   MVC   ERRNUM,=AL2(BADREC)                                              
         B     SPERREX                                                          
NOPWREC  MVC   ERRNUM,=AL2(NOPWRECM)                                            
         B     SPERREX                                                          
NOPWEL   MVC   ERRNUM,=AL2(NOPWELEM)                                            
         B     SPERREX                                                          
BADEST   MVC   ERRNUM,=AL2(BADESTM)                                             
         B     SPERREX                                                          
PWOVRERR MVC   ERRNUM,=AL2(PWOVERM)                                             
         B     SPERREX                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
BADREC   EQU   53                                                               
BADWEEK  EQU   464                                                              
BADWEEK2 EQU   463                                                              
BADWEEK3 EQU   466                                                              
BADWEEK4 EQU   467                                                              
BADWEEK5 EQU   468                                                              
NOPWRECM EQU   475                                                              
NOPWELEM EQU   476                                                              
BADESTM  EQU   478                                                              
PWOVERM  EQU   479                                                              
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
**************************************************************                  
*        GET  BINARY SPOT LENGTH                                                
**************************************************************                  
SLEN     MVC   WORK(6),=C'000000'                                               
         XC    DUB,DUB                                                          
         SR    R4,R4                                                            
         LLC   R5,5(R2)                                                         
         LH    R0,=H'2'                                                         
         DR    R4,R0                                                            
         LTR   R4,R4                                                            
         BZ    SLEN80                                                           
         B     SLEN90                                                           
*  IF NUMBER OF INPUT IS EVEN NUMBER   *                                        
SLEN80   LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),8(R2)                                                  
         LA    RF,1(RF)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         SR    RF,RF                                                            
         CVB   RF,DUB                                                           
         STC   RF,BYTE                                                          
         B     SLENX                                                            
*   NUMBER OF INPUT IS ODD NUMBER *                                             
SLEN90   LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         SR    RF,RF                                                            
         CVB   RF,DUB                                                           
         STC   RF,BYTE                                                          
SLENX    BR    RE                                                               
***********************************************************************         
*              CLEAR SCREEN                                                     
***********************************************************************         
CLR     NTR1                                                                    
         XC    STLDM1,STLDM1                                                    
         OI    STLDM1H+6,X'80'                                                  
         XC    STLDM2,STLDM2                                                    
         OI    STLDM2H+6,X'80'                                                  
         XC    STLDM3,STLDM3                                                    
         OI    STLDM3H+6,X'80'                                                  
*        XC    STLDM4,STLDM4                                                    
*        OI    STLDM4H+6,X'80'                                                  
*                                                                               
        LA     R2,STLWKH                                                        
CLR10   LLC    RE,0(R2)                                                         
        SH     RE,=H'8'                                                         
        BCTR   RE,0                                                             
        EX     RE,*+8                                                           
        B      *+10                                                             
        XC     8(0,R2),8(R2)                                                    
        OI     6(R2),X'80'                                                      
        BAS    RE,NXTFLD                                                        
        LA     R0,STLLAST                                                       
        CR     R2,R0                                                            
        BL     CLR10                                                            
CLRX    XIT1                                                                    
**********************************************************************          
*              CHECK IF WEEK IS DUPLICATE                                       
***********************************************************************         
CHKWK    NTR1                                                                   
         MVI   ELCODE,LOKELCDQ                                                  
         L     R6,AIO                                                           
*                                                                               
CHKWK15  BAS   RE,GETEL                                                         
         BNE   CHKWK40                                                          
         B     CHKWK30                                                          
*                                                                               
CHKWK20  BAS   RE,NEXTEL                                                        
         BNE   CHKWK40               IF NOT FOUND WK NOT DUPLICATE              
*                                                                               
CHKWK30  CLC   2(2,R6),TESTWK                                                   
         BNE   CHKWK20                                                          
         B     CHKWK50                                                          
*                                                                               
CHKWK40  LA    R0,1                  SET NOT EQUAL CONDITION CODE               
         LA    R1,2                                                             
         CR    R0,R1                                                            
         B     CHKWKX                                                           
*                                                                               
CHKWK50  LA    R0,1                                                             
         LA    R1,1                                                             
         CR    R0,R1                                                            
CHKWKX   XIT1                                                                   
*                                                                               
***********************************************************************         
*     UNMARK MARKED "FF ELEMENTS" FROM INVALID SCREEN                           
***********************************************************************         
UNMARK10 NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,GETEL                                                         
         BE    UNMARK30                                                         
         BNE   UNMARKX                                                          
*                                                                               
UNMARK20 BAS   RE,NEXTEL                                                        
         BNE   UNMARKX                                                          
*                                                                               
UNMARK30 MVI   0(R6),LOKELCDQ                                                   
         B     UNMARK20                                                         
UNMARKX  XIT1                                                                   
***********************************************************************         
*       CHECK MULTIPLE DATES ON ONE SCREEN                                      
***********************************************************************         
DUPWK    NTR1                                                                   
         LA    R2,STLWKH                                                        
         LR    R5,R2                                                            
         LA    R3,STLLAST                                                       
         LA    R4,STLWK2H                                                       
         SR    R4,R2                                                            
         AR    R5,R4                                                            
DUPWK05  OC    8(L'STLWK,R5),8(R5)                                              
         BZ    DUPWK10                                                          
*                                                                               
         OC    8(L'STLWK,R2),8(R2)                                              
         BZ    DUPWK10                                                          
*                                                                               
         CLC   8(L'STLWK,R2),8(R5)                                              
         BE    DUPWK50                                                          
DUPWK10  CR    R5,R3                                                            
         BNL   DUPWK30                                                          
         AR    R5,R4                                                            
         B     DUPWK05                                                          
*                                                                               
DUPWK30  AR    R2,R4                                                            
         LR    R5,R2                                                            
         AR    R5,R4                                                            
         CR    R2,R3                                                            
         BNL   DUPWK40                                                          
         B     DUPWK05                                                          
*                                                                               
DUPWK40  B     DUPWKX                                                           
*                                                                               
DUPWK50  B     ERRWEEK5                                                         
DUPWKX   XIT1                                                                   
***********************************************************************         
*              NEXT FIELD HEADER                                                
***********************************************************************         
NXTFLD   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
***********************************************************************         
*    IF NO ELEMENTS CLEAR DAY OF WEEK. IF DAY OF WEEK IS THERE                  
*    THAT MEANS IT IS AN ELEMENT NOT A NEW LINE                                 
***********************************************************************         
CLRDAY   LR    R1,R2                                                            
         ZICM  R0,DAYLLN,4                                                      
         AR    R1,R0                                                            
         XC    8(L'DAY,R1),8(R1)                                                
         OI    6(R1),X'80'                                                      
CLRDAYX  BR    RE                                                               
***********************************************************************         
*============  BUILT PW TABLE ===================================               
* PW TABLE IS BUILT TO CONTAIN THE EST WEEKS WITH DIFFERENCE OF DOLLARS         
**********************************************************************          
BLTPWTAB NTR1                                                                   
         LA    R5,PWTABLE          BUILD PWTABLE FIRST WITH WEEKS               
         USING PWTABLED,R5                                                      
*        XC    0(L'PWTABLE,R5),0(R5)                                            
         LA    RE,PWTABLE                                                       
         XCEF  (RE),L'PWTABLE                                                   
*                                                                               
         MVC   TMPDATE,MYESTART                                                 
BLT10    CLC   TMPDATE,MYEEND                                                   
         BH    BLT30                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,TMPDATE),(2,PWSTART)                              
         GOTO1 ADDAY,DMCB,(C'D',TMPDATE),(X'40',TMPDATE),6                      
         GOTO1 DATCON,DMCB,(0,TMPDATE),(2,PWEND)                                
         GOTO1 ADDAY,DMCB,(C'D',TMPDATE),(X'40',TMPDATE),1                      
         LA    R5,L'PWTABREC(R5)                                                
         STCM  R5,15,ATABEND                                                    
         B     BLT10                                                            
* FILL IN PW TABLE WITH CHANGED FSTLOCKIN DATES AND DOLS                        
BLT30    LA    R5,PWTABLE                                                       
         LA    R3,PTABLE                                                        
         USING PTABLED,R3                                                       
*                                                                               
BLT40    OC    PTAB,PTAB  IF TABLE ENTRY IS EMPTYWE HAVE REACH END              
         BZ    BLT60      OF THE TABLE                                          
*                                                                               
         OC    PWEEK,PWEEK IF WEEK ENTRY IS BLANK JUST GET NEXT TABLE           
         BZ    BLT55       ENTRY                                                
*                                                                               
         CLC   PWEND,PWEEK                                                      
         BNL   BLT50                                                            
*                                                                               
         LA    R5,L'PWTABREC(R5)                                                
         B     BLT40                                                            
*  WE HAVE FOUND THE PW TABLE WEEK RANGE                                        
BLT50    MVC   PWWEEK,PWEEK                                                     
         MVC   PWDOLS,PDOLS                                                     
         MVI   PWSTATUS,C'+'    SET STATUS TO ADD                               
         MVC   PWSPOTS,PSPOTS   MOVE IN SPOTS FOR NOW                           
         MVI   PWSTSTAT,C'+'   SET SPOTS STATUS TO ADD                          
BLT55    LA    R5,PWTABLE       RESET PW TBALE TPO BEGINNING                    
         LA    R3,L'PTABREC(R3) GET NEXT FSTLOCK TAB ENTRY                      
         B     BLT40                                                            
*NOW LETS MOVE INTO PTABLE THE DIFF OF DOLLS AND SPOTS                          
BLT60    LA    R5,PWTABLE                                                       
         LA    R3,PTABLE                                                        
*                                                                               
BLT70    OC    PTAB,PTAB  IF TABLE ENTRY IS EMPTYWE HAVE REACH END              
         BZ    BLTX       OF THE TABLE                                          
*                                                                               
         OC    POLDWK,POLDWK  WEEK ENTRY IS BLANK JUST GET NEXT TABLE           
         BZ    BLT90       ENTRY                                                
*                                                                               
         CLC   PWEND,POLDWK   CHECK OLD WEEKS NOW                               
         BNL   BLT80                                                            
*                                                                               
         LA    R5,L'PWTABREC(R5)                                                
         B     BLT70                                                            
*  WE HAVE FOUND THE PW TABLE WEEK RANGE                                        
BLT80    ZICM  RE,PWDOLS,4                                                      
         ZICM  R0,POLDDOLS,4                                                    
         CR    RE,R0            IF NEW IS > OLD                                 
         BH    BLT85                                                            
*                                                                               
         SR    R0,RE            IF OLD GREATER THAN NEW                         
         MVI   PWSTATUS,C'-'    TAKE THE DIFF AND SET STATUS TO MINUS           
         STCM  R0,15,PWDOLS                                                     
         B     BLT86                                                            
*                                                                               
BLT85    SR    RE,R0                                                            
         MVI   PWSTATUS,C'+'    SET STATUS TO ADD                               
         STCM  RE,15,PWDOLS                                                     
* ==== NOW DO THE DIFFERENCE FOR THE SPOTS AND SPOTS STATUS                     
BLT86    ZICM  RE,PWSPOTS,2                                                     
         ZICM  R0,POLDSPTS,2                                                    
         CR    RE,R0                                                            
         BH    BLT87                                                            
*                                                                               
         SR    R0,RE                                                            
         MVI   PWSTSTAT,C'-'                                                    
         STCM  R0,3,PWSPOTS                                                     
         B     BLT90                                                            
*                                                                               
BLT87    SR    RE,R0                                                            
         MVI   PWSTSTAT,C'+'                                                    
         STCM  RE,3,PWSPOTS                                                     
*                                                                               
*********************************************************                       
BLT90    LA    R5,PWTABLE       RESET PW TBALE TPO BEGINNING                    
         LA    R3,L'PTABREC(R3) GET NEXT FSTLOCK TAB ENTRY                      
         B     BLT70                                                            
*                                                                               
BLTX     XIT1                                                                   
         DROP  R5                                                               
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION ALLOWED                          
         OI     GENSTAT2,DISTHSPG                                               
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
SSV      NTR1                                                                   
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         XIT1                                                                   
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
RSV      NTR1                                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XIT1                                                                   
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
PROTWK  NTR1   BASE=*,LABEL=*                                                   
        LA     R2,STLWKH                                                        
*  GET THE LENGTH OF EACH LINE                                                  
        LA     RE,STLWK2H                                                       
        LA     R2,STLWKH                                                        
        SR     RE,R2                                                            
        STCM   RE,15,LINELN                                                     
PROT10  LLC    RE,0(R2)                                                         
        OI     1(R2),X'20'                                                      
        OI     6(R2),X'80'                                                      
*       BAS    RE,NXTFLD       TO SPOTS                                         
        ZICM   RE,LINELN,4                                                      
        AR     R2,RE                                                            
        LA     R0,STLLAST                                                       
        CR     R2,R0                                                            
        BL     PROT10                                                           
PROTX   XIT1                                                                    
        LTORG                                                                   
        DROP   RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMF3D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENXLK                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
SEQNUM   DS    XL3                                                              
DATETEMP DS    CL6                      TEMP DATE STORAGE                       
STATION  DS    XL5                                                              
CLIENT   DS    CL3                      CLIENT STORAGE                          
ESTIM    DS    XL1                                                              
MEDIA    DS    CL1                      MEDIA STORAGE                           
FAKEFLD  DS    XL18                                                             
LINELN   DS    XL4                                                              
DAYLLN   DS    XL4                                                              
LEN1     DS    XL1                                                              
LEN2     DS    XL1                                                              
TABLE    DS    0CL325        WAS 250                                            
TABREC   DS    13CL25        WAS 25                                             
TABEND   DS    CL25                                                             
DBLIST   DS    CL20                                                             
STARTWK  DS    H                                                                
TESTWK   DS    H                                                                
PRDNUM   DS    X                                                                
PRDNUM2  DS    X                                                                
PRD1     DS    CL3                                                              
PRD2     DS    CL3                                                              
PRD1LN   DS    X                                                                
PRD2LN   DS    X                                                                
SPLEN1   DS    CL3                                                              
SPLEN2   DS    CL3                                                              
SPLEN1LN DS    X                                                                
SPLEN2LN DS    X                                                                
ESTARTD  DS    H          ESTIMATE START DATE COMPRESSED                        
ESENDD   DS    H          ESTIMATE END DATE COMPRESSED                          
DAY      DS    CL3                                                              
DAY2     DS    CL3                                                              
ERRFLAG  DS    X                                                                
DUPFLAG  DS    CL3                                                              
LOW      DS    H                                                                
ELEMLN   DS    X                                                                
TMPDATE  DS    CL6                                                              
MYESTART DS    CL6                                                              
MYEEND   DS    CL6                                                              
ATABEND  DS    XL4                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
VDEMOVAL DS    F                                                                
SPPWCALC DS    F                                                                
*                                                                               
PTABLE   DS    0CL208                                                           
PTABREC  DS    13CL16                                                           
PTABEND  DS    CL16                                                             
*  TABLE TO COMPARE WITH PW ELEMENTS                                            
PWTABLE  DS    0CL280                                                           
PWTABREC DS    20CL14                                                           
*                                                                               
MYPWPCT  DS    F                                                                
PWBLKL   EQU   PWBLKX-PWBLKD      L(PWBLOCK)                                    
PWCLTF   DS    X                                                                
MYSPOTS  DS    XL(L'PWDOLSPT)                                                   
*                                                                               
COMDLIST DS    XL(L'ENONTDMS*20)                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE SPGENNDOV                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPPWBLOCK                                                      
         EJECT                                                                  
TABLED   DSECT                                                                  
TAB      DS    0XL25     WAS 24                                                 
TABWEEK  DS    XL2                                                              
TABSPOTS DS    XL2                                                              
TABDOLS  DS    XL4                                                              
TABDEM1  DS    XL4                                                              
TABDEM2  DS    XL4                                                              
TABDEM3  DS    XL4                                                              
TABDEM4  DS    XL4                                                              
TABFLAG  DS    X                                                                
*                                                                               
PTABLED  DSECT                                                                  
PTAB     DS    0XL16                                                            
PWEEK    DS    XL2                                                              
PDOLS    DS    XL4                                                              
POLDWK   DS    XL2                                                              
POLDDOLS DS    XL4                                                              
PSPOTS   DS    XL2                                                              
POLDSPTS DS    XL2                                                              
*                                                                               
PWTABLED DSECT                                                                  
PWTAB    DS    0XL14                                                            
PWSTART  DS    XL2                                                              
PWEND    DS    XL2                                                              
PWSTATUS DS    XL1                                                              
PWWEEK   DS    XL2                                                              
PWDOLS   DS    XL4                                                              
*                                                                               
PWSTSTAT DS    XL1                                                              
PWSPOTS  DS    XL2                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPSFM29   10/21/19'                                      
         END                                                                    
