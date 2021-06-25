*          DATA SET KWANSS4F   AT LEVEL 001 AS OF 12/02/97                      
*PHASE T2174F                                                                   
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2174F  -- MARKET GROUP DEFINITION                   *         
*                                                                     *         
*  COMMENTS:     MAINTAINS OFFICE RECORDS ON SPFILE                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMCF, AND SPSFMDF                         *         
*                                                                     *         
*  OUTPUTS:                                                                     
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
         TITLE 'T2174F - MARKET GROUP DEFINITION'                               
T2174F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**174F**,R7,RR=R3                                              
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
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (LIST)                           
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
         MVC   CLIENT,SPACES                                                    
         MVC   MKTGID,SPACES                                                    
         XC    BAGY,BAGY           BINARY AGENCY ONLY                           
         XC    BAGYHIGH,BAGYHIGH   NEEDED FOR CK SAME AGNECY                    
         XC    LKEYAGMD,LKEYAGMD   PART OF KEY FOR LIST FILTERING               
         XC    LKEYBCLT,LKEYBCLT   "                                            
         XC    LKEYMID,LKEYMID     "                                            
         MVI   LKEYFLAG,0          INITIALIZE FLAG FOR LIST KEY                 
*                                                                               
         LA    R2,MDMMEDH                                                       
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   ERRINV                                                           
         XC    TEMPFLD,TEMPFLD     FOR BLANK MEDIA                              
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   BAGY,BAGYMD                                                      
         NI    BAGY,X'F0'          TURN OFF MEDIA PORTION                       
         MVC   LKEYAGMD,BAGY                                                    
         MVC   BAGYHIGH,BAGYMD                                                  
         OI    BAGYHIGH,X'0F'                                                   
         B     BLKEY                                                            
*                                                                               
VK10     DS    0X                                                               
         GOTO1 VALIMED             FILLS IN BAGYMDY, MEDNM                      
         MVC   MDMMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    MDMMDNH+6,X'80'                                                  
         MVC   BAGYHIGH,BAGYMD                                                  
         OI    BAGYHIGH,X'0F'                                                   
         MVC   LKEYAGMD,BAGYMD     BINARY AGENCY-MEDIA                          
         OI    LKEYFLAG,X'80'      HAS AGENCY-MEDIA IN IT                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    BLKEY               MEDIA FOR LIST KEY IS DONE                   
*                                                                               
         LA    R2,MDMMIDH          VALIDATE MARKET ID                           
         CLI   8(R2),C' '          SPACE (X'40')?                               
         BNH   VK20                                                             
         CLI   8(R2),C'N'                                                       
         BE    ERRINV                                                           
         CLI   8(R2),C'Y'                                                       
         BE    ERRINV                                                           
         TM    4(R2),X'04'         INPUT FIELD IS VALID ALPHA?                  
         BNO   ERRINV                                                           
*                                                                               
VK20     MVC   MKTGID,8(R2)                                                     
*                                                                               
         LA    R2,MDMCLTH          VALIDATE CLIENT                              
         CLI   5(R2),0             CLIENT INPUT DATA LENGTH = 0?                
         BE    ERRMIS                                                           
*                                                                               
         CLC   =C'ALL',8(R2)       CLIENT INPUT = 'ALL' ?                       
         BNE   VK30                NO, SKIP AHEAD                               
         XC    MDMCLN,MDMCLN       NO CLIENT NAME                               
         OI    MDMCLNH+6,X'80'                                                  
         CLI   MKTGID,C'G'         CLT 'ALL', MGRID LESS THAN 'G'?              
         BL    ERRINV              ONLY INPUT 'ALL' IF MGRID > G                
         MVC   CLIENT,8(R2)                                                     
         XC    BCLT,BCLT                                                        
         B     VK40                                                             
*                                                                               
VK30     CLI   MKTGID,C'F'                                                      
         BH    ERRINV              ONLY A-F IF CLIENT INPUT                     
*                                                                               
         GOTO1 VALICLT             FILL IN BCLT                                 
         MVC   MDMCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    MDMCLNH+6,X'80'                                                  
*                                  EXCEPTION FOR CLIENT POL?                    
         MVC   CLIENT,8(R2)                                                     
         OC    CLIENT,SPACES                                                    
*                                                                               
VK40     DS    0X                                                               
         LA    R4,KEY              BUILD MARKET GROUP KEY                       
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD     AGENCY-MEDIA                                 
         MVC   MKGKCLT,BCLT        BINARY CLIENT CODE                           
         MVC   MKGKMID,MKTGID                                                   
         DROP  R4                  KEY FOR MAINTENACE ACTION IS BUILT           
         B     VK50                                                             
*                                                                               
BLKEY    DS    0H                                                               
         CLI   MDMMEDH+5,0                                                      
         BNE   *+14                                                             
         XC    MDMMDN,MDMMDN       CLEAR MEDIA NAME                             
         OI    MDMMDNH+6,X'80'                                                  
         CLI   MDMCLTH+5,0                                                      
         BE    *+14                                                             
         CLC   =C'ALL',MDMCLT                                                   
         BNE   *+14                                                             
         XC    MDMCLN,MDMCLN       CLEAR CLIENT NAME                            
         OI    MDMCLNH+6,X'80'                                                  
         LA    R2,MDMCLTH                                                       
         CLI   5(R2),0                                                          
         BE    BLKEY10                                                          
         CLC   =C'ALL',MDMCLT                                                   
         BNE   *+12                                                             
         OI    LKEYFLAG,X'40'      HAS CLIENT IN IT (ALL)                       
         B     BLKEY10                                                          
*                                                                               
         XC    TEMPFLD,TEMPFLD     FOR BLANK MEDIA                              
         MVC   TEMPFLD,=XL9'0900000000010000E3'                                 
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         LA    R2,MDMCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   LKEYBCLT,BCLT       GET THE BINARY CLIENT                        
         OI    LKEYFLAG,X'40'      HAS CLIENT IN IT                             
         MVC   MDMCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    MDMCLNH+6,X'80'                                                  
*                                                                               
BLKEY10  CLI   MDMMIDH+5,0                                                      
         BE    BLKEY20                                                          
         LA    R2,MDMMIDH          VALIDATE MARKET ID                           
         CLI   8(R2),C'N'                                                       
         BE    ERRINV                                                           
         CLI   8(R2),C'Y'                                                       
         BE    ERRINV                                                           
         TM    4(R2),X'04'         INPUT FIELD IS VALID ALPHA?                  
         BNO   ERRINV                                                           
         OI    LKEYFLAG,X'20'      HAS MKT GRP ID IN IT                         
         MVC   LKEYMID,MDMMID      TAKE THE MKT GRP ID                          
*                                                                               
BLKEY20  LA    R4,KEY                                                           
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,LKEYAGMD                                                
         MVC   MKGKCLT,LKEYBCLT                                                 
         MVC   MKGKMID,LKEYMID                                                  
         DROP  R4                  KEY FOR LIST ACTION IS BUILT                 
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
VK50     CLC   SAVEKEY,KEY         HAS CHANGED SINCE LAST TIME?                 
         BE    *+8                 NO                                           
         OI    STATUS,KEYCHG       YES                                          
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         MVI   ELCODE,X'01'        REMOVE MKTGRP BREAK DESP ELEMENT             
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEM                                                          
         USING MKGEL01,R4          MAPPING ONTO MKTGRP BREAK DESP ELEM          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'01'          ELEMENT CODE (THERE'S NO SYMBOL)             
         MVI   ELEM+1,48           ELEMENT LENGTH (ALSO NO SYMBOL)              
         XC    SUMBKLN,SUMBKLN     SUM OF BREAK LENGTHS IS CLEARED              
*                                                                               
         LA    R2,MDMBK1H          BREAK 1 HEADER                               
         CLI   MDMBK1H+5,0         IS BREAK 1 ZERO LENGTH?                      
         BE    ERRMIS              AT LEAST ONE BREAK IS REQUIRED               
         LA    R2,MDMLN1H          LENGTH 1 HEADER                              
*                                                                               
         BAS   RE,NOLNCHA          CAN'T CHANGE THE BK LENGTH                   
         BAS   RE,VALBKLN          VALIDATE THE LENGTH                          
*                                                                               
         MVC   MKGBK1,MDMBK1       BUILDING MKT BREAK NAME                      
         MVC   MKGBK1LN,SVBKLNB    BUILDING BINARY BK LN                        
*                                                                               
         LA    R2,MDMLN2H          LENGTH 2 HEADER                              
         BAS   RE,NOLNCHA          CAN'T CHANGE THE BK LENGTH                   
         LA    R2,MDMBK2H                                                       
         BAS   RE,NOBKCHA          CAN'T ADD A NEW BK TITLE                     
*                                                                               
         LA    R2,MDMBK2H                                                       
         CLI   MDMBK2H+5,0         BREAK AND LENGTH COME IN PAIR                
         BNE   CKBKLN2             IF BREAK CONTAINS INPUT, THEN                
         CLI   MDMLN2H+5,0         LENGTH MUST CONTAIN INPUT.                   
         BNE   ERRMIS              THE REVERSE IS ALSO TRUE                     
*                                                                               
         XC    MDMBK2,MDMBK2       NOTHING IS IN BREAK AND LENGTH               
         XC    SVBKLNB,SVBKLNB                                                  
         MVC   MKGBK2,MDMBK2       BUILDING MKT BREAK NAME                      
         MVC   MKGBK2LN,SVBKLNB    BUILDING BINARY BK LN                        
         B     DOBKLN3             DO THE 3RD BREAK LENGTH                      
*                                                                               
CKBKLN2  LA    R2,MDMLN2H          LENGTH 2 HEADER                              
         BAS   RE,VALBKLN          VALIDATE THE LENGTH                          
         MVC   MKGBK2,MDMBK2       BUILDING MKT BREAK NAME                      
         MVC   MKGBK2LN,SVBKLNB    BUILDING BINARY BK LN                        
*                                                                               
DOBKLN3  LA    R2,MDMLN3H          LENGTH 3 HEADER                              
         BAS   RE,NOLNCHA          CAN'T CHANGE THE BK LENGTH                   
         LA    R2,MDMBK3H                                                       
         BAS   RE,NOBKCHA          CAN'T ADD A NEW BK TITLE                     
*                                                                               
         LA    R2,MDMBK3H                                                       
         CLI   MDMBK3H+5,0         BREAK AND LENGTH COME IN PAIR                
         BNE   CKBKLN3             IF BREAK CONTAINS INPUT, THEN                
         CLI   MDMLN3H+5,0         LENGTH MUST CONTAIN INPUT.                   
         BNE   ERRMIS              THE REVERSE IS ALSO TRUE                     
*                                                                               
         XC    MDMBK3,MDMBK3       NOTHING IS IN BREAK AND LENGTH               
         XC    SVBKLNB,SVBKLNB                                                  
         MVC   MKGBK3,MDMBK3       BUILDING MKT BREAK NAME                      
         MVC   MKGBK3LN,SVBKLNB    BUILDING BINARY BK LN                        
         B     CKORDER                                                          
*                                                                               
CKBKLN3  LA    R2,MDMLN3H          LENGTH 3 HEADER                              
         BAS   RE,VALBKLN          VALIDATE THE LENGTH                          
         MVC   MKGBK3,MDMBK3       BUILDING MKT BREAK NAME                      
         MVC   MKGBK3LN,SVBKLNB    BUILDING BINARY BK LN                        
*                                                                               
CKORDER  CLI   MKGBK3LN,0          NO BLANK INPUTS BETWEEN BK LN                
         BE    DOPGA                                                            
         CLI   MKGBK2LN,0                                                       
         BNE   DOPGA                                                            
         LA    R2,MDMBK2H                                                       
         B     ERRMIS              CANNOT HAVE LENGTH 3 W/O LENGTH 2            
*                                                                               
DOPGA    LA    R2,MDMPGAH          PRODUCT GROUP ASSIGNMENT                     
         MVI   MKGPGA,0            SET FOR NO PRD GRP ASSIGNMENT                
         CLI   MDMPGAH+5,0                                                      
         BE    DOALLMKT            NO INPUT MEANS 'N'                           
         CLI   MDMPGA,C'N'                                                      
         BE    DOALLMKT                                                         
         CLI   SAVEKEY+8,C'F'      MKT GRP ID > F??                             
         BH    ERRINV              FOR MKT GRP ID > F, MKGPGA MUST='N'          
         CLI   MDMPGA,C'Y'                                                      
         BNE   ERRINV                                                           
         MVC   MKGPGA,MDMPGA       BUILDING PRD GRP ASSIGNMENT                  
*                                                                               
DOALLMKT LA    R2,MDMAMKH                                                       
         MVI   MKGALLM,0                                                        
         CLI   MDMAMKH+5,0                                                      
         BE    VR50                NO INPUT MEANS 'N'                           
         CLI   MDMAMK,C'N'                                                      
         BE    VR50                INPUT 'N' IS OKAY                            
         CLI   MDMAMK,C'Y'                                                      
         BNE   ERRINV                                                           
         MVC   MKGALLM,MDMAMK      BUILDING 'ALL MARKETS'                       
         DROP  R4                                                               
*                                                                               
VR50     GOTO1 ADDELEM             ELEMENT READY TO BE ADDED                    
*                                                                               
         LA    R2,MDMAEXH          ADD EXCEPTION HEADER                         
         CLI   MDMAEXH+5,0                                                      
         BE    VRX                 NO EXCEPTION NEED TO BE ADDED                
         CLI   SAVEKEY+8,C'F'      MKT GRP ID > F??                             
         BH    VR60                                                             
*                                                                               
         BAS   RE,ADDPRDEX         APPLIES TO MKT GRP ID A-F                    
         B     VRX                                                              
*                                                                               
VR60     BAS   RE,ADDCLTEX         APPLIES TO MKT GRP ID > F                    
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        MKTGRP BREAK DESCRIPTION                     
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING MKGEL01,R6                                                       
         MVC   MDMBK1,MKGBK1       DISPLAY BREAK TITLE1                         
         OI    MDMBK1H+6,X'80'                                                  
         MVC   MDMBK2,MKGBK2       DISPLAY BREAK TITLE2                         
         OI    MDMBK2H+6,X'80'                                                  
         MVC   MDMBK3,MKGBK3       DISPLAY BREAK TITLE3                         
         OI    MDMBK3H+6,X'80'                                                  
*                                                                               
         EDIT  MKGBK1LN,MDMLN1     DISPLAY BREAK LENGTH1                        
         OI    MDMLN1H+6,X'80'                                                  
         OI    MDMLN1H+4,X'20'     VALID'D BEFORE, FOR ACTCHA                   
         EDIT  MKGBK2LN,MDMLN2     DISPLAY BREAK LENGTH2                        
         OI    MDMLN2H+6,X'80'                                                  
         OI    MDMLN2H+4,X'20'                                                  
         EDIT  MKGBK3LN,MDMLN3     DISPLAY BREAK LENGTH3                        
         OI    MDMLN3H+6,X'80'                                                  
         OI    MDMLN3H+4,X'20'                                                  
*                                                                               
         MVI   MDMPGA,C'N'                                                      
         CLI   MKGPGA,0            NO DATA, PRINT 'N'                           
         BE    *+10                                                             
         MVC   MDMPGA,MKGPGA                                                    
         OI    MDMPGAH+6,X'80'                                                  
*                                                                               
         MVI   MDMAMK,C'N'                                                      
         CLI   MKGALLM,0           NO DATA, PRINT 'N'                           
         BE    *+10                                                             
         MVC   MDMAMK,MKGALLM                                                   
         OI    MDMAMKH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         CLI   KEY+8,C'F'          CLIENT OR PRODUCT EXCEPTION?                 
         BH    *+12                G-K ARE CLIENT EXCEPTIONS                    
*                                                                               
         BAS   RE,PRDEXCP          PRODUCT EXCEPTIONS                           
         B     DR10                                                             
*                                                                               
         BAS   RE,CLTEXCP          CLIENT EXCEPTIONS                            
*                                                                               
DR10     CLI   ACTNUM,ACTCHA       SAVED ACTION = CHANGE?                       
         BE    DR20                                                             
         CLI   ACTNUM,ACTSEL       SAVED ACTION = SEL?                          
         BNE   DRX                                                              
         CLI   LISTSEL,C'C'        SEL ACTION = CHANGE?                         
         BNE   DR20                                                             
*                                                                               
DR20     MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
DRX      MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         NI    STATUS,X'FF'-KEYCHG ALWAYS TURN OFF KEYCHG BIT                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         L     R6,AIO                                                           
         USING MKGRECD,R6                                                       
*                                                                               
         MVC   MKTGID,MKGKMID      COPY OF MKT GRP ID                           
         MVC   WKAGMD,MKGKAGMD                                                  
         NI    WKAGMD,X'0F'        TURN OFF THE AGENCY PORTION                  
         LA    R4,MEDTAB                                                        
*                                                                               
DK10     CLC   WKAGMD,1(R4)        COMPARE THE MEDIA CODE                       
         BE    DK20                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DK10                                                             
         B     ERRINV                                                           
*                                                                               
DK20     MVC   LSTMED,0(R4)        1 CHAR MEDIA                                 
         OI    LSTMEDH+6,X'80'                                                  
         LA    R2,LSTMEDH                                                       
         MVI   LSTMEDH+5,1         SKIP VALIMED                                 
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   LSTMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    LSTMDNH+6,X'80'                                                  
*                                                                               
         CLC   MKGKCLT,=X'0000'                                                 
         BNE   DK30                                                             
         MVC   LSTCLT,=C'ALL'      DISPLAY "ALL"                                
         OI    LSTCLTH+6,X'80'                                                  
         XC    LSTCLN,LSTCLN       THERE'S NO CLIENT NAME                       
         OI    LSTCLNH+6,X'80'                                                  
         XC    BCLT,BCLT                                                        
         B     DK40                                                             
*                                                                               
DK30     GOTO1 CLUNPK,DMCB,MKGKCLT,CLIENT                                       
         MVC   LSTCLT,CLIENT       3 CHARACTER CLIENT                           
         OI    LSTCLTH+6,X'80'                                                  
         LA    R2,LSTCLTH                                                       
         MVI   LSTCLTH+5,3         FAKE THE VALICLT                             
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   AIO,AIO1                                                         
         MVC   LSTCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    LSTCLNH+6,X'80'                                                  
DK40     MVC   LSTMID,MKGKMID      1 CHARACTER MARKET GROUP ID                  
         OI    LSTMIDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         MVC   LISTSEL,THISLSEL    THE SELECTED ACTION ON LIST                  
*                                                                               
DKX      DS    0H                                                               
         LA    R4,KEY              BUILD MARKET GROUP KEY                       
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD     AGENCY-MEDIA                                 
         MVC   MKGKCLT,BCLT        BINARY CLIENT CODE                           
         MVC   MKGKMID,MKTGID                                                   
         DROP  R4                  KEY FOR DISP REC IS BUILT                    
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       ONLINE RECORD LISTING                         *         
***********************************************************************         
LR       DS    0X                                                               
         LA    R1,KEY                                                           
         USING MKGRECD,R1                                                       
         OC    KEY,KEY             FIRST TIME THROUGHT?                         
         BNZ   LR10                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R1,KEY                                                           
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(2),KEYSAVE      SAME SYSTEM/PROGRAM?                         
         BNE   LRX                                                              
         CLC   BAGYHIGH,KEY+2      SAME AGENCY?                                 
         BL    LRX                                                              
         CLI   LKEYFLAG,0          NO FILTERS FOR LIST?                         
         BNE   *+18                                                             
         CLC   KEY(2),KEYSAVE      *** MEDIA AND MKT GRP ID                     
         BNE   LRX                 *** ARE STARTING POINTS                      
         B     LR35                *** CLIENT IS THE FILTER                     
         CLI   LKEYFLAG,X'80'                                                   
         BNE   *+18                                                             
         CLC   KEY+2(1),LKEYAGMD   KEY: MEDIA,0,0                               
         BNL   LR35                                                             
         B     LRX                                                              
         CLI   LKEYFLAG,X'40'                                                   
         BNE   *+18                                                             
         CLC   KEY+3(2),LKEYBCLT   KEY: 0,CLIENT,0                              
         BNE   LR20                                                             
         B     LR35                                                             
         CLI   LKEYFLAG,X'80'+X'40'                                             
         BNE   *+18                                                             
         CLC   KEY+3(2),KEYSAVE+3  KEY: MEDIA,CLIENT,0                          
         BNE   LR20                                                             
         B     LR35                                                             
         CLI   LKEYFLAG,X'80'+X'40'+X'20'                                       
         BNE   LR31                                                             
         CLC   KEY+3(2),LKEYBCLT   KEY: MEDIA,CLIENT,MKT GRP ID                 
         BNE   LR20                                                             
         CLC   KEY+2(1),LKEYAGMD                                                
         BL    LR20                                                             
         CLC   KEY+8(1),LKEYMID                                                 
         BL    LR20                                                             
         B     LR35                                                             
LR31     CLI   LKEYFLAG,X'20'                                                   
         BNE   *+18                                                             
         CLC   KEY+8(1),LKEYMID    KEY: 0,0,MKT GRP ID                          
         BL    LR20                                                             
         B     LR35                                                             
         CLI   LKEYFLAG,X'40'+X'20'                                             
         BNE   LR32                                                             
         CLC   KEY+3(6),KEYSAVE+3  KEY: 0,CLIENT,MKT GRP ID                     
         BL    LR20                                                             
         CLC   KEY+3(2),LKEYBCLT                                                
         BE    LR35                                                             
         B     LR20                                                             
LR32     CLI   LKEYFLAG,X'80'+X'20'                                             
         BNE   LR33                                                             
         CLC   KEY+2(1),LKEYAGMD   KEY: MEDIA,0,MKT GRP ID                      
         BL    LR20                                                             
         CLC   KEY+8(1),LKEYMID                                                 
         BL    LR20                                                             
         B     LR35                                                             
LR33     DC    H'0'                THERE'S NO OTHER COMBINATIONS!               
*                                                                               
LR35     GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LSMID,MKGKMID                                                    
         MVC   WKAGMD,KEY+2                                                     
         NI    WKAGMD,X'0F'        TURN OFF THE AGENCY PORTION                  
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
LR40     CLC   WKAGMD,1(R4)        COMPARE THE MEDIA CODE                       
         BE    LR50                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   LR40                                                             
         B     ERRINV                                                           
*                                                                               
LR50     MVC   LSMED,0(R4)         1 CHAR MEDIA                                 
         MVC   WKBCLI,KEY+3                                                     
         CLC   WKBCLI,=X'0000'     IS IT NULL?                                  
         BNE   *+14                                                             
         MVC   CLIENT,=C'ALL'                                                   
         B     LR60                                                             
         GOTO1 CLUNPK,DMCB,WKBCLI,CLIENT                                        
LR60     MVC   LSCLT,CLIENT                                                     
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR20                                                             
         USING MKGEL01,R6                                                       
         MVC   LSBKT1,MKGBK1                                                    
         EDIT  MKGBK1LN,LSBKLN1                                                 
         MVC   LSBKT2,MKGBK2                                                    
         EDIT  MKGBK2LN,LSBKLN2                                                 
         MVC   LSBKT3,MKGBK3                                                    
         EDIT  MKGBK3LN,LSBKLN3                                                 
         GOTO1 LISTMON                                                          
         B     LR20                                                             
         DROP  R1                                                               
         DROP  R6                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRTOTLN MVC   ERRNUM,=AL2(BKLNTOT)                                             
         B     SPERREX                                                          
ERREXIST MVC   ERRNUM,=AL2(EXCPEXIS)                                            
         B     SPERREX                                                          
ERRCHALN MVC   ERRNUM,=AL2(CHABKLN)                                             
         B     SPERREX                                                          
ERRCHABK MVC   ERRNUM,=AL2(CHANUMBK)                                            
         B     SPERREX                                                          
ERREXCEP MVC   ERRNUM,=AL2(MOREEXCP)                                            
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
         SPACE 2                                                                
*                                  SHORT DESP OF THE ERROR MSGS                 
GPOVFL   EQU   443                                                              
CHABKLN  EQU   444                 CAN'T CHANGE BREAK LENGTH                    
CHANUMBK EQU   445                 CAN'T CHANGE # OF BREAK TITLES               
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
BKLNTOT  EQU   450                 SUM OF BREAK LN MUST BE 1-4                  
EXCPEXIS EQU   451                 EXCEPTION ALREADY EXIST                      
MOREEXCP EQU   452                 EXCEPTION CAN'T BE ALL DISPLAYED             
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*          IF ACTION = CHANGE, BREAK LENGTH CANNOT BE CHANGED.        *         
***********************************************************************         
NOLNCHA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       ADD?                                         
         BE    XNOLNCHA                                                         
         TM    4(R2),X'20'         NOT ALLOWED TO CHANGE                        
         BZ    ERRCHALN            BREAK LENGTH                                 
*                                                                               
XNOLNCHA B     XIT                 END OF CLTEXCEP                              
         SPACE 5                                                                
***********************************************************************         
*          IF ACTION = CHANGE, NO NEW BREAK TITLE CAN BE ADDED.       *         
***********************************************************************         
NOBKCHA  NTR1                                                                   
         CLI   ACTNUM,ACTADD       ADD?                                         
         BE    XNOBKCHA                                                         
         LR    R5,R2               SAVE THE SCREEN FIELD POINTER                
         CLI   5(R2),0             BRAND NEW BREAK TITLE?                       
         BE    XNOBKCHA                                                         
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   XNOBKCHA                                                         
         LR    R2,R5               POINT BACK WHERE R2 INITIALLY AT             
         B     ERRCHABK                                                         
*                                                                               
XNOBKCHA B     XIT                 END OF CLTEXCEP                              
         EJECT                                                                  
***********************************************************************         
*                ADDING CLIENT EXCEPTIONS                             *         
***********************************************************************         
ADDCLTEX NTR1                                                                   
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                RESTORING THE ORIGINAL RECORD                
         GOTO1 GETREC              TO CHECK IF THE EXCEPTION ELEM               
         MVC   AIO,AIO1            IS ALREADY THERE.                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        EXCEPTION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   BUILTCEL            BUILT CLIENT EXCEPTION ELEMENT               
*                                                                               
MORECEX  CLC   MDMAEX(3),2(R6)     CK IF EXCEPTION ELEM ALREADY EXIST           
         BE    ERREXIST            ALREADY EXIST, NO GOOD                       
         CLI   MDMAEXH+5,2         IF CLIENT EXCEPTION CODE IS CL2              
         BNE   *+18                                                             
         MVI   MDMAEX+2,C' '       MOVE A SPACE TO THE 3RD BYTE                 
         CLC   MDMAEX(3),2(R6)     CK FOR 2 CHARACTERS CLIENT CODES             
         BE    ERREXIST            ALREADY EXIST, NO GOOD                       
         BAS   RE,NEXTEL                                                        
         BE    MORECEX             MORE CLIENT EXCEPTION ELEMENT                
*                                                                               
BUILTCEL XC    ELEM,ELEM           START BUILDING EXCEPTION ELEM                
         MVI   ELEM,X'02'          ELEMENT CODE (THERE'S NO SYMBOL)             
         MVI   ELEM+1,5            ELEMENT LENGTH (ALSO NO SYMBOL)              
         MVC   ELEM+2(3),MDMAEX    "ADD EXCEPTION", SCREEN FIELD                
         GOTO1 ADDELEM                                                          
*                                                                               
         B     XIT                 END OF ADD CLIENT EXCEPTION                  
         EJECT                                                                  
***********************************************************************         
*                ADDING PRODUCT EXCEPTIONS                            *         
***********************************************************************         
ADDPRDEX NTR1                                                                   
         LA    R1,KEY                                                           
         USING PRGRECD,R1          BUILT KEY FOR PRD GRP DEF                    
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'    PRODUCT GRP RECORD ID                        
         MVC   KEY+2(3),SAVEKEY+2  AGY-MEDIA/CLT, SAME AS BEFORE                
         MVC   PRGKID,8(R2)        GRP ID, GOT IT FROM THE SCREEN               
         DROP  R1                  KEY IS BUILT                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYSAVE     MIGHT NOT BE THE SAME RECORD                 
         BE    *+8                                                              
         B     ERRINV                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL01,R6                                                       
         SR    RE,RE                                                            
         IC    RE,PRGBK1LN                                                      
         SR    R0,R0                                                            
         IC    R0,PRGBK2LN                                                      
         AR    RE,R0                                                            
         STC   RE,BYTE2            SAVE TOTAL DIGITS                            
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,5(R2)            INPUT LENGTH OF 'ADD EXCEPTION'              
         BCTR  R0,0                MINUS 1 FOR THE LEADING CHAR                 
         CR    RE,R0               THEY SHOULD BE EQUAL                         
         BNE   ERRINV                                                           
         MVC   WORK(4),=4C'0'                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),9(R2)       8(R2) IS THE CHARACTER                       
         PACK  DUB,WORK(5)                                                      
         MVC   KEY+6(2),DUB+5                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   ERRINV                                                           
         MVC   SVPIDGRP,KEYSAVE+5  SAVE THE PRD ID AND PRD GRP                  
*                                                                               
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                RESTORING THE ORIGINAL RECORD                
         GOTO1 GETREC              TO CHECK IF THE EXCEPTION ELEM               
         MVC   AIO,AIO1            IS ALREADY THERE.                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        EXCEPTION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   BUILTPEL            BUILT PRODUCT EXCEPTION ELEMENT              
*                                                                               
MOREPEX  CLC   SVPIDGRP,2(R6)      CK IF EXCEPTION ELEM ALREADY EXIST           
         BE    ERREXIST            ALREADY EXIST, NO GOOD                       
         BAS   RE,NEXTEL                                                        
         BE    MOREPEX             MORE PRODUCT EXCEPTION ELEMENT               
*                                                                               
BUILTPEL XC    ELEM,ELEM           START BUILDING EXCEPTION ELEM                
         MVI   ELEM,X'02'          ELEMENT CODE (THERE'S NO SYMBOL)             
         MVI   ELEM+1,5            ELEMENT LENGTH (ALSO NO SYMBOL)              
         MVC   ELEM+2(3),SVPIDGRP                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         B     XIT                 END OF ADD PRODUCT EXCEPTION                 
         EJECT                                                                  
***********************************************************************         
*                LISTING THE CLIENT EXCEPTIONS                        *         
***********************************************************************         
CLTEXCP  NTR1                                                                   
         LA    R2,MDMLISTH                                                      
         LA    R3,8(R2)            POINT TO THE LISTING DATA AREA               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         USING MKGEL02,R6          MAP ONTO THE EXCEPTION ELEMENT               
         SR    R4,R4               EXCEPTIONS PER LISTING LINE COUNTER          
         SR    R5,R5               LISTING LINES COUNTER                        
MORECLEX MVC   0(3,R3),2(R6)       DISPLAY CLIENT EXCEPTION                     
         OI    MDMLISTH+6,X'80'                                                 
         C     R5,=F'9'                                                         
         BNH   *+8                                                              
         B     ERREXCEP            MAX NUMB OF EXCEPTION LINES IS 9             
*                                                                               
         LA    R3,6(R3)                                                         
         LA    R4,1(R4)            ONLY 10 EXCEPTION PER LINE                   
         C     R4,=F'10'                                                        
         BL    NEXTCLEX                                                         
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         LA    R3,8(R2)                                                         
         LA    R4,0                RESET THE COUNTER                            
         LA    R5,1(R5)            INCREMENT LISTING LINES COUNTER              
*                                                                               
NEXTCLEX BAS   RE,NEXTEL           NEXT CLIENT EXCEPTION ELEMENT                
         BE    MORECLEX            MORE  CLIENT EXCEPTION ELEMENT               
         DROP  R6                                                               
         B     XIT                 END OF CLTEXCEP                              
         EJECT                                                                  
***********************************************************************         
*             LISTING THE PRODUCT EXCEPTIONS                          *         
***********************************************************************         
PRDEXCP  NTR1                                                                   
         LA    R2,MDMLISTH                                                      
         LA    R3,8(R2)            POINT TO THE LISTING DATA AREA               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BNE   XPDEX                                                            
*                                                                               
         SR    R4,R4               EXCEPTIONS PER LISTING LINE COUNTER          
         SR    R5,R5               LISTING LINES COUNTER                        
*                                                                               
MOREPDEX ST    R6,SAVER6                                                        
         MVC   0(1,R3),2(R6)       1 CHARACTER PRODUCT EXCEPTION CODE           
         UNPK  DUB,3(3,R6)                                                      
*                                                                               
         LA    R1,KEY                                                           
         USING PRGRECD,R1                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'    PRODUCT GRP RECORD ID                        
         MVC   KEY+2(3),SAVEKEY+2  AGENCY-MEDIA/CLT ARE SAME AS BEFORE          
         MVC   PRGKID,2(R6)        GROUP ID, PRODUCT EXCEPTION CODE             
         DROP  R1                  KEY IS BUILT                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYSAVE     MIGHT NOT BE THE SAME RECORD                 
         BE    *+8                                                              
         B     ERRINV                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        PROD GRP BREAK DESCRIPTION                   
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         USING PRGEL01,R6                                                       
         SR    RE,RE                                                            
         IC    RE,PRGBK1LN                                                      
         SR    R0,R0                                                            
         IC    R0,PRGBK2LN                                                      
         AR    RE,R0                                                            
         STC   RE,BYTE2            SAVE TOTAL DIGITS                            
*                                                                               
         LA    R2,MDMLISTH                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),DUB+3       PRODUCT EXCEPTION DIGITS                     
         OI    MDMLISTH+6,X'80'    SHOW THE DIGITS                              
         DROP  R6                                                               
*                                                                               
         CH    R5,=H'9'                                                         
         BNH   *+8                                                              
         B     ERREXCEP            MAX NUMB OF LINES DISPLAYED IS 9             
*                                                                               
         LA    R3,6(R3)                                                         
         LA    R4,1(R4)                                                         
         CH    R4,=H'9'            9 EXCEPTION PER LINE                         
         BL    NEXTPDEX                                                         
         SR    R0,R0                                                            
         IC    R0,0(R2)            POINTS TO THE NEXT SCREEN FIELD              
         AR    R2,R0                                                            
         LA    R3,8(R2)                                                         
         LA    R4,0                RESET THE COUNTER                            
         LA    R5,1(R5)            INCREMENT LISTING LINES COUNTER              
*                                                                               
NEXTPDEX MVC   AIO,AIO1            NEXT PRODUCT EXCEPTION ELEMENT               
         L     R6,SAVER6                                                        
         MVI   ELCODE,X'02'        EXCEPTION ELEMENT                            
         BAS   RE,NEXTEL                                                        
         BE    MOREPDEX            MORE PRODUCT EXCEPTION ELEMENT               
*                                                                               
XPDEX    B     XIT                 END OF CLTEXCEP                              
         EJECT                                                                  
***********************************************************************         
*                VALIDATING THE BREAK LENGTHS                         *         
***********************************************************************         
VALBKLN  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   8(R2),C'1'          CAN ONLY BE 1,2,3, AND 4                     
         BL    ERRBKLN                                                          
         CLI   8(R2),C'4'                                                       
         BH    ERRBKLN                                                          
*                                                                               
         XC    DUB,DUB                                                          
         PACK  DUB,8(1,R2)                                                      
         CVB   RE,DUB              LENGTH NOW IS IN BINARY                      
         STC   RE,SVBKLNB          SAVE AWAY THE VALID BINARY BK LN             
         SR    R0,R0                                                            
         IC    R0,SUMBKLN                                                       
         AR    RE,R0                                                            
         CH    RE,=H'4'                                                         
         BH    ERRTOTLN            MAX SUM OF THE 3 LENGTHS IS 4                
         STC   RE,SUMBKLN          SAVE AWAY THE VALID TOTAL BK LN              
XVALBKLN B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*         CLEARS THE ENTIRE SCREEN, BUT NOT THE KEY FIELDS            *         
***********************************************************************         
CLRSCR   NTR1                                                                   
         XC    MDMBK1,MDMBK1       CLEAR BREAK TITLES1                          
         MVI   MDMBK1H+5,0                                                      
         OI    MDMBK1H+6,X'80'                                                  
         XC    MDMBK2,MDMBK2       CLEAR BREAK TITLES2                          
         MVI   MDMBK2H+5,0                                                      
         OI    MDMBK2H+6,X'80'                                                  
         XC    MDMBK3,MDMBK3       CLEAR BREAK TITLES3                          
         MVI   MDMBK3H+5,0                                                      
         OI    MDMBK3H+6,X'80'                                                  
         XC    MDMLN1,MDMLN1       CLEAR BREAK LENGTH1                          
         MVI   MDMLN1H+5,0                                                      
         OI    MDMLN1H+6,X'80'                                                  
         XC    MDMLN2,MDMLN2       CLEAR BREAK LENGTH2                          
         MVI   MDMLN2H+5,0                                                      
         OI    MDMLN2H+6,X'80'                                                  
         XC    MDMLN3,MDMLN3       CLEAR BREAK LENGTH3                          
         MVI   MDMLN3H+5,0                                                      
         OI    MDMLN3H+6,X'80'                                                  
         XC    MDMPGA,MDMPGA       CLEAR PRODUCT GROUP ASSGNS                   
         MVI   MDMPGAH+5,0                                                      
         OI    MDMPGAH+6,X'80'                                                  
         XC    MDMAMK,MDMAMK       CLEAR ALL MARKETS                            
         MVI   MDMAMKH+5,0                                                      
         OI    MDMAMKH+6,X'80'                                                  
         XC    MDMAEX,MDMAEX       CLEAR ADD EXCEPTION                          
         MVI   MDMAEXH+5,0                                                      
         OI    MDMAEXH+6,X'80'                                                  
*                                                                               
         LA    R2,MDMLISTH                                                      
         LA    R3,MDMENDH                                                       
*                                                                               
CLRSCR10 SR    R1,R1                                                            
         IC    R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0             CLEAR OUT THE FIELD                          
         OI    6(R2),X'80'         TRANSMIT                                     
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               PAST LAST OUTPUT FIELD?                      
         BNH   CLRSCR10            NO, CONTINUE                                 
*                                                                               
CLSCX    B     XIT                 YES, DONE                                    
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION FROM THE LIST                    
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLE FOR MEDIA AND ELEMENT CODES                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    X'FF'                                                            
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
       ++INCLUDE SPSFMCFD          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMDFD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
STATUS   DS    XL1                 STATUS BYTE FLAG                             
KEYCHG   EQU   X'80'               KEY HAS CHANGED                              
CLIENT   DS    CL3                                                              
MKTGID   DS    CL1                                                              
SAVER6   DS    F                   SAVES R6, THE AIO POINTER                    
WKAGMD   DS    XL1                 WORK FIELD: AGENCY-MEDIA CODE                
WKBCLI   DS    XL2                 WORK FIELD: BINARY CLIENT                    
SUMBKLN  DS    XL1                 SUM OF THE 3 BREAK LENGTHS                   
SVPIDGRP DS    XL3                 SAVE PRD ID AND PRD GRP                      
LISTSEL  DS    C                   FOR USE WITH LIST SELECTIONS                 
SAVEKEY  DS    CL13                                                             
SVBKLNS  DS    XL2                 SAVED BREAK LENGTHS                          
SVBKLNB  DS    XL1                 SAVED BINARY BREAK LENGTH                    
BYTE2    DS    XL1                 TOTAL BREAK LENGTHS                          
BAGY     DS    XL1                 BINARY AGENCY (NO MEDIA)                     
BAGYHIGH DS    XL1                                                              
LKEYAGMD DS    XL1                 PARTS OF KEY FOR ACT = LIST                  
LKEYBCLT DS    XL2                                                              
LKEYMID  DS    CL1                                                              
LKEYFLAG DS    XL1                 FLAG FOR KEY ORDERS FOR LIST                 
TEMPFLD  DS    XL11                TEMP FIELD FOR BLANK MEDIA                   
*                                                                               
SVPGB1   DS    CL12                SAVE FIELDS OF OLD 01 DATA                   
SVPGBL1  DS    XL1                                                              
SVPGB2   DS    CL12                                                             
SVPGBL2  DS    XL1                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LIST ACTION                       
LSMED    DS    CL1                                                              
         DS    CL4                                                              
LSCLT    DS    CL3                                                              
         DS    CL2                                                              
LSMID    DS    CL1                                                              
         DS    CL6                                                              
LSBKT1   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN1  DS    CL1                                                              
         DS    CL3                                                              
LSBKT2   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN2  DS    CL1                                                              
         DS    CL3                                                              
LSBKT3   DS    CL12                                                             
         DS    CL2                                                              
LSBKLN3  DS    CL1                                                              
         DS    CL3                                                              
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001KWANSS4F  12/02/97'                                      
         END                                                                    
