*          DATA SET SPADD04    AT LEVEL 021 AS OF 05/01/02                      
*PHASE T21204A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21204 - MAINTENANCE/LIST OF AVAIL MARKETS                            
*                                                                               
*  COMMENTS: MAINTAINS AVAIL MARKETS                                            
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T21200), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPADDFE (T212F4) -- MAINTENANCE                              
*                  SPADDEE (T212E4) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW AVAILS                                               
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - POINTS TO THE OVERLAY STORAGE AREA DSECT                        
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
***********************************************************************         
         TITLE 'SPADD04 MAINTENANCE OF AVAIL MARKETS'                           
T21204   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21204*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         XC    ACURSOR,ACURSOR     CLEAR CURSOR POISTION                        
*                                                                               
         LA    R2,LPFTABLE                                                      
         CLI   ACTNUM,ACTLIST      UNLESS WE'RE IN LIST MODE                    
         BE    GPFKEYS                                                          
         LA    R2,XPFTABLE                                                      
         CLI   ACTNUM,ACTXFER      XFER MODE HAS NO PFKEYS DEFINED              
         BE    GPFKEYS                                                          
         SR    R2,R2                                                            
         CLI   PFKEY,3                                                          
         BE    GPFKEYS                                                          
         CLI   PFKEY,4                                                          
         BE    GPFKEYS                                                          
         LA    R2,PFTABLE                                                       
*                                                                               
GPFKEYS  GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
CKMODES  CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,XRECADD        AFTER RECORD HAS BEEN ADDED?                 
         BE    XA                                                               
         CLI   MODE,XRECDEL        AFTER RECORD HAS BEEN DELETED?               
         BE    XD                                                               
         CLI   MODE,XRECREST       AFTER RECORD HAS BEEN RESTORED?              
         BE    XR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PROCPFK        PFKEY PRESSED IN A SELECT?                   
         BE    CKPFKEYS                                                         
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
*****                                                                           
* VALIDATE THE REFERENCE NUMBER                                                 
*****                                                                           
VK       DS    0H                                                               
         LA    R2,AVMREFNH         R2 = A(FLDHDR OF REFERENCE #)                
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLREFNH                                                      
         CLI   ACTNUM,ACTXFER                                                   
         BNE   *+8                                                              
         LA    R2,AVXREFNH                                                      
*                                                                               
         CLI   5(R2),0             IF NO REFERENCE NUMBER                       
         BE    MISSFLD             THEN MISSING, NEED A REFERENCE #             
*                                                                               
         CLI   5(R2),7             IF NOT A FULL REFERENCE NUMBER               
         BNE   INVLREFN            THEN INVALID                                 
*                                                                               
         GOTO1 VALIREFN            VALIDATE REFERENCE NUMBER                    
*                                                                               
         PACK  DUB(4),9(6,R2)      GET AVAIL RECORD AND DISPLAY INFO            
         GOTO1 PAKTOREF,DMCB,DUB                                                
*                                                                               
         MVC   BREFN,DMCB+1        MAKE A COPY OF THE REFERENCE NUMBER          
*                                                                               
         BAS   RE,DISPINFO         DISPLAY AVAIL INFO                           
         BE    *+16                                                             
         MVI   PFKEY,13            IF NO MARKETS IN LIST MODE                   
         MVI   ANYDATA,C'0'                                                     
         B     VKBKEY              THEN GO TO ADD SCREEN                        
*                                                                               
         TM    4(R2),X'80'         REFERENCE NUMBER CHANGED?                    
         BZ    VK20                                                             
         MVI   PREVFLAG,0          YES, DON'T USE OLD KEY                       
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK20                                                             
         CLI   ACTNUM,ACTXFER                                                   
         BE    VK20                                                             
         BAS   RE,GET0MKT          SHOW HEADLINE FOR MARKET 0 IF ANY            
*****                                                                           
* VALIDATE THE MARKET NUMBER                                                    
*****                                                                           
VK20     CLI   ACTNUM,ACTLIST                                                   
         BE    VKXIT                                                            
         CLI   ACTNUM,ACTXFER                                                   
         BE    VK30                                                             
*                                                                               
         LA    R2,AVMMKTH                                                       
         CLI   5(R2),0             IF NO MARKET                                 
         BE    MISSFLD             THEN WE HAVE A MISSING FIELD                 
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKT                                                        
         UNPK  QMKT,DUB                                                         
         XC    AVMMKXP,AVMMKXP                                                  
         OI    AVMMKXPH+6,X'80'                                                 
*                                                                               
         OC    BMKT,BMKT           ZERO MARKET IS MARKET HEADLINE               
         BNZ   *+14                                                             
         MVC   MKTNM,=CL24'MARKET HEADLINE IN AVAIL'                            
         B     VK25                                                             
*                                                                               
         BAS   RE,GETMKTNM         SHOW MARKET NAME                             
         BNE   INVLFLD             ERROR IF NO MARKET NAME                      
VK25     MVC   AVMMKXP,MKTNM                                                    
         EJECT                                                                  
*****                                                                           
* BUILD THE KEY FOR GENCON                                                      
*****                                                                           
VKBKEY   XC    KEY,KEY             CLEAN OUT THE KEY                            
*                                                                               
         LA    R4,KEY                                                           
         USING AVMRECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVI   AVMKTYP,AVMKTYPQ    LOAD UP THE IDENTIFIERS                      
         MVI   AVMKSUB,AVMKSUBQ    NOT PASSIVE KEY                              
         MVC   AVMKAM,BAGYMD                                                    
         MVC   AVMKREF,BREFN                                                    
         MVC   AVMKMKT,BMKT                                                     
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*****                                                                           
* VALIDATE THE BUYER                                                            
*****                                                                           
VK30     LA    R2,AVXBUYRH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         OC    AVXBUYR,SPACES                                                   
         GOTO1 VALIBUYR,DMCB,AVXBUYR                                            
         BNE   INVLFLD                                                          
         MVC   AVXBYXP(L'QBUYER),QBUYER                                         
         OI    AVXBYXPH+6,X'80'                                                 
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'BYRKEY),0(R6)   COPY BUYER KEY TO FOOL GENCON              
*                                                                               
         TM    AVXREFNH+4,X'80'    IF REFERENCE NUMBER                          
         BNZ   *+12                                                             
         TM    AVXBUYRH+4,X'80'        OR BUYER CHANGES                         
         BZ    *+10                                                             
         XC    LASTMKT,LASTMKT     THEN SHOW LIST FROM BEGINNING                
*                                                                               
         B     VKXIT                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DISPLAYS THE INFORMATION FROM THE AVAIL RECORD ONTO THE          
* INFORMATION LINE ON THE SCREEN.                                               
*                                                                               
* NOTE:  CONDITION CODE OF NE WILL BE RETURNED IF THERE ARE NO AVAIL            
*        MARKETS WHILE IN LIST MODE.                                            
***********************************************************************         
DISPINFO NTR1                                                                   
         XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING AVARECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVI   AVAKTYP,AVAKTYPQ    LOAD UP THE IDENTIFIERS                      
         MVI   AVAKSUB,AVAKSUB2    USE PASSIVE KEY                              
         MVC   AVAKAM,BAGYMD                                                    
         MVC   AVAKREF2,BREFN                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(AVAKBYR2-AVAKEY),KEYSAVE  IF NO MATCH ON REFERENCE #         
         BNE   INVLFLD             THEN INVALID REFERENCE #                     
*                                                                               
         LA    R3,AVMINFOH         R3 = A(FLDHDR OF INFORMATION LINE)           
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R3,AVLINFOH                                                      
         CLI   ACTNUM,ACTXFER                                                   
         BNE   *+8                                                              
         LA    R3,AVXINFOH                                                      
*                                                                               
         XC    8(L'AVMINFO,R3),8(R3)                                            
         MVI   5(R3),L'AVMINFO                                                  
         OI    6(R3),X'80'                                                      
         LA    R3,8(R3)                                                         
         USING INFOLIND,R3                                                      
*                                                                               
         MVC   LINBUYR,AVAKBYR2    DISPLAY THE BUYER                            
         MVC   BYRCODE,AVAKBYR2                                                 
         MVI   LINSLSH,C'/'                                                     
*        GOTO1 VALIBUYR,DMCB,LINBUYR     GET OFFICE CODE FROM ELEM              
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        MVC   LINOFID,QOFFICE         AND ITS OFFICE                           
*                                                                               
         MVC   BCLT,AVAKCLT2                                                    
         GOTO1 CLUNPK,DMCB,AVAKCLT2,LINCLT    DISPLAY THE CLIENT                
*                                                                               
         CLI   AVAKEST2,0          DISPLAY ESTIMATE IF ANY                      
         BE    DINF10                                                           
         ZIC   R1,AVAKEST2                                                      
         CVD   R1,DUB                                                           
         UNPK  LINEST(3),DUB+6(2)                                               
         OI    LINEST+2,X'F0'                                                   
*                                                                               
DINF10   MVC   BPRD,AVAKPRD2       COPY THE PRODUCT CODE                        
         GOTO1 GETQPRD             DISPLAY THE EBCDIC PRODUCT                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LINPRD,QPRD                                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
*                                                                               
         OC    AVARDMKT,AVARDMKT   NO MARKETS?                                  
         BNZ   *+12                                                             
         CLI   ACTNUM,ACTLIST      AND IN LIST MODE?                            
         BE    NO                  RETURN WITH NE                               
*                                                                               
         MVC   LINOFID,AVARDBYO    OFFICE ID IN ELEMENT                         
         GOTO1 DATCON,DMCB,(8,AVARDFLS),(11,LINFLTS)                            
         MVI   LINDASH,C'-'                                                     
         GOTO1 DATCON,DMCB,(8,AVARDFLE),(11,LINFLTE)                            
         OC    AVARDDUE,AVARDDUE                                                
         BZ    DINF15                                                           
         GOTO1 DATCON,DMCB,(8,AVARDDUE),(11,LINDUED)                            
*                                                                               
DINF15   LA    R4,AVARDSNT         LAST STATUS WAS SENT?                        
         OC    AVARDSNT,AVARDSNT                                                
         BZ    *+14                NO                                           
         MVC   LINSTAT,=CL12'   LAST SENT'                                      
         B     DINF20                                                           
*                                                                               
         LA    R4,AVARDUPT         LAST STATUS WAS CHANGE?                      
         OC    AVARDUPT,AVARDUPT                                                
         BZ    *+14                NO                                           
         MVC   LINSTAT,=CL12'LAST CHANGED'                                      
         B     DINF20                                                           
*                                                                               
         LA    R4,AVARDCRE         LAST STATUS WAS CREATED                      
         MVC   LINSTAT,=CL12'   OPENED ON'                                      
*                                                                               
DINF20   GOTO1 DATCON,DMCB,(8,(R4)),(11,LINDATE)                                
         DROP  R3                  DONE WITH INFO LINE                          
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BNE   DINFOXIT                                                         
*                                                                               
         OI    AVLNMKTH+6,X'80'    THEN SHOW # OF MARKETS WE HAVE               
         ZICM  R3,AVARDMKT,2                                                    
         EDIT  (R3),(4,AVLNMKT),ZERO=NOBLANK                                    
         MVC   AVLNMKT+5(7),=C'Markets'                                         
         LTR   R3,R3               IF 0 THEN DONE                               
         BZ    DINFOXIT                                                         
         BCTR  R3,0                IF 1 THEN NO 'S' IN MARKETS                  
         LTR   R3,R3                                                            
         BNZ   DINFOXIT                                                         
         MVI   AVLNMKT+11,C' '                                                  
*                                                                               
DINFOXIT B     YES                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SHOWS THE HEADLINE FOR MARKET ZERO.  IF THERE ISN'T A            
* MARKET ZERO THE DEFAULT HEADLINE WILL SHOW UP.                                
*                                                                               
* NOTE: IN THE AVAIL REQUEST THE DEFAULT HEADLINE WILL NOT SHOW UP              
*       BECAUSE WE DO NOT KNOW IF THE CLIENT WANTS A HEADLINE AT ALL.           
***********************************************************************         
GET0MKT  NTR1                                                                   
         XC    AVMFILL,AVMFILL     CLEAR HEADLINE                               
         OI    AVMFILLH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING AVMRECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVI   AVMKTYP,AVMKTYPQ    LOAD UP THE IDENTIFIERS                      
         MVI   AVMKSUB,AVMKSUBQ                                                 
         MVC   AVMKAM,BAGYMD                                                    
         MVC   AVMKREF,BREFN                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(AVMKMKT-AVMKEY),KEYSAVE   IF NOT SAME REFERENCE #            
         BNE   G0MKT10                                                          
*                                                                               
         OC    AVMKMKT,AVMKMKT     IF A ZERO MARKET EXISTS FOR AVAIL            
         BNZ   G0MKT10                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AVMRTCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AVMRTXTD,R6                                                      
         ZIC   R1,AVMRTXLN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVMFILL(0),AVMRTTXT                                              
         B     G0MKTX                                                           
*                                                                               
G0MKT10  XC    AVMFILL,AVMFILL     CLEAN OUT HEADING LINE                       
*G0MKT10  MVC   AVMFILL(37),=C'DPT-LEN  START    - END       DOLLARS'           
*         MVC   AVMFILL+45(18),=C'POINTS         CPP'                           
*                                                                               
G0MKTX   B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SHOWS THE MARKET NAME.                                           
***********************************************************************         
GETMKTNM NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         USING MKTHDRD,R4                                                       
         MVI   KEY,C'0'            C'0' FILL THE KEY                            
         MVC   KEY+1(16),KEY                                                    
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,QMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     R6,AIO                                                           
         CLC   MKTKEY(MKTKEYLN),0(R6)                                           
         BE    GETM10                                                           
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         B     NO                                                               
         DROP  R4                                                               
*                                                                               
         USING MKTREC,R6                                                        
GETM10   MVC   MKTNM,MKTNAME       SET MARKET NAME                              
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVI   PREVFLAG,1          KEY BEING USED BY LIST                       
         MVC   PREVKEY,KEY                                                      
         LA    R4,PREVKEY          POINT TO THE KEY TO DISPLAY                  
         USING AVMRECD,R4                                                       
*                                                                               
         MVC   AVMREFN(L'QMED),QMED    DISPLAY THE REFERENCE NUMBER             
         GOTO1 REFTOPAK,DMCB,AVMKREF                                            
         UNPK  AVMREFN+1(6),DMCB(4)                                             
         OI    AVMREFN+6,X'F0'                                                  
         OI    AVMREFNH+6,X'80'                                                 
*                                                                               
         ZICM  R1,AVMKMKT,2        DISPLAY THE MARKET NUMBER                    
         STCM  R1,3,BMKT                                                        
         CVD   R1,DUB                                                           
         UNPK  AVMMKT(4),DUB                                                    
         OI    AVMMKT+3,X'F0'                                                   
         OI    AVMMKTH+6,X'80'                                                  
*                                                                               
         MVC   QMKT,AVMMKT                                                      
         XC    AVMMKXP,AVMMKXP                                                  
         OI    AVMMKXPH+6,X'80'                                                 
*                                                                               
         OC    BMKT,BMKT                                                        
         BNZ   *+14                                                             
         MVC   MKTNM,=CL24'MARKET HEADLINE IN AVAIL'                            
         B     DK10                                                             
*                                                                               
         BAS   RE,GETMKTNM         SHOW MARKET NAME                             
         BE    *+6                 ERROR IF NO MARKET NAME                      
         DC    H'0'                                                             
DK10     MVC   AVMMKXP,MKTNM                                                    
*                                                                               
         MVC   BREFN,AVMKREF       COPY THE AVAIL REFERENCE NUMBER              
*                                                                               
         BAS   RE,DISPINFO         DISPLAY AVAIL INFO                           
         BAS   RE,GET0MKT          SHOW HEADLINE FOR MARKET 0 IF ANY            
*                                                                               
DKXIT    XC    KEY,KEY                                                          
         MVC   KEY(L'PREVKEY),PREVKEY                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
*****                                                                           
* DISPLAY THE DESCRIPTION ELEMENT (BUYER OVERRIDE)                              
*****                                                                           
         XC    AVMBUYR,AVMBUYR                                                  
         XC    AVMASST,AVMASST                                                  
         OI    AVMBUYRH+6,X'80'                                                 
         OI    AVMASSTH+6,X'80'                                                 
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,AVMRDCDQ     GET DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING AVMRDSCD,R6                                                      
         MVC   AVMBUYR,AVMRDBYR                                                 
         MVC   AVMASST,AVMRDAST                                                 
*****                                                                           
* DISPLAY THE TEXT LINE ELEMENTS                                                
*****                                                                           
DR10     LA    R2,AVMFLINH                                                      
         LA    R3,AVMLLINH                                                      
*                                                                               
DR10LP   MVC   8(L'AVMFLIN,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
         CR    R2,R3                                                            
         BE    DR15                                                             
         LA    R2,L'AVMFLINH+L'AVMFLIN(R2)                                      
         B     DR10LP                                                           
*                                                                               
DR15     L     R6,AIO                                                           
         MVI   ELCODE,AVMRTCDQ     GET TEXT LINE ELEMENTS                       
         BAS   RE,GETEL                                                         
         BNE   DRXIT                                                            
         USING AVMRTXTD,R6                                                      
*                                                                               
DR20LP   ZIC   R1,AVMRTLNM         CALCULATE WHERE TO PUT THE LINE              
         BCTR  R1,0                                                             
         MH    R1,=Y(L'AVMFLINH+L'AVMFLIN)                                      
         LA    R2,AVMFLINH                                                      
         AR    R2,R1                                                            
*                                                                               
         ZIC   R1,AVMRTXLN         DISPLAY THE LINE                             
         SH    R1,=Y(AVMRTOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),AVMRTTXT                                                 
         MVI   6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DR20LP                                                           
*                                                                               
DRXIT    MVC   ACURFORC,ACURSOR                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         MVI   CALLAGN,C'N'        DON'T GO AGAIN TO GENCON AFTER A RET         
*                                                                               
         CLI   ACTNUM,ACTXFER                                                   
         BE    XFVR                                                             
*                                                                               
         OI    GENSTAT2,NEXTSEL    TO GO TO NEXT SELECTION OR LIST              
         MVI   ANYDATA,C'N'                                                     
         CLI   PFKEY,3             ERASE LINE?                                  
         BE    *+12                                                             
         CLI   PFKEY,4             ADD LINE?                                    
         BNE   VR100                                                            
*                                                                               
         L     RE,ATIOB            A(TIOB)                                      
         USING TIOBD,RE                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  RE                                                               
*                                                                               
         LA    R2,AVMFILLH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR10     SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR100               NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR20                YES                                          
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         LA    RF,AVMLLINH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR100               YES                                          
         B     VR10                                                             
*                                                                               
VR20     LA    RF,AVMLLINH         A(LAST TEXT FIELD)                           
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   VR50                NO, ADD LINE                                 
*                                                                               
         LA    R0,AVMFILLH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR100               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VR30     CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR40                YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR30                                                             
*                                                                               
VR40     XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         B     VR100                                                            
*                                                                               
VR50     CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR100               YES                                          
*                                                                               
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,AVMLLINH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR60     ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR70                YES                                          
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR60                                                             
*                                                                               
VR70     XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         ST    R3,ACURSOR          KEEP CURSOR IN PLACE                         
         EJECT                                                                  
VR100    LA    R3,DESCELEM         SET BASIC ELEMENT INFORMATION FOR            
         USING AVMRDSCD,R3             DESCRIPTION ELEMENT                      
         XC    DESCELEM,DESCELEM   CLEAR THE DESCRIPTION ELEMENT                
         MVI   AVMRDCDE,AVMRDCDQ                                                
         MVI   AVMRDSLN,AVMRDSLQ                                                
*                                                                               
         LA    R3,TEXTELEM         SET BASIC ELEMENT INFORMATION FOR            
         USING AVMRTXTD,R3             DAYPART/SPOTLEN ELEMENT                  
         XC    TEXTELEM,TEXTELEM   CLEAR THE TEXT LINE ELEMENT                  
         MVI   AVMRTCDE,AVMRTCDQ                                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AVMRDCDQ     REMOVE OLD DESCRIPTION ELEMENTS              
         GOTO1 REMELEM                                                          
*                                                                               
         OC    BMKT,BMKT           IF ZERO MARKET                               
         BZ    VR110               THEN NO NEED FOR OVERRIDE BUYER              
*                                                                               
         LA    R2,AVMBUYRH         IF THERE IS AN OVERRIDE BUYER                
         CLI   5(R2),0                                                          
         BE    VR110               IGNORE ASSISTANT IF NO BUYER                 
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALIBUYR,DMCB,AVMBUYR                                            
         BNE   INVLFLD                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R6,DESCELEM         SAVE THE OVERRIDE BUYER NAME                 
         USING AVMRDSCD,R6                                                      
         MVC   AVMRDBYR,AVMBUYR                                                 
         OC    AVMRDBYR,SPACES                                                  
         MVC   AVMRDBYO,QOFFICE                                                 
         DROP  R6                                                               
*                                                                               
         LA    R2,AVMASSTH         ASSISTANT BUYER?                             
         CLI   5(R2),0                                                          
         BE    VR105                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALIBUYR,DMCB,AVMASST                                            
         BNE   INVLFLD                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R6,DESCELEM         SAVE THE ASSISTANT BUYER                     
         USING AVMRDSCD,R6                                                      
         MVC   AVMRDAST,AVMASST                                                 
         OC    AVMRDAST,SPACES                                                  
         DROP  R6                                                               
*                                  THEN ADD THE DESCRIPTION ELEMENT             
VR105    MVC   ELEM(L'DESCELEM),DESCELEM                                        
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION <> ADD                             
         BE    VR110                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 READ                THEN RESTORE DA FOR PUTREC                   
         GOTO1 GETREC              BECAUSE VALIBUYR FUCKED IT UP                
         MVC   AIO,AIO1                                                         
*                                                                               
VR110    L     R6,AIO                                                           
         MVI   ELCODE,AVMRTCDQ     REMOVE OLD TEXT LINE ELEMENTS                
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,AVMFLINH         R2 = A(1ST LINE OF TEXT)                     
         LA    R3,1                R3 = LINE NUMBER                             
         LA    R6,TEXTELEM         R6 = A(TEXT ELEMENT)                         
         USING AVMRTXTD,R6                                                      
*                                                                               
VR110LP  OC    8(L'AVMFLIN,R2),8(R2)    IF NO DATA IN THIS LINE                 
         BZ    VR110NXT                                                         
         CLC   8(L'AVMFLIN,R2),SPACES                                           
         BE    VR110NXT                 THEN CHECK NEXT LINE                    
         MVI   ANYDATA,C'Y'                                                     
*                                                                               
         ZIC   R1,5(R2)            MOVE IN TEXT, LINE #, AND L'ELEMENT          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVMRTTXT(0),8(R2)                                                
         STC   R3,AVMRTLNM                                                      
         LA    R1,1+AVMRTOVQ(R1)                                                
         STC   R1,AVMRTXLN                                                      
*                                                                               
         MVC   ELEM(L'TEXTELEM),TEXTELEM                                        
         GOTO1 ADDELEM             ADD TEXT LINE ELEMENT                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR110NXT OC    BMKT,BMKT           IF ZERO MARKET                               
         BNZ   VR120               THEN ONLY ONE LINE FOR HEADLINE              
         XC    AVMFILL,AVMFILL                                                  
         OI    AVMFILLH+6,X'80'                                                 
         MVC   AVMFILL,8(R2)                                                    
         B     VR130                                                            
*                                                                               
VR120    LA    R0,AVMLLINH         IF LINE IS LAST LINE                         
         CR    R2,R0                                                            
         BE    VR130               THEN DONE WITH TEXT LINE ELEMENTS            
*                                                                               
         LA    R2,L'AVMFLINH+L'AVMFLIN(R2)   R2 = A(NEXT LINE)                  
         LA    R3,1(R3)            R3 = NEXT LINE NUMBER                        
         B     VR110LP                                                          
*                                                                               
VR130    CLI   ANYDATA,C'Y'        ANY DATA IN DATA LINES?                      
         BE    VRXIT                                                            
         LA    R2,AVMFLINH         NONE                                         
         B     MISSFLD                                                          
*                                                                               
VRXIT    B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE # OF MARKETS ATTACHED TO AVAIL IN AVAIL RECORD AFTER ADD               
***********************************************************************         
XA       DS    0H                                                               
         MVC   SVKEY,KEY           SAVE THE KEY BEFORE WE KILL IT               
         L     R1,AIO                                                           
         MVC   KEY,0(R1)           USE THE KEY FROM AIO                         
         MVC   AIO,AIO2            USE AIO2 FOR UPDATING # OF MARKETS           
         LA    R4,KEY                                                           
         USING AVAKEY,R4                                                        
         MVI   AVAKSUB,AVAKSUB2    SUB-TYPE FOR PASSIVE KEYS                    
         XC    AVAKBYR2(AVAKCNTL-AVAKBYR2),AVAKBYR2                             
         GOTO1 HIGH                GET THE AVAIL                                
*                                                                               
         CLC   KEY(AVAKBYR2-AVAKEY),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'0'                DIE IF REFERENCE NON-EXISTANT                
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ     GET AVAIL DESCRIPTION ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO DESCRIPTION ELEMENT                
*                                                                               
         USING AVARDSCD,R6                                                      
         ZICM  R1,AVARDMKT,2       INCREMENT NUMBER OF MARKETS IN AVAIL         
         LA    R1,1(R1)                                                         
         STCM  R1,3,AVARDMKT                                                    
*                                                                               
         GOTO1 PUTREC              WRITE OUT THE UPDATED RECORD                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY  RESTORE THE KEY                              
         MVC   AIO,AIO1            USE AIO1 AGAIN                               
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* UPDATE # OF MARKETS ATTACHED TO AVAIL IN AVAIL RECORD AFTER DELETE            
***********************************************************************         
XD       DS    0H                                                               
         MVC   SVKEY,KEY           SAVE THE KEY BEFORE WE KILL IT               
         MVC   AIO,AIO2            USE AIO2 FOR UPDATING # OF MARKETS           
         LA    R4,KEY              1ST 4 BYTES OF KEY = ?? AFTER ADDREC         
         USING AVAKEY,R4                                                        
         MVI   AVAKSUB,AVAKSUB2    SUB-TYPE FOR PASSIVE KEYS                    
         XC    AVAKBYR2(AVAKCNTL-AVAKBYR2),AVAKBYR2                             
         GOTO1 HIGH                GET THE AVAIL                                
*                                                                               
         CLC   KEY(AVAKBYR2-AVAKEY),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'0'                DIE IF REFERENCE # NON-EXISTANT              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ     GET AVAIL DESCRIPTION ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO DESCRIPTION ELEMENT                
*                                                                               
         USING AVARDSCD,R6                                                      
         ZICM  R1,AVARDMKT,2       DECREMENT NUMBER OF MARKETS IN AVAIL         
         BCTR  R1,0                                                             
         STCM  R1,3,AVARDMKT                                                    
*                                                                               
         GOTO1 PUTREC              WRITE OUT THE UPDATED RECORD                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY  RESTORE THE KEY                              
         MVC   AIO,AIO1            USE AIO1 AGAIN                               
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* UPDATE # OF MARKETS ATTACHED TO AVAIL IN AVAIL RECORD AFTER RESTORE           
***********************************************************************         
XR       DS    0H                                                               
         MVC   SVKEY,KEY           SAVE THE KEY BEFORE WE KILL IT               
         MVC   AIO,AIO2            USE AIO2 FOR UPDATING # OF MARKETS           
         LA    R4,KEY              1ST 4 BYTES OF KEY = ?? AFTER ADDREC         
         USING AVAKEY,R4                                                        
         MVI   AVAKSUB,AVAKSUB2    SUB-TYPE FOR PASSIVE KEYS                    
         XC    AVAKBYR2(AVAKCNTL-AVAKBYR2),AVAKBYR2                             
         GOTO1 HIGH                GET THE AVAIL                                
*                                                                               
         CLC   KEY(AVAKBYR2-AVAKEY),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'0'                DIE IF REFERENCE # NON-EXISTANT              
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ     GET AVAIL DESCRIPTION ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO DESCRIPTION ELEMENT                
*                                                                               
         USING AVARDSCD,R6                                                      
         ZICM  R1,AVARDMKT,2       INCREMENT NUMBER OF MARKETS IN AVAIL         
         LA    R1,1(R1)                                                         
         STCM  R1,3,AVARDMKT                                                    
*                                                                               
         GOTO1 PUTREC              WRITE OUT THE UPDATED RECORD                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY  RESTORE THE KEY                              
         MVC   AIO,AIO1            USE AIO1 AGAIN                               
         B     XIT                                                              
         DROP  R4,R6               FOR ALL THE 'X' ROTUINES                     
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         MVC   AIO,AIO2                                                         
         MVI   NLISTS,12           # OF LINES TO LIST, DEFAULT = 15             
         LA    R4,KEY                                                           
         USING AVMKEY,R4                                                        
*                                                                               
         CLI   PREVFLAG,0          A PREVIOUS KEY EXAMINED?                     
         BE    LR10                                                             
         MVC   KEY(L'PREVKEY),PREVKEY   YES, THEN USED THAT KEY                 
         MVI   PREVFLAG,0                                                       
*                                                                               
LR10     CLI   AVMKTYP,AVMKTYPQ    IF NOT 0D32  -  KEY TYPE & SUB-TYPE          
         BNE   LR15                THEN SET UP THE KEY                          
         CLI   AVMKSUB,AVMKSUBQ                                                 
         BE    LRFRST                                                           
*                                                                               
LR15     XC    KEY,KEY                                                          
         MVI   AVMKTYP,AVMKTYPQ                                                 
         MVI   AVMKSUB,AVMKSUBQ    SEARCH WITH REFERENCE #'S                    
         MVC   AVMKAM,BAGYMD       LOAD UP THE AGENCY/MEDIA CODE                
         MVC   AVMKREF,BREFN           AND THE REFERENCE NUMBER                 
*                                                                               
LRFRST   MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR THE FIRST RECORD                    
*                                                                               
LRTEST   CLC   KEY(AVMKMKT-AVMKEY),KEYSAVE                                      
         BNE   LRXIT               EXIT, NOT SAME TYPE OR REFERENCE #           
*                                                                               
         OC    KEY+8(5),KEY+8      IF NOT AVAIL MARKET RECORD                   
         BNZ   LRNEXT              THEN GET NEXT RECORD                         
*                                                                               
LRDISP   MVC   LISTAR,SPACES       DISPLAY THE MARKET ON LIST LINE              
         ZICM  R1,AVMKMKT,2                                                     
         STCM  R1,3,BMKT                                                        
         CVD   R1,DUB                                                           
         UNPK  LSTMKT,DUB                                                       
         OI    LSTMKT+3,X'F0'                                                   
*                                                                               
         MVC   QMKT,LSTMKT                                                      
         OC    BMKT,BMKT                                                        
         BNZ   *+14                                                             
         MVC   MKTNM,=CL24'MARKET HEADLINE IN AVAIL'                            
         B     LR20                                                             
         BAS   RE,GETMKTNM         SHOW MARKET NAME                             
         BE    *+6                 ERROR IF NO MARKET NAME                      
         DC    H'0'                                                             
LR20     MVC   LSTMKXP,MKTNM                                                    
*                                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              SEE IF ANY DESCRIPTION ELEMENT               
         MVI   ELCODE,AVMRDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR30                                                             
         USING AVMRDSCD,R6                                                      
         MVC   LSTBUYR,AVMRDBYR    DISPLAY OVERRIDE BUYER AND OFFICE            
         MVI   LSTSLASH,C'/'                                                    
         MVC   LSTOFF,AVMRDBYO                                                  
         B     LR40                                                             
*                                                                               
LR30     MVC   AIO,AIO3                                                         
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(L'AVMKEY),KEY                                            
         MVI   KEY+1,X'C1'                                                      
         MVC   KEY+3(L'AVMKMKT),AVMKMKT                                         
         USING BYRKEY,R4                                                        
         XC    BYRKDAT2(BYRKCNTL-BYRKDAT2),BYRKDAT2                             
         GOTO1 HIGH                                                             
         CLC   KEY(BYRKDAT2-BYRKEY),KEYSAVE  SAME MARKET?                       
         BNE   LR80MASX                                                         
         CLC   BYRCODE,BYRKBUY2                                                 
         BE    LR80MASX                                                         
         MVC   ASSTBYR,BYRKBUY2                                                 
         GOTO1 VALIBUYR,DMCB,ASSTBYR                                            
         MVC   LSTBUYR,ASSTBYR                                                  
         MVI   LSTSLASH,C'/'                                                    
         MVC   LSTOFF,QOFFICE                                                   
*                                                                               
LR80MASX MVC   AIO,AIO2            POINT BACK TO AIO2                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'AVMKEY),SAVEKEY                                            
         USING AVMKEY,R4                                                        
*                                                                               
         GOTO1 READ                BLOCK OF KEYS GOT FUCKED BY VALIBUYR         
         GOTO1 GETREC                                                           
*                                                                               
LR40     L     R6,AIO              SEE IF ANY TEXT LINE ELEMENT                 
         MVI   ELCODE,AVMRTCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR50                                                             
         USING AVMRTXTD,R6                                                      
         CLI   AVMRTLNM,1          IF FIRST LINE                                
         BNE   LR50                                                             
         ZIC   R1,AVMRTXLN                                                      
         SH    R1,=Y(AVMRTOVQ)                                                  
         CH    R1,=Y(L'LSTTEXT)                                                 
         BNH   LR45                                                             
         MVC   LSTTEXT,AVMRTTXT                                                 
         B     LR50                                                             
*                                                                               
LR45     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTTEXT(0),AVMRTTXT                                              
*                                                                               
LR50     DS    0H                                                               
*                                                                               
LRSHOW   GOTO1 LISTMON                                                          
*                                                                               
LRNEXT   GOTO1 SEQ                 NOW WE CAN GET NEXT AVAIL MARKET KEY         
         B     LRTEST                                                           
*                                                                               
LRXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SELECTIONS MADE FOR THE MARKETS TO BE TRANSFERED                 
***********************************************************************         
XFVR     LA    R2,AVXSELH                                                       
         USING XFERLIND,R2                                                      
*                                                                               
XFVR10   CLI   XLINTXTH+7,0        IF NO MARKET HERE                            
         BE    XFLR                THEN WE'RE DONE                              
*                                                                               
         CLI   XLINSELH+5,0        NOTHING IN SELECTION                         
         BE    XFVRNXLN            THEN CHECK NEXT LINE                         
*                                                                               
         CLI   XLINSEL,C'S'        XFER THIS MARKET?                            
         BNE   INVLFLD             NO, INVALID THEN                             
*                                                                               
         MVC   TEXTMKT,XLINMKT                                                  
*                                                                               
         PACK  DUB,XLINMKT         GET MARKET NUMBER                            
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKT                                                        
*                                                                               
         LA    R4,KEY                                                           
         USING AVMKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   AVMKTYP,AVMKTYPQ                                                 
         MVI   AVMKSUB,AVMKSUBQ                                                 
         MVC   AVMKAM,BAGYMD                                                    
         MVC   AVMKREF,BREFN                                                    
         MVC   AVMKMKT,BMKT                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'AVMKEY),KEYSAVE                                            
         BNE   XFVRADD                                                          
*                                                                               
XFVRDISP MVI   PFKEY,13            AVAIL MARKET EXISTS, DISPLAY IT              
         B     *+8                                                              
XFVRADD  MVI   PFKEY,14            AVAIL MARKET DOESN'T EXIST, ADD IT           
         XC    XLINSEL,XLINSEL     CLEAR OUT THE SELECTION                      
         MVI   XLINSELH+5,0                                                     
         MVI   CALLAGN,C'Y'                                                     
         MVI   GOAGAIN,C'Y'                                                     
         MVI   CALLSP,0        *** DRAWBACK IS WE HAVE TO CLEAR STACK           
         B     XIT                                                              
*                                                                               
XFVRNXLN LA    R0,AVXLLINH                                                      
         CR    R2,R0                                                            
         BNL   XFLR                                                             
         LA    R2,XLINNXTL                                                      
         B     XFVR10                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LISTS THE MARKETS THAT CAN BE TRANSFERED                                      
***********************************************************************         
XFLR     XC    KEY,KEY             SET UP THE BUYER KEY                         
         LA    R4,KEY                                                           
         USING BYRRECD,R4                                                       
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKBYR,AVXBUYR                                                  
*                                                                               
         GOTO1 READ                SHOULD HAVE BEEN VALIDATED BEFORE            
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,AVXLLINH                                                      
         USING XFERLIND,R2                                                      
         TWAXC AVXSELH,XLINTXTH,PROT=Y                                          
*                                                                               
         LA    R2,AVXSELH          FIRST LINE                                   
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,BYRMKAEQ     GET MARKET ASSIGNMENT RECORD                 
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         XC    LASTMKT,LASTMKT     IF THERE IS NONE, EXIT                       
         B     XFLRX                                                            
*                                                                               
         USING BYRMKAD,R6                                                       
         OC    LASTMKT,LASTMKT     TAKE FIRST ONE?                              
         BZ    XFLR20              YES                                          
XFLR10   CLC   LASTMKT,BYRMKANO    OTHERWISE LOOP UNTIL WE GET LAST ONE         
         BE    XFLR20                  AND THEN SHOW LIST FROM THERE            
*                                                                               
         BAS   RE,NEXTEL                                                        
         CLI   0(R6),BYRMKAEQ                                                   
         BE    XFLR10                                                           
         XC    LASTMKT,LASTMKT                                                  
         B     XFLRX                                                            
*                                                                               
XFLR20   OC    LASTMKT,LASTMKT     STARTING FROM BEGINNING?                     
         BNZ   XFLR30              NO                                           
*                                  YES, SHOW ME HEADLINE MARKET ALSO            
         MVC   XLINMKT(30),=CL30'0000 MARKET HEADLINE IN AVAIL'                 
         OI    XLINTXTH+6,X'80'                                                 
         LA    R2,XLINNXTL         THEN SHOW THE FIRST MARKET                   
*                                                                               
XFLR30   MVC   LASTMKT,BYRMKANO                                                 
         XC    XLINTXT,XLINTXT                                                  
         EDIT  (B2,BYRMKANO),(4,XLINMKT),FILL=0                                 
         MVC   QMKT,XLINMKT                                                     
         BAS   RE,GETMKTNM         SHOW MARKET NAME                             
         BE    *+6                 ERROR IF NO MARKET NAME                      
         DC    H'0'                                                             
         MVC   XLINMKTN(L'MKTNM),MKTNM                                          
         OI    XLINTXTH+6,X'80'                                                 
*                                                                               
         BAS   RE,NEXTEL                                                        
*                                                                               
         CLI   0(R6),BYRMKAEQ                                                   
         BE    *+14                                                             
         XC    LASTMKT,LASTMKT                                                  
         B     XFLRX                                                            
*                                                                               
         LA    R0,AVXLLINH                                                      
         CR    R2,R0                                                            
         BNL   XFLRX                                                            
         LA    R2,XLINNXTL                                                      
         B     XFLR20              POINT TO NEXT ELEMENT                        
*                                                                               
XFLRX    B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK THE PFKEYS THAT WAS PRESSED DURING A SELECT                             
***********************************************************************         
CKPFKEYS DS    0H                                                               
         CLI   PFKEY,3                                                          
         BE    CKPFXIT                                                          
         CLI   PFKEY,4                                                          
         BE    CKPFXIT                                                          
         CLI   PFKEY,12                                                         
         BE    CKPFXIT                                                          
         MVI   PFKEY,0                                                          
CKPFXIT  B     XIT                                                              
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLREFN MVI   GERROR1,INVREFN                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN                                                                        
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
XPFTABLE DS    0C                                                               
*                                                                               
* NOTHING                                                                       
         DC    AL1(XPF12X-*,12,0,0,(XPF12X-XPF12)/KEYLNQ,0)                     
         DC    CL3' ',CL8'AVMKTS',CL8'XFER'                                     
XPF12    DC    AL1(KEYTYTWA,L'AVXREFN-1),AL2(AVXREFN-T212FFD)                   
         DC    AL1(KEYTYTWA,L'AVXBUYR-1),AL2(AVXBUYR-T212FFD)                   
XPF12X   EQU   *                                                                
*                                                                               
*                                                                               
* AVAIL MARKET DISPLAY                                                          
         DC    AL1(XPF13X-*,13,PFTCPROG,X'F4',(XPF13X-XPF13)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVMKTS  ',CL8'DISPLAY '                               
XPF13    DC    AL1(KEYTYTWA,L'AVXREFN-1),AL2(AVXREFN-T212FFD)                   
         DC    AL1(KEYTYWS,L'TEXTMKT-1),AL2(TEXTMKT-MYAREAD)                    
XPF13X   EQU   *                                                                
*                                                                               
* AVAIL MARKET ADD                                                              
         DC    AL1(XPF14X-*,14,PFTCPROG,X'F4',(XPF14X-XPF14)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVMKTS  ',CL8'ADD     '                               
XPF14    DC    AL1(KEYTYTWA,L'AVXREFN-1),AL2(AVXREFN-T212FFD)                   
         DC    AL1(KEYTYWS,L'TEXTMKT-1),AL2(TEXTMKT-MYAREAD)                    
XPF14X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
LPFTABLE DS     0C                                                              
*                                                                               
* AVAIL DISPLAY                                                                 
         DC    AL1(LPF02X-*,02,PFTCPROG,X'F3',(LPF02X-LPF02)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVAIL   ',CL8'DISPLAY '                               
LPF02    DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
LPF02X   EQU   *                                                                
*                                                                               
* AVAIL COMMENT CHANGE                                                          
         DC    AL1(LPF03X-*,03,PFTCPROG,X'F8',(LPF03X-LPF03)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVCOM   ',CL8'CHANGE  '                               
LPF03    DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
LPF03X   EQU   *                                                                
*                                                                               
* AVAIL SEND                                                                    
         DC    AL1(LPF04X-*,04,PFTCPROG,X'D3',(LPF04X-LPF04)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVAIL   ',CL8'SEND    '                               
LPF04    DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
LPF04X   EQU   *                                                                
*                                                                               
* AVAIL LIST LAST SCREEN                                                        
         DC    AL1(LPF07X-*,07,0,0,(LPF07X-LPF07)/KEYLNQ,0)                     
         DC    CL3' ',CL8'AVMKTS  ',CL8'LAST    '                               
LPF07    DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
LPF07X   EQU   *                                                                
*                                                                               
* AVAIL LIST FIRST SCREEN                                                       
         DC    AL1(LPF10X-*,10,0,0,(LPF10X-LPF10)/KEYLNQ,0)                     
         DC    CL3' ',CL8'AVMKTS  ',CL8'FIRST   '                               
LPF10    DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
LPF10X   EQU   *                                                                
*                                                                               
* RETURN                                                                        
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
*                                                                               
* GO TO ADD BECAUSE THERE ARE NO AVAIL MARKETS IN LIST MODE                     
         DC    AL1(LPF13X-*,13,0,0,(LPF13X-LPF13)/KEYLNQ,0)                     
         DC    CL3' ',CL8'AVMKTS  ',CL8'ADD     '                               
LPF13    DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
         DC    AL1(KEYTYWS,L'ANYDATA-1),AL2(ANYDATA-MYAREAD)                    
LPF13X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE FATIOB            (TERMINAL INPUT/OUTPUT BLOCK)                
         EJECT                                                                  
       ++INCLUDE SPADDFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDF4D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDE4D          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDD4D          (OUR TRANSFER SCREEN OVERLAY)                
         EJECT                                                                  
       ++INCLUDE SPADDSECTS                                                     
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPADDWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
ACURSOR  DS    A                                                                
PREVFLAG DS    XL1                 PREVIOUS KEY USED FLAG                       
PREVKEY  DS    XL(L'AVMKEY)        PREVIOUS KEY                                 
SAVEKEY  DS    XL(L'AVMKEY)        SAVED KEY                                    
BREFN    DS    XL3                 REFERENCE NUMBER USED BY DISPINFO            
BYRCODE  DS    CL3                 BUYER OF AVAIL                               
ASSTBYR  DS    CL3                 BUYER OF AVAIL                               
ANYDATA  DS    C                   ANY DATA (Y/N)                               
LASTMKT  DS    XL2                 LAST MARKET LISTED WITH XFER                 
TEXTMKT  DS    CL4                 EBCDIC REPRESENTATION OF MARKET              
*                                                                               
DESCELEM DS    XL(AVMRDSLQ)        DESCRIPTION ELEMENT                          
TEXTELEM DS    XL255               TEXT LINE ELEMENT                            
*                                                                               
ACTXFER  EQU   08                  ACTION XFER IS EQUATED TO 08                 
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTMKT   DS    CL4                                                              
         DS    C                                                                
LSTMKXP  DS    CL24                                                             
         DS    CL2                                                              
LSTBUYR  DS    CL3                                                              
LSTSLASH DS    C                                                                
LSTOFF   DS    CL2                                                              
         DS    CL3                                                              
LSTTEXT  DS    CL34                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
XFERLIND DSECT                                                                  
XLINSELH DS    CL8                                                              
XLINSEL  DS    CL3                                                              
XLINTXTH DS    CL8                                                              
XLINTXT  DS    0CL74                                                            
XLINMKT  DS    CL4                                                              
         DS    C                                                                
XLINMKTN DS    CL69                                                             
XLINNXTL DS    0C                                                               
*                                                                               
* ONLINE PRINT LINE                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
*                                                                               
* ONLINE INFORMATION LINE                                                       
*                                                                               
INFOLIND DSECT                                                                  
LINBUYR  DS    CL3                                                              
LINSLSH  DS    C                                                                
LINOFID  DS    CL2                                                              
         DS    CL2                                                              
LINCLT   DS    CL3                                                              
         DS    CL2                                                              
LINPRD   DS    CL3                                                              
         DS    CL2                                                              
LINEST   DS    CL3                                                              
         DS    CL2                                                              
LINFLTS  DS    CL8                                                              
LINDASH  DS    C                                                                
LINFLTE  DS    CL8                                                              
         DS    CL2                                                              
LINDUED  DS    CL8                                                              
         DS    CL2                                                              
LINSTAT  DS    CL12                                                             
         DS    C                                                                
LINDATE  DS    CL8                                                              
         DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPADD04   05/01/02'                                      
         END                                                                    
