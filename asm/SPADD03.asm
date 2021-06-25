*          DATA SET SPADD03    AT LEVEL 099 AS OF 05/01/02                      
*PHASE T21203A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21203 - MAINTENANCE/LIST OF AVAILS                                   
*                                                                               
*  COMMENTS: MAINTAINS AVAILS                                                   
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T21200), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPADDF3 (T212F3) -- MAINTENANCE                              
*                  SPADDE3 (T212E3) -- LIST                                     
*                  SPADDD3 (T212D3) -- SEND (REPORT)                            
*                  SPADDC3 (T212C3) -- STANDARD COMMENTS                        
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
T21203   TITLE 'SPADD03 MAINTENANCE OF AVAILS'                                  
T21203   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21203*,R7,RR=R3                                              
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
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   T21203A                                                          
*        TM    BITFLAG,X'01'       IF UNDER STANDARD COMMENTS                   
*        BNZ   INITPFKY                                                         
         MVI   CALLSP,0            CLEAR CALL STACK IF AVAIL LIST               
         LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
         B     INITPFKY                                                         
*                                                                               
T21203A  CLI   ACTNUM,ACTSEND      SEND?                                        
         BNE   *+12                                                             
         LA    R2,PPFTABLE         YES, USE SEND PFKEY TABLE                    
         B     INITPFKY                                                         
*                                                                               
         MVI   CALLSP,0            CLEAR CALL STACK IF AVAIL MAINT.             
*                                                                               
         CLI   PFKEY,10            LIST 'S' COMMENTS FROM AVAIL X'F3'?          
         BNE   T21203C                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    T21203B                                                          
         CLI   ACTNUM,ACTSEL                                                    
         BE    T21203B                                                          
         CLI   ACTNUM,ACTCHA                                                    
         BNE   T21203C                                                          
T21203B  TM    BITFLAG,X'01'       ALREADY IN LIST?                             
         BNZ   INITPFKY                                                         
         OI    BITFLAG,X'02'+X'01'    NO, LIST STANDARD COMMENTS                
         B     INITPFKY                                                         
*                                                                               
T21203C  CLI   PFKEY,12            RETURN FROM LIST 'S' COMMENTS?               
         BNE   *+12                                                             
         TM    BITFLAG,X'01'                                                    
         BNZ   INITPFKY            YES                                          
*                                                                               
         LA    R2,PFTABLE          DEFAULT PFKEY TABLE                          
*                                                                               
INITPFKY GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
         NI    BITFLAG,X'FF'-X'E0'       CLEAR ANY FLAGS SET PREVIOUSLY         
*                                            EXCEPT LIST COMMENTS MODE          
         TM    BITFLAG,X'01'       IF LIST COMMENTS STILL ACTIVE                
         BNZ   T21203E                                                          
         CLI   ACTNUM,ACTADD       CLEAR PF10 MESSAGE ON SCREEN IF NOT          
         BE    T21203D                 ADD, CHANGE, OR SELECT                   
         CLI   ACTNUM,ACTSEL                                                    
         BE    T21203D                                                          
         CLI   ACTNUM,ACTCHA                                                    
         BE    T21203D                                                          
         XC    AVMSCOM,AVMSCOM                                                  
         OI    AVMSCOMH+6,X'80'                                                 
         B     T21203E                                                          
*                                                                               
T21203D  MVC   AVMSCOM,=CL22'10=Standard Comments  '                            
         OI    AVMSCOMH+6,X'80'                                                 
*&&DO                                                                           
T21203E  LA    R1,NOTHING          DON'T PUT OUT HEADERS YET                    
         ST    R1,SPECS                                                         
         MVI   USEHDHK,C'Y'                                                     
*&&                                                                             
T21203E  MVI   RECFOUND,C'N'                                                    
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
*        CLI   FASYSID,1           IS IT THE TEST SYSTEM?                       
*        BNE   *+12                                                             
*        MVI   MYCLASS,C'Z'        REPORT CLASS 'Z' FOR TEST SYSTEM             
*        B     CKMODES                                                          
         MVI   MYCLASS,C'G'                                                     
         DROP  R1                                                               
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
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   XIT                                                              
         GOTO1 =A(PR),DMCB,(RC),RR=RELO                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
*****                                                                           
* VALIDATE THE REFERENCE NUMBER                                                 
*****                                                                           
VK       DS    0H                                                               
*                                  TELL GENCON WE GOT MULTIPLE FILES            
         OI    GENSTAT3,MULTFILS   AND WE MAY NEED TO CLOSE PRINTQ              
         MVI   FILTRFLG,0          CLEAR THE FILTER FLAG                        
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
         TM    BITFLAG,X'01'       LIST STANDARD COMMENTS MODE?                 
         BZ    *+12                                                             
         BAS   RE,LSTCMMNT                                                      
         BNE   MAKESELS            YES, PLEASE MAKE YOUR SELECTIONS             
*                                                                               
         LA    R2,AVMREFNH         R2 = A(FLDHDR OF REFERENCE #)                
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLREFNH                                                      
         CLI   ACTNUM,ACTSEND      SEND ACTION?                                 
         BNE   *+8                                                              
         LA    R2,AVPREFNH                                                      
*                                                                               
         TM    4(R2),X'80'         REFERENCE NUMBER CHANGED?                    
         BZ    VK05                                                             
         MVI   PREVFLAG,0          YES, DON'T USE OLD KEY                       
*                                                                               
VK05     CLI   5(R2),1             IF NO REFERENCE NUMBER                       
         BL    MISSFLD             THEN INVALID, NEED A MEDIA                   
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   VK10                                                             
         CLI   5(R2),1             AND NOT JUST MEDIA IN REF NUMBER             
         BNE   INVLFLD             THEN INVALID                                 
*                                                                               
VK10     GOTO1 VALIREFN            VALIDATE REFERENCE NUMBER                    
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BE    VK20                THEN VALIDATE BUYER                          
*                                                                               
         CLI   ACTNUM,ACTREST      IF ACTION RESTORE                            
         BNE   *+12                                                             
         OI    DMINBTS,X'08'       THEN READ BACK DELETED RECORDS               
         B     VK12                     MAKE SURE REFERENCE # IS FILLED         
*                                                                               
         CLI   ACTNUM,ACTSEND      IF ACTION SEND                               
         BE    VK12                THEN MAKE SURE REFERENCE # IS FILLED         
*                                                                               
         CLI   5(R2),1             IF JUST THE MEDIA                            
         BE    VK20                THEN VALIDATE BUYER                          
*                                                                               
VK12     CLI   5(R2),7             IF REFERENCE NUMBER IS FILLED                
         BNE   INVLFLD             THEN SEE IF REFERENCE # EXISTS               
*                                                                               
         PACK  DUB(4),9(6,R2)      REALLY FOR DISPLAY LOGIC                     
         GOTO1 PAKTOREF,DMCB,DUB                                                
*                                                                               
         XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING AVARECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVI   AVAKTYP,AVAKTYPQ    LOAD UP THE IDENTIFIERS                      
         MVI   AVAKSUB,AVAKSUB2    USE PASSIVE KEY                              
         MVC   AVAKAM,BAGYMD                                                    
         MVC   AVAKREF2,DMCB+1                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(AVAKBYR2-AVAKEY),KEYSAVE    IF MATCH ON REFERENCE #          
         BE    VK13                                                             
         MVC   KEY,KEYSAVE         GIVE GENCON BAD KEY                          
         CLI   ACTNUM,ACTSEND                                                   
         BNE   VKXIT                                                            
         LA    R2,AVPREFNH                                                      
         MVI   GERROR1,NOTFOUND                                                 
         B     ERREXIT                                                          
*                                                                               
VK13     CLI   ACTNUM,ACTSEND      IF SEND ACTION                               
         BNE   VK14                                                             
*                                  THEN DISPLAY INFORMATION LINE                
         GOTO1 =A(DISPINFO),DMCB,(RC),RR=RELO                                   
*                                       DISPLAY REPS FOR THE MEDIA              
         GOTO1 =A(DISPREPS),DMCB,(RC),RR=RELO                                   
         B     VKXIT                    EXIT                                    
*                                                                               
VK14     MVC   AVMBUYR,AVAKBYR2    DISPLAY THE BUYER                            
         MVI   AVMBUYRH+5,3                                                     
         OI    AVMBUYRH+6,X'80'                                                 
*                                                                               
         MVC   BCLT,AVAKCLT2                                                    
         GOTO1 CLUNPK,DMCB,AVAKCLT2,AVMCLT    DISPLAY THE CLIENT                
         MVI   AVMCLTH+5,3                                                      
         OI    AVMCLTH+6,X'80'                                                  
*                                                                               
         MVC   BEST,AVAKEST2                                                    
         CLI   AVAKEST2,0          DISPLAY ESTIMATE IF ANY                      
         BE    VK16                                                             
         ZIC   R1,AVAKEST2                                                      
         CVD   R1,DUB                                                           
         UNPK  AVMEST(3),DUB+6(2)                                               
         OI    AVMEST+2,X'F0'                                                   
         OI    AVMESTH+4,X'08'     VALID NUMERIC                                
         MVI   AVMESTH+5,3                                                      
         OI    AVMESTH+6,X'80'                                                  
         B     VK18                                                             
*                                                                               
VK16     XC    AVMEST,AVMEST                                                    
         XC    AVMESXP,AVMESXP                                                  
         MVI   AVMESTH+5,0                                                      
         OI    AVMESTH+6,X'80'                                                  
         OI    AVMESXPH+6,X'80'                                                 
*                                                                               
VK18     MVC   BPRD,AVAKPRD2       COPY THE PRODUCT CODE                        
*                                                                               
         GOTO1 GETQPRD             SHOW EBCDIC PRODUCT CODE                     
         BE    *+6                                                              
         DC    H'0'                DIE IF ERROR                                 
         MVC   AVMPRD,QPRD                                                      
         MVI   AVMPRDH+5,L'QPRD                                                 
         OI    AVMPRDH+6,X'80'                                                  
*****                                                                           
* VALIDATE THE BUYER                                                            
*****                                                                           
VK20     LA    R2,AVMBUYRH         R2 = A(FIELD HDR FOR BUYER)                  
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLBUYRH                                                      
*                                                                               
         CLI   5(R2),0             IF NO BUYER AND NOT LIST MODE                
         BNE   VK25                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   MISSFLD             THEN WE HAVE A MISSING FIELD                 
         B     VK30                                                             
*                                                                               
VK25     OI    FILTRFLG,X'40'      FILTER ON BUYER (LIST MODE)                  
         OC    8(L'AVMBUYR,R2),SPACES                                           
*                                                                               
         GOTO1 VALIBUYR,DMCB,8(R2)    VALIDATE BUYER                            
         BNE   INVLFLD                                                          
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK30                THEN WE HAVE A MISSING FIELD                 
         MVC   AVMBYXP,QBUYER                                                   
         OI    AVMBYXPH+6,X'80'    SHOW THE BUYER'S FULL NAME                   
*****                                                                           
* VALIDATE THE ADVERTISER (CLIENT)                                              
*****                                                                           
VK30     XC    BCLT,BCLT           CLEAR THE BINARY CLIENT CODE                 
*                                                                               
         LA    R2,AVMCLTH          R2 = A(FLDHDR OF ADVERTISER)                 
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLCLTH                                                       
*                                                                               
         CLI   5(R2),0             IF NO ADVERTISER AND NOT LIST MODE           
         BNE   VK35                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   MISSFLD             THEN WE HAVE A MISSING FIELD                 
         B     VK40                                                             
*                                                                               
VK35     OI    FILTRFLG,X'20'      FILTER ON ADVERTISER (LIST MODE)             
*                                                                               
         GOTO1 VALICLT             GET INFO FOR THE ADVERTISER                  
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK40                                                             
*                                                                               
         MVC   AVMCLXP(L'CLTNM),CLTNM   SHOW THE CLIENT NAME                    
         OI    AVMCLXPH+6,X'80'                                                 
*****                                                                           
* VALIDATE THE PRODUCT                                                          
*****                                                                           
VK40     XC    BPRD,BPRD           CLEAR THE BINARY PRODUCT CODE                
*                                                                               
         LA    R2,AVMPRDH          R2 = A(FLDHDR OF PRODUCT)                    
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLPRDH                                                       
*                                                                               
         CLI   5(R2),0             IF NO PRODUCT AND NOT LIST MODE              
         BNE   VK45                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   MISSFLD             THEN WE HAVE A MISSING FIELD                 
         B     VK50                                                             
*                                                                               
VK45     OC    BCLT,BCLT           IF NO CLIENT                                 
         BNZ   VK45A                                                            
         LA    R2,AVMCLTH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLCLTH                                                       
         B     MISSFLD             THEN ERROR, CAN'T VALIDATE PRODUCT           
*                                                                               
VK45A    OI    FILTRFLG,X'10'      FILTER ON PRODUCT (LIST MODE)                
*                                                                               
         GOTO1 VALIPRD             GET INFO FOR THE PRODUCT                     
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK50                                                             
*                                                                               
         MVC   AVMPRXP(L'PRDNM),PRDNM   SHOW THE PRODUCT NAME                   
         OI    AVMPRXPH+6,X'80'                                                 
*****                                                                           
* VALIDATE THE ESTIMATE                                                         
*****                                                                           
VK50     XC    BEST,BEST           CLEAR BINARY ESTIMATE CODE                   
*                                                                               
         LA    R2,AVMESTH          R2 = A(FLDHDR OF ESTIMATE)                   
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLESTH                                                       
*                                                                               
         CLI   5(R2),0             IF NO ESTIMATE                               
         BE    VKBKEY              THEN BUILD KEY, OPTIONAL                     
*                                                                               
         OC    BPRD,BPRD           IF NO PRODUCT                                
         BNZ   VK55                                                             
         LA    R2,AVMPRDH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLPRDH                                                       
         B     MISSFLD             THEN ERROR, CAN'T VALIDATE ESTIMATE          
*                                                                               
VK55     OI    FILTRFLG,X'08'      FILTER ON ESTIMATE (LIST MODE)               
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ESTIMATE IS ZERO IN LIST MODE             
         BNE   VK55A                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    VKBKEY              THEN IT'S OKAY                               
*                                                                               
VK55A    GOTO1 VALIEST             GET INFO FOR THE ESTIMATE                    
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKBKEY                                                           
         MVC   AVMESXP(L'ESTNM),ESTNM                                           
         GOTO1 DATCON,DMCB,(0,ESTSTRT),(11,AVMESXP+L'ESTNM+1)                   
         MVI   AVMESXP+L'ESTNM+1+8,C'-'                                         
         GOTO1 DATCON,DMCB,(0,ESTEND),(11,AVMESXP+L'ESTNM+1+8+1)                
         OI    AVMESXPH+6,X'80'                                                 
*                                                                               
         EJECT                                                                  
*****                                                                           
* BUILD THE KEY FOR GENCON                                                      
*****                                                                           
VKBKEY   CLI   ACTNUM,ACTLIST      LIST MODE DOESN'T BUILD A KEY IN VK          
         BE    VKXIT                                                            
*                                                                               
         XC    KEY,KEY             CLEAN OUT THE KEY                            
*                                                                               
         LA    R4,KEY                                                           
         USING AVARECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVI   AVAKTYP,AVAKTYPQ    LOAD UP THE IDENTIFIERS                      
         MVI   AVAKSUB,AVAKSUBQ    NOT PASSIVE KEY                              
         MVC   AVAKAM,BAGYMD                                                    
         MVC   AVAKBYR,AVMBUYR                                                  
         MVC   AVAKCLT,BCLT                                                     
         MVC   AVAKPRD,BPRD                                                     
         MVC   AVAKEST,BEST                                                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKB10                                                            
         GOTO1 GNEXTREF            GET NEXT REFERENCE NUMBER                    
         MVC   FULL,DMCB                                                        
         GOTO1 PAKTOREF,DMCB,FULL                                               
         MVC   AVAKREF,DMCB+1                                                   
         B     VKXIT                                                            
*                                                                               
VKB10    CLI   AVMREFNH+5,1        IF FULL REFERENCE NUMBER ENTERED             
         BE    VKB20                                                            
         PACK  DUB(4),AVMREFN+1(6)  THEN LOOK FOR THAT ONE                      
         GOTO1 PAKTOREF,DMCB,DUB                                                
         MVC   AVAKREF,DMCB+1                                                   
         B     VKXIT                                                            
*                                  MEDIA ONLY, LOOK FOR MULTIPLE REF #          
VKB20    GOTO1 HIGH                REFERENCE NUMBER NOT IN KEY                  
*                                                                               
*                                  IF NO REFERENCE NUMBER FOR SUCH              
         CLC   KEY(AVAKREF-AVAKEY),KEYSAVE                                      
         BE    VKB30                                                            
         MVC   KEY,KEYSAVE         THEN RETURN TO GENCON WITH SUCH              
         B     VKXIT                                                            
*                                                                               
VKB30    MVC   SVKEY,KEY           SAVE THE KEY WITH THE REFERENCE #            
         GOTO1 SEQ                                                              
*                                  IF ONLY 1 REF # FOR SUCH                     
         CLC   KEY(AVAKREF-AVAKEY),SVKEY                                        
         BE    VKB40                                                            
         MVC   KEY,SVKEY           THEN RETURN TO GENCON WITH SUCH              
         GOTO1 REFTOPAK,DMCB,AVAKREF                                            
         UNPK  AVMREFN+1(6),DMCB(4)                                             
         OI    AVMREFN+6,X'F0'                                                  
         OI    AVMREFNH+6,X'80'                                                 
         B     VKXIT                                                            
*                                                                               
VKB40    MVC   KEY,SVKEY           MORE THAN 1 REF # FOR THE KEY                
         MVI   PFKEY,13            SET PFKEY TO RUN THE CODE TO LIST            
         CLI   AVMESTH+5,0                                                      
         BNE   VKXIT                                                            
         MVI   AVMESTH+5,1         CHANGE ESTIMATE TO 0 IF NOTHING              
         MVI   AVMEST,C'0'             ENTERED SO THAT LIST WILL USE 0          
         OI    AVMESTH+4,X'08'                                                  
         OI    AVMESTH+6,X'80'                                                  
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVI   PREVFLAG,1          KEY BEING USED BY LIST                       
         MVC   PREVKEY,KEY                                                      
         LA    R4,PREVKEY          POINT TO THE KEY TO DISPLAY                  
         USING AVARECD,R4                                                       
*                                                                               
         MVC   AVMREFN(L'QMED),QMED    DISPLAY THE REFERENCE NUMBER             
         GOTO1 REFTOPAK,DMCB,AVAKREF                                            
         UNPK  AVMREFN+1(6),DMCB(4)                                             
         OI    AVMREFN+6,X'F0'                                                  
         OI    AVMREFNH+6,X'80'                                                 
*                                                                               
         MVC   AVMBUYR,AVAKBYR     DISPLAY THE BUYER                            
         MVI   AVMBUYRH+5,L'AVMBUYR                                             
         OI    AVMBUYRH+6,X'80'                                                 
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALIBUYR,DMCB,AVMBUYR     SHOW THE BUYER'S FULL NAME             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         MVC   AVMBYXP,QBUYER                                                   
         OI    AVMBYXPH+6,X'80'                                                 
*                                                                               
         GOTO1 CLUNPK,DMCB,AVAKCLT,AVMCLT   DISPLAY THE CLIENTP                 
         MVI   AVMCLTH+5,L'AVMCLT                                               
         OI    AVMCLTH+6,X'80'                                                  
         LA    R2,AVMCLTH                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 VALICLT                                                          
         MVC   AIO,AIO1                                                         
         MVC   AVMCLXP(L'CLTNM),CLTNM    SHOW THE CLIENT NAME                   
         OI    AVMCLXPH+6,X'80'                                                 
*                                                                               
DK10     MVC   BPRD,AVAKPRD        DISPLAY THE PRODUCT                          
         GOTO1 GETQPRD             SHOW EBCDIC PRODUCT CODE                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AVMPRD,QPRD                                                      
         MVI   AVMPRDH+5,L'QPRD                                                 
         OI    AVMPRDH+6,X'80'                                                  
*                                                                               
         LA    R2,AVMPRDH                                                       
         GOTO1 VALIPRD             GET INFO FOR THE PRODUCT                     
*                                                                               
         MVC   AVMPRXP(L'PRDNM),PRDNM   SHOW THE PRODUCT NAME                   
         OI    AVMPRXPH+6,X'80'                                                 
*                                                                               
         XC    AVMEST,AVMEST                                                    
         OI    AVMESTH+6,X'80'                                                  
         XC    AVMESXP,AVMESXP                                                  
         OI    AVMESXPH+6,X'80'                                                 
         CLI   AVAKEST,0           DISPLAY ESTIMATE IF ANY                      
         BE    DKXIT                                                            
         ZIC   R1,AVAKEST                                                       
         CVD   R1,DUB                                                           
         UNPK  AVMEST(3),DUB+6(2)                                               
         OI    AVMEST+2,X'F0'                                                   
         OI    AVMESTH+4,X'08'     VALID NUMERIC                                
         MVI   AVMESTH+5,3                                                      
*                                                                               
         LA    R2,AVMESTH                                                       
         GOTO1 VALIEST             GET INFO FOR THE ESTIMATE                    
*                                                                               
         MVC   AVMESXP(L'ESTNM),ESTNM   SHOW ESTIMATE NAME AND PERIOD           
         GOTO1 DATCON,DMCB,(0,ESTSTRT),(11,AVMESXP+L'ESTNM+1)                   
         MVI   AVMESXP+L'ESTNM+1+8,C'-'                                         
         GOTO1 DATCON,DMCB,(0,ESTEND),(11,AVMESXP+L'ESTNM+1+8+1)                
*                                                                               
DKXIT    MVC   KEY,PREVKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
*****                                                                           
* DISPLAY THE DESCRIPTION ELEMENT                                               
*****                                                                           
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,AVARDCDQ     GET DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
*                                                                               
         MVC   AVMASST,AVARDAST                                                 
         OI    AVMASSTH+6,X'80'                                                 
         XC    AVMASXP,AVMASXP                                                  
         OI    AVMASXPH+6,X'80'                                                 
*                                                                               
         CLC   AVARDAST,SPACES     IF ANY ASSISTANT                             
         BE    DR00                                                             
*                                                                               
         MVC   AIO,AIO2            SHOW THE ASSISTANT'S FULL NAME               
         GOTO1 VALIBUYR,DMCB,AVMASST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         MVC   AVMASXP,QBUYER                                                   
         OI    AVMASXPH+6,X'80'                                                 
*                                                                               
DR00     OC    AVARDDUE,AVARDDUE                                                
         BZ    DR01                                                             
         GOTO1 DATCON,DMCB,(8,AVARDDUE),(11,AVMDUED)                            
         OI    AVMDUEDH+6,X'80'                                                 
DR01     GOTO1 DATCON,DMCB,(8,AVARDFLS),(11,AVMFLTS)                            
         OI    AVMFLTSH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(8,AVARDFLE),(11,AVMFLTE)                            
         OI    AVMFLTEH+6,X'80'                                                 
         MVC   AVMSTD1,AVARDCM1                                                 
         OI    AVMSTD1H+6,X'80'                                                 
         MVC   AVMSTD2,AVARDCM2                                                 
         OI    AVMSTD2H+6,X'80'                                                 
         MVC   AVMSTD3,AVARDCM3                                                 
         OI    AVMSTD3H+6,X'80'                                                 
         MVC   AVMSTD4,AVARDCM4                                                 
         OI    AVMSTD4H+6,X'80'                                                 
         MVC   AVMSTD5,AVARDCM5                                                 
         OI    AVMSTD5H+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(8,AVARDCRE),(11,AVMCREA)                            
         OI    AVMCREAH+6,X'80'                                                 
         XC    AVMCHNG,AVMCHNG                                                  
         OI    AVMCHNGH+6,X'80'                                                 
         OC    AVARDUPT,AVARDUPT                                                
         BZ    DR03                                                             
         GOTO1 DATCON,DMCB,(8,AVARDUPT),(11,AVMCHNG)                            
DR03     XC    AVMLSNT,AVMLSNT                                                  
         OI    AVMLSNTH+6,X'80'                                                 
         OC    AVARDSNT,AVARDSNT                                                
         BZ    DR04                                                             
         GOTO1 DATCON,DMCB,(8,AVARDSNT),(11,AVMLSNT)                            
DR04     XC    AVMNMKT,AVMNMKT                                                  
         EDIT  (B2,AVARDMKT),(4,AVMNMKT),ALIGN=LEFT,ZERO=NOBLANK                
         OI    AVMNMKTH+6,X'80'                                                 
*****                                                                           
* DISPLAY THE DAYPART/SPOTLEN ELEMENT                                           
*****                                                                           
DR05     XC    AVMDAYP,AVMDAYP     CLEAR DAYPART & SPOT LENGTH LINES            
         XC    AVMSPLN,AVMSPLN                                                  
         OI    AVMDAYPH+6,X'80'    XMIT DAYPART AND SPOT LENGTH LINES           
         OI    AVMSPLNH+6,X'80'                                                 
*                                                                               
***** COMMENTED OUT BECAUSE WE'LL TAKE WHATEVER THEY PUT IN FOR NOW             
*                                                                               
*         L     R6,AIO              POINT TO THE RECORD                         
*         MVI   ELCODE,AVARSCDQ     GET DAYPART/SPOTLEN ELEMENT                 
*         BAS   RE,GETEL                                                        
*         BNE   DR10                                                            
*         USING AVARSPTD,R6                                                     
**                                                                              
*         ZIC   R1,AVARSPLN         FIND NUMBER OF DAYPART/SPOTLEN SETS         
*         SH    R1,=H'2'                                                        
*         SR    R0,R0                                                           
*         LA    R2,3                                                            
*         DR    R0,R2                                                           
*         LR    R0,R1               R0 = NUMBER OF DAYPART/SPOTLEN SETS         
**                                                                              
*         LA    R2,AVMDAYP          R2 = A(DAYPART LINE)                        
*         LA    R4,AVMDAYP+L'AVMDAYP                                            
*         LA    R3,AVARSDPT         R3 = A(DAYPART/SPOTLEN SET)                 
*         DROP  R6                                                              
*         LR    R6,R0               R6 = NUMBER OF DAYPART/SPOTLEN SETS         
*         USING AVARSDPT,R3                                                     
**                                                                              
*DR05LP   CLI   AVARSDPT,0          IF THERE IS A DAYPART                       
*         BE    *+14                                                            
*         MVC   0(L'AVARSDPT,R2),AVARSDPT      THEN SHOW IT                     
*         LA    R2,1(R2)                                                        
*                                                                               
*         CLI   AVARSSLN,0          IF THERE IS A SPOT LENGTH                   
*         BE    DR05NX              THEN SHOW IT                                
*         EDIT  (B1,AVARSSLN),(3,0(R2)),ALIGN=LEFT                              
*DR05LP1  LA    R2,1(R2)                                                        
*         CLI   0(R2),0             IF NULL OR BLANK                            
*         BE    DR05NX              THEN WE CAN PUT COMMA HERE                  
*         CLI   0(R2),C' '                                                      
*         BNE   DR05LP1                                                         
**                                                                              
*DR05NX   LR    R1,R2               IF ENOUGH ROOM ON LINE FOR MORE             
*         LA    R1,5(R1)                                                        
*         CR    R1,R4                                                           
*         BH    DR05A                                                           
*         C     R6,=F'1'            THEN DO WE NEED TO PUT A DELIMETER?         
*         BE    DR05NXA                                                         
*         MVI   0(R2),C','          YES                                         
*         LA    R2,1(R2)                                                        
*DR05NXA  LA    R3,AVARSSET(R3)     R3 = A(NEXT SET)                            
*         BCT   R6,DR05LP                                                       
*         B     DR10                                                            
**                                                                              
*DR05A    LA    R4,AVMSPLN          GO TO NEXT LINE?                            
*         CR    R2,R4                                                           
*         BL    *+6                                                             
*         DC    H'0'                                                            
*         LA    R2,AVMSPLN YES                                                  
*         LA    R4,AVMSPLN+L'AVMSPLN                                            
*         B     DR05NXA                                                         
*         DROP  R3                                                              
*                                                                               
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,AVARPCEQ     GET DAYPART(S) ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   DR08                                                             
         USING AVARDPTD,R6                                                      
*                                                                               
         ZIC   R1,AVARPLEN                                                      
         SH    R1,=Y(AVARPOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVMDAYP(0),AVARPTXT                                              
*                                                                               
DR08     L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,AVARSCEQ     GET SPOT LENGTH(S) ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING AVARSLND,R6                                                      
*                                                                               
         ZIC   R1,AVARSLEN                                                      
         SH    R1,=Y(AVARSOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVMSPLN(0),AVARSTXT                                              
*****                                                                           
* DISPLAY THE DEMO ELEMENT                                                      
*****                                                                           
DR10     XC    AVMRSRV,AVMRSRV     CLEAR DAYPART & SPOT LENGTH LINES            
         XC    AVMRBOK,AVMRBOK                                                  
         XC    AVMSURV,AVMSURV                                                  
         XC    AVMDEMO,AVMDEMO                                                  
         OI    AVMRSRVH+6,X'80'    XMIT DAYPART AND SPOT LENGTH LINES           
         OI    AVMRBOKH+6,X'80'                                                 
         OI    AVMSURVH+6,X'80'                                                 
         OI    AVMDEMOH+6,X'80'                                                 
*                                                                               
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,AVARMCDQ     GET DEMO ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         USING AVARDMOD,R6                                                      
         MVC   AVMRSRV,AVARMSRV                                                 
         OI    AVMRSRVH+6,X'80'                                                 
         MVC   AVMRBOK,AVARMBK                                                  
         OI    AVMRBOKH+6,X'80'                                                 
         MVC   AVMSURV,AVARMARA                                                 
         OI    AVMSURVH+6,X'80'                                                 
         ZIC   R1,AVARDMLN                                                      
         SH    R1,=Y(AVARDMLQ)                                                  
         BNP   DR20                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVMDEMO(0),AVARMDMO                                              
         OI    AVMDEMOH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR20     OI    AVMCOMSH+6,X'80'                                                 
         XC    AVMCOMS,AVMCOMS                                                  
         MVC   AVMCOMS(32),=CL32'   COMMENTS EXIST FOR THIS AVAIL'              
         L     R6,AIO                                                           
         MVI   ELCODE,AVARCCEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+10                                                             
         MVC   AVMCOMS(2),=C'NO'                                                
*                                                                               
DRXIT    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         TM    BITFLAG,X'01'       LIST STANDARD COMMENTS MODE?                 
         BZ    *+16                                                             
         BAS   RE,LSTCMMNT                                                      
         BNE   MAKESELS            YES, PLEASE MAKE YOUR SELECTIONS             
         B     *+8                                                              
         OI    GENSTAT2,NEXTSEL                                                 
*                                                                               
         LA    R6,DESCELEM         SET BASIC ELEMENT INFORMATION FOR            
         USING AVARDSCD,R6             DESCRIPTION ELEMENT                      
         XC    DESCELEM,DESCELEM   CLEAR THE DESCRIPTION ELEMENT                
         MVI   AVARDCDE,AVARDCDQ                                                
         MVI   AVARDSLN,AVARDSLQ                                                
*                                                                               
*         LA    R6,DPTELEM          SET BASIC ELEMENT INFORMATION FOR           
*         USING AVARSPTD,R6             DAYPART/SPOTLEN ELEMENT                 
*         XC    DPTELEM,DPTELEM     CLEAR THE DAYPART(S) ELEMENT                
*         MVI   AVARSCDE,AVARSPLQ                                               
*         MVI   AVARSPLN,2          SO FAR 2 BYTES ONLY                         
*                                                                               
         LA    R6,DEMOELEM         SET BASIC ELEMENT INFORMATION FOR            
         USING AVARDMOD,R6             DEMO ELEMENT                             
         XC    DEMOELEM,DEMOELEM   CLEAR THE DEMO ELEMENT                       
         MVI   AVARMCDE,AVARMCDQ                                                
         MVI   AVARDMLN,AVARDMLQ   SO FAR 19 BYTES ONLY                         
*                                                                               
         LA    R6,DESCELEM         SAVE THE OFFICE ID FOR BUYER                 
         USING AVARDSCD,R6                                                      
         MVC   AIO,AIO2                                                         
         LA    R2,AVMBUYRH                                                      
         GOTO1 VALIBUYR,DMCB,8(R2)   VALIDATE BUYER                             
         BNE   INVLFLD                                                          
         MVC   AVARDBYO,QOFFICE                                                 
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION NOT ADD                            
         BE    VR00                                                             
         GOTO1 READ                THEN GET THE DISK ADDRESS                    
         GOTO1 GETREC                                                           
*****                                                                           
* VALIDATE THE ASSISTANT BUYER                                                  
*****                                                                           
VR00     LA    R2,AVMASSTH         R2 = A(FIELD HDR FOR BUYER)                  
         CLI   5(R2),0             IF THERE IS AN ASSISTANT BUYER               
         BE    VR01                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALIBUYR,DMCB,8(R2)    VALIDATE BUYER                            
         BNE   INVLFLD                                                          
         MVC   AIO,AIO1                                                         
         MVC   AVMASXP,QBUYER                                                   
         OI    AVMASXPH+6,X'80'    SHOW THE BUYER'S FULL NAME                   
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION NOT ADD                            
         BE    VR01                                                             
         GOTO1 READ                THEN GET THE DISK ADDRESS                    
         GOTO1 GETREC                                                           
*                                                                               
VR01     MVC   AVARDAST,8(R2)      SPACE FILL IF ANY NULLS                      
         OC    AVARDAST,SPACES                                                  
*****                                                                           
* VALIDATE THE DUE DATE                                                         
*****                                                                           
         LA    R4,PERVALST                                                      
         USING PERVALD,R4                                                       
*                                                                               
         LA    R2,AVMDUEDH         VALIDATE DUE DATE                            
         CLI   5(R2),0                                                          
         BE    VR04                                                             
         BAS   RE,VRDATE                                                        
*                                  PUT DUE DATE IN DESCRIPTION ELEMENT          
         GOTO1 DATCON,DMCB,(0,PVALESTA),(19,AVARDDUE)                           
*****                                                                           
* VALIDATE THE FLIGHT START DATE                                                
*****                                                                           
VR04     LA    R2,AVMFLTSH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         BAS   RE,VRDATE                                                        
*                                  PUT FLIGHT START DATE IN DESC ELEM           
         GOTO1 DATCON,DMCB,(0,PVALESTA),(19,AVARDFLS)                           
*                                                                               
         CLI   AVMESTH+5,0         IF SOMETHING IN THE ESTIMATE                 
         BE    VR06                                                             
         CLC   PVALESTA,ESTSTRT    MAKE SURE FLT START DT IN EST PERIOD         
         BL    INVLFLD                                                          
         CLC   PVALESTA,ESTEND                                                  
         BH    INVLFLD                                                          
         B     VR06                                                             
*                                                                               
VRDATE   LR    R3,RE               SAVE WHERE TO GO BACK                        
         GOTO1 =A(VALIDTE),DMCB,(RC),(R2),RR=RELO                               
         BR    R3                  RETURN TO CALLER                             
*                                                                               
*****                                                                           
* VALIDATE THE FLIGHT START DATE                                                
*****                                                                           
VR06     LA    R2,AVMFLTEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         BAS   RE,VRDATE                                                        
*                                  PUT FLIGHT END DATE IN DESC ELEMENT          
         GOTO1 DATCON,DMCB,(0,PVALESTA),(19,AVARDFLE)                           
*                                                                               
         CLI   AVMESTH+5,0         IF SOMETHING IN THE ESTIMATE                 
         BE    VR10                                                             
         CLC   PVALESTA,ESTSTRT    MAKE SURE FLT END DATE IN EST PERIOD         
         BL    INVLFLD                                                          
         CLC   PVALESTA,ESTEND                                                  
         BH    INVLFLD                                                          
         DROP  R4                                                               
*****                                                                           
* VALIDATE THE DAYPART/SPOT LENGTHS                                             
*****                                                                           
*VR10     LA    R6,DPTELEM+AVARSPLQ   R6 = A(DAYPART/SPOTLEN ELEMENT)           
*         USING AVARSDPT,R6                                                     
*         LA    R2,AVMDAYPH         R2 = A(DAYPART ENTRY FIELD)                 
*         MVC   AIO,AIO2                                                        
*         CLI   AVMDAYPH+5,0        IF NO DAYPARTS NOR SPOT LENGTHS             
*         BNE   VR20                                                            
*         CLI   AVMSPLNH+5,0                                                    
*         BE    MISSFLD             THEN ERROR                                  
**                                                                              
*         MVC   AVMDAYPH+5(1),AVMSPLNH+5   IF NOTHING IN 1ST LINE, BUT          
*         MVC   AVMDAYP,AVMSPLN               SOMETHING IN 2ND LINE             
*         XC    AVMSPLN,AVMSPLN            THEN COPY 2ND LINE TO LST            
*         MVI   AVMSPLNH+5,0                                                    
*         OI    AVMDAYPH+6,X'80'                                                
*         OI    AVMSPLNH+6,X'80'                                                
**                                                                              
*VR20     MVI   NUMBDPSP,0          ZERO COUNT                                  
**                                                                              
*VR20LP0  GOTO1 SCANNER,DMCB,(R2),(15,BLOCK)                                    
**                                                                              
*         CLI   DMCB+4,0            ERROR IF NO FIELDS AFTER SCANNER            
*         BE    INVLFLD                                                         
**                                                                              
*         ZIC   R0,DMCB+4           R0 = # OF DAYPARTS/SPOT LENGTHS             
*         ZIC   R1,NUMBDPSP         CALCULATE # OF DAYPARTS/SPOTLENS            
*         AR    R1,R0                                                           
*         STC   R1,NUMBDPSP                                                     
**                                                                              
*         LA    R3,BLOCK            R3 = A(1ST ENTRY)                           
*                                                                               
*VR20LP1  CLI   1(R3),0             IS THERE A SECOND HALF?                     
*         BNE   INVLFLD             THERE SHOULDN'T BE ANY                      
**                                                                              
*         TM    2(R3),X'C0'         COMBINATION OF DAYPART AND SPOTLEN?         
*         BZ    VR20C                                                           
*         TM    2(R3),X'80'         NUMERIC?  SPOT LENGTH ONLY                  
*         BNZ   VR20B                                                           
**                                                                              
*         CLI   0(R3),1             DAYPART SHOULD ONLY BE ONE BYTE             
*         BNE   INVLFLD                                                         
**                                                                              
*VR20A    MVC   AVARSDPT,12(R3)     MOVE DAYPART INTO ELEMENT                   
**                                                                              
*         CLI   AVMESTH+5,0         IF NO ESTIMATE SPECIFIED                    
*         BE    VR20NX              THEN CHECK NEXT ENTRY                       
**                                                                              
*         TM    BITFLAG,X'80'       DAYPART MENU READ BEFORE?                   
*         BNZ   VR20A00                                                         
**                                  NO, THEN READ IT ONCE                       
*         GOTO1 =A(READDPTS),DMCB,(RC),(RA),(R9),(R2),RR=RELO                   
*         OI    BITFLAG,X'80'       DON'T HAVE TO READ IT AGAIN                 
**                                                                              
*VR20A00  MVC   QDPT,12(R3)         VALIDATE USING THIS DAYPART                 
*         GOTO1 =A(VALIDPT),DMCB,(RC),(R5),(R9),(R2),RR=RELO                    
**                                                                              
*         MVC   AVARSDNB,BDPTNUM    MOVE DAYPART NUMBER IN ELEMENT              
**                                                                              
*         B     VR20NX              CHECK NEXT ENTRY                            
**                                                                              
*VR20B    CLC   =F'255',4(R3)       ERROR IF SPOT LENGTH NOT IN 1 BYTE          
*         BL    INVLFLD                                                         
**                                                                              
*         MVC   BSLN,7(R3)          VALIDATE USING THIS SPOT LENGTH             
*         GOTO1 =A(VALISPLN),DMCB,(RC),(R9),(R2),RR=RELO                        
**                                                                              
*         MVC   AVARSSLN,BSLN       MOVE SPOT LENGTH IN ELEMENT                 
**                                                                              
*         B     VR20NX              CHECK NEXT ENTRY                            
**                                                                              
*VR20C    MVC   AVARSDPT,12(R3)     MOVE DAYPART INTO ELEMENT                   
**                                                                              
*         CLI   AVMESTH+5,0         IF NO ESTIMATE SPECIFIED                    
*         BE    VR20C10             THEN CHECK SPOT LENGTH                      
**                                                                              
*         TM    BITFLAG,X'80'       DAYPART MENU READ BEFORE?                   
*         BNZ   VR20C00                                                         
**                                  NO, THEN READ IT ONCE                       
*         GOTO1 =A(READDPTS),DMCB,(RC),(RA),(R9),(R2),RR=RELO                   
*         OI    BITFLAG,X'80'       DON'T HAVE TO READ IT AGAIN                 
**                                                                              
*VR20C00  MVC   QDPT,12(R3)         VALIDATE USING THIS DAYPART                 
*         GOTO1 =A(VALIDPT),DMCB,(RC),(R5),(R9),(R2),RR=RELO                    
**                                                                              
*         MVC   AVARSDNB,BDPTNUM    MOVE DAYPART NUMBER IN ELEMENT              
**                                                                              
*VR20C10  LA    R1,13(R3)           R1 = A(SPOT LENGTH AFTER DAYPART)           
*         ZIC   R4,0(R3)            R4 = # OF BYTES FOR SPOT LENGTH             
*         BCTR  R4,0                                                            
**                                                                              
*VR20CLP  CLI   0(R1),C'0'          TEST BYTE IS NOT NUMERIC                    
*         BL    INVLFLD                                                         
*         CLI   0(R1),C'9'                                                      
*         BH    INVLFLD                                                         
**                                                                              
*         LA    R1,1(R1)            BUMP TO NEXT BYTE AND TRY AGAIN             
*         BCT   R4,VR20CLP                                                      
**                                                                              
*         ZIC   R1,0(R3)            R1 = (# OF BYTES FOR SPOTLEN) - 1           
*         BCTR  R1,0                                                            
*         BCTR  R1,0                                                            
*         EX    R1,*+8                                                          
*         B     *+10                                                            
*         PACK  DUB,13(0,R3)                                                    
*         CVB   R4,DUB                                                          
**                                                                              
*         C     R4,=F'255'          ERROR IF SPOT LENGTH NOT IN 1 BYTE          
*         BH    INVLFLD                                                         
**                                                                              
*         STC   R4,BSLN             VALIDATE USING THIS SPOT LENGTH             
*         GOTO1 =A(VALISPLN),DMCB,(RC),(R9),(R2),RR=RELO                        
**                                                                              
*         MVC   AVARSSLN,BSLN       MOVE SPOT LENGTH IN ELEMENT                 
**                                                                              
*VR20NX   LA    R3,32(R3)           EXAMINE NEXT ENTRY IN BLOCK                 
*         LA    R6,AVARSSET(R6)     R6 = A(NEXT SET OF DAYPART/SPOTLEN)         
*         BCT   R0,VR20LP1          UNTIL THERE IS NO MORE                      
**                                                                              
*         LA    R0,AVMSPLNH         DID WE CHECK THE SPOT LENGTH LINE?          
*         CR    R2,R0                                                           
*         BE    VR30                YES                                         
*         LA    R2,AVMSPLNH                                                     
*         CLI   5(R2),0             DONE IF SPOTLEN LINE HAS NOTHING            
*         BNE   VR20LP0                                                         
**                                                                              
*VR30     ZIC   R1,NUMBDPSP         GET L(DAYPART/SPOTLEN ELEMENT)              
*         MH    R1,=Y(AVARSSET)                                                 
*         LA    R1,AVARSPLQ(R1)                                                 
*         STC   R1,DPTELEM+1        STORE THE LENGTH OF THE ELEMENT             
*         DROP  R6                                                              
*                                                                               
VR10     LA    R6,DPTELEM                                                       
         XC    DPTELEM,DPTELEM     CLEAR THE DAYPART(S) ELEMENT                 
         USING AVARDPTD,R6                                                      
         LA    R2,AVMDAYPH         R2 = A(DAYPART ENTRY FIELD)                  
         CLI   5(R2),0                                                          
         BE    VR15                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVARPTXT(0),8(R2)                                                
         AH    R1,=Y(AVARPOVQ+1)                                                
         STC   R1,AVARPLEN                                                      
         MVI   AVARPCDE,AVARPCEQ                                                
*                                                                               
VR15     LA    R6,SLNELEM                                                       
         XC    SLNELEM,SLNELEM     CLEAR THE SPOT LENGTH(S) ELEMENT             
         USING AVARSLND,R6                                                      
         LA    R2,AVMSPLNH         R2 = A(SPOTLEN ENTRY FIELD)                  
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVARSTXT(0),8(R2)                                                
         AH    R1,=Y(AVARSOVQ+1)                                                
         STC   R1,AVARSLEN                                                      
         MVI   AVARSCDE,AVARSCEQ                                                
*****                                                                           
* VALIDATE THE RATING SERVICE                                                   
*****                                                                           
VR20     LA    R6,DEMOELEM                                                      
         USING AVARDMOD,R6                                                      
         MVC   AIO,AIO1                                                         
         LA    R2,AVMRSRVH         R2 = A(RATING SERVICE)                       
         CLI   5(R2),3             IF NOT 3 CHARACTERS                          
         BNE   INVLFLD             THEN ERROR                                   
         CLC   =C'ARB',8(R2)       ARBITRON?                                    
         BE    VR30                                                             
         CLC   =C'BIR',8(R2)       BIRCH?                                       
         BE    VR30                                                             
         CLC   =C'NSI',8(R2)       NEILSEN?                                     
         BNE   INVLFLD                                                          
VR30     MVC   AVARMSRV,8(R2)                                                   
*                                                                               
*****                                                                           
* VALIDATE THE RATING BOOK                                                      
*****                                                                           
VR40     LA    R2,AVMRBOKH         R2 = A(RATING BOOK)                          
         CLI   5(R2),0             OKAY IF NOTHING ENTERED                      
         BE    VR50                                                             
         MVC   AVARMBK,8(R2)                                                    
*****                                                                           
* VALIDATE THE SURVEY AREA                                                      
*****                                                                           
VR50     LA    R2,AVMSURVH         R2 = A(SURVEY AREA)                          
         CLI   5(R2),0             OKAY IF NOTHING ENTERED                      
         BE    VR60                                                             
         CLI   AVMREFN,C'R'        ERROR IF MEDIA ISN'T RADIO                   
         BNE   INVLFLD                                                          
         MVC   AVARMARA,8(R2)                                                   
*****                                                                           
* VALIDATE THE DEMOS                                                            
*****                                                                           
VR60     LA    R2,AVMDEMOH         R2 = A(DEMOS)                                
         CLI   5(R2),0             OKAY IF NOTHING ENTERED                      
         BE    VR70                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVARMDMO(0),8(R2)                                                
         LA    R1,AVARDMLQ+1(R1)                                                
         STC   R1,AVARDMLN                                                      
*****                                                                           
* VALIDATE THE STANDARD COMMENTS                                                
*****                                                                           
VR70     LA    R6,DESCELEM                                                      
         USING AVARDSCD,R6                                                      
         LA    R2,AVMSTD1H         IF THERE ARE ANY COMMENTS                    
         CLI   5(R2),0                                                          
         BE    VR72                                                             
         GOTO1 VALISCOM,DMCB,8(R2)                                              
         BNE   INVLFLD                                                          
         MVC   AVARDCM1,8(R2)      THEN SAVE THE COMMENTS                       
         OC    AVARDCM1,SPACES                                                  
*                                                                               
VR72     LA    R2,AVMSTD2H         IF THERE ARE ANY COMMENTS                    
         CLI   5(R2),0                                                          
         BE    VR74                                                             
         GOTO1 VALISCOM,DMCB,8(R2)                                              
         BNE   INVLFLD                                                          
         MVC   AVARDCM2,8(R2)      THEN SAVE THE COMMENTS                       
         OC    AVARDCM2,SPACES                                                  
*                                                                               
VR74     LA    R2,AVMSTD3H         IF THERE ARE ANY COMMENTS                    
         CLI   5(R2),0                                                          
         BE    VR76                                                             
         GOTO1 VALISCOM,DMCB,8(R2)                                              
         BNE   INVLFLD                                                          
         MVC   AVARDCM3,8(R2)      THEN SAVE THE COMMENTS                       
         OC    AVARDCM3,SPACES                                                  
*                                                                               
VR76     LA    R2,AVMSTD4H         IF THERE ARE ANY COMMENTS                    
         CLI   5(R2),0                                                          
         BE    VR78                                                             
         GOTO1 VALISCOM,DMCB,8(R2)                                              
         BNE   INVLFLD                                                          
         MVC   AVARDCM4,8(R2)      THEN SAVE THE COMMENTS                       
         OC    AVARDCM4,SPACES                                                  
*                                                                               
VR78     LA    R2,AVMSTD5H         IF THERE ARE ANY COMMENTS                    
         CLI   5(R2),0                                                          
         BE    BLDREC                                                           
         GOTO1 VALISCOM,DMCB,8(R2)                                              
         BNE   INVLFLD                                                          
         MVC   AVARDCM5,8(R2)      THEN SAVE THE COMMENTS                       
         OC    AVARDCM5,SPACES                                                  
         EJECT                                                                  
*****                                                                           
* BUILD THE RECORD                                                              
*****                                                                           
BLDREC   DS    0H                                                               
         CLI   ACTNUM,ACTADD       CHANGE MODE?                                 
         BE    BLDR05                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
*                                  YES, COPY OLD DATA                           
         MVC   DESCELEM+AVARDCRE-AVARDSCD(AVARDMKT+L'AVARDMKT-AVARDCRE)X        
               ,AVARDCRE                                                        
         DROP  R6                                                               
*                                                                               
BLDR05   L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ     REMOVE OLD DESCRIPTION ELEMENTS              
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R3,DESCELEM                                                      
         USING AVARDSCD,R3                                                      
         CLI   ACTNUM,ACTADD                                                    
         BNE   BLDR10                                                           
         GOTO1 DATCON,DMCB,(5,0),(19,AVARDCRE)                                  
         DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,AVARDCRE),(11,AVMCREA)                            
         MVI   AVMCREAH+5,8                                                     
         OI    AVMCREAH+6,X'80'                                                 
         GOTO1 GNEXTREF            SHOW NEW REFERENCE NUMBER                    
         UNPK  AVMREFN+1(6),DMCB(4)                                             
         OI    AVMREFN+6,X'F0'                                                  
         MVI   AVMREFNH+5,7                                                     
         OI    AVMREFNH+6,X'80'                                                 
         B     BLDR20                                                           
*                                                                               
BLDR10   GOTO1 DATCON,DMCB,(5,0),(19,AVARDUPT)                                  
         DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,AVARDUPT),(11,AVMCHNG)                            
         MVI   AVMCHNGH+5,8                                                     
         OI    AVMCHNGH+6,X'80'                                                 
*                                                                               
************************************************                                
* ASK MEL WHAT WOULD HAPPEN IF THE BUYER                                        
* CHANGES OFFICE.  THE OFFICE CODE IN THE                                       
* DESCRIPTION ELEMENT WOULD BE WRONG.                                           
************************************************                                
*                                                                               
BLDR20   MVC   ELEM(L'DESCELEM),DESCELEM                                        
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
*         MVI   ELCODE,AVARSCDQ     REMOVE OLD DAYPART/SPOTLEN ELEMENTS         
         MVI   ELCODE,AVARPCEQ     REMOVE OLD DAYPART(S) ELEMENTS               
         GOTO1 REMELEM                                                          
         CLI   DPTELEM,0                                                        
         BE    BLDR30                                                           
         MVC   ELEM(L'DPTELEM),DPTELEM                                          
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDR30   L     R6,AIO                                                           
         MVI   ELCODE,AVARSCEQ     REMOVE OLD DAYPART(S) ELEMENTS               
         GOTO1 REMELEM                                                          
         CLI   SLNELEM,0                                                        
         BE    BLDR40                                                           
         MVC   ELEM(L'SLNELEM),SLNELEM                                          
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDR40   L     R6,AIO                                                           
         MVI   ELCODE,AVARMCDQ     REMOVE OLD DEMO ELEMENTS                     
         GOTO1 REMELEM                                                          
         MVC   ELEM(L'DEMOELEM),DEMOELEM                                        
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRXIT    MVC   SVKEY,KEY           SAVE KEY SO WE CAN GET DA LATER              
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LIST THE STANDARD COMMENTS FOR THE USER TO SELECT.  MAXIMUM OF 5.             
*                                                                               
* NOTE:  UPON SELECTING THE FIFTH ONE, IT WILL RETURN BACK WITH THE 5.          
*        IT WILL ALSO OVERWRITE WHAT WAS ALREADY INPUTTED ON SCREEN.            
***********************************************************************         
LSTCMMNT NTR1                                                                   
         MVC   AIO,AIO2                                                         
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
         MVI   IOOPT,C'Y'                                                       
*                                                                               
LSC05    TM    BITFLAG,X'02'       FIRST TIME IN?                               
         BZ    LSC10               NO                                           
         L     RE,ATIA                                                          
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         LA    R2,X'81'            WRITE TWA RECORD #1                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
         GOTO1 CALLOV,DMCB,(X'C3',CONTAGH),0,0                                  
         LA    R4,CMMNTKEY                                                      
         USING COMKEY,R4                                                        
         XC    COMKEY,COMKEY                                                    
         MVI   COMKTYP,COMKTYPQ                                                 
         MVI   COMKSUB,COMKSUBQ                                                 
         MVC   COMKAM,BAGYMD                                                    
         DROP  R4                                                               
         MVI   CMMNTCNT,0          NO COMMENTS IN LIST                          
         XC    CMMNTLST,CMMNTLST                                                
         NI    BITFLAG,X'FF'-X'02'   NOT FIRST TIME ANYMORE                     
         B     LSC90                                                            
*                                                                               
LSC10    LA    R2,AVCFRSTH                                                      
         USING LSCDSECT,R2                                                      
LSC10LP  LA    R0,AVCTAGH          LAST LINE CHECKED?                           
         CR    R2,R0                                                            
         BE    LSC80               YES                                          
*                                                                               
         CLI   CMMNTCNT,0          ANY SELECTIONS?                              
         BE    LSC13               NO                                           
         LA    R1,CMMNTLST                                                      
         ZIC   R0,CMMNTCNT                                                      
LSC12    CLC   LSCCOMCD,0(R1)      A SELECTED COMMENT?                          
         BNE   LSC12A              NO                                           
         CLI   LSCSEL,C'S'         IF STILL SELECTED                            
         BE    LSC15               THEN GO TO NEXT LINE                         
*                                                                               
         ZIC   RE,CMMNTCNT         DECREMENT # OF SELECTED COMMENTS             
         BCTR  RE,0                                                             
         STC   RE,CMMNTCNT                                                      
*                                                                               
         BCTR  R0,0                ANY MORE SELECTED AFTER THIS ONE?            
         LTR   R0,R0                                                            
         BZ    LSC15               NO                                           
*                                                                               
         LR    RE,R0               YES, MOVE THEM FORWARD                       
         MH    RE,=Y(L'COMKCOM)                                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),L'COMKCOM(R1)                                            
         B     LSC15                                                            
*                                                                               
LSC12A   LA    R1,L'COMKCOM(R1)                                                 
         BCT   R0,LSC12                                                         
*                                                                               
LSC13    CLI   LSCSEL,C'S'         NOT SELECTED?                                
         BNE   LSC15               NO, CHECK NEXT LINE                          
*                                                                               
         ZIC   R1,CMMNTCNT                                                      
         MH    R1,=Y(L'COMKCOM)                                                 
         LA    R0,CMMNTLST                                                      
         AR    R1,R0                                                            
         MVC   0(L'COMKCOM,R1),LSCCOMCD                                         
         OC    0(L'COMKCOM,R1),SPACES                                           
         ZIC   R1,CMMNTCNT                                                      
         LA    R1,1(R1)                                                         
         STC   R1,CMMNTCNT                                                      
         CLI   CMMNTCNT,5          FIFTH ONE ALREADY?                           
         BE    LSC80A              YES, RETURN TO THE AVAIL SCREEN              
*                                                                               
LSC15    LA    R2,LSCNEXTL         CHECK THE NEXT LINE                          
         B     LSC10LP                                                          
         DROP  R2                                                               
*                                                                               
LSC80    CLI   PFKEY,12            RETURN TO THE AVAIL SCREEN?                  
         BNE   LSC90                                                            
LSC80A   NI    BITFLAG,X'FF'-X'01'   DON'T LIST STANDARD COMMENTS               
         LA    R2,1                READ TWA RECORD #1                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
         L     RE,ATIA                                                          
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         L     R2,ATWA             MUST SET INDICATOR TO XMIT FIELDS            
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         LA    R2,AVMSTD1H                                                      
         ST    R2,ACURFORC                                                      
         MVI   PFKEY,0                                                          
         MVI   IOOPT,C'N'                                                       
*                                                                               
         CLI   CMMNTCNT,0          NO COMMENTS?                                 
         BE    LSCYES                                                           
*                                                                               
         TWAXC AVMSTD1H,AVMSTD5H,TRNS=N                                         
         MVI   AVMSTD1H+5,0        CLEAR OLD COMMENTS                           
         MVI   AVMSTD2H+5,0            AND THE LENGTH IN THE FIELDS             
         MVI   AVMSTD3H+5,0                                                     
         MVI   AVMSTD4H+5,0                                                     
         MVI   AVMSTD5H+5,0                                                     
*                                                                               
         ZIC   R1,CMMNTCNT                                                      
         LA    R3,CMMNTLST                                                      
LSC80LP  MVC   8(L'COMKCOM,R2),0(R3)                                            
         MVI   5(R2),L'COMKCOM                                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,L'COMKCOM(R3)                                                 
         BCT   R1,LSC80LP                                                       
         B     LSCYES                                                           
*                                                                               
LSC90    TWAXC AVCFRSTH,AVCTAGH,PROT=Y    CLEAR SCREEN FOR NEXT PAGE            
         LA    R2,AVCFRSTH                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'CMMNTKEY),CMMNTKEY                                         
         GOTO1 HIGH                                                             
*                                                                               
LSC90TST LA    R0,AVCTAGH          NO MORE ROOM TO LIST?                        
         CR    R2,R0                                                            
         BE    LSCNO               YES                                          
         USING LSCDSECT,R2                                                      
*                                                                               
         CLC   KEY(COMKCOM-COMKEY),KEYSAVE   SAME AGENCY MEDIA?                 
         BE    LSC95                                                            
         LA    R4,CMMNTKEY         NO, START LIST FROM BEGINNING                
         USING COMKEY,R4                                                        
         XC    COMKEY,COMKEY                                                    
         MVI   COMKTYP,COMKTYPQ                                                 
         MVI   COMKSUB,COMKSUBQ                                                 
         MVC   COMKAM,BAGYMD                                                    
         B     LSCNO                                                            
         DROP  R4                                                               
*                                                                               
LSC95    GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING COMRECD,R6                                                       
         OI    LSCSELH+6,X'01'                                                  
*                                                                               
         CLI   CMMNTCNT,0          NO NEED TO CHECK FOR DUPLICATES              
         BE    LSC96                                                            
         LA    R1,CMMNTLST                                                      
         ZIC   R0,CMMNTCNT                                                      
LSC95A   CLC   COMKCOM,0(R1)       DISREGARD ANY DUPLICATES                     
         BNE   *+12                                                             
         MVI   LSCSEL,C'S'                                                      
         B     LSC96                                                            
         LA    R1,L'COMKCOM(R1)                                                 
         BCT   R0,LSC95A                                                        
*                                                                               
LSC96    MVC   LSCCOMCD,COMKCOM                                                 
         MVI   ELCODE,COMTXTEQ                                                  
         BAS   RE,GETEL                                                         
         USING COMTXTD,R6                                                       
         CLI   COMTXTLN,L'LSCLINE+COMTXTOV                                      
         BL    *+14                                                             
         MVC   LSCLINE,COMTXTTX                                                 
         B     LSC97                                                            
         ZIC   R1,COMTXTLN                                                      
         SH    R1,=Y(COMTXTOV+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSCLINE(0),COMTXTTX                                              
*                                                                               
LSC97    L     R6,AIO                                                           
         USING COMRECD,R6                                                       
         MVC   CMMNTKEY,COMKEY                                                  
         DROP  R6                                                               
         LA    R2,LSCNEXTL                                                      
         GOTO1 SEQ                                                              
         B     LSC90TST                                                         
         DROP  R2                                                               
*                                                                               
LSCNO    MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         B     NO                                                               
*                                                                               
LSCYES   MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ADD PASSIVE KEY FOR THE RECORD AFTER IT HAS BEEN ADDED                        
***********************************************************************         
XA       DS    0H                                                               
         LA    R4,KEY              1ST 4 BYTES OF KEY = ?? AFTER ADDREC         
         USING AVAKEY,R4                                                        
         MVC   KEY,SVKEY           RESTORE KEY THAT WAS ADDED                   
         GOTO1 HIGH                READ TO GET DISK ADDRESS FROM KEY            
         CLC   KEY(L'AVAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVKEY,KEY           GOT DISK ADDRESS                             
         MVI   AVAKSUB,AVAKSUB2    SUB-TYPE FOR PASSIVE KEYS                    
         MVC   AVAKBYR2(AVAKCNTL-AVAKBYR2),SVKEY+AVAKBYR-AVAKEY                 
         MVC   AVAKREF2,SVKEY+AVAKREF-AVAKEY                                    
         MVC   AVAKDA,SVKEY+AVAKDA-AVAKEY                                       
         GOTO1 ADD                 ADD PASSIVE KEY                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY           RESTORE KEY WE ADDED IN 1ST PLACE            
         MVI   AVMNMKT,C'0'                                                     
         OI    AVMNMKTH+6,X'80'                                                 
         MVC   AVMCOMS(32),=CL32'   COMMENTS EXIST FOR THIS AVAIL'              
         MVC   AVMCOMS(2),=C'NO'                                                
         OI    AVMCOMSH+6,X'80'                                                 
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* DELETE PASSIVE KEY FOR THE RECORD AFTER IT HAS BEEN DELETED                   
***********************************************************************         
XD       DS    0H                                                               
         LA    R4,KEY                                                           
         USING AVAKEY,R4                                                        
         MVC   SVKEY,KEY                                                        
         MVI   AVAKSUB,AVAKSUB2    SUB-TYPE FOR PASSIVE KEYS                    
         MVC   AVAKBYR2(AVAKCNTL-AVAKBYR2),SVKEY+AVAKBYR-AVAKEY                 
         MVC   AVAKREF2,SVKEY+AVAKREF-AVAKEY                                    
         MVC   AVAKDA,SVKEY+AVAKDA-AVAKEY                                       
         GOTO1 HIGH                READ TO GET DISK ADDRESS FROM KEY            
         CLC   KEY(L'AVAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                DIE IF NO PASSIVE KEY                        
         OI    AVAKCNTL,X'80'      SET DELETED BIT ON                           
         GOTO1 WRITE                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* RESTORE PASSIVE KEY FOR THE RECORD AFTER IT HAS BEEN RESTORED                 
***********************************************************************         
XR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING AVAKEY,R4                                                        
         MVC   SVKEY,KEY                                                        
         MVI   AVAKSUB,AVAKSUB2    SUB-TYPE FOR PASSIVE KEYS                    
         MVC   AVAKBYR2(AVAKCNTL-AVAKBYR2),SVKEY+AVAKBYR-AVAKEY                 
         MVC   AVAKREF2,SVKEY+AVAKREF-AVAKEY                                    
         MVC   AVAKDA,SVKEY+AVAKDA-AVAKEY                                       
         OI    DMINBTS,X'08'       READ THE DELETED RECORD                      
         GOTO1 HIGH                READ TO GET DISK ADDRESS FROM KEY            
         CLC   KEY(L'AVAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                DIE IF NO PASSIVE KEY                        
         NI    AVAKCNTL,X'FF'-X'80'  SET DELETED BIT OFF                        
         GOTO1 WRITE                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       MVI   NLISTS,12           # OF LINES TO LIST, DEFAULT = 15             
         OI    GLSTSTAT,RETEXTRA   LISTMON WILL COME BACK EVEN IF LAST          
         LA    R4,KEY                                                           
         USING AVAKEY,R4                                                        
*                                                                               
         CLI   PREVFLAG,0          A PREVIOUS KEY EXAMINED?                     
         BE    LR10                                                             
         MVC   KEY(L'PREVKEY),PREVKEY   YES, THEN USED THAT KEY                 
         MVI   PREVFLAG,0                                                       
*                                                                               
         CLI   AVAKSUB,AVAKSUB2    IF NOT PASSIVE KEY                           
         BE    LR10                                                             
         MVI   AVAKSUB,AVAKSUB2    THEN MAKE KEY A PASSIVE KEY                  
         MVC   AVAKBYR2(AVAKCNTL-AVAKBYR2),PREVKEY+AVAKBYR-AVAKEY               
         MVC   AVAKREF2,PREVKEY+AVAKREF-AVAKEY                                  
*                                                                               
LR10     CLI   AVAKTYP,AVAKTYPQ    IF NOT 0DB1  -  PASSIVE KEY SUB-TYPE         
         BNE   *+12                THEN MAKE IT THE PASSIVE KEY                 
         CLI   AVAKSUB,AVAKSUB2                                                 
         BE    LRFRST                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   AVAKTYP,AVAKTYPQ                                                 
         MVI   AVAKSUB,AVAKSUB2    SEARCH WITH REFERENCE #'S                    
         MVC   AVAKAM,BAGYMD       LOAD UP THE AGENCY/MEDIA CODE                
*                                                                               
         CLI   AVLREFNH+5,1        IF ONLY MEDIA                                
         BE    LR28                THEN CHECK THE YEAR                          
*                                                                               
         MVC   WORK(6),AVLREFN+1   COPY DIGITS IF ANY                           
*                                                                               
         CLI   AVLREFNH+5,7        IF FULL REFERENCE #                          
         BNE   LR20                                                             
LR15     PACK  DUB(4),WORK(6)      THEN START LIST FROM THERE                   
         GOTO1 PAKTOREF,DMCB,DUB                                                
         MVC   AVAKREF2,DMCB+1                                                  
         B     LRFRST                                                           
*                                                                               
LR20     ZIC   R1,AVLREFNH+5       GET THE LENGTH INPUTTED                      
LR21     LA    R0,7                                                             
         SR    R0,R1               NUMBER OF DIGITS TO FULL REFERENCE #         
         LA    R2,WORK+5                                                        
LR25     MVI   0(R2),C'9'          GET LATEST FIRST                             
         BCTR  R2,0                                                             
         BCT   R0,LR25                                                          
         B     LR15                                                             
*                                                                               
LR28     GOTO1 DATCON,DMCB,(5,0),(0,DUB)                                        
         MVC   WORK(1),DUB+1       PUT YEAR DIGIT IN REFERENCE #                
         LA    R1,2                                                             
         B     LR21                                                             
*                                                                               
LRFRST   MVI   RDUPDATE,C'N'       LOOK FOR THE FIRST RECORD                    
         GOTO1 HIGH                                                             
*                                                                               
LRTEST   CLC   KEY(3),KEYSAVE      CHECK IF STILL AVAIL RECORD TYPE             
         BE    *+12                                                             
         MVI   PREVFLAG,0                                                       
         B     LRXIT               EXIT, NOT SAME AVAIL RECORD TYPE             
*                                                                               
         OC    KEY+8(5),KEY+8      IF NOT AVAIL RECORD (SEE DCSPOTKEYS)         
         BZ    LRNEXT              THEN GET NEXT RECORD                         
*                                                                               
         TM    FILTRFLG,X'80'      FILTER ON REFERENCE NUMBER?                  
         BZ    LRF10                                                            
*                                                                               
LRF10    TM    FILTRFLG,X'40'      FILTER ON BUYER?                             
         BZ    LRF20                                                            
         CLC   AVLBUYR,AVAKBYR2    IF BUYER DOESN'T MATCH FILTER                
         BNE   LRNEXT              THEN CHECK THE NEXT RECORD                   
*                                                                               
LRF20    TM    FILTRFLG,X'20'      FILTER ON ADVERTISER?                        
         BZ    LRF30                                                            
         CLC   BCLT,AVAKCLT2       IF CLIENT DOESN'T MATCH FILTER               
         BNE   LRNEXT              THEN CHECK THE NEXT RECORD                   
*                                                                               
LRF30    TM    FILTRFLG,X'10'      FILTER ON PRODUCT?                           
         BZ    LRF40                                                            
         CLC   BPRD,AVAKPRD2       IF PRODUCT DOESN'T MATCH FILTER              
         BNE   LRNEXT              THEN CHECK THE NEXT RECORD                   
*                                                                               
LRF40    TM    FILTRFLG,X'08'      FILTER ON ESTIMATE?                          
         BZ    LRF50                                                            
         CLC   BEST,AVAKEST2       IF ESTIMATE DOESN'T MATCH FILTER             
         BNE   LRNEXT              THEN CHECK THE NEXT RECORD                   
*                                                                               
LRF50    TM    FILTRFLG,X'04'      FILTER ON PERIOD?                            
         BZ    LRDISP                                                           
*                                                                               
LRDISP   MVC   LISTAR,SPACES                                                    
         MVC   LSTREFN(L'QMED),AVLREFN                                          
         GOTO1 REFTOPAK,DMCB,AVAKREF2                                           
         UNPK  LSTREFN+1(6),DMCB(4)                                             
         OI    LSTREFN+6,X'F0'                                                  
         MVC   LSTBUYR,AVAKBYR2                                                 
         MVI   LSTSLASH,C'/'                                                    
         GOTO1 CLUNPK,DMCB,AVAKCLT2,LSTCLT                                      
*                                                                               
         CLI   AVAKEST2,0          IF THERE IS AN ESTIMATE                      
         BE    LR30                                                             
         ZIC   R1,AVAKEST2         THEN DISPLAY IT                              
         CVD   R1,DUB                                                           
         UNPK  LSTEST(3),DUB+6(2)                                               
         OI    LSTEST+2,X'F0'                                                   
*                                                                               
LR30     MVC   SAVEKEY,KEY         SAVE KEY BEFORE IT GETS CLOBBERED            
*                                                                               
         MVC   BCLT,AVAKCLT2                                                    
         MVC   BPRD,AVAKPRD2       DISPLAY THE PRODUCT                          
         GOTO1 GETQPRD                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTPRD,QPRD         SHOW THE PRODUCT ON LIST LINE                
*                                                                               
LR40     LA    R4,SAVEKEY                                                       
         USING AVAKEY,R4                                                        
*        GOTO1 VALIBUYR,DMCB,AVAKBYR2                                           
*        DROP  R4                                                               
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        MVC   LSTOFF,QOFFICE      SHOW THE OFFICE ON LIST LINE                 
*                                                                               
LR50     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   KEY(L'SAVEKEY),SAVEKEY    RESTORE KEY SAVED                      
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO              GET THE DESCRIPTION ELEMENT                  
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
         MVC   LSTOFF,AVARDBYO     SHOW THE OFFICE ON LIST LINE                 
         GOTO1 DATCON,DMCB,(8,AVARDFLS),(11,LSTFLTD)                            
         MVI   LSTFLTD+L'AVMFLTS,C'-'                                           
         GOTO1 DATCON,DMCB,(8,AVARDFLE),(11,LSTFLTD+L'AVMFLTS+1)                
         OC    AVARDDUE,AVARDDUE                                                
         BZ    LR55                                                             
         GOTO1 DATCON,DMCB,(8,AVARDDUE),(11,LSTDUED)                            
*                                                                               
LR55     LA    R4,AVARDSNT         LAST STATUS WAS SENT?                        
         OC    AVARDSNT,AVARDSNT                                                
         BZ    *+14                NO                                           
         MVC   LSTSTAT,=C'SENT'                                                 
         B     LR60                                                             
*                                                                               
         LA    R4,AVARDUPT         LAST STATUS WAS CHANGE?                      
         OC    AVARDUPT,AVARDUPT                                                
         BZ    *+14                NO                                           
         MVC   LSTSTAT,=C'CHNG'                                                 
         B     LR60                                                             
*                                                                               
         LA    R4,AVARDCRE         LAST STATUS WAS CREATED                      
         MVC   LSTSTAT,=C'OPEN'                                                 
*                                                                               
LR60     GOTO1 DATCON,DMCB,(8,(R4)),(11,LSTDATE)                                
         LA    R4,KEY                                                           
*                                                                               
LRSHOW   GOTO1 LISTMON                                                          
         MVC   PREVKEY,SAVEKEY     SAVE THE LAST KEY EXAMINED                   
         MVI   PREVFLAG,1                                                       
*                                                                               
LRNEXT   GOTO1 SEQ                                                              
         B     LRTEST                                                           
*                                                                               
LRXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* HEADER ROUTINE                                                                
***********************************************************************         
HDHK     NTR1                                                                   
         CLI   USEHDHK,C'Y'        OKAY TO USE HEADER?                          
         BNE   HDHKX               NO                                           
*                                                                               
         NI    SPOOLIND,X'FF'-SPNSPACE   PUT A SPACE AFTER HEADLINES            
         MVC   H3,SPACES                                                        
         MVC   H3+34(13),=CL13' A. D. D. S. '                                   
         MVC   H5,SPACES                                                        
         MVC   H5+34(13),=CL13'AVAIL REQUEST'                                   
         MVC   H7,SPACES                                                        
         MVC   H7(12),=C'Reference #:'                                          
         MVC   H7+15(7),AVPREFN                                                 
         CLI   MRKHEAD1,0          NOTHING IN THE HEADLINE YET?                 
         BE    *+10                                                             
         MVC   H9,MRKHEAD1         ELSE SHOW THE HEADLINES                      
         CLI   MRKHEAD2,0          NOTHING IN THE HEADLINE YET?                 
         BE    *+10                                                             
         MVC   H10,MRKHEAD2                                                     
*                                                                               
HDHKX    XIT1                                                                   
         SPACE 2                                                                
NOTHING  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
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
MAKESELS MVI   GERROR1,9           LIST SHOWN-SELECT OR HIT ENTER               
         MVI   GMSGTYPE,C'I'                                                    
         LA    R2,AVCFRSTH                                                      
         B     ERREXIT                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,55,RUN                                                        
         DC    X'00'                                                            
RELO     DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                                       
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* AVAIL MARKETS LIST                                                            
         DC    AL1(PF02X-*,02,PFTCPROG,X'E4',(PF02X-PF02)/KEYLNQ,0)             
         DC    CL3' ',CL8'AVMKTS  ',CL8'LIST    '                               
PF02     DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
PF02X    EQU   *                                                                
*                                                                               
* AVAIL COMMENTS CHANGE                                                         
         DC    AL1(PF03X-*,03,PFTCPROG,X'F8',(PF03X-PF03)/KEYLNQ,0)             
         DC    CL3' ',CL8'AVCOM   ',CL8'CHANGE  '                               
PF03     DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
PF03X    EQU   *                                                                
*                                                                               
* AVAIL SEND                                                                    
         DC    AL1(PF04X-*,04,PFTCPROG,X'D3',(PF04X-PF04)/KEYLNQ,0)             
         DC    CL3' ',CL8'AVAIL   ',CL8'SEND    '                               
PF04     DC    AL1(KEYTYTWA,L'AVMREFN-1),AL2(AVMREFN-T212FFD)                   
PF04X    EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
* AVAIL LIST OF MORE THAN 1 REF #                                               
         DC    AL1(PF13X-*,13,0,0,(PF13X-PF13)/KEYLNQ,0)                        
         DC    CL3' ',CL8'AVAIL   ',CL8'LIST    '                               
PF13     DC    AL1(KEYTYTWA,L'QMED-1),AL2(AVMREFN-T212FFD)                      
         DC    AL1(KEYTYTWA,L'AVMBUYR-1),AL2(AVMBUYR-T212FFD)                   
         DC    AL1(KEYTYTWA,L'AVMCLT-1),AL2(AVMCLT-T212FFD)                     
         DC    AL1(KEYTYTWA,L'AVMPRD-1),AL2(AVMPRD-T212FFD)                     
         DC    AL1(KEYTYTWA,L'AVMEST-1),AL2(AVMEST-T212FFD)                     
PF13X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
LPFTABLE DS    0C                                                               
*                                                                               
* AVAIL MARKETS LIST                                                            
         DC    AL1(LPF02X-*,02,PFTCPROG,X'E4',(LPF02X-LPF02)/KEYLNQ,0)          
         DC    CL3'M',CL8'AVMKTS  ',CL8'LIST    '                               
LPF02    DC    AL1(KEYTYCUR,L'AVLREFN-1),AL2(0)  REF # IS FIRST                 
LPF02X   EQU   *                                                                
*                                                                               
* AVAIL COMMENTS CHANGE                                                         
         DC    AL1(LPF03X-*,03,PFTCPROG,X'F8',(LPF03X-LPF03)/KEYLNQ,0)          
         DC    CL3'R',CL8'AVCOM   ',CL8'CHANGE  '                               
LPF03    DC    AL1(KEYTYCUR,L'AVLREFN-1),AL2(0)                                 
LPF03X   EQU   *                                                                
*                                                                               
* AVAIL SEND                                                                    
         DC    AL1(LPF04X-*,04,PFTCPROG,X'D3',(LPF04X-LPF04)/KEYLNQ,0)          
         DC    CL3'X',CL8'AVAIL   ',CL8'SEND    '                               
LPF04    DC    AL1(KEYTYCUR,L'AVLREFN-1),AL2(0)  REF # IS FIRST                 
LPF04X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
PPFTABLE DS    0C                                                               
*                                                                               
* AVAIL MARKETS LIST                                                            
         DC    AL1(PPF02X-*,02,PFTCPROG,X'E4',(PPF02X-PPF02)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVMKTS  ',CL8'LIST    '                               
PPF02    DC    AL1(KEYTYTWA,L'AVPREFN-1),AL2(AVPREFN-T212FFD)                   
PPF02X   EQU   *                                                                
*                                                                               
* AVAIL COMMENTS CHANGE                                                         
         DC    AL1(PPF03X-*,03,PFTCPROG,X'F8',(PPF03X-PPF03)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVCOM   ',CL8'CHANGE  '                               
PPF03    DC    AL1(KEYTYTWA,L'AVPREFN-1),AL2(AVPREFN-T212FFD)                   
PPF03X   EQU   *                                                                
*                                                                               
* AVAIL DISPLAY                                                                 
         DC    AL1(PPF04X-*,04,PFTCPROG,X'F3',(PPF04X-PPF04)/KEYLNQ,0)          
         DC    CL3' ',CL8'AVAIL   ',CL8'DISPLAY '                               
PPF04    DC    AL1(KEYTYTWA,L'AVPREFN-1),AL2(AVPREFN-T212FFD)                   
PPF04X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE AVAIL INFORMATION ON ONE LINE.                                    
***********************************************************************         
DISPINFO DS    0H                                                               
         NMOD1 0,**DINF**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
*                                                                               
         LA    R4,KEY                                                           
         USING AVARECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         LA    R3,AVPINFOH         R3 = A(FLDHDR OF INFORMATION LINE)           
*                                                                               
         XC    8(L'AVPINFO,R3),8(R3)                                            
         MVI   5(R3),L'AVPINFO                                                  
         OI    6(R3),X'80'                                                      
         LA    R3,8(R3)                                                         
         USING INFOLIND,R3                                                      
*                                                                               
         MVC   LINBUYR,AVAKBYR2    DISPLAY THE BUYER                            
         MVI   LINSLSH,C'/'                                                     
* CODE COMMENTED OUT BECAUSE WE'RE GETTING OFFICE FROM ELEMENT                  
*        GOTO1 VALIBUYR,DMCB,LINBUYR                                            
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
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
         MVC   LINOFID,AVARDBYO    OFFICE ID                                    
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
         LA    R4,AVARDCRE         LAST STATUS WAS OPENED                       
         MVC   LINSTAT,=CL12'   OPENED ON'                                      
*                                                                               
DINF20   GOTO1 DATCON,DMCB,(8,(R4)),(11,LINDATE)                                
*                                                                               
         OC    AVARDMKT,AVARDMKT                                                
         BNZ   DINF30                                                           
         LA    R2,AVPREFNH                                                      
         XC    AVPLIN1,AVPLIN1                                                  
         MVC   AVPLIN1+13(34),=CL34'THIS AVAIL DOES NOT HAVE A MARKET '         
         MVC   AVPLIN1+47(20),=CL20'LIST ATTACHED TO IT '                       
         OI    AVPLIN1H+6,X'80'                                                 
         XC    AVPLIN2,AVPLIN2                                                  
         OI    AVPLIN2H+6,X'80'                                                 
         B     INVLFLD                                                          
*                                                                               
DINF30   B     XIT                                                              
         DROP  R3,R4,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE REPS FOR THE MEDIA.                                               
***********************************************************************         
DISPREPS DS    0H                                                               
         NMOD1 0,**DREP**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(L'AVAKEY),KEY                                              
*                                                                               
         TWAXC AVPREPH,AVPLIN2H,PROT=Y                                          
*                                                                               
         LA    R2,AVPREPH          R2 = A(1ST REP HEADER)                       
         L     R3,ATREPTAB                                                      
         CLI   QMED,C'T'                                                        
         BE    *+8                                                              
         L     R3,ARREPTAB                                                      
*                                                                               
DREP00   CLI   0(R3),0             END OF REP TABLE?                            
         BE    DREP10              YES                                          
*                                                                               
         LA    R0,AVPLREPH         MORE THAN WE CAN FIT ON SCREEN?              
         CR    R2,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                DIE IF SO                                    
*                                                                               
         MVC   8(L'AVPREP,R2),2(R3)   DISPLAY REP                               
*                                                                               
         ZIC   R1,0(R3)            GO TO NEXT TABLE ENTRY                       
         AR    R3,R1                                                            
         ZIC   R1,0(R2)            GO TO OFFICE FIELD                           
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            GO TO NEXT REP FIELD                         
         AR    R2,R1                                                            
         B     DREP00                                                           
*                                                                               
DREP10   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DIRKEYD,R4                                                       
         MVC   DATADISP,=Y(DIRELDQ)                                             
         MVI   DIRKSYS,DIRKSYSQ                                                 
         MVI   DIRTYPE,DIRTYPEQ                                                 
         MVC   DIRMED,QMED                                                      
         MVC   DIRID,TWAORIG                                                    
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO                                                           
         USING DIRKEYD,R6                                                       
*                                  KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         CLC   DIRKEY,KEY             IT ISN'T A GENCON READ HIGH               
         BNE   DREPXIT             NO DIRADD RECORD, NO REPS                    
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,DIRREPEQ     GET REP ELEMENTS                             
         BAS   RE,GETEL                                                         
         BNE   DREPXIT                                                          
*                                                                               
         USING DIRREPD,R6                                                       
DREPLP   LA    R2,AVPREPH                                                       
         ZIC   R1,DIRREPIN         GET SEQUENCE NUMBER                          
         MH    R1,=Y(AVPREP2H-AVPREPH)                                          
         AR    R2,R1               R2 = A(REP'S FULL NAME)                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 = A(REP'S OFFICE)                         
*                                                                               
         CLI   DIRREPID+4,C' '     2 CHAR AGENCY?                               
         BE    *+14                                                             
         MVC   8(2,R2),DIRREPID+3  NO                                           
         B     *+10                                                             
         MVC   8(2,R2),DIRREPID+2  YES                                          
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DREPLP                                                           
         DROP  R6                                                               
*                                                                               
DREPXIT  XC    KEY,KEY                                                          
         MVC   KEY(L'AVAKEY),SVKEY                                              
         MVC   DATADISP,=H'24'     RESET DATADISP                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES A DATE FIELD                                           
*                                                                               
* ON ENTRY:    R2                  A(DATE FIELD HEADER)                         
***********************************************************************         
VALIDTE  DS    0H                                                               
         NMOD1 0,**VDTE**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R5,SYSSPARE                                                      
*                                                                               
         LA    R3,8(R2)                                                         
         ICM   R3,8,5(R2)                                                       
         LA    R4,PERVALST                                                      
         ICM   R4,8,=X'40'         SINGLE DATE ONLY                             
         GOTO1 PERVAL,DMCB,(R3),(R4)                                            
         TM    DMCB+4,X'03'                                                     
         BNZ   INVLDATE                                                         
         USING PERVALD,R4                                                       
         MVC   8(8,R2),PVALCPER                                                 
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
*                                                                               
INVLDATE MVI   GERROR1,INVDATE                                                  
         GOTO1 MYERR                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS THE DAYPARTS FOR A DAYPART MENU INTO AIO.                  
***********************************************************************         
READDPTS DS    0H                                                               
         NMOD1 0,**RDPT**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     R2,12(R1)                                                        
*                                                                               
         LA    R1,DMCB             READ THE DAYPART MENU                        
         MVC   0(2,R1),14(RA)      GET SIGNON ID                                
         MVC   2(1,R1),QMED                                                     
         MVC   3(1,R1),ESTDYMNU                                                 
         GOTO1 DPTRD,(R1),,AIO,DATAMGR                                          
*                                                                               
         CLI   8(R1),X'FF'         DIE IF ERROR READING MENU                    
         BE    INVLDPT1                                                         
         TM    8(R1),X'08'                                                      
         BO    INVLDPT1                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
INVLDPT1 MVI   GERROR1,INVDPT                                                   
         GOTO1 MYERR                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE DAYPART FOR A GIVEN DAYPART MENU IN AIO.           
*                                                                               
* ON ENTRY:    QDPT                DAYPART TO BE VALIDATED                      
*                                                                               
* ON EXIT:     BDPTNUM             DAYPART NUMBER                               
***********************************************************************         
VALIDPT  DS    0H                                                               
         NMOD1 0,**VDPT**                                                       
         L     RC,0(R1)                                                         
         L     R5,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     R2,12(R1)                                                        
*                                                                               
         LA    R0,36               R0 = MAXIMUM NUMBER OF DAYPART CODES         
         L     R6,AIO              R6 = A(FIRST DAYPART IN MENU)                
*                                                                               
VD10     CLI   0(R6),0             ERROR IF NO MORE DAYPARTS                    
         BE    INVLDPT2                                                         
*                                                                               
         CLI   0(R6),C'Z'          IF DAYPART IS A 'Z'                          
         BE    VD20                THEN SKIP IT (SEE SPBUY01, EST21)            
*                                                                               
         CLC   QDPT,0(R6)          IF DAYPART MATCHES                           
         BNE   VD20                                                             
         MVC   BDPTNUM,1(R6)       THEN SAVE DAYPART NUMBER                     
         B     VDX                      AND RETURN TO CALLER                    
*                                                                               
VD20     LA    R6,5(R6)            R1 = A(NEXT DAYPART IN MENU)                 
*                                                                               
         BCT   R0,VD10             LOOP BACK UNTIL MATCH OR NO MORE             
         B     INVLDPT2                                                         
*                                                                               
VDX      B     XIT                                                              
*                                                                               
INVLDPT2 MVI   GERROR1,INVDPT                                                   
         GOTO1 MYERR                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE SPOT LENGTH.                                       
*                                                                               
* ON ENTRY:    BSLN                SPOT LENGTH TO BE VALIDATED                  
***********************************************************************         
VALISPLN DS    0H                                                               
         NMOD1 0,**VSLN**                                                       
         L     RC,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     R2,8(R1)                                                         
*                                                                               
         LA    R1,SLNTAB           R1 = A(VALID SPOT LENGTHS)                   
*                                                                               
VSP10    CLC   BSLN,0(R1)          IF SPOT LENGTH IN LIST                       
         BE    VSPX                THEN EXIT                                    
*                                                                               
         LA    R1,1(R1)            CHECK NEXT ONE IN LIST                       
*                                                                               
         CLI   0(R1),0             IF END OF LIST                               
         BNE   VSP10                                                            
         B     INVLSLN             THEN ERROR                                   
*                                                                               
VSPX     B     XIT                                                              
*                                                                               
INVLSLN  MVI   GERROR1,BADSLN                                                   
         GOTO1 MYERR                                                            
*                                                                               
SLNTAB   DC    AL1(10,15,20,30,40,45,50,60,90,120,105,150,75,5)                 
         DC    5AL1(0)                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FETCHS THE MARKET NAME.                                          
***********************************************************************         
GETMKTNM DS    0H                                                               
         NMOD1 0,**GMKT**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
*                                                                               
         MVC   SVKEY,KEY                                                        
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
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         B     GETMNO                                                           
         DROP  R4                                                               
*                                                                               
         USING MKTREC,R6                                                        
GETM10   MVC   MKTNM,MKTNAME       SET MARKET NAME                              
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         B     GETMYES                                                          
*                                                                               
GETMYES  B     YES                                                              
GETMNO   B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FETCHS THE CLIENT NAME.                                          
***********************************************************************         
GETCLTNM DS    0H                                                               
         NMOD1 0,**GCLT**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
*                                                                               
         MVC   SVKEY,KEY                                                        
         LA    R4,KEY                                                           
         USING CLTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   GETCNO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         CLC   CKEY,KEY                                                         
         BNE   GETCNO                                                           
         MVC   CLTNM,CNAME                                                      
         DROP  R6                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
GETCYES  B     YES                                                              
GETCNO   B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FETCHS THE PRODUCT NAME.                                         
***********************************************************************         
GETPRDNM DS    0H                                                               
         NMOD1 0,**GPRD**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
*                                                                               
         MVC   SVKEY,KEY                                                        
         LA    R4,KEY                                                           
         USING PRDHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,QPRD                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BNE   GETPNO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
         CLC   PKEY,KEY                                                         
         BNE   GETPNO                                                           
         MVC   PRDNM,PNAME                                                      
         DROP  R6                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
GETPYES  B     YES                                                              
GETPNO   B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEND AVAIL                                                                    
***********************************************************************         
PR       DS    0H                                                               
         NMOD1 0,**PRAV**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         L     R8,ASPOOLD                                                       
         LA    R5,SYSSPARE                                                      
*                                                                               
         NI    BITFLAG,X'FF'-X'20'   PRINT QUEUE HASN'T BEEN OPENED YET         
         XC    THEUSER,THEUSER                                                  
         XC    MRKHEAD1,MRKHEAD1   NO MARKET HEADLINES YET                      
         XC    MRKHEAD2,MRKHEAD2                                                
         MVI   WHEN,X'40'          SET REPORT TO PRINT NOW                      
         MVI   TWAWHEN,0                                                        
*                                                                               
         GOTO1 =A(DIGUPMKT),DMCB,(RC),RR=RELO                                   
*                                                                               
         CLI   AVPOKAY,C'Y'                                                     
         BE    PR00                                                             
         CLI   AVPOKAY,C'N'                                                     
         BE    PR200                                                            
         LA    R2,AVPOKAYH                                                      
         B     INVLFLD                                                          
*                                                                               
PRTLINE  LR    R0,RE                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PR00     MVC   AIO,AIO3            USE AIO3 FOR NOW, AIO1 HAS AVAIL             
         MVI   DATADISP+1,DIRELDQ  *** NEW DISPLACEMENT                         
         GOTO1 =A(FINDQ2QS),DMCB,(RC),RR=RELO                                   
         MVC   DATADISP,=H'24'     *** RESTORE DISPLACEMENT                     
         BNE   PR200               IF NO REPS, THEN NO REPORT                   
         MVC   AIO,AIO1            AIO1 HAS AVAIL                               
*                                                                               
         LA    R1,BLOCK            SAVE A(REP ID IN BLOCK)                      
         ST    R1,AREPID                                                        
         CLI   BLOCK,0                                                          
         BNE   PR00A                                                            
         MVC   THEUSER,=C'QEZ'     SET PRINT ID FOR EDICT                       
         MVC   BLOCK(240),BLOCK+240  MOVE EDICT REPS OVER                       
         B     PR10                                                             
PR00A    MVC   THEUSER,=C'Q2Q'     SET PRINT ID FOR QUEUE TO QUEUE              
*                                                                               
PR10     NI    BITFLAG,X'FF'-X'20'                                              
*                                                                               
*&&DO                                                                           
PR10     LA    R1,SPOOLKEY                                                      
         USING PQPLD,R1                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=CL11'ADDS REQ'                                           
         OI    GENSTAT3,NOCLRSPK   DON'T CLEAR SPOOL KEY IN OPENPQ              
         MVC   PLCLASS,MYCLASS                                                  
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
         DROP  R1                                                               
         GOTO1 OPENPQ              OPEN PRINTQ MYSELF                           
*                                                                               
         LA    R1,HDHK             ADDRESS OF HEADER ROUTINE                    
         ST    R1,HEADHOOK                                                      
*&&                                                                             
PR15     MVI   USEHDHK,C'N'        HOLD OFF ON THE HEADINGS                     
         LA    R1,NOTHING                                                       
         ST    R1,SPECS                                                         
*                                                                               
         CLI   QMED,C'R'           RADIO REP                                    
         BNE   PR15A                                                            
         GOTO1 =A(RADIOREP),DMCB,(RC),RR=RELO  REP GETS AVAIL?                  
         BNE   PRNXTREP            NO, IT DOESN'T, GET NEXT REP                 
*                                                                               
PR15A    BAS   RE,OPENSPQ                                                       
         MVC   P+4(5),=C'*HDR*'    HEADER RECORD                                
         MVI   P+35,C'P'           /PAGE FOR EASYLINK AND SUPRESS TOP           
         MVC   P+9(6),=C'EDICT='                                                
         L     R1,AREPID           SHOW REP ID REPORT WILL BE SENT TO           
         MVC   P+15(L'DIRREPID),0(R1)                                           
*                                                                               
         MVI   MAXLINES,60                                                      
         CLC   =C'KAT',0(R1)       KATZ GET 45 LINES PER PAGE                   
         BNE   *+8                                                              
         MVI   MAXLINES,45                                                      
*                                                                               
         MVC   P+54+8(L'QMED),QMED    1ST 8 BYTES IS AGENCY BY DAVID            
         LA    R2,AVPINFO                                                       
         USING INFOLIND,R2                                                      
         MVC   P+54+8+L'QMED(L'LINCLT),LINCLT                                   
         DROP  R2                                                               
         MVC   P+38(L'DIRREPID),0(R1)                                           
         MVI   FORCEHED,C'Y'       START A NEW PAGE BEFORE ANYTHING             
*                                                                               
         BAS   RE,PRTLINE                                                       
*                                                                               
         BAS   RE,PRTHDRR          PRINT OUT HEADER RECORD                      
*                                                                               
         CLI   TMKTCNT,0           ANY MARKETS USED FOR REP?                    
         BE    PR15B               NONE, THAT'S IT FOR THIS REP                 
*                                                                               
         MVI   USEHDHK,C'Y'        CAN USE HEADINGS NOW                         
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         MVI   FORCEHED,C'Y'       START A NEW PAGE AFTER INFO LINE             
*                                                                               
         BAS   RE,PRTBODY          PRINT BODY OF REPORT                         
*                                                                               
PR15B    MVI   P1,0                                                             
         MVC   P2(30),=CL30'*** END OF DDS MESSAGE ***'                         
         BAS   RE,PRTLINE                                                       
*                                                                               
PRNXTREP L     R1,AREPID           CHECK NEXT REP ID                            
         LA    R1,L'DIRREPID(R1)                                                
         ST    R1,AREPID                                                        
         CLI   0(R1),0                                                          
         BE    PR100                                                            
*                                                                               
         NI    BITFLAG,X'FF'-X'40'                                              
         XC    MRKHEAD1,MRKHEAD1                                                
         XC    MRKHEAD2,MRKHEAD2                                                
*                                                                               
         B     PR15                PRINT FOR NEXT REP                           
*                                                                               
PR100    TM    BITFLAG,X'20'       WAS PRINTQ OPENED?                           
         BZ    PR110               NO, NOTHING TO CLOSE                         
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         BAS   RE,PRTLINE                                                       
*                                                                               
PR110    CLC   THEUSER,=C'QEZ'     IS PRINT ID EDICT?                           
         BE    PR150               YES, THEN NO MORE PRINTS                     
         MVC   THEUSER,=C'QEZ'     NO, SET PRINT ID TO EDICT                    
         MVC   BLOCK(240),BLOCK+240  MOVE EDICT REPS OVER                       
         LA    R1,BLOCK                                                         
         ST    R1,AREPID                                                        
         MVC   AIO,AIO1            POINT BACK TO AIO1                           
*                                                                               
         CLI   0(R1),0                                                          
         BE    PR150                                                            
         NI    BITFLAG,X'FF'-X'40'                                              
         XC    MRKHEAD1,MRKHEAD1                                                
         XC    MRKHEAD2,MRKHEAD2                                                
*                                                                               
         B     PR10                PRINT REPORT FOR EDICT                       
*                                                                               
PR150    MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         USING AVAKEY,R6                                                        
         MVC   KEY(L'AVAKEY),AVAKEY                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ     GET DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
         GOTO1 DATCON,DMCB,(5,0),(19,AVARDSNT)                                  
         DROP  R6                                                               
         GOTO1 PUTREC              WRITE RECORD OUT W/ LAST SENT DATE           
*                                                                               
         MVC   AVPLIN1+24(32),=CL32'AVAIL REQUEST SENT TO ABOVE REPS'           
         OI    AVPLIN1H+6,X'80'                                                 
*                                                                               
PR200    LA    R2,AVPHARDH                                                      
         CLI   8(R2),C'Y'                                                       
         BE    PR210                                                            
         CLI   8(R2),C'N'                                                       
         BNE   INVLFLD                                                          
         B     PRXIT                                                            
*                                                                               
PR210    DS    0H                                                               
         OI    BITFLAG,X'20'       SO WE WON'T OPEN AGAIN                       
         MVC   REMUSER,=C'ADD'     SET PRINT ID TO 'ADD,....'                   
         LA    R1,SPOOLKEY                                                      
         USING PQPLD,R1                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
*                                                                               
         L     R6,AIO              PUT BUYER CODE IN REPORT NAME                
         USING AVAKEY,R6                                                        
         MVC   PLDESC(4),=CL4'BYR='                                             
         MVC   PLDESC+4(L'AVAKBYR),AVAKBYR                                      
         DROP  R6                                                               
*                                                                               
         OI    GENSTAT3,NOCLRSPK   DON'T CLEAR SPOOL KEY IN OPENPQ              
         MVI   PLCLASS,C'Z'        CLASS Z FOR HARD COPIES                      
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
         DROP  R1                                                               
         XC    BIGSPLKY,BIGSPLKY                                                
         LA    R1,BIGSPLKY                                                      
         ST    R1,SPOOLQLK                                                      
         USING PQPLD,R1                                                         
         MVC   QLRETNL,=H'48'      SET PRINTQ RETAIN TIMES  48 ACTIVE           
         MVC   QLRETND,=H'24'                               24 PRINTED          
         DROP  R1                                                               
         GOTO1 OPENPQ              OPEN PRINTQ MYSELF                           
*                                                                               
         MVI   USEHDHK,C'Y'                                                     
         LA    R1,HDHK             ADDRESS OF HEADER ROUTINE                    
         ST    R1,HEADHOOK                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         OI    BITFLAG,X'40'       DON'T PRINT OUT INFO LINE                    
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTBODY          PRINT THE BODY OF THE REPORT                 
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         BAS   RE,PRTLINE                                                       
*                                                                               
         MVC   AVPLIN2+21(32),=CL32'AVAIL REQUEST SENT TO YOUR PRINT'           
         MVC   AVPLIN2+54(5),=C'QUEUE'                                          
         OI    AVPLIN2H+6,X'80'                                                 
*                                                                               
PRXIT    MVI   AVPOKAY,C'N'        NOT OKAY, SO IT WON'T PRINT AGAIN            
         OI    AVPOKAYH+6,X'80'                                                 
         MVI   AVPHARD,C'N'        NO HARD COPY AGAIN                           
         OI    AVPHARDH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
OPENSPQ  NTR1                                                                   
         TM    BITFLAG,X'20'       OPENPQ ALREADY?                              
         BNZ   OPQX                                                             
*                                                                               
         MVC   REMUSER,THEUSER     ONLY FOR  Q2Q  &  QEZ                        
         LA    R1,SPOOLKEY                                                      
         USING PQPLD,R1                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=CL11'ADDS REQ'                                           
         OI    GENSTAT3,NOCLRSPK   DON'T CLEAR SPOOL KEY IN OPENPQ              
         MVC   PLCLASS,MYCLASS                                                  
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
         DROP  R1                                                               
         XC    BIGSPLKY,BIGSPLKY                                                
         LA    R1,BIGSPLKY                                                      
         ST    R1,SPOOLQLK                                                      
         USING PQPLD,R1                                                         
         MVC   QLRETNL,=H'48'      SET PRINTQ RETAIN TIMES  48 ACTIVE           
         MVC   QLRETND,=H'24'                               24 PRINTED          
         DROP  R1                                                               
         GOTO1 OPENPQ              OPEN PRINTQ MYSELF                           
*                                                                               
         LA    R1,HDHK             ADDRESS OF HEADER ROUTINE                    
         ST    R1,HEADHOOK                                                      
         OI    BITFLAG,X'20'                                                    
*                                                                               
OPQX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PRINTS OUT THE AVAIL REQUEST HEADER REOCRD                       
*                                                                               
* NOTE: AIO1 POINTS TO THE AVAIL RECORD                                         
*       AIO POINTS TO AIO1                                                      
***********************************************************************         
PRTHDRR  NTR1                                                                   
         MVC   SVKEY,KEY                                                        
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   DATADISP,=H'24'                                                  
*                                                                               
* DAVID MUST GET THE SIGNON ID FROM PRINTQ ID FOR THE BILLING REPORT            
* SO WE CAN LEAVE IT BLANK.                                                     
*                                                                               
         MVC   P(16),=CL16'++DDS SPAVLTRN A'                                    
         MVC   P+16(L'AVPREFN),AVPREFN                                          
*                                                                               
         L     R6,AIO                                                           
         USING AVAKEY,R6                                                        
         MVC   P+23(L'AVAKBYR),AVAKBYR                                          
*                                                                               
         MVC   P+26(1),AVPREFN                                                  
*                                                                               
         MVC   BCLT,AVAKCLT                                                     
         GOTO1 CLUNPK,DMCB,BCLT,P+27                                            
*                                                                               
         MVC   BPRD,AVAKPRD                                                     
         MVC   AIO,AIO2                                                         
         GOTO1 GETQPRD                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         MVC   P+30(L'QPRD),QPRD                                                
*                                                                               
         CLI   AVAKEST,0                                                        
         BE    PHDR00                                                           
         ZIC   R1,AVAKEST                                                       
         CVD   R1,DUB                                                           
         UNPK  P+33(3),DUB                                                      
         OI    P+35,X'F0'                                                       
*                                                                               
PHDR00   L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         USING AVARDSCD,R6                                                      
         GOTO1 DATCON,DMCB,(8,AVARDFLS),(11,P+36)                               
         MVI   P+44,C'-'                                                        
         GOTO1 DATCON,DMCB,(8,AVARDFLE),(11,P+45)                               
         DROP  R6                                                               
*                                                                               
         MVC   P+53(L'LOCOV),LOCOV   PUT OUT THE LOCATION OVERRIDE              
*                                                                               
         BAS   RE,PHDRLINE         PRINT OUT TRANSACTION LINE                   
*                                                                               
         MVC   P(15),=CL15'++DDS SPADDHDR '                                     
*                                                                               
         MVC   P+15(2),=C'AR'      REPORT ID                                    
*                                                                               
         MVC   P+17(L'AVPREFN),AVPREFN   AGENCY REFERENCE NUMBER                
*                                                                               
         L     R6,AIO              RATING SERVICE                               
         MVI   ELCODE,AVARMCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDMOD,R6                                                      
         MVC   P+41(1),AVARMSRV                                                 
         CLI   AVARMSRV,C'B'                                                    
         BNE   *+8                                                              
         MVI   P+41,C'N'                                                        
*                                                                               
         CLI   P+41,C'N'                                                        
         BNE   *+12                                                             
         MVI   RTNGCHAR,C'0'       X'F0' IS NSI IN CLTHDR FORMAT                
         B     *+8                                                              
         MVI   RTNGCHAR,C'1'       X'F1' IS ARB IN CLTHDR FORMAT                
*                                                                               
         MVC   P+49(L'LOCOV),LOCOV   PUT OUT LOCATION OVERRIDE IF ANY           
*                                                                               
         BAS   RE,PHDRLINE                                                      
         B     PHDR10                                                           
*                                                                               
PHDRLINE LR    R0,RE                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PHDR10   MVC   AIO,AIO2                                                         
*                                                                               
         LA    R3,RMKTBLCK                                                      
         OC    0(2,R3),0(R3)                                                    
         BZ    PHDRX                                                            
*                                                                               
         MVI   TMKTCNT,0           NO MARKETS YET                               
PHDR30   MVI   MKTCNT,0                                                         
         MVC   P(15),=CL15'++DDS SPADDMKT '                                     
         LA    R2,P+15                                                          
*                                                                               
PHDR30LP CLI   MKTCNT,20                                                        
         BL    PHDR40                                                           
         BAS   RE,PHDRLINE                                                      
*                                                                               
         ZIC   R1,TMKTCNT          UPDATE # OF MKTS USED FOR THIS REP           
         LA    R1,20(R1)                                                        
         STC   R1,TMKTCNT                                                       
*                                                                               
         B     PHDR30                                                           
*                                                                               
PHDR40   CLI   QMED,C'R'           RADIO?                                       
         BNE   PHDR45                                                           
*                                                                               
         CLC   =X'FFFF',0(R3)      NO RATING SERVICE MARKET?                    
         BNE   *+14                                                             
         MVC   0(4,R2),=C'****'    NONE                                         
         B     PHDR50                                                           
*                                                                               
         CLC   =X'FFFE',0(R3)      ZERO RATING SERVICE MARKET?                  
         BNE   *+14                                                             
         MVC   0(4,R2),=C'0000'    YES                                          
         B     PHDR50                                                           
*                                                                               
         EDIT  (B2,(R3)),(4,0(R2)),FILL=0,ZERO=NOBLANK                          
         B     PHDR50                                                           
*                                                                               
***********************************************************************         
* THESE STARRED OUT STATEMENTS ARE FOR THE OPTION WHERE REPS ONLY               
* GET AN AVAIL IF THEY HAVE A STATION IN THE RATING SERVICE MARKET              
***********************************************************************         
*PHDR45   CLC  =X'FFFF',0(R3)      NO RATING SERVICE MARKET?                    
*         BE   PHDR55                                                           
**                                                                              
*         LA   R4,KEY              SEE IF THE REP HAS THIS RATING               
*         XC   KEY,KEY                 SERVICE MARKET                           
*         USING STTNKEY,R4                                                      
*         MVI  STTNSYS,STTNSYSQ                                                 
*         MVI  STTNTYP,STTNTYP2                                                 
*         MVC  STTNMED,QMED                                                     
**                                                                              
*         L    R1,AREPID                                                        
*         MVC  STTNREP,0(R1)                                                    
*         CLI  4(R1),C' '          TWO CHAR REP?                                
*         BNE  *+8                                                              
*         MVI  STTNREP+2,C' '      YES                                          
**                                                                              
*         CLI  RTNGCHAR,C'0'       NEILSEN?                                     
*         BNE  *+12                                                             
*         MVI  STTNRSRV,C'N'       YES                                          
*         B    *+8                                                              
*         MVI  STTNRSRV,C'A'       NO, ARBITRON                                 
*                                                                               
*         MVC  STTNRSMK,0(R3)                                                   
*         CLC  =X'FFFE',0(R3)                                                   
*         BNE  *+10                                                             
*         XC   STTNRSMK,STTNRSMK                                                
**                                                                              
*         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                  
**                                                                              
*         L    R6,AIO                                                           
*         CLC  KEY(STTNSTAC-STTNKEY),0(R6)                                      
*         BNE  PHDR55                                                           
*                                                                               
*         EDIT (B2,STTNRSMK),(4,0(R2)),FILL=0,ZERO=NOBLANK                      
**                                                                              
*                                                                               
***********************************************************************         
* ALL THE REPS GET A COPY OF THE AVAIL                                          
***********************************************************************         
*                                                                               
PHDR45   CLC   =X'FFFF',0(R3)      NO RATING SERVICE MARKET?                    
         BNE   *+14                                                             
         MVC   0(4,R2),=C'****'    THEN PUT STARS OUT                           
         B     PHDR50                                                           
*                                                                               
         CLC   =X'FFFE',0(R3)      ZERO RATING SERVICE MARKET?                  
         BNE   *+14                                                             
         MVC   0(4,R2),=C'0000'                                                 
         B     PHDR50                                                           
*                                                                               
         EDIT  (B2,0(R3)),(4,0(R2)),FILL=0,ZERO=NOBLANK                         
*                                                                               
PHDR50   IC    R1,MKTCNT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,MKTCNT                                                        
         LA    R2,5(R2)                                                         
PHDR55   LA    R3,2(R3)                                                         
         DROP  R6                                                               
*                                                                               
         OC    0(2,R3),0(R3)       IF NO MORE MARKETS                           
         BNZ   PHDR30LP                                                         
*                                                                               
         CLI   MKTCNT,0            IF NO MARKETS ON THIS LINE                   
         BNE   PHDR60                                                           
         XC    P,P                                                              
         B     PHDRX               THEN DON'T PRINT THIS LINE                   
*                                                                               
PHDR60   BAS   RE,PHDRLINE                                                      
         ZIC   R0,MKTCNT           UPDATE # OF MKTS USED FOR THIS REP           
         ZIC   R1,TMKTCNT                                                       
         AR    R1,R0                                                            
         STC   R1,TMKTCNT                                                       
*                                                                               
PHDRX    MVC   P(14),=CL14'++DDS SPADDTLR'                                      
         BAS   RE,PHDRLINE                                                      
*                                                                               
         OI    BITFLAG,X'40'       DON'T NEED TO PRINT INFO LINE LATER          
         MVC   AIO,AIO1                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PRINTS OUT THE BODY OF THE REPORT                                
***********************************************************************         
PRTBODY  NTR1                                                                   
         MVC   P1(7),=C'Agency:'                                                
         MVC   P1+15(L'USERNAME),USERNAME                                       
         BAS   RE,PRTLINE                                                       
*                                                                               
         L     R6,AIO1                                                          
         USING AVAKEY,R6                                                        
         MVC   BYRCODE,AVAKBYR                                                  
         MVC   AIO,AIO2            USE AIO2 FOR THE BUYER                       
         GOTO1 VALIBUYR,DMCB,AVAKBYR                                            
         MVC   AIO,AIO1            POINT BACK TO AIO1                           
*                                                                               
         MVC   P1(11),=C'Buyer Name:'                                           
         MVC   P1+15(L'QBUYER),QBUYER                                           
*                                                                               
         L     R6,AIO2             GET ASSISTANT BUYER CODE FROM BUYER          
         MVI   ELCODE,BYRDCDQ          RECORD                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BYRDSCD,R6                                                       
         MVC   ASSTBYR,BYRASBYR                                                 
         DROP  R6                                                               
         L     R6,AIO                                                           
         USING AVAKEY,R6                                                        
*                                                                               
         MVC   P1+40(7),=C'Office:'                                             
         MVC   P1+55(L'QOFFICE),QOFFICE                                         
         MVC   P2+40(13),=C'Phone Number:'                                      
         MVC   P2+55(L'QPHONE),QPHONE                                           
         OC    QPHONEXT,QPHONEXT                                                
         BZ    PR16                                                             
         MVI   P2+68,C'x'                                                       
         MVC   P2+69(L'QPHONEXT),QPHONEXT                                       
PR16     BAS   RE,PRTLINE                                                       
*                                                                               
         L     R6,AIO              GET THE DESCRIPTION ELEMENT                  
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
         MVC   P1(11),=C'Asst Buyer:'                                           
         MVC   P1+40(13),=C'Phone Number:'                                      
*                                                                               
         CLC   AVARDAST,SPACES                                                  
         BNE   PR18                                                             
         CLC   ASSTBYR,SPACES                                                   
         BE    PR20                                                             
         MVC   AVARDAST,ASSTBYR                                                 
*                                                                               
PR18     MVC   AIO,AIO2            USE AIO2 FOR THE BUYER                       
         GOTO1 VALIBUYR,DMCB,AVARDAST                                           
         MVC   AIO,AIO1            POINT BACK TO AIO1                           
*                                                                               
         MVC   P1+15(L'QBUYER),QBUYER                                           
         MVC   P1+55(L'QPHONE),QPHONE                                           
         OC    QPHONEXT,QPHONEXT                                                
         BZ    PR20                                                             
         MVI   P1+68,C'x'                                                       
         MVC   P1+69(L'QPHONEXT),QPHONEXT                                       
*                                                                               
PR20     MVC   P2+40(9),=C'Due Date:'                                           
         OC    AVARDDUE,AVARDDUE                                                
         BZ    PR25                                                             
         GOTO1 DATCON,DMCB,(8,AVARDDUE),(11,P2+55)                              
PR25     MVC   P3(19),=C'Medium:        Spot Radio'                             
         CLI   QMED,C'R'                                                        
         BE    *+10                                                             
         MVC   P3+20(10),=C'Television'                                         
         BAS   RE,PRTLINE                                                       
*                                                                               
         L     R6,AIO                                                           
         USING AVAKEY,R6                                                        
         MVC   P1(11),=C'Advertiser:'                                           
         MVC   BCLT,AVAKCLT                                                     
         GOTO1 CLUNPK,DMCB,BCLT,P1+15                                           
         MVC   AIO,AIO2                                                         
         GOTO1 =A(GETCLTNM),DMCB,(RC),RR=RELO                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         MVC   P1+35(L'CLTNM),CLTNM                                             
         MVC   P2(8),=C'Product:'                                               
         MVC   BPRD,AVAKPRD                                                     
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FOR THE PRODUCT                     
         GOTO1 GETQPRD                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            POINT BACK TO AIO1                           
*                                                                               
         MVC   P2+15(L'QPRD),QPRD                                               
         MVC   AIO,AIO2                                                         
         GOTO1 =A(GETPRDNM),DMCB,(RC),RR=RELO                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         MVC   P2+35(L'PRDNM),PRDNM                                             
         MVC   P3(11),=C'Estimate #:'                                           
         CLI   AVAKEST,0                                                        
         BE    PR30                                                             
         ZIC   R1,AVAKEST                                                       
         CVD   R1,DUB                                                           
         UNPK  P3+15(3),DUB                                                     
         OI    P3+17,X'F0'                                                      
PR30     BAS   RE,PRTLINE                                                       
*                                                                               
         L     R6,AIO              GET THE DESCRIPTION ELEMENT                  
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         USING AVARDSCD,R6                                                      
         MVC   P1(13),=C'Flight Dates:'                                         
         GOTO1 DATCON,DMCB,(8,AVARDFLS),(11,P1+15)                              
         MVI   P1+23,C'-'                                                       
         GOTO1 DATCON,DMCB,(8,AVARDFLE),(11,P1+24)                              
         MVI   P2,C'*'                                                          
         MVC   P3(11),=C'Daypart(s):'                                           
         MVC   P4(15),=C'Spot Length(s):'                                       
*                                                                               
*PR40     L     R6,AIO              POINT TO THE RECORD                         
*         MVI   ELCODE,AVARSCDQ     GET DAYPART/SPOTLEN ELEMENT                 
*         BAS   RE,GETEL                                                        
*         BNE   PR50                                                            
*         USING AVARSPTD,R6                                                     
**                                                                              
*         ZIC   R1,AVARSPLN         FIND NUMBER OF DAYPART/SPOTLEN SETS         
*         SH    R1,=H'2'                                                        
*         SR    R0,R0                                                           
*         LA    R2,3                                                            
*         DR    R0,R2               R1 = NUMBER OF DAYPART/SPOTLEN SETS         
**                                                                              
*         LA    R2,P3+20            DAYPART LINE                                
*         LA    R4,P3+79            R4 = END OF THAT LINE                       
*         LA    R3,AVARSDPT         R3 = A(DAYPART/SPOTLEN SET)                 
*         DROP  R6                                                              
*         LR    R6,R1               R6 = NUMBER OF DAYPART/SPOTLEN SETS         
*         USING AVARSDPT,R3                                                     
**                                                                              
*PR40LP   CLI   AVARSDPT,0          IF THERE IS A DAYPART                       
*         BE    *+14                                                            
*         MVC   0(L'AVARSDPT,R2),AVARSDPT      THEN SHOW IT                     
*         LA    R2,1(R2)                                                        
**                                                                              
*         CLI   AVARSSLN,0          IF THERE IS A SPOT LENGTH                   
*         BE    PR40NX              THEN SHOW IT                                
*         EDIT  (B1,AVARSSLN),(3,0(R2)),ALIGN=LEFT    R0 GETS CLOBBERED         
*PR40LP1  LA    R2,1(R2)                                                        
*         CLI   0(R2),0             IF NULL OR BLANK                            
*         BE    PR40NX              THEN WE CAN PUT COMMA HERE                  
*         CLI   0(R2),C' '                                                      
*         BNE   PR40LP1                                                         
**                                                                              
*PR40NX   LR    R1,R2               IF ENOUGH ROOM ON LINE FOR MORE             
*         LA    R1,5(R1)                                                        
*         CR    R1,R4                                                           
*         BH    PR40A                                                           
*         C     R6,=F'1'            THEN DO WE NEED TO PUT A DELIMETER?         
*         BE    PR40NXA                                                         
*         MVI   0(R2),C','          YES                                         
*         LA    R2,1(R2)                                                        
*PR40NXA  LA    R3,AVARSSET(R3)     R3 = A(NEXT SET)                            
*         BCT   R6,PR40LP                                                       
*         B     PR50                                                            
**                                                                              
*PR40A    LA    R4,P4+20            GO TO NEXT LINE?                            
*         CR    R2,R4                                                           
*         BL    *+6                                                             
*         DC    H'0'                                                            
*         LA    R2,P4+20            YES                                         
*         LA    R4,P4+79                                                        
*         B     PR40NXA                                                         
*         DROP  R3                                                              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AVARPCEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR45                                                             
         USING AVARDPTD,R6                                                      
         ZIC   R1,AVARPLEN                                                      
         SH    R1,=Y(AVARPOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P3+20(0),AVARPTXT                                                
*                                                                               
PR45     L     R6,AIO                                                           
         MVI   ELCODE,AVARSCEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR50                                                             
         USING AVARSLND,R6                                                      
         ZIC   R1,AVARSLEN                                                      
         SH    R1,=Y(AVARSOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P4+20(0),AVARSTXT                                                
*                                                                               
PR50     BAS   RE,PRTLINE                                                       
*                                                                               
         MVC   P1(15),=C'Rating Service:'                                       
         MVC   P2(12),=C'Rating Book:'                                          
         MVC   P3(12),=C'Survey Area:'                                          
         MVC   P4(6),=C'Demos:'                                                 
         L     R6,AIO              GET THE DEMO ELEMENT                         
         MVI   ELCODE,AVARMCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR60                                                             
         USING AVARDMOD,R6                                                      
         MVC   P1+20(L'AVARMSRV),AVARMSRV                                       
         MVC   P2+20(L'AVARMBK),AVARMBK                                         
         MVC   P3+20(L'AVARMARA),AVARMARA                                       
         CLI   AVARDMLN,AVARDMLQ                                                
         BE    PR60                                                             
         ZIC   R1,AVARDMLN                                                      
         SH    R1,=Y(AVARDMLQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P4+20(0),AVARMDMO                                                
PR60     BAS   RE,PRTLINE                                                       
*                                                                               
         MVI   P,C'*'                                                           
         BAS   RE,PRTLINE                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,AVARDCDQ     GET DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         USING AVARDSCD,R6                                                      
         LA    R2,AVARDCM1         R2 = A(FIRST STANDARD COMMENT)               
         LA    R3,AVARDCM5         CHECK FROM COMMENTS 1 TO 5                   
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FOR COMMENTS                        
*                                                                               
PR60LP   OC    0(L'AVARDCM1,R2),0(R2)  IF NO COMMENT                            
         BZ    PR60NXT             THEN CHECK NEXT ONE                          
*                                                                               
         LA    R4,KEY              SET UP THE KEY                               
         USING COMRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   COMKTYP,COMKTYPQ                                                 
         MVI   COMKSUB,COMKSUBQ                                                 
         MVC   COMKAM,BAGYMD                                                    
         MVC   COMKCOM,0(R2)                                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'COMKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                DIE IF ERROR READING COMMENT                 
         GOTO1 GETREC                                                           
*                                                                               
         MVI   PREVSEQN,0                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,COMTXTEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR60NXT             CHECK NEXT COMMENT                           
         USING COMTXTD,R6                                                       
PR60LP1  ZIC   R4,COMTXTSQ         GET THE LINE NUMBER                          
         ZIC   R1,PREVSEQN                                                      
         SR    R4,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PR60LP2                                                          
PR60LP1A MVI   P,0                                                              
         BAS   RE,PRTLINE                                                       
         BCT   R4,PR60LP1A                                                      
PR60LP2  ZIC   R1,COMTXTLN                                                      
         SH    R1,=Y(COMTXTOV+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P1(0),COMTXTTX                                                   
         BAS   RE,PRTLINE                                                       
         MVC   PREVSEQN,COMTXTSQ   SAVE SEQUENCE NUMBER OF LINE                 
*                                                                               
         BAS   RE,NEXTEL           IF MORE LINES FOR THAT COMMENT               
         BE    PR60LP1             THEN PRINT THEM ALSO                         
         DROP  R6                                                               
*                                                                               
PR60NXT  CR    R2,R3               IF LAST COMMENT CHECKED                      
         BE    PR70                                                             
         LA    R2,L'COMKCOM(R2)    CHECK NEXT COMMENT                           
         MVI   P,C'*'                                                           
         BAS   RE,PRTLINE                                                       
         B     PR60LP                                                           
*                                                                               
PR70     MVC   AIO,AIO1            POINT BACK TO AIO1                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,AVARCCEQ     LOOK FOR AVAIL COMMENTS                      
         BAS   RE,GETEL                                                         
         BNE   PR80                                                             
         USING AVARCOMD,R6                                                      
*                                                                               
         MVI   PREVSEQN,0                                                       
PR70LP1  ZIC   R4,AVARCLNM         GET THE LINE NUMBER                          
         ZIC   R1,PREVSEQN                                                      
         SR    R4,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PR70LP2                                                          
PR70LP1A MVI   P,0                                                              
         BAS   RE,PRTLINE                                                       
         BCT   R4,PR70LP1A                                                      
PR70LP2  ZIC   R1,AVARCLEN                                                      
         SH    R1,=Y(AVARCOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P1(0),AVARCTXT                                                   
         BAS   RE,PRTLINE                                                       
         MVC   PREVSEQN,AVARCLNM   SAVE SEQUENCE NUMBER OF LINE                 
*                                                                               
         BAS   RE,NEXTEL           IF MORE LINES FOR THAT COMMENT               
         BE    PR70LP1             THEN PRINT THEM ALSO                         
         DROP  R6                                                               
*                                                                               
         MVI   P,C'*'                                                           
         BAS   RE,PRTLINE                                                       
*                                                                               
PR80     XC    KEY,KEY             SET UP AVAIL MARKET KEY                      
         LA    R4,KEY                                                           
         USING AVMKEY,R4                                                        
         MVI   AVMKTYP,AVMKTYPQ                                                 
         MVI   AVMKSUB,AVMKSUBQ                                                 
         L     R6,AIO                                                           
         USING AVAKEY,R6                                                        
         MVC   AVMKAM,AVAKAM                                                    
         MVC   AVMKREF,AVAKREF                                                  
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO2            USE AIO2 TO GET MARKET RECORDS               
*                                                                               
         GOTO1 HIGH                                                             
*                                  MAKE SURE WITHIN SAME REFERENCE #            
PR80TST  CLC   KEY(AVMKMKT-AVMKEY),KEYSAVE                                      
         BNE   PR90                                                             
         GOTO1 GETREC                                                           
*                                                                               
         OC    AVMKMKT,AVMKMKT     IF MARKET IS ZERO                            
         BNZ   PR80MRKT                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,AVMRTCDQ     THEN PRINT OUT THE HEADLINE                  
         BAS   RE,GETEL                                                         
         BNE   PR80NXT                                                          
         USING AVMRTXTD,R6                                                      
         ZIC   R1,AVMRTXLN                                                      
         SH    R1,=Y(AVMRTOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),AVMRTTXT                                                    
         MVC   TMPHEAD,P          MAKE A COPY OF THE HEADLINE                   
         BAS   RE,PRTLINE                                                       
         MVC   MRKHEAD1,TMPHEAD                                                 
         DROP  R6                                                               
         B     PR80NXT                                                          
*                                                                               
PR80MRKT MVI   PRTSTARS,C'*'                                                    
         MVI   PRTSTARS+1,C'*'                                                  
         ZICM  R1,AVMKMKT,2                                                     
         CVD   R1,DUB                                                           
         UNPK  PRTMKT,DUB                                                       
         OI    PRTMKT+3,X'F0'                                                   
         MVC   QMKT,PRTMKT                                                      
*                                                                               
         MVC   AIO,AIO3            USE AIO3 TO FETCH MARKET NAME                
         GOTO1 =A(GETMKTNM),DMCB,(RC),RR=RELO                                   
         MVC   AIO,AIO2            POINT BACK TO AIO2                           
*                                                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRTMKTNM,MKTNM                                                   
         GOTO1 HIGH                                                             
*                                                                               
PR80BUYR L     R6,AIO              IF NO OVERRIDE BUYER                         
         MVI   ELCODE,AVMRDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR80MASS            THEN CHECK THE MARKET ASSIGNMENT             
         USING AVMRDSCD,R6                                                      
*                                                                               
         MVC   AIO,AIO3            USE AIO3 FOR THE BUYER                       
         GOTO1 VALIBUYR,DMCB,AVMRDBYR                                           
         MVC   AIO,AIO2            POINT BACK TO AIO2                           
*                                                                               
         MVC   PRTBUYR,QBUYER                                                   
         MVI   PRTSLASH,C'/'                                                    
         MVC   PRTBYROF,AVMRDBYO                                                
         MVC   PRTPHONE,QPHONE                                                  
         OC    QPHONEXT,QPHONEXT                                                
         BZ    PR80TEXT                                                         
         MVI   PRTEXT,C'x'                                                      
         MVC   PRTEXTNM,QPHONEXT                                                
         B     PR80TEXT                                                         
*                                                                               
PR80MASS MVC   AIO,AIO3                                                         
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(L'AVMKEY),KEY                                            
         MVI   KEY+1,X'C1'                                                      
         MVC   KEY+3(L'AVMKMKT),AVMKMKT                                         
         USING BYRKEY,R4                                                        
         XC    BYRKDAT2(BYRKCNTL-BYRKDAT2),BYRKDAT2                             
         GOTO1 HIGH                                                             
         CLC   KEY(BYRKDAT2-BYRKEY),KEYSAVE  SAME MARKET?                       
         BNE   PR80MASX                                                         
         CLC   BYRCODE,BYRKBUY2                                                 
         BE    PR80MASX                                                         
         MVC   ASSTBYR,BYRKBUY2                                                 
         GOTO1 VALIBUYR,DMCB,ASSTBYR                                            
         MVC   PRTBUYR,QBUYER                                                   
         MVI   PRTSLASH,C'/'                                                    
         MVC   PRTBYROF,QOFFICE                                                 
         MVC   PRTPHONE,QPHONE                                                  
         OC    QPHONEXT,QPHONEXT                                                
         BZ    PR80MASX                                                         
         MVI   PRTEXT,C'x'                                                      
         MVC   PRTEXTNM,QPHONEXT                                                
PR80MASX MVC   AIO,AIO2            POINT BACK TO AIO2                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'AVMKEY),SAVEKEY                                            
         USING AVMKEY,R4                                                        
*                                                                               
PR80TEXT MVC   TMPHEAD,P                                                        
         BAS   RE,PRTLINE          SHOW MARKET, MARKET NAME, AND BUYER          
         MVC   MRKHEAD2,TMPHEAD                                                 
*                                                                               
         MVI   PREVSEQN,0          NO LINES BEFORE FIRST LINE                   
         L     R6,AIO                                                           
         MVI   ELCODE,AVMRTCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR80NXT                                                          
         USING AVMRTXTD,R6                                                      
PR80TXT1 ZIC   R2,AVMRTLNM         GET THE LINE NUMBER                          
         ZIC   R1,PREVSEQN                                                      
         SR    R2,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    PR80TXT3                                                         
PR80TXT2 MVI   P,0                                                              
         BAS   RE,PRTLINE                                                       
         BCT   R2,PR80TXT2                                                      
PR80TXT3 ZIC   R1,AVMRTXLN                                                      
         SH    R1,=Y(AVMRTOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),AVMRTTXT                                                    
         BAS   RE,PRTLINE                                                       
         MVC   PREVSEQN,AVMRTLNM   SAVE SEQUENCE NUMBER OF LINE                 
*                                                                               
         BAS   RE,NEXTEL           GET NEXT LINE                                
         BE    PR80TXT1                                                         
         MVI   P,0                                                              
         BAS   RE,PRTLINE                                                       
         DROP  R6                                                               
*                                                                               
PR80NXT  GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         XC    MRKHEAD2,MRKHEAD2   NO PAGE MARKET HEADER                        
         B     PR80TST                                                          
*                                                                               
PR90     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FINDS OUT WHICH REPS ARE Q2Q AND WHICH ARE EDICT.                
*                                                                               
* ON EXIT:     CONDITION CODE      EQUAL: THERE IS AT LEAST ONE REP             
*              1ST HALF OF BLOCK   Q2Q REPS                                     
*              2ND HALF OF BLOCK   EDICT REPS                                   
***********************************************************************         
FINDQ2QS DS    0H                                                               
         NMOD1 0,**FQ2Q**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R5,SYSSPARE                                                      
*                                                                               
         LA    R4,KEY              SET UP THE DIRADD KEY                        
         USING DIRKEYD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   DIRKSYS,DIRKSYSQ                                                 
         MVI   DIRTYPE,DIRTYPEQ                                                 
         MVC   DIRMED,QMED                                                      
         MVC   DIRID,TWAORIG       USE THE ID NUM FOR THE KEY                   
         MVI   DATADISP+1,DIRELDQ  NEW DISPLACEMENT                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO                                                           
         USING DIRKEYD,R6                                                       
*                                  KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         CLC   DIRKEY,KEY             IT ISN'T A GENCON READ HIGH               
         BNE   Q2QNO               NO DIRADD RECORD, NO REPS                    
         DROP  R6                                                               
*                                                                               
* NOTE: ASSUMMING THERE WILL BE NO MORE THAN 20 REPS/AGENCY, BLOCK              
*       WILL HOLD A REP FOR EVERY 10 BYTES AND A X'00' WILL TERMINATE           
*       THE LIST.                                                               
*                                                                               
         MVC   LOCOV,SPACES                                                     
         MVI   ELCODE,DIRLOVEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   Q2Q10                                                            
         USING DIRLOVD,R6                                                       
         MVC   LOCOV,DIRLOVER                                                   
*                                                                               
Q2Q10    LA    R0,BLOCK                                                         
         LA    R1,480                                                           
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R2,BLOCK                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DIRREPEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   Q2QNO               NO REPS TO SEND TO, NO REPORTS               
         USING DIRREPD,R6                                                       
Q2Q10LP  MVC   0(L'DIRREPID,R2),DIRREPID                                        
         LA    R2,L'DIRREPID(R2)                                                
         BAS   RE,NEXTEL                                                        
         BE    Q2Q10LP                                                          
*                                                                               
* NOTE: Q2Q REPS WILL BE IN FIRST HALF OF BLOCK                                 
*       EDICT REPS WILL BE IN SECOND HALF                                       
*                                                                               
         LA    R2,BLOCK            R2 = A(1ST Q2Q REPS)                         
         LA    R3,BLOCK+240        R3 = A(1ST EDICT REPS)                       
Q2Q20LP  LA    R4,KEY                                                           
         USING EDIKEYD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   EDIKSYS,EDIKSYSQ                                                 
         MVI   EDITYPE,EDITYPEQ                                                 
         MVC   EDINAME,0(R2)                                                    
         DROP  R4                                                               
*                                  EDICT RECORDS                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R6,AIO                                                           
         USING EDIKEYD,R6                                                       
         CLC   EDIKEY,KEY                                                       
         BE    Q2Q25                                                            
         DROP  R6                                                               
*                                  NO EDICT RECORD FOR THIS REP                 
Q2Q20A   MVC   0(L'DIRREPID,R3),0(R2)   MOVE ID OVER TO EDICT REPS              
         LA    R3,L'DIRREPID(R3)   R3 = A(NEXT EDICT REP ID)                    
*                                                                               
         LA    RE,BLOCK+240        REMOVE THIS REP ID FROM Q2Q LIST             
         LR    R1,R2                                                            
         LA    R1,L'DIRREPID(R1)                                                
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     Q2Q20NX                                                          
         MVC   0(0,R2),0(R1)                                                    
*                                                                               
Q2Q25    MVI   ELCODE,EDILNKEQ     GET EDICT ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDILNKD,R6                                                       
         CLI   EDIMETHR,EDINONEQ   ANY METHOD OF ADDS TRANSMISSION?             
         BNE   *+6                                                              
         DC    H'0'                NO, DIE.  MUST HAVE SOME METHOD              
         CLI   EDIMETHR,EDIEASYQ   DO WE USE EDICT METHOD?                      
         BE    Q2Q20A              YES                                          
         DROP  R6                                                               
         LA    R2,L'DIRREPID(R2)   YES, CHECK NEXT REP ID                       
*                                                                               
Q2Q20NX  CLI   0(R2),0             IF NO MORE TO LIST                           
         BE    Q2QYES              THEN WE GOT AT LEAST ONE REP                 
         B     Q2Q20LP             OTHERWISE LOOP BACK UNTIL NO MORE            
*                                                                               
Q2QYES   B     YES                                                              
Q2QNO    B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DIGS UP THE MARKETS FOR THE MARKET LIST                          
***********************************************************************         
DIGUPMKT DS    0H                                                               
         NMOD1 0,**DMKT**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     R8,ASPOOLD                                                       
         LA    R5,SYSSPARE                                                      
*                                                                               
         L     R6,AIO              GET RATING SERVICE                           
         MVI   ELCODE,AVARMCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDMOD,R6                                                      
         MVC   P+41(1),AVARMSRV                                                 
         CLI   AVARMSRV,C'B'                                                    
         BNE   *+8                                                              
         MVI   P+41,C'N'                                                        
*                                                                               
         CLI   P+41,C'N'                                                        
         BNE   *+12                                                             
         MVI   RTNGCHAR,C'0'       X'F0' IS NSI IN CLTHDR FORMAT                
         B     *+8                                                              
         MVI   RTNGCHAR,C'1'       X'F1' IS ARB IN CLTHDR FORMAT                
*                                                                               
         LA    R0,MKTBLOCK         DO THE MARKET LISTS HEADER                   
         LA    R1,600                                                           
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R3,MKTBLOCK                                                      
*                                                                               
         LA    R4,KEY              LOOK FOR ALL THE AVAIL MARKETS               
         USING AVMRECD,R4              FOR THIS REFERENCE NUMBER                
         XC    KEY,KEY                                                          
         MVI   AVMKTYP,AVMKTYPQ                                                 
         MVI   AVMKSUB,AVMKSUBQ                                                 
         L     R6,AIO                                                           
         USING AVAKEY,R6                                                        
         MVC   AVMKAM,AVAKAM                                                    
         MVC   AVMKREF,AVAKREF                                                  
         DROP  R6                                                               
         MVC   AIO,AIO2            USE AIO2 FOR MARKETS                         
*                                                                               
         GOTO1 HIGH                                                             
DMKTLP   CLC   KEY(AVMKMKT-AVMKEY),KEYSAVE                                      
         BNE   DMKT10                                                           
         OC    AVMKMKT,AVMKMKT                                                  
         BZ    DMKTNX                                                           
         MVC   0(L'AVMKMKT,R3),AVMKMKT                                          
         LA    R3,L'AVMKMKT(R3)                                                 
DMKTNX   GOTO1 SEQ                                                              
         B     DMKTLP                                                           
*                                                                               
DMKT10   LA    R0,RMKTBLCK         GET THE RATING SERVICE MARKETS               
         LA    R1,600                                                           
         LA    RE,MKTBLOCK                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R3,RMKTBLCK                                                      
*                                                                               
DMKT10LP OC    0(2,R3),0(R3)                                                    
         BZ    DMKTX                                                            
*                                                                               
         LA    R4,KEY                                                           
         USING MKTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'            C'0' FILL THE KEY                            
         MVC   KEY+1(16),KEY                                                    
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         EDIT  (B2,0(R3)),(4,MKTKMKT),FILL=0                                    
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     R6,AIO                                                           
         CLC   MKTKEY(MKTKEYLN),0(R6)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         USING MKTREC,R6                                                        
         CLC   MKTRS1,RTNGCHAR                                                  
         BNE   DMKT20                                                           
         MVC   0(2,R3),MKTRSM1                                                  
         B     DMKT40                                                           
*                                                                               
DMKT20   CLC   MKTRS2,RTNGCHAR                                                  
         BNE   DMKT30                                                           
         MVC   0(2,R3),MKTRSM2                                                  
         B     DMKT40                                                           
*                                                                               
DMKT30   MVC   0(2,R3),=X'FFFF'    PUT X'FFFF' IF NO RATING SVC MARKET          
         B     DMKT50                                                           
*                                                                               
DMKT40   OC    0(2,R3),0(R3)                                                    
         BNZ   *+10                                                             
         MVC   0(2,R3),=X'FFFE'    PUT X'FFFE' IF ZERO RSRV MARKET              
*                                                                               
DMKT50   LA    R3,2(R3)            CHECK NEXT STATION MARKET                    
         B     DMKT10LP                                                         
*                                                                               
DMKTX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THIS RADIO REP GET A COPY OF THIS               
* AVAIL                                                                         
***********************************************************************         
RADIOREP DS    0H                                                               
         NMOD1 0,**RREP**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     R8,ASPOOLD                                                       
         LA    R5,SYSSPARE                                                      
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R3,MKTBLOCK                                                      
RREPLOOP OC    0(2,R3),0(R3)                                                    
         BZ    RREPNO                                                           
*                                                                               
         LA    R4,KEY              SET UP SPOT MARKET RECORD                    
         USING MKTKEY,R4                                                        
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   HALF,0(R3)                                                       
         EDIT  (B2,HALF),(4,MKTKMKT),ZERO=YES,FILL=0                            
         MVC   MKTKAGY,AGENCY                                                   
         MVI   MKTKFILL,C'0'                                                    
         MVC   MKTKFILL+1(L'MKTKFILL-1),MKTKFILL                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     R6,AIO                                                           
         CLC   MKTKEY(MKTKEYLN),0(R6)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING MKTREC,R6                                                        
         LA    R6,MKTALST          R6=A(LIST OF ALPHA MKT CODES)                
         DROP  R6                                                               
*                                                                               
         OC    0(3,R6),0(R6)       ANY ALPHA MKT CODE?                          
*                                    NONE, AVAIL SHOULD GO TO REP               
         BZ    RREPYES                 ACCORDING TO SBARN                       
*                                                                               
         BAS   RE,MKTINSTA         ANY OF THE MARKETS IN STATION REC?           
         BE    RREPYES                                                          
*                                                                               
RREPNEXT LA    R3,L'MKTBLOCK(R3)                                                
         B     RREPLOOP                                                         
*                                                                               
RREPYES  MVC   AIO,AIO1                                                         
         B     YES                                                              
RREPNO   MVC   AIO,AIO1                                                         
         B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS IF ANY OF THE MARKET RECORD'S ALPHA MARKET CODES          
* MATCHES THE ORIGINATING MARKET ON THE ADDS STATION RECORD                     
*                                                                               
* ON ENTRY:    AIO2                A(MARKET RECORD)                             
***********************************************************************         
MKTINSTA NTR1                                                                   
         MVC   AIO,AIO3                                                         
         L     R6,AIO2                                                          
         USING MKTREC,R6                                                        
         LA    R6,MKTALST          R6=A(LIST OF ALPHA MKT CODES)                
         DROP  R6                                                               
*                                                                               
MISLOOP  OC    0(3,R6),0(R6)       ANY ALPHA MKT CODE?                          
         BZ    MISNO               NONE, NOTHING TO CHECK                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STTNKEY,R4                                                       
         MVI   STTNSYS,STTNSYSQ                                                 
         MVI   STTNTYP,STTNTYP2                                                 
         MVC   STTNMED,QMED                                                     
         MVC   STTNAMKT,0(R6)                                                   
         L     R1,AREPID                                                        
         MVC   STTNREP,0(R1)                                                    
         CLI   4(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   STTNREP+2,C' '                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
         L     R2,AIO                                                           
         CLC   STTNKEY(STTNSTAC-STTNKEY),0(R2)                                  
         BE    MISYES                                                           
         DROP  R4                                                               
*                                                                               
         LA    R6,3(R6)                                                         
         B     MISLOOP                                                          
*                                                                               
MISYES   MVC   AIO,AIO2                                                         
         B     YES                                                              
MISNO    MVC   AIO,AIO2                                                         
         B     NO                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE SPADDFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDF3D          (OUR MAINTENANCE SCREEN)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDE3D          (OUR LIST SCREEN)                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDD3D          (OUR SEND SCREEN)                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDC3D          (OUR STANDARD COMMENTS LIST SCREEN)          
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPADDSECTS                                                     
         EJECT                                                                  
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPADDWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
AREPID   DS    A                   A(REP ID IN BLOCK)                           
USEHDHK  DS    CL1                 USE HEADER HOOK ROUTINE (Y/N)                
RECFOUND DS    CL1                                                              
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE AREA                          
QDPT     DS    CL1                 DAYPART CHARACTER CODE                       
BDPTNUM  DS    XL1                 DAYPART NUMBER USED FOR CPP'S                
BITFLAG  DS    XL1                 VARIOUS FLAGS                                
*                                  X'80' - DAYPART MENU READ FROM FILE          
*                                  X'40' - 0=PUT OUT INFO LINE ON HDR           
*                                  X'20' - OPENED PRINT QUEUE                   
*                                  X'02' - SHOW ME LIST COMMENTS SCRN           
*                                  X'01' - LIST STANDARD COMMENTS MODE          
FILTRFLG DS    XL1                 VARIOUS FILTER BITS FOR LIST MODE            
*                                  X'80' - REFERENCE #                          
*                                  X'40' - BUYER                                
*                                  X'20' - ADVERTISER                           
*                                  X'10' - PRODUCT                              
*                                  X'08' - ESTIMATE                             
*                                  X'04' - PERIOD                               
THEUSER  DS    CL(L'REMUSER)                                                    
NUMBDPSP DS    XL1                 NUMBER OF DAYPART/SPOTLEN ENTRIES            
PREVFLAG DS    XL1                 PREVIOUS KEY USED FLAG                       
PREVSEQN DS    XL1                 PREVIOUS SEQUENCE NUMBER                     
MYCLASS  DS    CL1                 PRINT CLASS ('Z' FOR TST, ELSE 'G')          
MKTCNT   DS    CL1                 NUMBER OF MARKETS IN MARKET RECORD           
TMKTCNT  DS    CL1                 TOTAL # OF MKTS USED FOR REP                 
RTNGCHAR DS    CL1                 RATING SERVICE CHARACTER                     
*                                  X'F0' IS NSI IN CLIENT HEADER FORMAT         
*                                  X'F1' IS ARB IN CLIENT HEADER FORMAT         
LOCOV    DS    CL2                 LOCATION OVERRIDE (SPACES IF NONE)           
BYRCODE  DS    CL3                 BUYER ASSIGNED TO THE AVAIL HEADER           
ASSTBYR  DS    CL3                 ASSISTANT BUYER IN BUYER RECORD              
CMMNTCNT DS    XL1                 NUM OF COMMENTS SELECTED FROM LIST           
CMMNTLST DS    5CL8                SELECTED COMMENTS LIST                       
*                                                                               
PREVKEY  DS    XL(L'AVAKEY)        SAVED KEYS                                   
CMMNTKEY DS    XL(L'COMKEY)                                                     
SAVEKEY  DS    XL(L'AVAKEY)                                                     
*                                                                               
MRKHEAD1 DS    CL(L'P)             TEMPORARY HEADLINES                          
MRKHEAD2 DS    CL(L'P)                                                          
TMPHEAD  DS    CL(L'P)                                                          
*                                                                               
DESCELEM DS    XL(AVARDSLQ)        DESCRIPTION ELEMENT                          
DPTELEM  DS    XL80                DAYPART(S) ELEMENT                           
SLNELEM  DS    XL80                SPOT LENGTH(S) ELEMENT                       
DEMOELEM DS    XL255               DEMO ELEMENT                                 
*                                                                               
BIGSPLKY DS    XL128               BIG SPOOLKEY                                 
*                                                                               
MKTBLOCK DS    300XL2              LIST OF MARKETS                              
*                                                                               
RMKTBLCK DS    300XL2              LIST OF RATING SERVICE MARKETS               
*                                                                               
ACTSEND  EQU   13                  ACTION SEND IS EQUATED TO 13                 
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTREFN  DS    CL7                                                              
         DS    C                                                                
LSTBUYR  DS    CL3                                                              
LSTSLASH DS    C                                                                
LSTOFF   DS    CL2                                                              
         DS    CL3                                                              
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTPRD   DS    CL3                                                              
         DS    CL2                                                              
LSTEST   DS    CL3                                                              
         DS    CL2                                                              
LSTFLTD  DS    CL17                                                             
         DS    CL2                                                              
LSTDUED  DS    CL8                                                              
         DS    CL2                                                              
LSTSTAT  DS    CL4                                                              
         DS    C                                                                
LSTDATE  DS    CL8                                                              
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PRTSTARS DS    CL2                                                              
         DS    CL1                                                              
PRTMKT   DS    CL4                                                              
         DS    CL1                                                              
PRTMKTNM DS    CL24                                                             
         DS    CL1                                                              
PRTBUYR  DS    CL24                                                             
PRTSLASH DS    C                                                                
PRTBYROF DS    CL2                                                              
         DS    C                                                                
PRTPHONE DS    CL12                                                             
         DS    C                                                                
PRTEXT   DS    C                                                                
PRTEXTNM DS    CL4                                                              
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
*                                                                               
* STANDARD COMMENTS LIST LINE DSECT                                             
*                                                                               
LSCDSECT DSECT                                                                  
LSCSELH  DS    CL8                                                              
LSCSEL   DS    CL1                                                              
         DS    CL8                                                              
LSCCOMCD DS    CL8                                                              
         DS    CL8                                                              
LSCLINE  DS    CL60                                                             
LSCNEXTL DS    0C                                                               
*                                                                               
* DMPRTQL                                                                       
* FAFACTS                                                                       
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099SPADD03   05/01/02'                                      
         END                                                                    
