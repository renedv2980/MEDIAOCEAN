*          DATA SET FAPROGDSP  AT LEVEL 011 AS OF 08/28/17                      
*PHASE PROGDSPA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE DDWTO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'FAPROGDSP - HANDLE PROGRAM FILE DATASPACE'                      
         PRINT NOGEN                                                            
PROGDSP  CSECT                                                                  
         NBASE WORKL,*PROGDSP,=A(WORKAREA),RA,R9                                
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         ST    RC,SAVERC                                                        
*                                                                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
TDSP01   BRAS  RE,INIT             READ CARDS ETC                               
         BNE   XBASE                                                            
*                                                                               
         CLI   MODE,C'I'           INITIALISE                                   
         BNE   TDSP04                                                           
*                                                                               
         BRAS  RE,MAIN             MAIN LOOP FOR MODE=INIT                      
*                                                                               
         CLI   TEST,C'Y'           TEST RUN?                                    
         BNE   TDSP02                                                           
         CLI   KILL,C'Y'                                                        
         BNE   TDSP04                                                           
         DC    H'0',C'KILL PROCESS'                                             
*                                                                               
TDSP02   BRAS  RE,SETWAIT                                                       
         BL    XBASE                                                            
         B     TDSP01                                                           
*                                                                               
TDSP04   CLI   MODE,C'R'           REPORT ON EXISTING DSPACE                    
         BNE   TDSP06                                                           
         BRAS  RE,GETSPC                                                        
         B     XBASE                                                            
*                                                                               
TDSP06   B     XBASE                                                            
*                                                                               
ALITS    DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  ,                                                                
         MVC   TITLE(L'CTITLE),CTITLE                                           
*                                                                               
         XR    R1,R1               TELL IT YOU HAVE PRINT LINE COVERED          
         MVC   P(L'I1),I1                                                       
         BRAS  RE,DOMSG                                                         
         MVC   P(L'I2),I2                                                       
         BRAS  RE,DOMSG                                                         
         MVC   P(L'I3),I3                                                       
         BRAS  RE,DOMSG                                                         
         MVC   P(L'I4),I4                                                       
         BRAS  RE,DOMSG                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         LHI   R1,1                BEGAN READING INPUT CARDS                    
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R3,ASVCARD                                                       
INIT02   GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'/*',0(R3)        END OF CARDS?                                
         BE    INIT04              YES                                          
         AHI   R3,80                                                            
         B     INIT02                                                           
*                                                                               
INIT04   LHI   R1,2                ENDED READING INPUT CARDS                    
         BRAS  RE,DOMSG                                                         
         LHI   R1,3                BEGAN PROCESSING INPUT CARDS                 
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R3,ASVCARD          NOW PROCESS CARDS INDIVIDUALLY               
INIT06   CLC   =C'/*',0(R3)                                                     
         BE    INIT08                                                           
         MVC   P(80),0(R3)                                                      
         GOTO1 VPRINTER            PRINT PARAMETER CARD                         
         MVC   CARD,0(R3)                                                       
         BRAS  RE,CARDVAL          VALIDATE KEYWORD=VALUE                       
         AHI   R3,80                                                            
         B     INIT06                                                           
*                                                                               
INIT08   LHI   R1,4                ENDED PROCESSING INPUT CARDS                 
         BRAS  RE,DOMSG                                                         
*                                                                               
         OC    ERRCNT,ERRCNT       ERRORS?                                      
         BZ    INIT10              NO                                           
         LHI   R1,7                                                             
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
*                                                                               
INIT10   LHI   R1,5                BEGAN SAVING INPUT CARDS TO OLDPARMS         
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R3,ASVCARD                                                       
         OPEN  (OLDPARMS,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    INIT12                                                           
         LHI   R1,8                OLDPARMS WON'T OPEN                          
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
*                                                                               
INIT12   PUT   OLDPARMS,0(R3)                                                   
         CLC   =C'/*',0(R3)                                                     
         BE    INIT14                                                           
         AHI   R3,80                                                            
         B     INIT12                                                           
*                                                                               
INIT14   CLOSE OLDPARMS                                                         
         LHI   R1,6                ENDED SAVING INPUT CARDS TO OLDPARMS         
         BRAS  RE,DOMSG                                                         
*                                                                               
         CLC   =CL4'TEST',RUN      TEST RUN GOES WITHIN CORE                    
         BNE   *+8                                                              
         MVI   TEST,C'Y'                                                        
*                                                                               
         CLI   MODE,C'I'           TEST INIT MODE                               
         BNE   EXITOK                                                           
*                                                                               
         BRAS  RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         LA    R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN CODE FOR MODE=INIT                                             *         
***********************************************************************         
MAIN     NTR1  ,                                                                
         BAS   RE,FRKPAGES         SET COUNT OF 4K PAGES REQUIRED               
*                                                                               
MAIN02   BAS   RE,FREESPC          FREE ANY OLD DATASPACES                      
         BAS   RE,MAKESPC          MAKE A NEW ONE                               
         BAS   RE,GETSPC           GET ADDRESS OF IT                            
*                                                                               
         MVC   AHEADER,DMOFFS                                                   
         MVC   ADSDATA,DMOFFS      STARTING FROM OFFS                           
*                                                                               
MAIN04   L     R1,ADSDATA                                                       
         LHI   RF,((RMAXRES*64)+1023)          TABLE HEADERS                    
         SRL   RF,10                                                            
         SLL   RF,10                                                            
         AR    R1,RF                                                            
         ST    R1,ADSDATA                                                       
         BRAS  RE,HEADERS          BUILD HEADER ENTRIES                         
         BRAS  RE,BLOCKS           BUILD STORAGE BLOCKS                         
*                                                                               
MAINX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET PAGES REQUIRED FOR EACH TABLE. TOTAL RETURNED IN KB             *         
***********************************************************************         
FRKPAGES NTR1  ,                                                                
         LHI   RF,((RMAXRES*64)+1023)                                           
         SRL   RF,10               TABLE HEADERS                                
         A     RF,DCPGMA           KB FOR PGM AREA                              
*                                                                               
FRKP02   LA    R3,HDRTAB           TABLE OF DATASPACE ENTRIES                   
         USING HDRTABD,R3                                                       
*                                                                               
FRKP04   CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    FRKP08              YES                                          
         ICM   R0,15,HDRNUM        NUMBER OF ENTRIES                            
         BZ    FRKP06                                                           
         MH    R0,HDRWIDTH         * WIDTH OF 1 ENTRY                           
         AHI   R0,1023                                                          
         SRL   R0,10               ROUND TO NEXT HIGHEST 1K MULTIPLE            
         AR    RF,R0               ADD TO TOTAL REQUIRED                        
*                                                                               
FRKP06   AHI   R3,HDRTABL          NEXT TABLE ENTRY                             
         B     FRKP04                                                           
*                                                                               
FRKP08   AHI   RF,4                EXTRA FOR CORE REPORT AREA                   
         AHI   RF,4                EXTRA FOR DUMMY WRITE AREA                   
         AHI   RF,3                ROUND TO NEXT HIGHEST 4K                     
         SRL   RF,2                                                             
         ST    RF,PAGES            SET TOTAL NUMBER OF PAGES REQUIRED           
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER INFO                                                   *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
HEADERS  NTR1  ,                                                                
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         SAC   512                 SET UP AMODE                                 
         USING DMSPACED,R2                                                      
*                                                                               
         SAM31                                                                  
         LA    R3,HDRTAB           TABLE OF DATASPACE ENTRIES                   
         USING HDRTABD,R3                                                       
         B     HDR08               FIRST 64 BYTES HAS ADCONS                    
*                                                                               
HDR02    CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    HDR10               YES                                          
         STCM  R2,15,HDRADR        SAVE A(IN DATASPACE) FOR LATER               
*                                                                               
         MVC   DSPNAME,HDRNAME     EYECATCHER                                   
         MVC   DSPTYPE,HDRTYPE     TYPE                                         
         MVC   DSPTWIDE,HDRWIDTH   WIDTH                                        
*                                                                               
         OC    HDRNUM,HDRNUM       ANYTHING SET?                                
         BNZ   HDR04               YES                                          
         MVC   DSPUSER,DEFUSR      SET EMPTY ROW                                
*                                                                               
         ICM   RF,15,ADUMMY                                                     
         STCM  RF,15,DSPECB                                                     
         AH    RF,HDRWIDTH                                                      
         BCTR  RF,0                                                             
         STCM  RF,15,DSPTEND                                                    
         B     HDR06                                                            
*                                                                               
HDR04    ICM   RF,15,ADSDATA       A(FIRST ROW)                                 
         STCM  RF,15,DSPECB                                                     
         ICM   R1,15,HDRNUM          NUMBER OF ENTRIES                          
         MH    R1,HDRWIDTH         * WIDTH OF 1 ENTRY                           
         AR    RF,R1               + A(START)                                   
         BCTR  RF,0                                                             
         STCM  RF,15,DSPTEND       A(END-1)                                     
         AHI   RF,1                                                             
*                                                                               
         AHI   RF,1023             ROUND TO NEXT HIGHEST 1K MULTIPLE            
         SRL   RF,10                                                            
         SLL   RF,10                                                            
         STCM  RF,15,ADSDATA       SET A(NEXT TABLE)                            
*                                                                               
HDR06    AHI   R3,HDRTABL          NEXT TABLE ENTRY                             
*                                                                               
HDR08    AHI   R2,L'DSPHDR         NEXT HEADER                                  
         B     HDR02                                                            
*                                                                               
HDR10    SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD STORAGE BLOCKS                                                *         
***********************************************************************         
BLOCKS   NTR1  ,                                                                
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         SAC   512                 SET UP AMODE                                 
         USING FAPGMSD,R2                                                       
         L     RF,ADSDATA                                                       
         STCM  RF,15,PGMSCORE      SET A(CORE TABLE BUILD AREA)                 
         AHI   RF,FOURK                                                         
         STCM  RF,15,PGMSPGM       SET A(PROGRAMS BUILD AREA)                   
         L     RE,DCPGMA                                                        
         SLL   RE,10                                                            
         AR    RF,RE                                                            
         ST    RF,PGMSLAST                                                      
         SAC   0                                                                
         SAM24                                                                  
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  ,                                                                
         LHI   R1,9                BEGAN SETTING OPERATOR COMMS                 
         BRAS  RE,DOMSG                                                         
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         LHI   R1,10               ENDED SETTING OPERATOR COMMS                 
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
FREESPC  ST    RE,SAVERE           DELETE DATASPACE                             
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'DEL '                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                 NB - IGNORE CONDITON CODE                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MAKESPC  ST    RE,SAVERE           CREATE NEW DATASPACE                         
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'MAKE'                                                 
         MVC   WORK+4(12),DSPACE                                                
         ICM   RF,15,PAGES         NUMBER OF 4K PAGES                           
         STCM  RF,15,WORK+16                                                    
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GETSPC   ST    RE,SAVERE           GET ALET                                     
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   DMALET,WORK+24                                                   
         MVC   DMTOKN,WORK+28                                                   
         OC    DMALET,DMALET                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SETWAIT - SET TIMER AND WAIT INDEFINITELY - ONLY RETURN WHEN AN     *         
* INTERRUPT IS DETECTED                                               *         
***********************************************************************         
SETWAIT  NTR1  ,                                                                
*                                                                               
WAIT02   LA    R1,ECBLST           BUILD ECBLIST                                
         MVC   0(4,R1),AOPERECB    OPERATOR ECB COMES FIRST                     
         OI    0(R1),X'80'         FLAG EOL                                     
         LA    R1,ECBLST                                                        
         WAIT  ECBLIST=(R1)                                                     
*                                                                               
         L     RF,AOPERECB         OPERATOR ECB POST MEANS TERMINATE            
         TM    0(RF),X'40'                                                      
         BZ    WAIT04                                                           
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BE    EXITL                                                            
*                                                                               
         CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         XR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         MVC   CARD(0),CIBDATA                                                  
         EX    R1,*-6                                                           
*                                                                               
         CLC   =C'EOJ',CARD        EOJ WILL END JOB                             
         BE    EXITL                                                            
*                                                                               
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         MVC   PLINE+16(13),CARD                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
*                                                                               
         L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     WAIT02                                                           
         DROP  RF                                                               
*                                                                               
WAIT04   DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT INFORMATION MESSAGE                                          *         
* NTRY: R1  NZ      INDEX TO MESSAGE                                  *         
*       R1  ZERO    MESSAGE IS ALREADY ON PRINT LINE                  *         
***********************************************************************         
DOMSG    NTR1  ,                                                                
         LTR   R1,R1                                                            
         BZ    DOMSG02                                                          
         BCTR  R1,0                                                             
         MHI   R1,L'MESSTAB                                                     
         A     R1,AMESSTAB                                                      
         MVC   P(L'MESSTAB),0(R1)                                               
*                                                                               
DOMSG02  GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LA    R2,CARD             R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),(1,SCNBLK)                          
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         MVC   0(0,RE),SC2NDFLD                                                 
         EX    RF,*-6                                                           
         B     EXITOK                                                           
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,INVLIN                                                        
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,INVKEY                                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,NOTNUM                                                        
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,NOTCHR                                                        
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,TOOSHT                                                        
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,TOOLNG                                                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,TOOLOW                                                        
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,TOOBIG                                                        
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,NOINP                                                         
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   P(15),=CL15'!! ***ERROR*** '                                     
         MVC   P+15(40),0(R1)                                                   
         GOTO1 VPRINTER                                                         
         BRAS  RE,CARDHLP                                                       
         LH    R0,ERRCNT                                                        
         AHI   R0,1                                                             
         STH   R0,ERRCNT                                                        
         B     EXITL                                                            
*                                                                               
INVKEY   DC    CL40'Invalid Keyword'                                            
INVLIN   DC    CL40'Invalid line format'                                        
TOOLOW   DC    CL40'Numeric value too small'                                    
TOOBIG   DC    CL40'Numeric value too large'                                    
NOINP    DC    CL40'Invalid/Missing value'                                      
NOTNUM   DC    CL40'Value not a valid number'                                   
NOTCHR   DC    CL40'Value not a valid character string'                         
TOOSHT   DC    CL40'Length of input string too short'                           
TOOLNG   DC    CL40'Length of input string too long'                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT HELP FOR THIS CARD BASED ON LAST TIME INFORMATION *         
* NTRY:  R2    = A(SCANBLK ENTRY FOR THIS FIELD)                      *         
*        R3    = A(CARDTAB ENTRY FOR THIS FIELD)                      *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
         USING CARDTABD,R3                                                      
CARDHLP  NTR1  ,                                                                
         LA    R4,P                                                             
         MVC   0(L'CHMS01,R4),CHMS01                                            
         CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BE    CHLP02              NO                                           
         MVC   0(L'CHMS02,R4),CHMS02                                            
         CLI   CTYPE,CTCHR         CHARACTER INPUT?                             
         BE    CHLP02              NO                                           
         MVC   P,SPACES                                                         
         B     EXITOK                                                           
*                                                                               
CHLP02   AHI   R4,L'CHMS01+1                                                    
         EDIT  (B4,CMIN),(9,(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R4,R0                                                            
         MVC   1(L'TO,R4),TO                                                    
         AHI   R4,L'TO+2                                                        
         EDIT  (B4,CMAX),(9,(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                     
         GOTO1 VPRINTER                                                         
*                                                                               
CHLP04   OPEN  (OLDPARMS,INPUT)                                                 
         LTR   RF,RF                                                            
         BZ    CHLP06                                                           
         LHI   R1,8                OLDPARMS WON'T OPEN                          
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
*                                                                               
CHLP06   GET   OLDPARMS,OLDCARD                                                 
         CLC   =C'/*',OLDCARD                                                   
         BE    CHLP08                                                           
         CLI   OLDCARD,C'*'                                                     
         BE    CHLP06                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SC1STLEN                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   CHLP06                                                           
         CLC   SC1STFLD(0),OLDCARD                                              
*                                                                               
         MVC   P(L'CHMS03),CHMS03                                               
         MVC   P+L'CHMS03(80),OLDCARD                                           
         GOTO1 VPRINTER                                                         
         B     CARDHLPX                                                         
*                                                                               
CHLP08   MVC   P(L'CHMS04),CHMS04                                               
         GOTO1 VPRINTER                                                         
         B     CARDHLPX                                                         
*                                                                               
CARDHLPX CLOSE OLDPARMS                                                         
         B     EXITOK                                                           
*                                                                               
CHMS01   DC    CL33'!! Numeric value must be in range'                          
CHMS02   DC    CL33'!! Input string must be of length'                          
CHMS03   DC    CL33'!! OLDPARMS parameter value was: '                          
CHMS04   DC    CL41'!! OLDPARMS parameter value was not found'                  
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         ORG   PROGDSP+((((*-PROGDSP)/16)+1)*16)                                
                                                                                
CARDTAB  DC    CL8'MODE    ',F'001',F'0000010'                                  
         DC    AL1(03,CTCHR,L'MODE,0),AL4(MODE)                                 
         DC    CL8'RUN     ',F'001',F'0000010'                                  
         DC    AL1(02,CTCHR,L'RUN,0),AL4(RUN)                                   
         DC    CL8'DSPACE  ',F'001',F'0000012'                                  
         DC    AL1(05,CTCHR,L'DSPACE,0),AL4(DSPACE)                             
         DC    CL8'PGMLIST ',F'001',F'0100000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCPGMS)                                    
         DC    CL8'PGMAREA ',F'001',F'9999999'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCPGMA)                                    
*                                                                               
CARDTABX DC    AL1(CARDEOT)                                                     
                                                                                
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
PROGDSP  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
LITERALS DS    0D                                                               
         LTORG                                                                  
*                                                                               
ASVCARD  DC    A(SVCARD)                                                        
AMESSTAB DC    A(MESSTAB)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
VCARDS   DC    V(CARDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VDADDS   DC    V(DADDS)                                                         
*                                                                               
SAVERC   DC    A(0)                                                             
SAVERD   DC    A(0)                                                             
*                                                                               
ARZERO   DC    16F'0'                                                           
RMAXRES  EQU   256                 INCREASE IF MORE THAN 256 TABLES             
DSTABLE  EQU   C'T'                DSPTYPE=TABLE                                
FOURK    EQU   4*1024                                                           
*                                                                               
DCPGMA   DC    A(1)                                                             
*                                                                               
DEFUSR   DC    C'MPTY'             TABLE IS NOT USED                            
TO       DC    C'To'                                                            
*                                                                               
CTITLE   DC    C'Input Card Details'                                            
DTITLE   DC    C'Test results'                                                  
*                   123456789012345678901234567890123456789                     
I1       DS    0CL78' '                                                         
         DC    CL39'The parameter cards listed below come f'                    
         DC    CL39'rom DDS.PARMS(PROGXXX) where the XXX is'                    
I2       DS    0CL78' '                                                         
         DC    CL39'one of the systems with a programs file'                    
         DC    CL39' still defined.                        '                    
I3       DS    0CL78' '                                                         
         DC    CL39'The dataset OLDPARMS (see the input JCL'                    
         DC    CL39' for the DSN) holds the parameters reco'                    
I4       DS    0CL78' '                                                         
         DC    CL39'rded for the last successful initialisa'                    
         DC    CL39'tion.                                  '                    
*                                                                               
* FAPGMSDEQU                                                                    
       ++INCLUDE FAPGMSDEQU                                                     
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
OLDPARMS DCB   DSORG=PS,MACRF=(GM,PM),RECFM=FB,LRECL=80,BLKSIZE=400,   +        
               DDNAME=OLDPARMS                                                  
*                                                                               
AOPERECB DC    A(0)                                                             
ACOMM    DC    A(0)                                                             
         DC    CL8'ECBLST  '       ECBLIST                                      
ECBLST   DC    4F'0'                                                            
*                                                                               
MODE     DC    CL10'INIT'                                                       
RUN      DC    CL4'DSP '                                                        
KILL     DC    CL4'N   '                                                        
TEST     DC    C'N'                                                             
DSPACE   DC    CL12' '                                                          
SYSTEM   DC    CL4' '                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATASPACE HEADER TABLE ENTRY DETAILS                                *         
***********************************************************************         
         ORG   PROGDSP+((((*-PROGDSP)/16)+1)*16)                                
                                                                                
HDRTAB   DS    0XL16                                                            
*                                                                               
         DC    CL8'Pgm List'   *** PROGRAM LIST                                 
DCPGMS   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(DSTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
DEND     DC    X'FFFFFFFF'         END OF TABLE                                 
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER DATASPACE HEADERS                                    *         
***********************************************************************         
HDRTABD  DSECT                                                                  
HDRNAME  DS    CL8                 IDENTIFIER IN DATASPACE                      
HDRNUM   DS    F                   NUMBER OF ENTRIES (FROM INPUT PARMS)         
HDRWIDTH DS    H                   WIDTH OF A SINGLE ENTRY                      
HDRTYPE  DS    X                   TYPE (USUALLY TABLE)                         
HDRUSED  DS    X                   ENTRY IS REALLY IN USE                       
HDRUDMY  EQU   C'D'                TABLE IS NOT USED YET                        
HDRADR   DS    A                   A(ENTRY IN DATASPACE)                        
HDRTABL  EQU   *-HDRTABD                                                        
*                                                                               
PROGDSP  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SAVE AREAS AND BUFFERS                                              *         
***********************************************************************         
IOA      DS    4096C                                                            
*                                                                               
SVCARD   DC    100CL80' '                                                       
         EJECT                                                                  
***********************************************************************         
* MESSAGES                                                            *         
***********************************************************************         
MESSTAB  DS    0CL60                                                            
  DC CL60'Began reading input parameters from cards                   '         
  DC CL60'Ended reading input parameters from cards                   '         
  DC CL60'Began processing input parameters                           '         
  DC CL60'Ended processing input parameters                           '         
  DC CL60'Began saving parameter cards to OLDPARMS dataset            '         
  DC CL60'Ended saving parameter cards to OLDPARMS dataset            '         
  DC CL60'Card validation error - Application terminating             '         
  DC CL60'Error on open of OLDPARMS dataset - Application terminating '         
  DC CL60'Began setting operator communications                       '         
  DC CL60'Ended setting operator communications                       '         
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
         DS    0D                                                               
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WORKD    DSECT                                                                  
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
NDMPS    DS    F                   NUMBER OF DUMPS PER SYSTEM                   
RDMPS    DS    F                   REMAINDER                                    
ADUMMY   DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
GETLEN   DS    F                   LENGTH RETURNED IF GETMAIN                   
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
WAITER   DS    A                                                                
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
WORK     DS    CL64                                                             
*                                                                               
CARD     DS    CL80                                                             
OLDCARD  DS    CL80                                                             
ERRCNT   DS    H                                                                
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
PLINE    DS    CL166                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         TITLE 'VARIABLE SCAN MODULE'                                           
         EJECT                                                                  
***********************************************************************         
* SCANNER REPLACEMENT                                                 *         
***********************************************************************         
SCANNER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SWORKX-SWORKD,**SCAN**                                           
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         SR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN1                                                            
*                                                                               
SCAN1    SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
*                                                                               
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
*                                                                               
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*HANDLE LINES OF DATA                                                           
*                                                                               
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BAS   RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
*                                                                               
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
*                                                                               
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
*                                                                               
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
*                                                                               
VARPAK   PACK  SDUB,0(0,R2)                                                     
*                                                                               
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
*                                                                               
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
*                                                                               
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
*                                                                               
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
*                                                                               
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
                                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
*                                                                               
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
*                                                                               
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
*                                                                               
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
*                                                                               
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
*                                                                               
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
*                                                                               
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
*                                                                               
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
         LTORG                                                                  
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
                                                                                
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
SWORKX   DS    0C                                                               
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FAPGMSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPGMSD                                                        
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011FAPROGDSP 08/28/17'                                      
         END                                                                    
