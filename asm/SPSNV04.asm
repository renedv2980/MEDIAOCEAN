*          DATA SET SPSNV04    AT LEVEL 026 AS OF 06/07/16                      
*PHASE T21004A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21004 - NET OF DETAILS                                               
*                                                                               
*  CALLED FROM: INVOICE CONTROLLER (T21000), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     SPSNVFB (T210FB) -- MAINTENANCE                                 
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
T21004   TITLE 'SPSNV04 - SPOT INVOICE DETAIL NET OVERLAY'                      
T21004   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21004*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          WE DO OUR OWN I/O'S                          
*                                                                               
         BAS   RE,SETPFTBL                                                      
*                                                                               
MAIN00   L     R1,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(R1)                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'INV',3,GLVPGM                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VREC                                                             
*                                                                               
MAINX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SETUP THE PFKEYS                                                              
***********************************************************************         
SETPFTBL NTR1                                                                   
         SR    R2,R2                                                            
         CLI   PFKEY,0                                                          
         BE    STPF10                                                           
         CLI   PFKEY,12                                                         
         BNE   STPF20                                                           
*                                                                               
STPF10   LA    R2,PFTABLE                                                       
         OI    CTLRFLG1,CF1NOCLR                                                
*                                                                               
STPF20   GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
         CLI   PFKEY,12                                                         
         BNE   STPFX                                                            
*                                                                               
         LA    R2,SPFTABLE                                                      
         CLI   CALLSP,1            CAN WE GO BACK FROM WHENCE WE CAME?          
         BNE   *+8                 NO                                           
         MVI   PFKEY,24            YES                                          
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
*                                                                               
         CLI   CALLSP,1                                                         
         BNE   *+14                                                             
         MVC   NETPFLN(11),=CL11'PF12=Return'                                   
         B     *+10                                                             
         MVC   NETPFLN(11),=CL11'PF12=List'                                     
         OI    NETPFLNH+6,X'80'                                                 
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
VKMED00  DS    0H                                                               
         LA    R2,NETMEDH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   VKMED10                                                          
         OI    MISCFLG1,MF1KYCHG                                                
         NI    NETCLTH+4,X'FF'-X'20'                                            
         NI    NETSTAH+4,X'FF'-X'20'                                            
*                                                                               
VKMED10  CLI   5(R2),0                                                          
         BNE   VKMED20                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPMD                             
         CLI   5(R2),0                                                          
         BE    NEEDMDIA                                                         
*                                                                               
VKMED20  GOTO1 VALIMED                                                          
         MVC   NETMDNM(L'MEDNM),MEDNM       SHOW MEDIA NAME                     
         OI    NETMDNMH+6,X'80'                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPMD                             
         OI    4(R2),X'20'                                                      
*                                                                               
VKMEDX   DS    0H                                                               
*****                                                                           
* VALIDATE THE CLIENT                                                           
*****                                                                           
VKCLT00  DS    0H                                                               
         LA    R2,NETCLTH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKCLT10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPCLT                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKCLT10  GOTO1 VALICLT                                                          
         MVC   NETCLNM,CLTNM       SHOW CLIENT NAME                             
         OI    NETCLNMH+6,X'80'                                                 
*                                                                               
         TM    MISCFLG1,MF1KYCHG   1ST TWO KEY FIELDS CHANGE?                   
         BZ    VKCLT20             NO, DON'T HAVE TO LOOK UP PROFILES           
         GOTO1 GETPRFIL,DMCB,=C'S0I2',PROF0I2                                   
*                                                                               
VKCLT20  DS    0H                                                               
         GOTO1 GETPRFIL,DMCB,=C'sA0A',PROF0A0A ****lowercase 's'**              
*                                                                               
VKCLTX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPCLT                            
         OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VKSTA00  DS    0H                                                               
         LA    R2,NETSTAH                                                       
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKSTA10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPSTA                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKSTA10  GOTO1 VALISTA                                                          
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   *+12                                                             
         CLI   QSTA+4,C'C'                                                      
         BE    BADDSTA                                                          
*                                                                               
         CLI   QSTA+4,C'S'                                                      
         BE    BADDSTA                                                          
         CLI   QSTA+4,C'D'                                                      
         BE    BADDSTA                                                          
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   VKSTA20                                                          
         DROP  R1                                                               
         GOTO1 GETPRFIL,DMCB,=C'sI2Z',PROFI2Z    !!! lowercase 's' !!!          
         CLI   PROFI2Z+6,C'Y'     READ PROFILES BY SUBMEDIA?                    
         BNE   VKSTA20                                                          
         L     R1,AIO                                                           
         USING STAREC,R1                                                        
         CLI   STYPE,C' '                                                       
         BE    VKSTA20                                                          
         CLC   STYPE,QMED                                                       
         BE    VKSTA20                                                          
         MVC   SVMEDIA,QMED                                                     
         MVC   QMED,STYPE          GET SUBMEDIA                                 
*                                                                               
         GOTO1 GETPRFIL,DMCB,=C'S0I2',PROF0I2     RE-READ I2 PROFILE            
         GOTO1 GETPRFIL,DMCB,=C'sA0A',PROF0A0A ****lowercase 's'**              
*                                                                               
         MVC   QMED,SVMEDIA                                                     
         DROP  R1                                                               
*                                                                               
VKSTA20  DS    0H                                                               
***************                                                                 
* NETWORK NOT ALLOWED SO MATCHING COULD BE SIMPLIFIED                           
***************                                                                 
         CLC   QNTWK,SPACES                                                     
         BH    INVLFLD                                                          
*                                                                               
         MVC   NETSTNM,MKTNM       SHOW STATION NAME                            
         OI    NETSTNMH+6,X'80'                                                 
*                                                                               
VKSTAX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPSTA                            
         OI    4(R2),X'20'                                                      
*****                                                                           
* VALIDATE THE PERIOD                                                           
*****                                                                           
VKPER00  DS    0H                                                               
         LA    R2,NETPERDH                                                      
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VKPER10                                                          
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPPER                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKPER10  GOTO1 PERVAL,DMCB,(5(R2),8(R2)),PERVALST                               
         TM    DMCB+4,X'03'                                                     
         BNZ   BADDTFMT                                                         
*                                                                               
VKPVALD  USING PERVALD,PERVALST                                                 
* WAS THE START DAY OR ANY PART OF THE END DAY ENTERED?                         
         TM    VKPVALD.PVALASSM,PVALASD+PVALAED+PVALAEM+PVALAEY                 
         BNO   BADDTFMT                WITHOUT THE DAY                          
*                                                                               
         MVC   BMOSS,VKPVALD.PVALCSTA     COMPRESSED DATES                      
         MVC   BMOSE,VKPVALD.PVALCEND                                           
*                                                                               
         MVC   VKPVALD.PVALESTA+4(2),=C'15'                                     
         GOTO1 GETBROAD,DMCB,(1,VKPVALD.PVALESTA),BRDDATES,GETDAY,ADDAY         
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                DATE SHOULDN'T BE INVALID                    
         DROP  VKPVALD                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(X'10',BRDDATES),(2,WORK)                            
         MVC   BRDCSDAT,WORK                                                    
         MVC   BRDCEDAT,WORK+3                                                  
*                                                                               
VKPERX   GOTO1 VGLOBBER,DMCB,=C'PUTF',(R2),,GLVSPPER                            
         OI    4(R2),X'20'                                                      
*                                                                               
         XC    WORK,WORK           SHOW PERIOD DATES                            
         MVC   WORK(L'BMOSS+L'BMOSE),BMOSS                                      
         CLI   P0I2BC,C'C'         CALENDAR MONTHS?                             
         BE    *+10                                                             
         MVC   WORK(L'BRDCSDAT+L'BRDCEDAT),BRDCSDAT                             
*                                                                               
         OI    NETPDNMH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(X'12',WORK),(8,NETPDNM)                             
         GOTO1 (RF),(R1),(2,WORK),(0,PERDSYMD)                                  
         GOTO1 (RF),(R1),(2,WORK+L'BMOSS),(0,PERDEYMD)                          
*****                                                                           
* VALIDATE THE INVOICE                                                          
*****                                                                           
VKINV00  DS    0H                                                               
         LA    R2,NETINVCH                                                      
*                                                                               
         TM    4(R2),X'20'         FIELD CHANGED?                               
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         MVC   QINVOICE,NETINVC                                                 
         OC    QINVOICE,SPACES                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING SNVKEY,R3                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
         MVC   SNVKSTA,BSTA                                                     
         MVC   SNVKMOS,BMOSS                                                    
         XC    SNVKMOS,=X'FFFF'                                                 
         MVC   SNVKINV,QINVOICE                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),INVDIR,KEY,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEYD,R6                                                       
         CLC   SNVKMAST,KEY        RECORD EXISTS?                               
         BNE   RECNTFND                                                         
         MVC   SVMASTKY(L'SNVKMAST),SNVKMAST                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 INITMNIO                                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    LOOK FOR THE HEADER ELEMENT                  
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         CLI   SNVHDEL,SNVHDELQ                                                 
         BE    *+6                                                              
         DC    H'0'                DIE IF WE'RE MISSING HEADER ELEMENT          
*                                                                               
         CLI   P0A0APAY,C'Y'       NO CHANGES TO PAID INVSOICES?                
         BNE   *+12                                                             
         TM    SNVHDCTL,SNVHDPDQ   INVOICE PAID?                                
         BO    INVPAID             YES - PRINT ERROR?                           
*                                                                               
         TM    SNVHDCTL,SNVHDRSQ   RESPONSE INVOICE?                            
         BNZ   RESPINVC                                                         
         TM    SNVHDCTL,SNVHDNTQ   NET INVOICE?                                 
         BNZ   ALRDYNET                                                         
         TM    SNVHDCTL,SNVHDDNQ   INVOICE WAS NETTED DOWN?                     
         BNZ   ALRDYNET                                                         
*                                                                               
         OI    4(R2),X'20'                                                      
         DROP  R6                                                               
*****                                                                           
* DONE VALIDATING ALL THE KEY FIELDS                                            
*****                                                                           
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VREC     DS    0H                                                               
         LA    R2,NETOKAYH                                                      
*                                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         BZ    VR10                                                             
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0             CLEAR IT                                     
         MVI   8(R2),0                                                          
         B     MISSFLD                                                          
*                                                                               
VR10     CLI   5(R2),0             NOTHING HERE?                                
         BE    MISSFLD                                                          
         CLI   8(R2),C'N'                                                       
         BE    VRX                                                              
         CLI   8(R2),C'Y'          OKAY?                                        
         BNE   INVLFLD                                                          
*                                                                               
VR20     XC    MINEKEY,MINEKEY     LOOK FOR INVOICE DETAILS                     
         MVI   MINEKEY,SNVIDELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    VR30                                                             
         CLI   MINERR,MINEEOF      NO INVOICE DETAILS?                          
         BE    VR40                NONE, CHANGE INVOICE TO NET INVOICE          
         DC    H'0'                                                             
*                                                                               
VR30     L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
         CLI   0(R6),SNVIDELQ      X'40' - DETAIL ELEM                          
         BNE   VR40                                                             
         ICM   R1,15,SNVIDCST                                                   
         CVD   R1,DUB                                                           
         MP    DUB,=P'85'          MULTIPLY BY 85%                              
         SRP   DUB,64-2,5          AND ROUND UP                                 
         CVB   R1,DUB              RESULT IS < ORIGINAL (SHOULD FIT)            
         STCM  R1,15,SNVIDCST                                                   
         DROP  R6                                                               
*                                                                               
         BAS   RE,MINIOWRT         SAVE THIS DETAIL                             
*&&DO                                                                           
         LA    R0,8                                                             
         STC   R0,WORK    DATA LENGTH IN BYTE 0                                 
         MVI   WORK+1,C'H'                                                      
         MVC   WORK+2(6),MINEKEY                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',WORK                           
*&&                                                                             
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(R6)                                                 
         MVC   MINEKEY+1(L'SNVKMINK-1),2(R6)                                    
         BAS   RE,MINIOHI          RE-ESTABLISH THE POINTER                     
*                                                                               
         BAS   RE,MINIOSEQ         EXAMINE NEXT DETAIL                          
         BE    VR30                LOOP BACK IF WE HAVE ONE                     
*&&DO                                                                           
         LA    R0,8                                                             
         STC   R0,WORK    DATA LENGTH IN BYTE 0                                 
         MVI   WORK+1,C'S'                                                      
         MVC   WORK+2(6),MINEKEY                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',WORK                           
         B     VR30                LOOP BACK IF WE HAVE ONE                     
*&&                                                                             
VR40     XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*        OI    SNVHDCTL,SNVHDNTQ   INVOICE IS NOW A NET INVOICE                 
         OI    SNVHDCTL,SNVHDDNQ   INVOICE IS NOW NETTED DOWN                   
         ZAP   DUB,SNVHDTCS                                                     
         MP    DUB,=P'85'          MULTIPLY BY 85%                              
         SRP   DUB,64-2,5          AND ROUND UP                                 
         ZAP   SNVHDTCS,DUB                                                     
         DROP  R6                                                               
*                                                                               
         BAS   RE,MINIOWRT                                                      
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         B     NO                  OTHERWISE RETURN 'NO'                        
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         EJECT                                                                  
***********************************************************************         
* NOTE: FROM THIS POINT ON WE WON'T USE R5 AS A GLOBAL MINIO DSECT REG          
***********************************************************************         
         DROP  R5                                                               
RELO     DS    A                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
***********************************************************************         
* REGULAR INFO MESSAGES                                                         
***********************************************************************         
NEEDMDIA MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
***********************************************************************         
* REGULAR ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
***********************************************************************         
* SYSTEM 23 ERROR MESSAGES                                                      
***********************************************************************         
RESPINVC MVI   GERROR1,RSPINVCE    ERROR: THIS IS A RESPONSE INVOICE            
         B     SYS23ERR                                                         
*                                                                               
ALRDYNET MVI   GERROR1,NETINVCE    ERROR: THIS IS A NET INVOICE                 
         B     SYS23ERR                                                         
*                                                                               
SYS23ERR MVI   GETMSYS,23                                                       
         B     ERREXIT                                                          
***********************************************************************         
* SYSTEM SPOT ERROR MESSAGES                                                    
***********************************************************************         
BADMEDIA MVI   GERROR1,INVMED      INVALID MEDIA                                
         B     ERREXIT2                                                         
*                                                                               
BADCLNT  MVI   GERROR1,INVMED      INVALID CLIENT                               
         B     ERREXIT2                                                         
*                                                                               
BADPROD  MVI   GERROR1,INVPROD     INVALID PRODUCT                              
         B     ERREXIT2                                                         
*                                                                               
BADPROD2 MVI   GERROR1,INVPROD2    INVALID PRODUCT #2                           
         B     ERREXIT2                                                         
*                                                                               
BADESTMT MVI   GERROR1,INVESTMT    INVALID ESTIMATE                             
         B     ERREXIT2                                                         
*                                                                               
BADSTATN MVI   GERROR1,INVSTATN    INVALID STATION                              
         B     ERREXIT2                                                         
*                                                                               
BADDTFMT MVI   GERROR1,INVDTFMT    INVALID DATE FORMAT                          
         B     ERREXIT2                                                         
*                                                                               
BADSLEN  MVI   GERROR1,INVSLEN     INVALID LENGTH                               
         B     ERREXIT2                                                         
*                                                                               
BADTIME  MVI   GERROR1,INVTIMXP    INVALID TIME EXPRESSION                      
         B     ERREXIT2                                                         
*                                                                               
BADFLPRD MVI   GERROR1,INVFLPRD    FILM AND PRODUCT DO NOT AGREE                
         B     ERREXIT2                                                         
*                                                                               
BADFLSLN MVI   GERROR1,INVFLSLN    FILM AND SPOT LENGTH DO NOT AGREE            
         B     ERREXIT2                                                         
*                                                                               
BADDTEST MVI   GERROR1,INVDTEST    DATES NOT WITHIN ESTIMATE PERIOD             
         B     ERREXIT2                                                         
*                                                                               
BADDTOUT MVI   GERROR1,INVDTOUT    DATE IS OOUT OF INPUT MONTH                  
         B     ERREXIT2                                                         
*                                                                               
BADDSTA  MVC   GERROR,=AL2(ACTNODIG) ACT NOT ALLOWED FOR DIG STATNS             
         B     ERREXIT                                                          
*                                                                               
INVPAID  MVC   GERROR,=AL2(1223)   INVOICE PAID                                 
         B     ERREXIT                                                          
*                                                                               
ERREXIT2 OI    GENSTAT2,USMYERSY   STICK WITH THIS MESSAGE SYSTEM               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         B     MYERRXIT                                                         
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS (SEE  PFTABD  IN  SPSNVWORKD)                         
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* PF12 KEY DRIVER                                                               
         DC    AL1(PF12X-*,12,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
***********************************************************************         
* SPECIAL PFKEY TABLE DEFINITIONS                                               
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* GOTO TO LIST IF CALLSP=0 FOR PF12                                             
         DC    AL1(SPF12X-*,12,0,(SPF12X-SPF12)/KEYLNQ,0)                       
         DC    CL3' ',CL8'INVOICE',CL8'LIST'                                    
SPF12    DC    AL1(KEYTYTWA,L'NETMED-1),AL2(NETMED-T210FFD)                     
         DC    AL1(KEYTYTWA,L'NETCLT-1),AL2(NETCLT-T210FFD)                     
         DC    AL1(KEYTYTWA,L'NETSTA-1),AL2(NETSTA-T210FFD)                     
SPF12X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF24X-*,24,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
SPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPSNVWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENSNV          (INVOICE DSECT)                              
         EJECT                                                                  
       ++INCLUDE SPSNVFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSNVFBD          (OUR MAINT SCREEN)                           
         EJECT                                                                  
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* DDGLOBEQUS                                                                    
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDMINBLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
VGLOBBER DS    A                   A(GLOBBER)                                   
*                                                                               
SVDDMCB  DS    6F                  SAVED PARAMETER LIST                         
*                                                                               
PROF0I2  DS    CL16                I2 PROFILE                                   
P0I2BC   EQU   PROF0I2+9           BROADCAST/CALENDAR MONTHS                    
*                                                                               
PROF0A0A DS    CL16                A0A PROFILE                                  
P0A0APAY EQU   PROF0A0A+14         NO PAID INVOICE CHA/DEL/MOVE                 
*                                                                               
MISCFLG1 DS    X                                                                
MF1KYCHG EQU   X'80'               KEY CHANGED, BUILD THE SCREEN                
*                                                                               
BRDDATES DS    CL12                BROADCAST DATES                              
BRDCSDAT DS    XL2                           COMPRESSED START DATE              
BRDCEDAT DS    XL2                           COMPRESSED END DATE                
*                                                                               
PERDSYMD DS    CL6                 PERIOD'S START DATE YYMMDD                   
PERDEYMD DS    CL6                          END   DATE YYMMDD                   
*                                                                               
NXTUPKEY DS    XL(L'SNVKMINK)      LAST DETAIL/UPDATE KEY                       
PERVALST DS    XL56                PERVAL STORAGE                               
TWAELEM  DS    XL85                TWA BUILD ELEMENT                            
*                                                                               
MELEM2   DS    XL(L'MELEM)         SECONDARY MINIO ELEMENT                      
SVDMELEM DS    XL(L'MELEM)         SAVED MINIO ELEMENT                          
PROFI2Z  DS    CL16                I2z PROFILE                                  
SVMEDIA  DS    C                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPSNV04   06/07/16'                                      
         END                                                                    
