*          DATA SET SP131      AT LEVEL 075 AS OF 10/04/11                      
*PHASE SP131A                                                                   
*INCLUDE SPLDDCNT                                                               
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT110                                                               
*INCLUDE PRINT                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'SP131 - SPOT/TRF/UNT RECOVERY FILE DUMP'                        
********************************************************************            
* THIS MODULE READS THE DISK RECOVERY FILE AND GENERATES TWO TAPES.*            
*                                                                  *            
* TAPES HAVE ALL SPOTPAK RECORDS EXCEPT POINTER COPIES AND CHANGES.*            
*        (THESE RECORDS HAVE X'80' ON IN RRECTYPE)                 *            
********************************************************************            
         PRINT NOGEN                                                            
SP131    CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,*SP131*,=A(WORK)                                               
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE+6(25),=CL25' RECOVERY FILE DUMP'                           
*                                                                               
         MVI   FLAGS,0                                                          
*                                                                               
RDCARD   DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'/*',CARD                                                      
         BE    RC40                                                             
*                                                                               
         CLI   CARD,C'*'           TEST IGNORE                                  
         BE    RDCARD                                                           
*                                                                               
RC1      CLC   =C'DSPACE=',CARD                                                 
         BNE   RC2                                                              
         L     RE,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RE),CARD+7                                     
         B     RDCARD                                                           
*                                                                               
RC2      CLC   =C'DDSIO=',CARD                                                  
         BNE   RC4                                                              
         L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6                                                   
         B     RDCARD                                                           
*                                                                               
RC4      CLC   =C'UPDID=',CARD                                                  
         BNE   RC6                                                              
         GOTO1 =V(DATAMGR),DMCB,=C'UPDID'                                       
         L     RE,12(R1)                                                        
         MVC   0(2,RE),CARD+6                                                   
         B     RDCARD                                                           
*                                                                               
RC6      CLC   =C'SPOT',CARD                                                    
         BE    RC20                                                             
*                                                                               
         CLC   =C'NET',CARD                                                     
         BE    RC22                                                             
*                                                                               
         CLC   =C'TRF',CARD                                                     
         BE    RC24                                                             
         B     RC26                                                             
*                                                                               
RC20     MVC   TITLE(6),CARD       SPOTX                                        
         MVI   DUMPSYS,X'02'       SET SYSTEM TYPE                              
         MVC   DUMPFIL,CARD+4                                                   
         MVC   SESNAM+4(3),=C'SPT'                                              
         MVC   SESNAM+7(2),CARD+4                                               
         MVI   RCVFLNUM,QSPTRCV    SET SPOT RECOVERY FILE NUMBER                
         BAS   RE,GETSE                                                         
         B     RDCARD                                                           
*                                                                               
RC22     MVC   TITLE(5),CARD       NETX                                         
         MVI   DUMPSYS,X'03'       SET SYSTEM TYPE                              
         MVC   DUMPFIL,CARD+3                                                   
         MVC   SESNAM+4(3),=C'NET'                                              
         MVC   SESNAM+7(2),CARD+3                                               
         MVI   RCVFLNUM,QSPTRCV    SET RECOVERY FILE NUMBER (NET=SPOT)          
         BAS   RE,GETSE                                                         
         B     RDCARD                                                           
*                                                                               
RC24     MVC   TITLE(5),CARD       TRFX                                         
         MVI   DUMPSYS,X'0D'       SET SYSTEM TYPE                              
         MVC   DUMPFIL,CARD+3                                                   
         MVC   SESNAM+4(3),=C'STR'                                              
         MVC   SESNAM+7(2),CARD+3                                               
         MVI   RCVFLNUM,QTRFRCV    SET TRF RECOVERY FILE NUMBER                 
         BAS   RE,GETSE                                                         
         B     RDCARD                                                           
         EJECT                                                                  
RC26     DS    0H                                                               
         CLC   =C'ERASE=',CARD                                                  
         BNE   RC28                                                             
*                                                                               
         OI    FLAGS,FL_ERASE                                                   
         CLC   =C'YES',CARD+6                                                   
         BE    RDCARD                                                           
         CLC   =C'NO',CARD+6                                                    
         BNE   CRDERR                                                           
         MVI   ERASESW,C'N'                                                     
         MVI   FLIST,C' '          DON'T OPEN RECOVERY FOR UPDATE               
         MVI   TLIST,C' '          DON'T OPEN RECOVERY FOR UPDATE               
         B     RDCARD                                                           
*                                                                               
RC28     DS    0H                                                               
         CLC   =C'FIRST=',CARD                                                  
         BNE   RC30                                                             
         GOTO1 =V(HEXIN),DMCB,CARD+6,DA,8                                       
         B     RDCARD                                                           
*                                                                               
RC30     DS    0H                                                               
         B     CRDERR                                                           
*                                                                               
RC40     DS    0H                                                               
         L     RE,=A(UTL)          MAKE SURE A SENUM IS SET!                    
         CLI   4(RE),0                                                          
         BNH   SYSERR                                                           
*                                                                               
         TM    FLAGS,FL_ERASE      MAKE SURE AN ERASE= CARD PASSED              
         BZ    ERRERASE                                                         
*                                                                               
         B     OPEN                                                             
         EJECT                                                                  
***********************************************************************         
* GET SE NUMBER FOR SPOT/NET/TRF FILE                                 *         
***********************************************************************         
         SPACE 1                                                                
GETSE    NTR1  ,                                                                
         GOTO1 =V(DMDDNAME),DMCB,(X'24',=C'DDNAME'),SESNAM,0                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING DDNADATA,RF                                                      
         L     RF,8(,R1)           GET A(FILE INFO LIST)                        
         MVC   SESNUM,DDNASENO     EXTRACT SENUM                                
         MVC   SVDDNAME,DDNADDN                                                 
         DROP  RF                                                               
                                                                                
         L     RE,=A(UTL)                                                       
         MVC   4(1,RE),SESNUM                                                   
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* OPEN RECOVERY FILE *                                                          
***********************************************************************         
         SPACE 1                                                                
OPEN     LA    R2,=C'SPOT'                                                      
         LA    R3,FLIST                                                         
         CLI   TITLE,C'S'          THIS A SPOT RECV DUMP                        
         BE    OPEN10                                                           
         CLI   TITLE,C'N'          OR A NET RECV DUMP                           
         BE    OPEN10                                                           
         LA    R2,=C'STR'                                                       
         LA    R3,TLIST                                                         
         CLI   TITLE,C'T'          THIS A TRF RECV DUMP                         
         BE    OPEN10                                                           
         DC    H'0'                                                             
OPEN10   DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',(R2),(R3),REC                        
                                                                                
*==================================================================             
* IF ERASING FILE, CHECK FOR CONCURRENT FILE UPDATE NOW                         
*==================================================================             
                                                                                
         CLI   ERASESW,C'N'                                                     
         BE    OPEN12                                                           
*                                                                               
         LA    R2,DMCB             POINT TO DMCB FOR ERRORS                     
         GOTO1 =V(DMOD000),DMODPARS,DMODUEXT,REC,0,(RCVFLNUM,0)                 
*                                                                               
OPEN12   OPEN  (RCVTAPE,(OUTPUT),RCVCOPY,(OUTPUT))                              
         SPACE                                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD FILE OPEN                                
*                                                                               
         B     GET2                                                             
*                                                                               
FLIST    DC    CL8'URECV   '                                                    
         DC    CL8'X       '                                                    
*                                                                               
TLIST    DC    CL8'UTRFRCV '                                                    
         DC    CL8'X       '                                                    
*                                                                               
GET2     GOTO1 =V(DATAMGR),DMCB,(X'11',=C'DMRSEQ'),=C'RECOVER',        X        
               DA,REC,A(BUFF)                                                   
         TM    8(R1),X'40'         TEST ERROR                                   
         BNZ   GET1X                                                            
         TM    8(R1),X'80'         TEST EOF                                     
         BNZ   END1                                                             
         AP    TRECSIN+14(4),=P'1'                                              
*                                                                               
GET4     LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         CLC   RFILTY(2),0(R3)     MATCH FILE/REC                               
         BE    GET6                                                             
         BXLE  R3,R4,*-10                                                       
         B     GET8                                                             
*                                                                               
GET6     AP    14(4,R3),=P'1'                                                   
*                                                                               
GET8     TM    RRECTY,X'80'        SKIP POINTER COPIES/CHANGES                  
         BNZ   GET2                                                             
         CLI   RSIN,X'FF'          SKIP DELETED RECORDS                         
         BE    GET2                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,DM5+2          GET REC LEN                                  
         C     RE,=F'6024'         5976+24+4+24                                 
         BNH   GET10                                                            
         AP    TREC2BIG+14(4),=P'1'                                             
         MVC   P(11),=C'REC TOO BIG'                                            
         GOTO1 =V(HEXOUT),DMCB,REC,P+12,24,=C'TOG' RECOVERY HEADER              
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,REC+24,P+12,36                                   
         GOTO1 =V(PRINTER)                                                      
         B     GET2                                                             
*                                                                               
* WRITE RECORDS TO TAPE1 AND TAPE 2*                                            
*                                                                               
GET10    LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,RECLEN                                                        
         LA    R0,RECLEN                                                        
         L     R1,=A(RCVTAPE)                                                   
         PUT   (1),(0)                                                          
         AP    TRECS1+14(4),=P'1'                                               
*                                                                               
         LA    R0,RECLEN                                                        
         L     R1,=A(RCVCOPY)                                                   
         PUT   (1),(0)                                                          
*                                                                               
         CLI   RFILTY,QSPTFIL      TEST SPTFILE                                 
         BNE   *+14                                                             
         AP    TRECS2SP+14(4),=P'1'                                             
         B     GET2                                                             
*                                                                               
         CLI   RFILTY,QXSPFIL      TEST XSPFILE                                 
         BNE   *+14                                                             
         AP    TRECS2SX+14(4),=P'1'                                             
         B     GET2                                                             
*                                                                               
         CLI   RFILTY,QSTAFIL      TEST STATION                                 
         BNE   *+14                                                             
         AP    TRECS2ST+14(4),=P'1'                                             
         B     GET2                                                             
*                                                                               
         CLI   RFILTY,QTRFFIL      TEST TRFFILE                                 
         BNE   *+14                                                             
         AP    TRECS2TR+14(4),=P'1'                                             
         B     GET2                                                             
*                                                                               
         CLI   RFILTY,QUNTFIL      TEST UNTFIL                                  
         BNE   *+10                                                             
         AP    TRECS2UN+14(4),=P'1'                                             
*                                                                               
         CLI   RFILTY,QUNTDIR      TEST UNTDIR                                  
         BNE   *+10                                                             
         AP    TRECS2UD+14(4),=P'1'                                             
*                                                                               
         CLI   RFILTY,QSPTDIR      TEST SPTDIR                                  
         BNE   GET2                                                             
         CLI   RRECTY,QADD                                                      
         BNE   GET2                                                             
* TABULATE SPTDIR ADDS                                                          
         GOTO1 =V(LDCOUNT),DMCB,(1,REC+24),=C'RCVDUMP'                          
         B     GET2                                                             
         SPACE 2                                                                
GET1X    AP    ERRORS,=P'1'                                                     
         SPACE 1                                                                
* BUMP DISK ADDRESS TO START OF NEXT TRACK *                                    
         SPACE 1                                                                
         LH    RE,DA                                                            
         LA    RE,1(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,DA                                                            
         B     GET2                                                             
         EJECT                                                                  
END1     CP    TRECSIN+14(4),=P'1' TEST ANY RECORDS                             
         BL    ENDX                                                             
*                                                                               
         CLOSE (RCVTAPE,,RCVCOPY,)                                              
         SPACE 2                                                                
         CP    ERRORS,=P'0'        CHECK FOR ERRORS                             
         BE    ENDX                NO - GO ON                                   
*                                                                               
         OI    ERRORS+3,X'0F'                                                   
         UNPK  ERRMSG(4),ERRORS                                                 
         GOTO1 =V(LOGIO),DMCB,(X'FF',1),(69,ERRMSG)                             
         B     ENDX                                                             
*                                                                               
ERRMSG   DC    CL30'0000 RECOVERY FILE READ ERRORS'                             
         DC    X'15'                                                            
         DC    CL37'********** FILE NOT ERASED **********'                      
         DC    X'15'                                                            
ERRORS   DC    PL4'0'                                                           
         EJECT                                                                  
ENDX     MVC   P(5),CARD                                                        
         MVC   P+6(24),=C'RECOVERY RECORD COUNTERS'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         SPACE 1                                                                
* PRINT COUNTERS *                                                              
         SPACE 1                                                                
         LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         LA    R5,TCTRSX-1         SET TO INCLUDE TOTALS                        
         LA    R3,6(R3)                                                         
*                                                                               
PRTCNT   MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         MVC   P(12),2(R3)                                                      
         OI    17(R3),X'0F'                                                     
         UNPK  P+13(6),14(4,R3)                                                 
         GOTO1 =V(PRINTER)                                                      
         BXLE  R3,R4,PRTCNT                                                     
*                                                                               
         GOTO1 =V(LDCOUNT),DMCB,(X'FF',REC),=C'RCVDUMP'                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DO NOT ERASE RECOVERY FILE IF READ ERRORS OCCURED *                           
***********************************************************************         
ERRCHK   CP    ERRORS,=P'0'                                                     
         BNE   *+12                                                             
         CLI   ERASESW,C'Y'                                                     
         BE    ERASE                                                            
         MVC   P(32),=C'RECOVERY FILE *** NOT *** ERASED'                       
         GOTO1 =V(PRINTER)                                                      
         B     EOJ                                                              
                                                                                
*==================================================================             
* GET RECOVERY FILE DTF ADDRESS                                                 
*==================================================================             
                                                                                
ERASE    GOTO1 =V(DMOD000),ERSPARS,A(DMEXT),,,(RCVFLNUM,0)                      
                                                                                
*==================================================================             
* CALL DADDS TO ERASE FILE *                                                    
*==================================================================             
                                                                                
         GOTO1 =V(DADDS),ERSPARS,A(WTERASE)                                     
         MVC   HALF,8(R1)                                                       
         NI    HALF+1,X'FB'                                                     
         OC    HALF,HALF                                                        
         BZ    EROKAY                                                           
         MVC   P(29),=C'ERASE ERROR ON RECOVERY FILE='                          
         MVC   P+29(8),SVDDNAME                                                 
         GOTO1 =V(HEXOUT),DMCB,HALF,P+38,2,=C'TOG'                              
         GOTO1 =V(LOGIO),DMCB,1,(42,P)                                          
         GOTO1 =V(PRINTER)                                                      
         ABEND 661                 FILE ERASE ERROR                             
                                                                                
EROKAY   MVC   P(20),=C'RECOVERY FILE ERASED'                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      XBASE                                                                  
*                                                                               
CRDERR   MVC   P(34),=C'SP131 ** INVALID PARAMETER CARD **'                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
         SPACE 2                                                                
*                                                                               
ERRERASE GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'ERASEMSG,ERASEMSG)                 
         ABEND 201                                                              
ERASEMSG DC    C'** SP131 ** WARNING - MISSING ERASE= CARD'                     
*                                                                               
SYSERR   MVC   SYSMSG+12(7),CARD   MOVE BAD SYSTEM NAME                         
         GOTO1 =V(LOGIO),DMCB,1,(L'SYSMSG,SYSMSG)                               
         ABEND 999                                                              
SYSMSG   DC    C'** SP131 ** XXXXXXX NOT A VALID SYSTEM - ABORT'                
         SPACE 2                                                                
         CNOP  2,4                                                              
CTRS     DC    H'18'                                                            
         DC    A(CTRSX-1)                                                       
         DC    AL1(QSPTDIR,QADD),CL12'SPTDIR  ADDS',PL4'0'                      
         DC    AL1(QSPTDIR,QCPY),CL12'SPTDIR  CPYS',PL4'0'                      
         DC    AL1(QSPTDIR,QCHG),CL12'SPTDIR  CHGS',PL4'0'                      
         DC    AL1(QXSPDIR,QADD),CL12'XSPDIR  ADDS',PL4'0'                      
         DC    AL1(QXSPDIR,QCPY),CL12'XSPDIR  CPYS',PL4'0'                      
         DC    AL1(QXSPDIR,QCHG),CL12'XSPDIR  CHGS',PL4'0'                      
         DC    AL1(QSPTFIL,QADD),CL12'SPTFILE ADDS',PL4'0'                      
         DC    AL1(QSPTFIL,QCPY),CL12'SPTFILE CPYS',PL4'0'                      
         DC    AL1(QSPTFIL,QCHG),CL12'SPTFILE CHGS',PL4'0'                      
         DC    AL1(QXSPFIL,QADD),CL12'XSPFILE ADDS',PL4'0'                      
         DC    AL1(QXSPFIL,QCPY),CL12'XSPFILE CPYS',PL4'0'                      
         DC    AL1(QXSPFIL,QCHG),CL12'XSPFILE CHGS',PL4'0'                      
         DC    AL1(QSPTFIL,QCPY+128),CL12'SPTF PTR CPY',PL4'0'                  
         DC    AL1(QSTAFIL,QADD),CL12'STATION ADDS',PL4'0'                      
         DC    AL1(QSTAFIL,QCPY),CL12'STATION CPYS',PL4'0'                      
         DC    AL1(QSTAFIL,QCHG),CL12'STATION CHGS',PL4'0'                      
         DC    AL1(QTRFFIL,QADD),CL12'TRFFILE ADDS',PL4'0'                      
         DC    AL1(QTRFFIL,QCPY),CL12'TRFFILE CPYS',PL4'0'                      
         DC    AL1(QTRFFIL,QCHG),CL12'TRFFILE CHGS',PL4'0'                      
         DC    AL1(QUNTDIR,QADD),CL12'UNTDIR ADDS ',PL4'0'                      
         DC    AL1(QUNTDIR,QCPY),CL12'UNTDIR CPYS ',PL4'0'                      
         DC    AL1(QUNTDIR,QCHG),CL12'UNTDIR CHGS ',PL4'0'                      
         DC    AL1(QUNTFIL,QADD),CL12'UNTFIL ADDS ',PL4'0'                      
         DC    AL1(QUNTFIL,QCPY),CL12'UNTFIL CPYS ',PL4'0'                      
         DC    AL1(QUNTFIL,QCHG),CL12'UNTFIL CHGS ',PL4'0'                      
CTRSX    EQU   *                                                                
*                                                                               
TRECSIN  DC    X'0000',CL12'RECOVERY IN ',PL4'0'                                
TRECS1   DC    X'0000',CL12'OUT1- TOTAL ',PL4'0'                                
TRECS2SP DC    X'0000',CL12'OUT2-SPTFIL ',PL4'0'                                
TRECS2SX DC    X'0000',CL12'OUT2-XSPFIL ',PL4'0'                                
TRECS2ST DC    X'0000',CL12'OUT2-STATION',PL4'0'                                
TRECS2TR DC    X'0000',CL12'OUT2-TRFFIL ',PL4'0'                                
TRECS2UN DC    X'0000',CL12'OUT2-UNTFIL ',PL4'0'                                
TRECS2UD DC    X'0000',CL12'OUT2-UNTDIR ',PL4'0'                                
TREC2BIG DC    X'0000',CL12'TOO LARGE   ',PL4'0'                                
TCTRSX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
DMCB     DS    6A                                                               
         ORG   *-24                                                             
DM1      DS    A                                                                
DM2      DS    A                                                                
DM3      DS    A                                                                
DM4      DS    A                                                                
DM5      DS    A                                                                
DM6      DS    A                                                                
*                                                                               
DMODPARS DS    6A                                                               
*                                                                               
DA       DC    F'0'                                                             
*                                                                               
HALF     DS    H                                                                
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
ERSPARS  DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(ERSPAR6)                                                       
ERSPAR6  DC    A(0)                ** ERASE WHOLE FILE  **                      
*                                                                               
         LTORG                                                                  
*                                                                               
FLAGS    DS    X                                                                
FL_ERASE EQU   X'80'               ERASE SWITCH SET                             
*                                                                               
ERASESW  DC    C'Y'                                                             
RCVFLNUM DC    X'00'                                                            
DUMPSYS  DC    X'00'                                                            
DUMPFIL  DC    CL2' '                                                           
SESNAM   DC    C'SYS=XXX##'                                                     
SESNUM   DC    X'00'                                                            
SVDDNAME DC    CL8' '                                                           
                                                                                
         EJECT                                                                  
RCVTAPE  DCB   DDNAME=OUT1,                                            X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=8200,                                             X        
               BLKSIZE=27648,                                          X        
               MACRF=PM                                                         
*                                                                               
RCVCOPY  DCB   DDNAME=OUT2,                                            X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=8200,                                             X        
               BLKSIZE=27648,                                          X        
               MACRF=PM                                                         
*                                                                               
RECLEN   DS    F                                                                
REC      DS    8000C                                                            
         DS    60000C              OVERFLOW BUFFER!                             
         ORG   REC                                                              
       ++INCLUDE DMRCVRHDR                                                      
         ORG                                                                    
*                                                                               
BUFF     DS    64000C              BUFFER FOR FULL TRACK READ                   
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'**UTL **'                                                    
UTL      DC    F'0',X'00',XL3'00',XL56'00'                                      
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      DC    X'0000FF',X'02',4X'00',CL8' ',32X'00',A(0),204X'00'              
         DS    0D                                                               
         DC    CL8'**WORK**'                                                    
WORK     DS    2000D                                                            
         EJECT                                                                  
*=====================================================*                         
* RECOVERY FILE RECORD AND ACTION EQUATES             *                         
*=====================================================*                         
         SPACE 1                                                                
QSPTFIL  EQU   X'21'                                                            
QSTAFIL  EQU   X'22'                                                            
QSPTDIR  EQU   X'23'                                                            
QSPTRCV  EQU   X'24'                                                            
QXSPDIR  EQU   X'36'                                                            
QXSPFIL  EQU   X'37'                                                            
*                                                                               
QREQUEST EQU   X'25'                                                            
QUNTDIR  EQU   X'27'                                                            
QUNTFIL  EQU   X'2A'                                                            
*                                                                               
QTRFFIL  EQU   X'32'                                                            
QTRFDIR  EQU   X'33'                                                            
QTRFRCV  EQU   X'34'                                                            
*                                                                               
QCPY     EQU   1                                                                
QCHG     EQU   2                                                                
QADD     EQU   3                                                                
*                                                                               
       ++INCLUDE DMGREQUS                                                       
* DDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
* DMDDNAMED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMDDNAMED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075SP131     10/04/11'                                      
         END                                                                    
